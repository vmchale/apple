{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Functor     (void)
import qualified Data.Text        as T
import           H
import           System.Directory (getCurrentDirectory, setCurrentDirectory)
import           System.FilePath  ((</>))
import           System.IO.Temp   (withSystemTempDirectory)
import           System.Process   (proc, readCreateProcess)
import           Test.Tasty       (DependencyType (AllFinish), TestTree, defaultMain, sequentialTestGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

readCc :: FilePath
       -> FilePath -- ^ Apple source file
       -> T.Text
       -> Arch
       -> IO String
readCc pwd aSrc tyt arch =
    withSystemTempDirectory "apple" $ \dir -> do
        setCurrentDirectory dir
        let n=T.unpack tyt
        run (pwd </> aSrc, arch, tyt)
        let c = pwd </> "test/harness" </> n <> "_harness.c"
        {-# SCC "cc" #-} void $ readCreateProcess ((proc "cc" [n <> ".o", c, "-I", dir, "-I", pwd </> "include"])) ""
        {-# SCC "a.out" #-} readCreateProcess (proc (dir </> "a.out") []) ""

ccOut :: FilePath -> FilePath -> T.Text -> Arch -> String -> TestTree
ccOut pwd fp tyt arch expected = testCase (T.unpack tyt) $ do
    actual <- readCc pwd fp tyt arch
    actual @?= expected

main :: IO ()
main = do
    pwd <- getCurrentDirectory
    defaultMain $
        sequentialTestGroup "link object files" AllFinish
            [ ccOut pwd "test/examples/shoelace.🍎" "aaf" Aarch64 "6.000000"
            , ccOut pwd "test/data/predictionStep.🍏" "aafa" Aarch64 "1 4\n0.716413,0.721679,0.727807,0.731693\n"
            , ccOut pwd "test/data/map.🍏" "aaa" Aarch64 "2 2,2\n1.000000,2.000000,2.000000,2.000000\n"
            , ccOut pwd "test/data/maa.🍎" "aa" Aarch64 "1 2\n1.000000,3.000000\n"
            , ccOut pwd "test/data/mfa.🍎" "a" Aarch64 "2 4,2\n1.000000,1.000000,3.000000,3.000000,2.000000,2.000000,5.000000,5.000000\n"
            ]
