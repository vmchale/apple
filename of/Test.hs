{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Functor     (void)
import qualified Data.Text        as T
import           H
import           System.Directory (getCurrentDirectory, setCurrentDirectory)
import           System.FilePath  ((</>))
import           System.Info      (arch)
import           System.IO.Temp   (withSystemTempDirectory)
import           System.Process   (proc, readCreateProcess)
import           Test.Tasty       (DependencyType (AllFinish), TestTree, defaultMain, sequentialTestGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

readCc :: FilePath
       -> FilePath -- ^ Apple source file
       -> T.Text
       -> Arch
       -> IO String
readCc pwd aSrc tyt a =
    withSystemTempDirectory "apple" $ \dir -> do
        setCurrentDirectory dir
        let n=T.unpack tyt
        run (pwd </> aSrc, a, tyt)
        let c = pwd </> "test/harness" </> n <> "_harness.c"
        {-# SCC "cc" #-} void $ readCreateProcess (proc "cc" [n <> ".o", c, "-I", dir, "-I", pwd </> "include"]) ""
        {-# SCC "a.out" #-} readCreateProcess (proc (dir </> "a.out") []) ""

ccOut :: FilePath -> FilePath -> T.Text -> Arch -> String -> TestTree
ccOut pwd fp tyt a expected = testCase (T.unpack tyt) $ do
    actual <- readCc pwd fp tyt a
    actual @?= expected

main :: IO ()
main = do
    pwd <- getCurrentDirectory
    defaultMain $
        sequentialTestGroup "link object files" AllFinish
            [ ccOut pwd "test/examples/shoelace.🍎" "aaf" sys "6.000000"
            , ccOut pwd "test/data/predictionStep.🍏" "aafa" sys "1 4\n0.716413,0.721679,0.727807,0.731693\n"
            , ccOut pwd "test/data/map.🍏" "aaa" sys "2 2,2\n1.000000,2.000000,2.000000,2.000000\n"
            , ccOut pwd "test/data/maa.🍎" "aa" sys "1 2\n1.000000,3.000000\n"
            , ccOut pwd "test/data/bha.🍏" "bha" sys "1 2\n0.792800,0.663306\n"
            , ccOut pwd "test/data/mfa.🍎" "a" sys "2 4,2\n1.000000,1.000000,3.000000,3.000000,2.000000,2.000000,5.000000,5.000000\n"
            , ccOut pwd "test/data/sin.🍏" "ff" sys "-1.000000\n"
            , ccOut pwd "test/data/conv.🍏" "conv" sys "2 3,3\n9.000000,9.000000,9.000000,9.000000,9.000000,9.000000,9.000000,9.000000,9.000000\n"
            , ccOut pwd "math/hypergeometric.🍏" "hyper" sys "2.030078"
            , ccOut pwd "math/numbertheory/radical.🍎" "ii" sys "30"
            , ccOut pwd "math/numbertheory/factors.🍏" "ia" sys "1 3\n2,3,5\n"
            , ccOut pwd "test/data/primeMask.🍎" "ib" sys "1 10\n1,1,0,1,0,1,0,0,0,1\n"
            , ccOut pwd "test/examples/regress.🍎" "aaf2" sys "-0.950000 1.000000"
            ]
  where
    sys = case arch of {"x86_64" -> X64; "aarch64" -> Aarch64}
