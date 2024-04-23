{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Data.Functor     (void)
import qualified Data.Text        as T
import           H
import           System.Directory (getCurrentDirectory, setCurrentDirectory)
import           System.FilePath  ((</>))
import           System.IO.Temp   (withSystemTempDirectory)
import           System.Process   (proc, readCreateProcess)
import           Test.Tasty       (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit (testCase, (@?=))

readCc :: FilePath -- ^ Apple source file
       -> T.Text
       -> Arch
       -> IO String
readCc aSrc tyt arch = do
    pwd <- getCurrentDirectory
    withSystemTempDirectory "apple" $ \dir -> do
        setCurrentDirectory dir
        let n=T.unpack tyt
        run (pwd </> aSrc, arch, tyt)
        let c = pwd </> "test/harness" </> n <> "_harness.c"
        {-# SCC "cc" #-} void $ readCreateProcess (proc "cc" [n <> ".o", c, "-I", dir, "-I", pwd </> "include"]) ""
        {-# SCC "a.out" #-} readCreateProcess (proc (dir </> "a.out") []) ""

ccOut :: FilePath -> T.Text -> Arch -> String -> TestTree
ccOut fp tyt arch expected = testCase (T.unpack tyt) $ do
    actual <- readCc fp tyt arch
    actual @?= expected

main :: IO ()
main = defaultMain $ testGroup "link object files" [ccOut "test/examples/shoelace.🍎" "aaf" Aarch64 "6.000000"]
