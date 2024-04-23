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
       -> IO String
readCc aSrc tyt = do
    pwd <- getCurrentDirectory
    withSystemTempDirectory "apple" $ \dir -> do
        setCurrentDirectory dir
        let n=T.unpack tyt
        run (pwd </> aSrc, Aarch64, tyt)
        let c = pwd </> "test/harness" </> n <> "_harness.c"
        void $ readCreateProcess (proc "cc" [n <> ".o", c, "-I", dir, "-I", pwd </> "include"]) ""
        readCreateProcess (proc (dir </> "a.out") []) ""

ccOut :: FilePath -> T.Text -> String -> TestTree
ccOut fp tyt expected = testCase (T.unpack tyt) $ do
    actual <- readCc fp tyt
    actual @?= expected

main :: IO ()
main = defaultMain $ testGroup "link object files" [ccOut "test/examples/shoelace.🍎" "aaf" "6.000000"]
