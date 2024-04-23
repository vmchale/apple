{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           As
import           CGen
import           Control.Exception         (throwIO)
import qualified Data.ByteString.Lazy      as BSL
import           Data.Functor              (void)
import qualified Data.Text                 as T
import           P                         (getTy)
import           Prettyprinter.Render.Text (hPutDoc)
import           System.Directory          (getCurrentDirectory, setCurrentDirectory)
import           System.FilePath           ((</>))
import           System.IO                 (IOMode (WriteMode), withFile)
import           System.IO.Temp            (withSystemTempDirectory)
import           System.Process            (proc, readCreateProcess)
import           Test.Tasty                (TestTree, defaultMain, testGroup)
import           Test.Tasty.HUnit          (testCase, (@?=))

readCc :: FilePath -- ^ Apple source file
       -> T.Text
       -> IO String
readCc aSrc tyt = do
    pwd <- getCurrentDirectory
    withSystemTempDirectory "apple" $ \dir -> do
        setCurrentDirectory dir
        contents <- BSL.readFile (pwd </> aSrc)
        let n=T.unpack tyt
        t <- either throwIO (pure.fst) (getTy contents)
        ct <- either throwIO pure $ pCty tyt t
        writeO tyt contents True
        withFile (n <> ".h") WriteMode $ \h -> hPutDoc h ct
        let c = pwd </> "test/harness" </> n <> "_harness.c"
        void $ readCreateProcess (proc "cc" [n <> ".o", c, "-I", dir, "-I", pwd </> "include"]) ""
        readCreateProcess (proc (dir </> "a.out") []) ""

ccOut :: FilePath -> T.Text -> String -> TestTree
ccOut fp tyt expected = testCase (T.unpack tyt) $ do
    actual <- readCc fp tyt
    actual @?= expected

main :: IO ()
main = defaultMain $ testGroup "link object files" [ccOut "test/examples/shoelace.🍎" "aaf" "6.000000"]
