module Main (main) where

import           Control.Monad.Trans.State.Strict (evalStateT)
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BSL
import           REPL
import           System.Console.Haskeline         (defaultSettings, runInputTBehavior, useFile)
import           System.IO                        (hFlush)
import           System.IO.Temp                   (withSystemTempFile)
import           Test.Tasty                       (defaultMain, testGroup)
import           Test.Tasty.Golden                (goldenVsString)

main = defaultMain $ testGroup "REPL"
  [ goldenVsString "pythagorean means" "i/golden/pythagoreanMeans.out" (testRepl "i/ex/pythagoreanMeans.🍏")
  ]

testRepl :: FilePath -> IO BSL.ByteString
testRepl fp = withSystemTempFile "REPL" $ \_ h -> do
    st <- iSt h
    flip evalStateT st $ runInputTBehavior (useFile fp) defaultSettings loop
    hFlush h
    BSL.fromStrict <$> BS.hGetContents h
