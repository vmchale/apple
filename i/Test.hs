module Main (main) where

import           Control.Monad.Trans.State.Strict (evalStateT)
import qualified Data.ByteString.Lazy             as BSL
import           REPL
import           System.Console.Haskeline         (defaultSettings, runInputTBehavior, useFile)
import           System.IO                        (hClose)
import           System.IO.Silently               (silence)
import           System.IO.Temp                   (withSystemTempFile)
import           Test.Tasty                       (defaultMain, testGroup)
import           Test.Tasty.Golden                (goldenVsString)

main = defaultMain $ testGroup "REPL"
  [ goldenVsString "pythagorean means" "i/golden/pythagoreanMeans.out" (testRepl "i/ex/pythagoreanMeans.🍏")
  , goldenVsString "median" "i/golden/median.out" (testRepl "i/ex/median.🍏")
  , goldenVsString "tutorial" "i/golden/doc.out" (testRepl "i/ex/doc.🍎")
  ]

testRepl :: FilePath -> IO BSL.ByteString
testRepl fp = withSystemTempFile "REPL" $ \t h -> do
    st <- iSt h
    silence $ flip evalStateT st $ runInputTBehavior (useFile fp) defaultSettings loop
    hClose h *> BSL.readFile t
