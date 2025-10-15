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
  [ goldenRepl "pythagorean means" "i/golden/pythagoreanMeans.out" "i/ex/pythagoreanMeans.🍏"
  , goldenRepl "median" "i/golden/median.out" "i/ex/median.🍏"
  , goldenRepl "tutorial" "i/golden/doc.out" "i/ex/doc.🍎"
  , goldenRepl "var" "i/golden/var.out" "i/ex/var.🍏"
  , goldenRepl "rank" "i/golden/sum.out" "i/ex/sum.🍎"
  , goldenRepl "small" "i/golden/loop.out" "i/ex/loop.🍏"
  , goldenRepl "slowft" "i/golden/ft.out" "i/ex/ft.🍏"
  , goldenRepl "bwf" "i/golden/rot.out" "i/ex/rot.🍏"
  -- https://dl.acm.org/doi/pdf/10.1145/382109.382124
  , goldenRepl "fannkuch" "i/golden/fannkuch.out" "i/ex/fannkuch.🍏"
  , goldenRepl "shuffle" "i/golden/shuf.out" "i/ex/shuf.🍎"
  ]
  where
    goldenRepl str out src = goldenVsString str out (testRepl src)

testRepl :: FilePath -> IO BSL.ByteString
testRepl fp = withSystemTempFile "REPL" $ \t h -> do
    st <- iSt h
    silence $ flip evalStateT st $ runInputTBehavior (useFile fp) defaultSettings loop
    hClose h *> BSL.readFile t
