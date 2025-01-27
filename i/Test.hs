module Main (main) where

import           Control.Monad.Trans.State.Strict (evalStateT)
import qualified Data.ByteString.Lazy.Char8       as ASCIIL
import           REPL
import           System.Console.Haskeline         (defaultSettings, runInputTBehavior, useFile)
import           System.IO.Silently               (capture_)
import           Test.Tasty                       (defaultMain, testGroup)
import           Test.Tasty.Golden                (goldenVsString)

main = defaultMain $ testGroup "REPL"
  [ goldenVsString "pythagorean means" "i/golden/pythagoreanMeans.out" (ASCIIL.pack <$> testRepl "i/ex/pythagoreanMeans.🍏")
  ]

testRepl :: FilePath -> IO String
testRepl fp = capture_ $ do
    st <- iSt
    flip evalStateT st $ runInputTBehavior (useFile fp) defaultSettings loop
