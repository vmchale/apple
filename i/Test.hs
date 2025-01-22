module Main (main) where

import           System.IO.Silently (capture_)
import           Test.Tasty         (defaultMain, testGroup)

main = defaultMain $ testGroup "REPL" []
