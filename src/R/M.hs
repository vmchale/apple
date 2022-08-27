{-# LANGUAGE OverloadedStrings #-}

module R.M ( RM
           , runR
           , nextN
           , nextU
           ) where

import           Control.Monad.State.Strict (State, get, modify, runState)
import           Data.Functor               (($>))
import qualified Data.Text                  as T
import           Name
import           U

type RM = State Int

nextU :: T.Text -> a -> RM (Name a)
nextU n l = do { i <- get; modify (+1) $> Name n (U$i+1) l }

nextN :: a -> RM (Name a)
nextN = nextU "x"

runR :: Int -> RM x -> (x, Int)
runR = flip runState
