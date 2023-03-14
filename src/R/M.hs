{-# LANGUAGE OverloadedStrings #-}

module R.M ( RM
           , runR
           , nextN
           , nextU
           ) where

import           Control.Monad.State.Strict (State, get, modify, runState)
import           Data.Functor               (($>))
import qualified Data.Text                  as T
import           Nm
import           U

type RM = State Int

nextU :: T.Text -> a -> RM (Nm a)
nextU n l = do { i <- get; modify (+1) $> Nm n (U$i+1) l }

nextN :: a -> RM (Nm a)
nextN = nextU "x"

runR :: Int -> RM x -> (x, Int)
runR = flip runState
