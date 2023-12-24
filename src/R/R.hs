module R.R ( RM
           , nextU
           , runM
           , module R
           ) where

import           Control.Monad.State.Strict (State, gets, runState)
import           Data.Bifunctor             (second)
import           Data.Functor               (($>))
import qualified Data.Text                  as T
import           Lens.Micro.Mtl             (modifying)
import           Nm
import           R
import           U

type RM = State Rs

nextU :: T.Text -> a -> RM (Nm a)
nextU n l = do {i <- gets max_; modifying maxLens (+1) $> Nm n (U$i+1) l }

runM :: Int -> RM a -> (a, Int)
runM i = second max_ . flip runState (Rs i mempty)
