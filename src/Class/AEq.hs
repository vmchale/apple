module Class.AEq ( AEq (..), derivEq ) where

import           Control.Monad.State.Strict (State, evalState)
import qualified Data.IntMap                as IM
import           Name

type EqM = State (IM.IntMap (TyName ()))

runEqM = flip evalState IM.empty
derivEq x x' = runEqM $ eqSt x x'

class AEq a where
    eqSt :: a -> a -> EqM Bool
