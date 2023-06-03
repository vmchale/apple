module Asm.CF ( FreshM
              , runFreshM
              , getFresh
              , fm
              , lookupLabel
              , lC
              , broadcast
              , b3
              , singleton
              , fromList
              ) where

import           Asm.M
import           Class.E                    as E
import           Control.Monad.State.Strict (State, evalState, gets, modify)
import           Data.Functor               (($>))
import qualified Data.IntSet                as IS
import qualified Data.Map                   as M
import           Data.Tuple.Extra           (first3, fst3, second3, snd3, thd3, third3)

-- map of labels by node
type FreshM = State (Int, M.Map Label Int, M.Map Label [Int])

runFreshM :: FreshM a -> a
runFreshM = flip evalState (0, mempty, mempty)

getFresh :: FreshM Int
getFresh = gets fst3 <* modify (first3 (+1))

fm :: Label -> FreshM Int
fm l = do {st <- gets snd3; case M.lookup l st of {Just i -> pure i; Nothing -> do {i <- getFresh; broadcast i l $> i}}}

lookupLabel :: Label -> FreshM Int
lookupLabel l = gets (M.findWithDefault (error "Internal error in control-flow graph: node label not in map.") l . snd3)

lC :: Label -> FreshM [Int]
lC l = gets (M.findWithDefault (error "Internal error in CF graph: node label not in map.") l . thd3)

broadcast :: Int -> Label -> FreshM ()
broadcast i l = modify (second3 (M.insert l i))

b3 :: Int -> Label -> FreshM ()
b3 i l = modify (third3 (M.alter (\k -> Just$case k of {Nothing -> [i]; Just is -> i:is}) l))

singleton :: E reg => reg -> IS.IntSet
singleton = IS.singleton . E.toInt

fromList :: E reg => [reg] -> IS.IntSet
fromList = foldMap singleton
