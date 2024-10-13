module Asm.CF ( N, FreshM
              , runFreshM
              , getFresh, fm
              , lookupLabel, lC
              , broadcast
              , b3
              , insert
              , singleton
              , fromList
              ) where

import           Asm.M
import           Class.E                          as E
import           Control.Monad.Trans.State.Strict (State, evalState, gets, modify, state)
import           Data.Functor                     (($>))
import qualified Data.IntSet                      as IS
import qualified Data.Map                         as M
import           Data.Tuple.Extra                 (second3, snd3, thd3, third3)

type N=Int

-- map of labels by node
type FreshM = State (N, M.Map Label N, M.Map Label [N])

runFreshM :: FreshM a -> a
runFreshM = flip evalState (0, mempty, mempty)

getFresh :: FreshM N
getFresh = state (\(i,m0,m1) -> (i,(i+1,m0,m1)))

fm :: Label -> FreshM N
fm l = do {st <- gets snd3; case M.lookup l st of {Just i -> pure i; Nothing -> do {i <- getFresh; broadcast i l $> i}}}

lookupLabel :: Label -> FreshM N
lookupLabel l = gets (M.findWithDefault (error "Internal error in control-flow graph: node label not in map.") l . snd3)

lC :: Label -> FreshM [N]
lC l = gets (M.findWithDefault (error "Internal error in CF graph: node label not in map.") l . thd3)

broadcast :: N -> Label -> FreshM ()
broadcast i l = modify (second3 (M.insert l i))

b3 :: N -> Label -> FreshM ()
b3 i l = modify (third3 (M.alter (\k -> Just$case k of {Nothing -> [i]; Just is -> i:is}) l))

singleton :: E reg => reg -> IS.IntSet
singleton = IS.singleton . E.toInt

insert r = IS.insert (E.toInt r)

fromList :: E reg => [reg] -> IS.IntSet
fromList = foldMap singleton
