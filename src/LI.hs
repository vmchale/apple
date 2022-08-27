{-# LANGUAGE FlexibleContexts #-}

-- live intervals
module LI ( intervals
          ) where

import           CF
import           Control.Monad.State.Strict (execState, get, put)
import           Data.Copointed
import           Data.Foldable              (traverse_)
import qualified Data.IntMap.Lazy           as IM
import qualified Data.IntSet                as IS
import           Data.Semigroup             ((<>))

-- {-# SCC collate #-}
collate :: IM.IntMap Int -> IM.IntMap IS.IntSet
collate = IM.unionsWith IS.union . fmap g . IM.toList where g (r, n) = IM.singleton n (IS.singleton r)

-- forward pass (first mentioned, indexed by register)
pF :: Copointed p => [p NLiveness] -> IM.IntMap Int
pF is = snd $ execState (traverse_ g is) (IS.empty, IM.empty) where
    g x = do
        (previouslySeen, upd) <- get
        let ann = copoint x
            potentiallyNew = let lx = liveness ann in ins lx <> out lx
            newS = potentiallyNew IS.\\ previouslySeen
            nAt = IM.fromList (zip (IS.toList newS) (repeat $ nx ann))
        put (previouslySeen `IS.union` newS, nAt `IM.union` upd)

-- backward pass (last mentioned, ...)
pB :: Copointed p => [p NLiveness] -> IM.IntMap Int
pB = pF.reverse

intervals :: (Copointed p, Functor p) => [p NLiveness] -> [p Interval]
intervals asms = fmap (fmap lookupL) asms
    where lookupL x = let n = nx x in Interval (lI n findFirst) (lI n findLast)
          lI = IM.findWithDefault IS.empty
          findFirst = collate (pF asms)
          findLast = collate (pB asms)
