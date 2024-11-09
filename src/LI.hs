-- live intervals
module LI ( intervals ) where

import           CF
import           Control.Monad.Trans.State.Strict (execState, get, put)
import           Data.Copointed
import           Data.Foldable                    (traverse_)
import qualified Data.IntMap.Lazy                 as IM
import qualified Data.IntSet                      as IS

collate :: IM.IntMap Int -> IM.IntMap IS.IntSet
collate = IM.unionsWith IS.union . fmap g . IM.toList where g (r, n) = IM.singleton n (IS.singleton r)

fpF is = snd $ execState (traverse_ g is) (IS.empty, IM.empty) where
    g x = do
        (previouslySeen, upd) <- get
        let ann = copoint x
            potentiallyNew = let lx = liveness ann in fins lx <> fout lx
            newS = potentiallyNew IS.\\ previouslySeen
            nAt = IM.fromList (fmap (,nx ann) (IS.toList newS))
        put (previouslySeen `IS.union` newS, nAt `IM.union` upd)

-- forward pass (first mentioned, indexed by register)
fpF, pF :: Copointed p => [p NLiveness] -> IM.IntMap Int
pF is = snd $ execState (traverse_ g is) (IS.empty, IM.empty) where
    g x = do
        (previouslySeen, upd) <- get
        let ann = copoint x
            potentiallyNew = let lx = liveness ann in ins lx <> out lx
            newS = potentiallyNew IS.\\ previouslySeen
            nAt = IM.fromList (fmap (,nx ann) (IS.toList newS))
        put (previouslySeen `IS.union` newS, nAt `IM.union` upd)

-- backward pass (last mentioned, ...)
pB, fpB :: Copointed p => [p NLiveness] -> IM.IntMap Int
pB = pF.reverse; fpB = fpF.reverse

intervals :: (Copointed p) => [p NLiveness] -> [p Live]
intervals asms = fmap (fmap lookupL) asms
    where lookupL x = let n = nx x in Live (lI n findFirst) (lI n findLast) (lI n findFirstF) (lI n findLastF)
          lI = IM.findWithDefault IS.empty
          findFirst = collate (pF asms); findLast = collate (pB asms)
          findFirstF = collate (fpF asms); findLastF = collate (fpB asms)
