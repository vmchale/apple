-- | From Appel
module Asm.G ( build ) where

import           CF
import           Control.Monad.State.Strict (State)
import qualified Data.Array                 as A
import           Data.Copointed
import           Data.Graph                 (Bounds, Edge, Graph, Vertex, buildG)
import qualified Data.IntSet                as IS
import qualified Data.Set as S
import qualified Data.IntMap as IM
import Asm.Ar

-- same for xmm0, r15
-- k = 16

-- move list: map from abstract registers (def ∪ used) to nodes
type Movs = IM.IntMap IS.IntSet
type GS = S.Set (Int, Int)
type GL = IM.IntMap IS.IntSet

data St = St { mvs :: Movs, aS :: GS, aL :: GL, wlm :: IS.IntSet }

thread :: [a -> a] -> a -> a
thread = foldr (.) id

-- | To be called in reverse order, init with liveOut for the block
build :: (Arch (p (ControlAnn, NLiveness)), Copointed p) => IS.IntSet -> St -> [p (ControlAnn, NLiveness)] -> (IS.IntSet, St)
build l st [] = (l, st)
build l (St ml as al mv) (isn:isns) | isM isn =
    let ca = fst (copoint isn)
        nl = snd (copoint isn)
        u = usesNode ca
        d = defsNode ca
        lm = l IS.\\ u
        nIx = nx nl
        ml' = thread [ IM.alter (\k -> Just $ case k of {Nothing -> IS.singleton nIx; Just nxes -> IS.insert nIx nxes}) kϵ | kϵ <- IS.toList (u `IS.union` d) ] ml
        le = lm `IS.union` d
        es = thread [ S.insert (lϵ, dϵ) | lϵ <- IS.toList le, dϵ <- IS.toList d ] as
        l' = u `IS.union` (lm IS.\\ d)
        st' = St ml' es al (IS.insert nIx mv)
    in build l' st' isns
