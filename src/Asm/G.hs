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


-- move list: map from abstract registers (def ∪ used) to nodes
type Movs = IM.IntMap IS.IntSet
type GS = S.Set (Int, Int)
type GL = IM.IntMap IS.IntSet

data Wk = Wk { sp :: IS.IntSet, fr :: IS.IntSet, simp :: IS.IntSet }

data St = St { mvs :: Movs, aS :: GS, aL :: GL, wlm :: IS.IntSet, degs :: IM.IntMap Int, initial :: IS.IntSet, wkls :: Wk }

thread :: [a -> a] -> a -> a
thread = foldr (.) id

(@!) :: IM.Key -> Int -> GL -> GL
(@!) k i = IM.alter (\kϵ -> Just$case kϵ of {Nothing -> IS.singleton i; Just is -> IS.insert i is}) k

-- | To be called in reverse order, init with liveOut for the block
build :: (Arch (p (ControlAnn, NLiveness)), Copointed p) => IS.IntSet -> St -> [p (ControlAnn, NLiveness)] -> (IS.IntSet, St)
build l st [] = (l, st)
build l (St ml as al mv ds i wk) (isn:isns) | isM isn =
    let ca = fst (copoint isn)
        nl = snd (copoint isn)
        u = usesNode ca
        d = defsNode ca
        lm = l IS.\\ u
        nIx = nx nl
        ml' = thread [ kϵ @! nIx | kϵ <- IS.toList (u `IS.union` d) ] ml
        le = lm `IS.union` d
        es = thread [ S.insert (lϵ, dϵ) | lϵ <- IS.toList le, dϵ <- IS.toList d ] as
        l' = u `IS.union` (lm IS.\\ d)
        st' = St ml' es al (IS.insert nIx mv) ds i wk
    in build l' st' isns
                                  | otherwise =
    let ca = fst (copoint isn)
        u = usesNode ca
        d = defsNode ca
        le = l `IS.union` d
        es = thread [ S.insert (lϵ, dϵ) | lϵ <- IS.toList le, dϵ <- IS.toList d ] as
        l' = u `IS.union` (l IS.\\ d)
        st' = St ml es al mv ds i wk
    in build l' st' isns

precoloredS :: IS.IntSet
precoloredS = undefined

addEdge :: Int -> Int -> St -> St
addEdge u v st@(St ml as al mv ds i wk) =
    if (u, v) `S.notMember` as && u /= v
        then
            let as' = as `S.union` S.fromList [(u,v), (v, u)]
                uC = u `IS.notMember` precoloredS
                vC = v `IS.notMember` precoloredS
                al' = (if uC then u @! v else id)$(if vC then v @! u else id) al
                idg = IM.alter (\k -> Just$case k of {Nothing -> 1; Just d -> d+1})
                ds' = (if uC then idg u else id)$(if vC then idg v else id) ds
            in St ml as' al' mv ds' i wk
        else st

-- FIXME: initial is a NODE of instructions aaah
mkWorklist :: St -> St
mkWorklist st@(St _ _ _ _ ds i wk) =
    let i' = IS.empty
        wk' = thread [ case () of { _ | ds IM.! n >= k -> (\w -> w { sp = IS.insert n (sp w) })} | n <- IS.toList i ] wk
    in st { initial = i', wkls = wk' }
    -- same for xmm0, r15
    where k = 16
