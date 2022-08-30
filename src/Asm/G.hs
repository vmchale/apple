module Asm.G ( mG ) where

import           CF
import           Control.Monad.State.Strict (State)
import qualified Data.Array                 as A
import           Data.Copointed
import           Data.Graph                 (Bounds, Edge, Graph, Vertex, buildG)
import qualified Data.IntSet                as IS
import qualified Data.IntMap as IM

-- same for xmm0, r15
k = 16

data St

-- move list: map from abstract registers (def ∪ used) to nodes
type Movs = IM.IntMap IS.IntSet
type GS = S.Set (Int, Int)
type GL = IM.IntMap IS.IntSet -- Array IS.IntSet?

type M = State St

-- make worklists (simplify, freeze, coalescedNodes,

simplify :: Graph -> (Graph, [Vertex])
simplify x = (xϵ,stack) where
    dG = deg x
    toPrune v = fmap (<k) dG A.! v
    stack = [ v | (v,d) <- A.assocs dG, d>k ]
    es = fmap (filter (not.toPrune)) x
    xϵ = A.array (A.bounds x) [ y | y@(v,_) <- A.assocs es, not (toPrune v) ]

deg :: Graph -> A.Array Vertex Int
deg = fmap length

mG :: Copointed p => [p Liveness] -> Graph
mG asms = buildG bounds (concatMap (ls.copoint) asms)
    where bounds = (minimum mins, maximum maxs) where (mins, maxs) = unzip (fmap (boundLiveness.copoint) asms)
    -- TODO: EdgeSet

maxM :: IS.IntSet -> IS.Key
maxM is | IS.null is = minBound
        | otherwise = IS.findMax is

minM :: IS.IntSet -> IS.Key
minM is | IS.null is = maxBound
        | otherwise = IS.findMin is

ls :: Liveness -> [Edge]
ls (Liveness is os _ _) = cross (IS.toList is) (IS.toList os)

-- FIXME: handle float vs. int registers as separate graphs
boundLiveness :: Liveness -> Bounds
boundLiveness (Liveness is os _ _) = let vs = is `IS.union` os in (minM vs, maxM vs)

cross :: [a] -> [b] -> [(a,b)]
cross xs ys = (,) <$> xs <*> ys
