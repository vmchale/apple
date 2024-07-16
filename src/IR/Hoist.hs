module IR.Hoist ( loop
                , hoist
                ) where

import           Data.Bifunctor (first)
import           Data.Graph     (Tree (Node))
import qualified Data.IntMap    as IM
import           Dom
import           IR             (Stmt)
import           IR.CF

loop :: [Stmt] -> ([[N]], IM.IntMap Stmt)
loop = first cTree.hoist

hoist :: [Stmt] -> ([Tree N], IM.IntMap Stmt)
hoist = mkG.mkControlFlow

cTree :: [Tree N] -> [[N]]
cTree []              = []
cTree (Node _ []:ts)  = cTree ts
cTree (Node n [t]:ts) = (n:et t):cTree ts

et :: Tree N -> [N]
et (Node n [])  = [n]
et (Node n [t]) = n:et t
