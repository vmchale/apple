module IR.Hoist ( hoist, pall ) where

import           CF
import           Control.Composition (thread)
import           Data.Bifunctor      (first, second)
import           Data.Graph          (Tree (Node))
import qualified Data.IntMap         as IM
import qualified Data.IntSet         as IS
import           Data.Tuple.Extra    (snd3)
import           Dom
import           IR
import           IR.CF
import           LR

type Loop = (N, IS.IntSet)

lm :: [(Stmt, NLiveness)] -> IM.IntMap NLiveness
lm = IM.fromList.fmap (\(_,n) -> (nx n, n))

hl :: (Loop, IM.IntMap (Stmt, ControlAnn), IM.IntMap NLiveness) -> [(N, N, Stmt)]
hl ((n,ns), info, linfo) = go ss
  where
    liveInH = fins.liveness$gN n linfo
    go ((s@(MX x ConstF{}), a):ssϵ) | rToInt x `IS.notMember` liveInH && notFDef x (node a) = (n, node a, s):go ssϵ
    go (_:ssϵ)                      = go ssϵ
    go []                           = []
    otherDefFs nL = defsFNode.ud.snd.flip gN info<$>(IS.toList$IS.delete nL ns)
    notFDef r nL = not $ any (rToInt r `IS.member`) (otherDefFs nL)
    ss = flip gN info<$>IS.toList ns
    gN = IM.findWithDefault (error "internal error: node not in map.")

pall :: [Stmt] -> [Stmt]
pall ss =
    let (cf,_)=mkControlFlow ss
        ss' = fmap (second node) cf
    in go ss'
  where
    go ((_,n):ssϵ) | n `IS.member` dels = go ssϵ
    go ((s,n):ssϵ) | Just cs <- IM.lookup n is = cs++s:go ssϵ
    go ((s,_):ssϵ) = s:go ssϵ
    go [] = []
    (is, dels) = ids ss

ids :: [Stmt] -> (IM.IntMap [Stmt], IS.IntSet)
ids ss = (is IM.empty, ds)
  where
    h = hs ss
    ds = IS.fromList (snd3<$>h)
    go n s = IM.alter (\d -> case d of {Nothing -> Just [s]; Just ss -> Just$s:ss}) n
    is = thread ((\(n,_,s) -> go n s)<$>h)

hs :: [Stmt] -> [(N, N, Stmt)]
hs ss = let (ls, dm) = loop ss
            -- TODO: recomputed?
            mm = lm (reconstruct.fst$mkControlFlow ss)
     in concatMap (\l -> (hl (l,dm,mm))) ls

loop :: [Stmt] -> ([Loop], IM.IntMap (Stmt, ControlAnn))
loop = first (fmap mkL.cTree).hoist
  where
    mkL ns@(n:_) = (n, IS.fromList ns)

hoist :: [Stmt] -> ([Tree N], IM.IntMap (Stmt, ControlAnn))
hoist = mkG.mkControlFlow

cTree :: [Tree N] -> [[N]]
cTree []              = []
cTree (Node _ []:ts)  = cTree ts
cTree (Node n [t]:ts) = (n:et t):cTree ts

et :: Tree N -> [N]
et (Node n [])  = [n]
et (Node n [t]) = n:et t
