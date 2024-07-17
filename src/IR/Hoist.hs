module IR.Hoist ( hoist, pall ) where

import           CF
import           Control.Composition (thread)
import           Data.Bifunctor      (second)
import           Data.Graph          (Tree (Node))
import qualified Data.IntMap         as IM
import qualified Data.IntSet         as IS
import           Data.Tuple.Extra    (first3, snd3)
import           Dom
import           IR
import           IR.CF
import           LR

type Loop = (N, IS.IntSet)

lm :: [(Stmt, NLiveness)] -> IM.IntMap NLiveness
lm = IM.fromList.fmap (\(_,n) -> (nx n, n))

hl :: (Loop, IM.IntMap (Stmt, ControlAnn), IM.IntMap NLiveness) -> [(N, N, (Temp, Double))]
hl ((n,ns), info, linfo) = go ss
  where
    lH=liveness (gN n linfo)
    fliveInH=fins lH
    go (((MX x (ConstF i)), a):ssϵ) | rToInt x `IS.notMember` fliveInH && notFDef x (node a) = (n, node a, (x,i)):go ssϵ
    go (_:ssϵ)                      = go ssϵ
    go []                           = []
    otherDefFs nL = defsFNode.ud.snd.flip gN info<$>(IS.toList$IS.delete nL ns)
    notFDef r nL = not $ any (rToInt r `IS.member`) (otherDefFs nL)
    ss = flip gN info<$>IS.toList ns
    gN = IM.findWithDefault (error "internal error: node not in map.")

pall :: [Stmt] -> [Stmt]
pall ss =
    let ss' = fmap (second node) cf
    in go ss'
  where
    go ((_,n):ssϵ) | n `IS.member` dels = go ssϵ
    -- TODO: consolidate
    go ((s,n):ssϵ) | Just cs <- IM.lookup n is = consolidate cs++s:go ssϵ
    go ((s,_):ssϵ) = s:go ssϵ
    go [] = []
    (cf, is, dels) = indels ss
    consolidate = fmap (\(t,x) -> MX t (ConstF x))

indels :: [Stmt] -> ([(Stmt, ControlAnn)], IM.IntMap [(Temp, Double)], IS.IntSet)
indels ss = (c, is IM.empty, ds)
  where
    (c,h) = hs ss
    ds = IS.fromList (snd3<$>h)
    go n s = IM.alter (\d -> case d of {Nothing -> Just [s]; Just ssϵ -> Just$s:ssϵ}) n
    is = thread ((\(n,_,s) -> go n s)<$>h)

hs :: [Stmt] -> ([(Stmt, ControlAnn)], [(N, N, (Temp, Double))])
hs ss = let (ls, cf, dm) = loop ss
            mm = lm (reconstruct cf)
     in (cf, concatMap (\l -> (hl (l,dm,mm))) ls)

loop :: [Stmt] -> ([Loop], [(Stmt, ControlAnn)], IM.IntMap (Stmt, ControlAnn))
loop = first3 (fmap mkL.cTree).hoist
  where
    mkL ns@(n:_) = (n, IS.fromList ns)

hoist :: [Stmt] -> ([Tree N], [(Stmt, ControlAnn)], IM.IntMap (Stmt, ControlAnn))
hoist ss = (\c@(ssϵ,_) -> (\(x,y) -> (x,ssϵ,y))$mkG c) (mkControlFlow ss)

cTree :: [Tree N] -> [[N]]
cTree []              = []
cTree (Node _ []:ts)  = cTree ts
cTree (Node n [t]:ts) = (n:et t):cTree ts

et :: Tree N -> [N]
et (Node n [])      = [n]
et (Node n [t])     = n:et t
et (Node n [t0,t1]) = n:et t0++et t1
