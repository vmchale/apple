module IR.Hoist ( loop, graphParts, pall ) where

import           CF
import           Control.Composition        (thread)
import           Control.Monad.Trans.State.Strict (gets, modify, runState)
import qualified Data.Array                 as A
import           Data.Bifunctor             (bimap, first, second)
import           Data.Foldable              (toList)
import           Data.Function              (on)
import           Data.Functor               (($>))
import           Data.Graph                 (Tree (Node))
import           Data.Graph.Dom             (Graph, Node, domTree)
import qualified Data.IntMap                as IM
import qualified Data.IntSet                as IS
import           Data.List                  (sortBy)
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (catMaybes, fromJust, fromMaybe)
import           Data.Tuple.Extra           (first3, second3, fst3, snd3, thd3,third3)
import           Data.Void                  (Void, absurd)
import           IR
import           IR.CF
import           LR

type N=Int
type CfTbl=A.Array Int (Stmt, ControlAnn); type StmtTbl=A.Array Int Stmt

mapFA :: (FTemp -> FTemp) -> AE -> AE
mapFA f (AP t (Just e) l) = AP t (Just$mapFE f e) l
mapFA _ a                 = a

mapFE :: (FTemp -> FTemp) -> Exp -> Exp
mapFE f (IRFloor x)      = IRFloor (mapFF f x)
mapFE f (EAt a)          = EAt (mapFA f a)
mapFE f (BAt a)          = BAt (mapFA f a)
mapFE _ e@ConstI{}       = e
mapFE _ e@Reg{}          = e
mapFE _ e@Is{}           = e
mapFE f (FRel rel x0 x1) = FRel rel (mapFF f x0) (mapFF f x1)
mapFE f (IB op e0 e1)    = IB op (mapFE f e0) (mapFE f e1)
mapFE f (IU op e)        = IU op (mapFE f e)
mapFE f (BU op e)        = BU op (mapFE f e)
mapFE f (IRel rel e0 e1) = IRel rel (mapFE f e0) (mapFE f e1)
mapFE _ e@LA{}           = e

mapFF2 :: (FTemp -> FTemp) -> FExp F2 c Void -> FExp F2 c Void
mapFF2 _ x@ConstF{}    = x
mapFF2 f (FAt a)       = FAt (mapFA f a)
mapFF2 _ r@FReg{}      = r
mapFF2 f (FB op e0 e1) = FB op (mapFF2 f e0) (mapFF2 f e1)
mapFF2 f (FU op e)     = FU op (mapFF2 f e)
mapFF2 _ (FConv x)     = absurd x

mapFF :: (FTemp -> FTemp) -> FExp FTemp c Exp -> FExp FTemp c Exp
mapFF _ x@ConstF{}    = x
mapFF f (FAt a)       = FAt (mapFA f a)
mapFF f (FB op e0 e1) = FB op (mapFF f e0) (mapFF f e1)
mapFF f (FU op e)     = FU op (mapFF f e)
mapFF f (FReg r)      = FReg (f r)
mapFF f (FConv e)     = FConv (mapFE f e)

mapF :: (FTemp -> FTemp) -> Stmt -> Stmt
mapF f (MX t e)       = MX (f t) (mapFF f e)
mapF f (MX2 t e)      = MX2 t (mapFF2 f e)
mapF f (S2 t r)       = S2 (f t) r
mapF _ s@L{}          = s
mapF _ s@C{}          = s
mapF _ s@R{}          = s
mapF _ s@J{}          = s
mapF _ s@IRnd{}       = s
mapF _ s@RA{}         = s
mapF f (FRnd t)       = FRnd (f t)
mapF _ s@Free{}       = s
mapF f (MT t e)       = MT t (mapFE f e)
mapF f (Wr a e)       = Wr (mapFA f a) (mapFE f e)
mapF f (WrF a x)      = WrF (mapFA f a) (mapFF f x)
mapF f (WrF2 a v)     = WrF2 (mapFA f a) (mapFF2 f v)
mapF f (WrB a e)      = WrB (mapFA f a) (mapFE f e)
mapF f (Fcmov e t x)  = Fcmov (mapFE f e) (f t) (mapFF f x)
mapF f (MJ e l)       = MJ (mapFE f e) l
mapF f (Ma l t e)     = Ma l t (mapFE f e)
mapF f (Sa t e)       = Sa t (mapFE f e)
mapF f (Pop e)        = Pop (mapFE f e)
mapF f (Sa8 t e)      = Sa8 t (mapFE f e)
mapF f (Pop8 e)       = Pop8 (mapFE f e)
mapF f (Cpy a0 a1 e)  = Cpy (mapFA f a0) (mapFA f a1) (mapFE f e)
mapF f (Cpy1 a0 a1 e) = Cpy1 (mapFA f a0) (mapFA f a1) (mapFE f e)
mapF f (Cmov p t e)   = Cmov (mapFE f p) t (mapFE f e)
mapF f (Cset t p)     = Cset t (mapFE f p)

type Loop = (N, IS.IntSet)

-- TODO: array?
lm :: [(Stmt, NLiveness)] -> IM.IntMap NLiveness
lm = IM.fromList.fmap (\(_,n) -> (nx n, n))

data CM = FM !FTemp !Double | F2M !F2 !(Double, Double)

hl :: (Loop, CfTbl, IM.IntMap NLiveness) -> [(N, N, CM)]
hl ((n,ns), info, linfo) = go ss
  where
    fliveInH=fins lH; lH=liveness (gN n linfo)
    go ((MX x (ConstF i), a):ssϵ) | fToInt x `IS.notMember` fliveInH && notFDef (fToInt x) (node a) = (n, node a, FM x i):go ssϵ
    go ((MX2 x (ConstF i), a):ssϵ) | f2ToInt x `IS.notMember` fliveInH && notFDef (f2ToInt x) (node a) = (n, node a, F2M x i):go ssϵ
    go (_:ssϵ)                      = go ssϵ
    go []                           = []
    otherDefFs nL = defsFNode.ud.snd.(info A.!)<$>IS.toList(IS.delete nL ns)
    notFDef r nL = not $ any (r `IS.member`) (otherDefFs nL)
    ss = (info A.!)<$>IS.toList ns
    gN = IM.findWithDefault (error "internal error: node not in map.")

pall :: [Stmt] -> [Stmt]
pall ss =
    let ss' = fmap (second node) cf
        (s, ss'') = go ss'
    in {-# SCC "applySubst" #-} applySubst s ss''
  where
    go ((_,n):ssϵ) | n `IS.member` dels = go ssϵ
    go ((s,n):ssϵ) | Just cs <- IM.lookup n is = let (css, (_, subst, _)) = {-# SCC "consolidate" #-} consolidate cs in bimap (subst<>) ((css++[s])++) (go ssϵ)
    go ((s,_):ssϵ) = second (s:)$go ssϵ
    go [] = (M.empty, [])
    (cf, is, dels) = indels ss
    applySubst s = fmap (mapF (\t -> fromMaybe t (M.lookup t s)))
    consolidate = first catMaybes . flip runState (M.empty, M.empty, M.empty) . traverse gg
    gg (FM t x) = do
        seen <- gets fst3
        case M.lookup x seen of
            Nothing -> modify (first3 (M.insert x t)) $> Just (MX t (ConstF x))
            Just r  -> modify (second3 (M.insert t r)) $> Nothing
    gg (F2M t x) = do
        seen <- gets thd3
        case M.lookup x seen of
            Nothing -> modify (third3 (M.insert x t)) $> Just (MX2 t (ConstF x))

indels :: [Stmt] -> ([(Stmt, ControlAnn)], IM.IntMap [CM], IS.IntSet)
indels ss = (c, is IM.empty, ds)
  where
    (c,h) = hs ss
    ds = IS.fromList (snd3<$>h)
    go n s = IM.alter (\d -> case d of {Nothing -> Just [s]; Just ssϵ -> Just$s:ssϵ}) n
    is = thread ((\(n,_,s) -> go n s)<$>h)

hs :: [Stmt] -> ([(Stmt, ControlAnn)], [(N, N, CM)])
hs ss = let (ls, cf, dm) = loop ss
            mm = lm (reconstructFlat cf)
     in (cf, concatMap (\l -> hl (l,dm,mm)) (ols ls))

loop :: [Stmt] -> ([Loop], [(Stmt, ControlAnn)], CfTbl)
loop = first3 (fmap mkL).(\(w,x,y,z) -> (et w (fmap fst z) [] x,y,z)).graphParts
  where
    mkL (n, ns) = (n, IS.fromList ns)

graphParts :: [Stmt] -> (Graph, Tree N, [(Stmt, ControlAnn)], CfTbl)
graphParts ss = (\ssϵ -> (\(x,y,z) -> (x,y,fst ssϵ,z))$mkG ssϵ) (mkControlFlow ss)

{-# SCC ols #-}
ols :: [Loop] -> [Loop]
ols ls = filter (\(_,ns) -> not $ any (\(_,ns') -> ns `IS.isProperSubsetOf` ns') ls) ls

-- het (HLoop)
et :: Graph -> StmtTbl -> [N] -> Tree N -> [(N, [N])]
et g ss seen t = expandLoop t <$> loopHeads g ss seen t

-- everything the start node dominates
expandLoop :: Tree N -> N -> (N,[N])
--- wir müssen wissen, wir werden wissen
expandLoop t s = (s, fromJust (go t))
  where
    go (Node n tϵ) | n==s = Just$concatMap toList tϵ
    go (Node _ ns) = mh (go<$>ns) where mh xs=case catMaybes xs of {[] -> Nothing; (nϵ:_) -> Just nϵ}

loopHeads :: Graph -> StmtTbl -> [N] -> Tree N -> [N]
loopHeads g ss seen (Node n cs) =
    let bes=filter (hasEdge g n) seen
    in (if isMJ n then (bes++) else id) $ concatMap (loopHeads g ss (n:seen)) cs
  where
    isMJ nϵ = p (ss A.! nϵ)
    p MJ{}=True; p _=False

hasEdge :: Graph -> Node -> Node -> Bool
hasEdge g n0 n1 = case IM.lookup n0 g of {Nothing -> False; Just ns -> n1 `IS.member` ns}

mkG :: ([(Stmt, ControlAnn)], Int) -> (Graph, Tree N, CfTbl)
mkG (ns,m) = (domG, domTree (node (snd (head ns)), domG), sa)
  where
    domG = IM.fromList [ (node ann, IS.fromList (conn ann)) | (_, ann) <- ns ]
    sa = A.listArray (0,m-1) (sortBy (compare `on` (node.snd)) ns)
