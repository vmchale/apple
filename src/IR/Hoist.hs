{-# LANGUAGE TupleSections #-}

module IR.Hoist ( loop, hoist, pall ) where

import           CF
import           Control.Composition        (thread)
import           Control.Monad.State.Strict (gets, modify, runState)
import qualified Data.Array                 as A
import           Data.Bifunctor             (bimap, first, second)
import           Data.Functor               (($>))
import           Data.Graph                 (Tree (Node))
import           Data.Graph.Dom             (Graph, Node, domTree)
import qualified Data.IntMap                as IM
import qualified Data.IntSet                as IS
import qualified Data.Map.Strict            as M
import           Data.Maybe                 (catMaybes, fromJust)
import           Data.Tuple.Extra           (first3, snd3)
import           IR
import           IR.CF
import           LR

type N=Int

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

mapFF :: (FTemp -> FTemp) -> FExp -> FExp
mapFF _ x@ConstF{}    = x
mapFF f (FAt a)       = FAt (mapFA f a)
mapFF f (FB op e0 e1) = FB op (mapFF f e0) (mapFF f e1)
mapFF f (FU op e)     = FU op (mapFF f e)
mapFF f (FReg r)      = FReg (f r)
mapFF f (FConv e)     = FConv (mapFE f e)

mapF :: (FTemp -> FTemp) -> Stmt -> Stmt
mapF f (MX t e)      = MX (f t) (mapFF f e)
mapF _ s@L{}         = s
mapF _ s@C{}         = s
mapF _ s@R{}         = s
mapF _ s@IRnd{}      = s
mapF f (FRnd t)      = FRnd (f t)
mapF _ s@J{}         = s
mapF _ s@Free{}      = s
mapF _ s@RA{}        = s
mapF f (MT t e)      = MT t (mapFE f e)
mapF f (Wr a e)      = Wr (mapFA f a) (mapFE f e)
mapF f (WrF a x)     = WrF (mapFA f a) (mapFF f x)
mapF f (WrB a e)     = WrB (mapFA f a) (mapFE f e)
mapF f (Fcmov e t x) = Fcmov (mapFE f e) (f t) (mapFF f x)
mapF f (MJ e l)      = MJ (mapFE f e) l
mapF f (Ma l t e)    = Ma l t (mapFE f e)
mapF f (Sa t e)      = Sa t (mapFE f e)
mapF f (Pop e)       = Pop (mapFE f e)
mapF f (Cpy a0 a1 e) = Cpy (mapFA f a0) (mapFA f a1) (mapFE f e)
mapF f (Cmov p t e)  = Cmov (mapFE f p) t (mapFE f e)
mapF f (Cset t p)    = Cset t (mapFE f p)

type Loop = (N, IS.IntSet)

-- TODO: array?
lm :: [(Stmt, NLiveness)] -> IM.IntMap NLiveness
lm = IM.fromList.fmap (\(_,n) -> (nx n, n))

hl :: (Loop, A.Array Int (Stmt, ControlAnn), IM.IntMap NLiveness) -> [(N, N, (FTemp, Double))]
hl ((n,ns), info, linfo) = go ss
  where
    lH=liveness (gN n linfo)
    fliveInH=fins lH
    go (((MX x (ConstF i)), a):ssϵ) | fToInt x `IS.notMember` fliveInH && notFDef x (node a) = (n, node a, (x,i)):go ssϵ
    go (_:ssϵ)                      = go ssϵ
    go []                           = []
    otherDefFs nL = defsFNode.ud.snd.(info A.!)<$>(IS.toList$IS.delete nL ns)
    notFDef r nL = not $ any (fToInt r `IS.member`) (otherDefFs nL)
    ss = (info A.!)<$>IS.toList ns
    gN = IM.findWithDefault (error "internal error: node not in map.")

pall :: [Stmt] -> [Stmt]
pall ss =
    let ss' = fmap (second node) cf
        (s, ss'') = go ss'
    in {-# SCC "applySubst" #-} applySubst s ss''
  where
    go ((_,n):ssϵ) | n `IS.member` dels = go ssϵ
    go ((s,n):ssϵ) | Just cs <- IM.lookup n is = let (css, (_, subst)) = {-# SCC "consolidate" #-} consolidate cs in bimap (subst<>) ((css++[s])++) (go ssϵ)
    go ((s,_):ssϵ) = second (s:)$go ssϵ
    go [] = (M.empty, [])
    (cf, is, dels) = indels ss
    applySubst s = fmap (mapF (\t -> case M.lookup t s of Just r -> r; Nothing -> t))
    consolidate = first catMaybes . flip runState (M.empty, M.empty) . traverse (\(t,x) -> do
        seen <- gets fst
        case M.lookup x seen of
            Nothing -> modify (first (M.insert x t)) $> Just (MX t (ConstF x))
            Just r  -> modify (second (M.insert t r)) $> Nothing)

indels :: [Stmt] -> ([(Stmt, ControlAnn)], IM.IntMap [(FTemp, Double)], IS.IntSet)
indels ss = (c, is IM.empty, ds)
  where
    (c,h) = hs ss
    ds = IS.fromList (snd3<$>h)
    go n s = IM.alter (\d -> case d of {Nothing -> Just [s]; Just ssϵ -> Just$s:ssϵ}) n
    is = thread ((\(n,_,s) -> go n s)<$>h)

hs :: [Stmt] -> ([(Stmt, ControlAnn)], [(N, N, (FTemp, Double))])
hs ss = let (ls, cf, dm) = loop ss
            mm = lm (reconstruct cf)
     in (cf, concatMap (\l -> (hl (l,dm,mm))) (ols ls))

loop :: [Stmt] -> ([Loop], [(Stmt, ControlAnn)], A.Array Int (Stmt, ControlAnn))
loop = first3 (fmap mkL).(\(w,x,y,z) -> (et w (fmap fst z) [] x,y,z)).hoist
  where
    mkL (n, ns) = (n, IS.fromList ns)

hoist :: [Stmt] -> (Graph, Tree N, [(Stmt, ControlAnn)], A.Array Int (Stmt, ControlAnn))
hoist ss = (\ssϵ -> (\(x,y,z,_) -> (x,y,fst ssϵ,z))$mkG ssϵ) (mkControlFlow ss)

{-# SCC ols #-}
ols :: [Loop] -> [Loop]
ols ls = filter (\(_,ns) -> not $ any (\(_,ns') -> ns `IS.isSubsetOf` ns') ls) ls

et :: Graph -> A.Array Int Stmt -> [N] -> Tree N -> [(N, [N])]
et g ss seen t = expandLoop t <$> tLoops g ss seen t

{-# SCC expandLoop #-}
expandLoop :: Tree N -> (N,N) -> (N,[N])
-- wir müssen wissen, wir werden wissen
expandLoop t se = fromJust (go [] se t)
  where
    go seen (s,e) (Node n _) | e == n = Just (s, dropWhile (/=s) (reverse seen))
    go _ _ (Node _ [])       = Nothing
    go seen seϵ (Node n ns)  = mh ((go (n:seen) seϵ) <$> ns) where mh xs=case catMaybes xs of {[] -> Nothing; (nϵ:_) -> Just nϵ}

tLoops :: Graph -> A.Array Int Stmt -> [N] -> Tree N -> [(N, N)]
tLoops g ss seen (Node n cs) =
    let bes=filter (hasEdge g n) seen
    in (if isMJ n then (fmap (,n) bes++) else id) $ concatMap (tLoops g ss (n:seen)) cs
  where
    isMJ nϵ = p (ss A.! nϵ)
    p MJ{}=True; p _=False

hasEdge :: Graph -> Node -> Node -> Bool
hasEdge g n0 n1 = case IM.lookup n0 g of {Nothing -> False; Just ns -> n1 `IS.member` ns}

mkG :: ([(Stmt, ControlAnn)], Int) -> (Graph, Tree N, A.Array Int (Stmt, ControlAnn), IM.IntMap (Stmt, ControlAnn))
mkG (ns,m) = (domG, domTree ((node (snd (head ns))), domG), sa, IM.fromList ((\(s, ann) -> (node ann, (s, ann)))<$>ns))
  where
    domG = IM.fromList [ (node ann, IS.fromList (conn ann)) | (_, ann) <- ns ]
    sa = A.listArray (0,m-1) ns
