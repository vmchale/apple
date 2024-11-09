-- | From Appel
module Asm.G ( alloc, allocF ) where

import           Asm.Ar
import           Asm.BB
import           CF
import           Data.Copointed
import qualified Data.IntMap      as IM
import qualified Data.IntSet      as IS
import qualified Data.Set         as S
import           Data.Tuple.Extra (fst3, snd3, thd3)
import           Q

type K=Int

-- move list; map from abstract registers (def ∪ used) to nodes
type Movs = IM.IntMap MS
type GS = S.Set (Int, Int)
type GL = IM.IntMap [Int]

-- TODO: might work as lazy lists idk (deletion)
-- difference would still be annoying though...
data Wk = Wk { pre, sp, fr, simp :: IS.IntSet }

iSp r w = w { sp = IS.insert r (sp w) }; iFr r w = w { fr = IS.insert r (fr w) }
iSimp r w = w { simp = IS.insert r (simp w) }
mvFrSimp r = mapWk (\w -> w { fr = IS.delete r (fr w), simp = IS.insert r (simp w) })
mvSpSimp r = mapWk (\w -> w { sp = IS.delete r (sp w), simp = IS.insert r (simp w) })
mvSpFr r = mapWk (\w -> w { sp = IS.delete r (sp w), fr = IS.insert r (fr w) })

type M = (Int, Int); type MS = S.Set M

-- TODO: appel says to make these doubly-linked lists
data Mv = Mv { coal, constr, frz, wl, actv :: MS }

actvIns m = mapMv (\mv -> mv { actv = S.insert m (actv mv) })

setWl wlϵ = mapMv (\mv -> mv { wl = wlϵ })
iWl m mv = mv { wl = S.insert m (wl mv) }
iCoal m = mapMv (\mv -> mv { coal = S.insert m (coal mv) })
iConstr m = mapMv (\mv -> mv { constr = S.insert m (constr mv) })
mvActvWl m = mapMv (\mv -> mv { actv = S.delete m (actv mv), wl = S.insert m (wl mv) })
mvActvFrz m = mapMv (\mv -> mv { actv = S.delete m (actv mv), frz = S.insert m (frz mv) })

data Ns = Ns { coalN, colN, spN :: IS.IntSet }

data St = St { mvs :: Movs, aS :: GS, aL :: GL, mvS :: Mv, ɴs :: Ns, degs :: !(IM.IntMap Int), initial :: [Int], wkls :: Wk, stack :: [Int], alias :: !(IM.IntMap Int) }

mapMv f st = st { mvS = f (mvS st) }; mapWk f st = st { wkls = f (wkls st) }; mapNs f st = st { ɴs = f (ɴs st) }

iColN r = mapNs (\ns -> ns { colN = IS.insert r (colN ns)})
iSpN r = mapNs (\ns -> ns { spN = IS.insert r (spN ns) })
iCoalN r = mapNs (\ns -> ns { coalN = IS.insert r (coalN ns) })

thread :: [a -> a] -> a -> a
thread = foldr (.) id

(@!) :: IM.Key -> M -> Movs -> Movs
(@!) k i = IM.alter (\kϵ -> Just$case kϵ of {Nothing -> S.singleton i; Just is -> S.insert i is}) k

(!.) :: Monoid m => IM.IntMap m -> IM.Key -> m
(!.) m k = IM.findWithDefault mempty k m

n !* d = IM.findWithDefault maxBound n d

dec :: IM.Key -> IM.IntMap Int -> IM.IntMap Int
dec = IM.alter (\case {Nothing -> Nothing;Just d -> Just$!d-1})

inc :: IM.Key -> IM.IntMap Int -> IM.IntMap Int
inc = IM.alter (\case {Nothing -> Just 1;Just d -> Just$!d+1})

emptySt :: IS.IntSet -- ^ Precolored registers
        -> [Int]
        -> St
emptySt preC rs = St IM.empty S.empty IM.empty (Mv S.empty S.empty S.empty S.empty S.empty) (Ns IS.empty IS.empty IS.empty) IM.empty rs (Wk preC IS.empty IS.empty IS.empty) [] IM.empty

getIs :: Copointed p => [p Liveness] -> IS.IntSet
getIs = foldMap (g.copoint) where g (Liveness is os _ _) = is<>os

getIFs :: Copointed p => [p Liveness] -> IS.IntSet
getIFs = foldMap (g.copoint) where g (Liveness _ _ fis fos) = fis<>fos

{-# SCC builds #-}
builds :: Copointed p => [[p (UD, Liveness, Maybe M)]] -> St -> St
builds bs = thread [ \s -> snd $ build (out (snd3 (copoint (last isns)))) s (reverse isns) | isns <- bs ]

{-# SCC buildsF #-}
buildsF :: Copointed p => [[p (UD, Liveness, Maybe M)]] -> St -> St
buildsF bs = thread [ \s -> snd $ buildF (fout (snd3 (copoint (last isns)))) s (reverse isns) | isns <- bs ]

alloc :: (Ord reg, Arch arch areg afreg, Copointed (arch areg afreg))
      => [arch areg afreg (UD, Liveness, Maybe (Int,Int))]
      -> [reg] -- ^ available registers
      -> IS.IntSet -- ^ Precolored @areg@
      -> IM.IntMap reg -- ^ Precolored
      -> Either IS.IntSet (IM.IntMap reg) -- ^ Map from abs reg. id (temp) to concrete reg.
alloc aIsns regs preC preCM =
    let st0 = builds (unBB<$>bb aIsns) (emptySt preC (IS.toList $ getIs nIsns IS.\\ preC))
        st1 = mkWorklist ᴋ st0
        st2 = emptyWkl ᴋ st1
        (st3, rs) = assign preCM regs st2
        s = spN (ɴs st3)
    in if IS.null s then Right rs else Left s
    where nIsns = fmap snd3 <$> aIsns; ᴋ = length regs

allocF :: (Ord freg, Arch arch areg afreg, Copointed (arch areg afreg))
       => [arch areg afreg (UD, Liveness, Maybe (Int,Int))]
       -> [freg] -- ^ available registers
       -> IS.IntSet -- ^ Precolored @afreg@
       -> IM.IntMap freg -- ^ Precolored
       -> Either IS.IntSet (IM.IntMap freg) -- ^ Map from abs reg. id (temp) to concrete reg.
allocF aIsns regs preC preCM =
    let st0 = buildsF (unBB<$>bb aIsns) (emptySt preC (IS.toList $ getIFs nIsns IS.\\ preC))
        st1 = mkWorklist ᴋ st0
        st2 = emptyWkl ᴋ st1
        (st3, rs) = assign preCM regs st2
        s = spN (ɴs st3)
    in if IS.null s then Right rs else Left s
    where nIsns = fmap snd3 <$> aIsns; ᴋ = length regs

{-# SCC emptyWkl #-}
emptyWkl :: K -> St -> St
emptyWkl ᴋ s | not $ IS.null (simp (wkls s)) = emptyWkl ᴋ (simplify ᴋ s)
             | not $ S.null (wl (mvS s)) = emptyWkl ᴋ (coalesce ᴋ s)
             | not $ IS.null (fr (wkls s)) = emptyWkl ᴋ (freeze ᴋ s)
             | not $ IS.null (sp (wkls s)) = emptyWkl ᴋ (sspill ᴋ s)
             | otherwise = s

{-# SCC buildF #-}
buildF :: (Copointed p) => IS.IntSet -> St -> [p (UD, Liveness, Maybe M)] -> (IS.IntSet, St)
buildF l st [] = (l, st)
buildF l st@(St ml as al mv ns ds i wk s a) (isn:isns) | Just mIx <- thd3 (copoint isn) =
    let ca = fst3 (copoint isn)
        u = usesFNode ca; d = defsFNode ca
        lm = l IS.\\ u; le = lm `IS.union` d
        ml' = thread [ kϵ @! mIx | kϵ <- IS.toList (u `IS.union` d) ] ml
        st' = St ml' as al (iWl mIx mv) ns ds i wk s a
        st'' = thread [ addEdge lϵ dϵ | lϵ <- IS.toList le, dϵ <- IS.toList d ] st'
        l' = u `IS.union` (lm IS.\\ d)
    in buildF l' st'' isns
                                  | otherwise =
    let ca = fst3 (copoint isn)
        u = usesFNode ca; d = defsFNode ca
        le = l `IS.union` d
        st'' = thread [ addEdge lϵ dϵ | lϵ <- IS.toList le, dϵ <- IS.toList d ] st
        l' = u `IS.union` (l IS.\\ d)
    in buildF l' st'' isns

{-# SCC build #-}
-- | To be called in reverse order
build :: (Copointed p)
      => IS.IntSet -- ^ Live-out for the block
      -> St
      -> [p (UD, Liveness, Maybe M)]
      -> (IS.IntSet, St)
build l st [] = (l, st)
build l st@(St ml as al mv ns ds i wk s a) (isn:isns) | Just mIx <- thd3 (copoint isn) =
    let ca = fst3 (copoint isn)
        u = usesNode ca; d = defsNode ca
        lm = l IS.\\ u; le = lm `IS.union` d
        ml' = thread [ kϵ @! mIx | kϵ <- IS.toList (u `IS.union` d) ] ml
        st' = St ml' as al (iWl mIx mv) ns ds i wk s a
        st'' = thread [ addEdge lϵ dϵ | lϵ <- IS.toList le, dϵ <- IS.toList d ] st'
        l' = u `IS.union` (lm IS.\\ d)
    in build l' st'' isns
                                  | otherwise =
    let ca = fst3 (copoint isn)
        u = usesNode ca
        d = defsNode ca
        le = l `IS.union` d
        st'' = thread [ addEdge lϵ dϵ | lϵ <- IS.toList le, dϵ <- IS.toList d ] st
        l' = u `IS.union` (l IS.\\ d)
    in build l' st'' isns

{-# SCC addEdge #-}
addEdge :: Int -> Int -> St -> St
addEdge u v st@(St ml as al mv ns ds i wk s a) =
    if (u, v) `S.notMember` as && u /= v
        then
            let as' = S.insert (u,v) $ S.insert (v,u) as
                preC = pre wk
                uC = u `IS.notMember` preC; vC = v `IS.notMember` preC
                al' = (if uC then u !: v else id)$(if vC then v !: u else id) al
                ds' = (if uC then inc u else id)$(if vC then inc v else id) ds
            in St ml as' al' mv ns ds' i wk s a
        else st

{-# SCC mkWorklist #-}
mkWorklist :: K -> St -> St
mkWorklist ᴋ st@(St _ _ _ _ _ ds i wk _ _) =
    let wk' = thread [ (case () of { _ | n !* ds >= ᴋ -> iSp; _ | isMR n st -> iFr; _-> iSimp}) n | n <- i ] wk
    in st { initial = [], wkls = wk' }

isMR :: Int -> St -> Bool
isMR i st = not $ S.null (nodeMoves i st)

{-# SCC nodeMoves #-}
nodeMoves :: Int -> St -> MS
nodeMoves n (St ml _ _ mv _ _ _ _ _ _) = ml !. n `S.intersection` (actv mv `S.union` wl mv)

{-# SCC simplify #-}
simplify :: K -> St -> St
simplify ᴋ s@(St _ _ _ _ _ _ _ wk@(Wk _ _ _ stϵ) st _) =
    case IS.minView stϵ of
        Just (n,ns) ->
            let s' = s { wkls = wk { simp = ns }, stack = n:st }
            in thread [ ddg ᴋ m | m <- adj n s' ] s'
        _ -> s

{-# SCC ddg #-}
-- decrement degree
ddg :: K -> Int -> St -> St
ddg ᴋ m s | m `IS.member` pre (wkls s) = s
        | otherwise =
    let d = degs s; s' = s { degs = dec m d }
    in if d IM.! m == ᴋ
        then let s'' = enMv (m:adj m s) s'
             in (if isMR m s'' then mvSpFr else mvSpSimp) m s''
        else s'

-- enable moves
enMv :: [Int] -> St -> St
enMv ns = thread (fmap g ns) where
    g n st = let ms = S.toList (nodeMoves n st) in thread (fmap h ms) st
        where h m stϵ | m `S.member` actv(mvS stϵ) = mvActvWl m st
                      | otherwise = st

{-# SCC addWkl #-}
addWkl :: K -> Int -> St -> St
addWkl ᴋ u st | u `IS.notMember` pre (wkls st) && not (isMR u st) && u !* degs st < ᴋ = mvFrSimp u st
              | otherwise = st

{-# SCC ok #-}
ok :: K -> Int -> Int -> St -> Bool
ok ᴋ t r s = t `IS.member` pre (wkls s) || degs s IM.! t < ᴋ || (t,r) `S.member` aS s

{-# SCC conserv #-}
conserv :: K -> [Int] -> St -> Bool
conserv ᴋ is s =
    let d = degs s
        k = length (filter (\n -> (n !* d)>=ᴋ) is)
    in k<ᴋ

{-# SCC getAlias #-}
getAlias :: Int -> St -> Int
getAlias i s = case IM.lookup i (alias s) of {Just i' -> getAlias i' s; Nothing -> i}

{-# SCC combine #-}
combine :: K -> Int -> Int -> St -> St
combine ᴋ u v st =
    let st0 = mapWk (\(Wk p s f sm) -> if v `IS.member` f then Wk p s (IS.delete v f) sm else Wk p (IS.delete v s) f sm) st
        st1 = iCoalN v st0
        st2 = st1 { alias = IM.insert v u (alias st1) }
        -- https://github.com/sunchao/tiger/blob/d083a354987b7f1fe23f7065ab0c19c714e78cc4/color.sml#L265
        st3 = let m = mvs st2 -- default to S.empty if we haven't filled it in
                  mvu = m !. u; mvv = m !. v in st2 { mvs = IM.insert u (mvu `S.union` mvv) m }
        st4 = thread [ ddg ᴋ t.addEdge t u | t <- adj v st2 ] st3
    in if u `IS.member` fr(wkls st3) && u !* degs st4 >= ᴋ then mapWk(\(Wk p s f sm) -> Wk p (IS.insert u s) (IS.delete u f) sm) st4 else st4

freeze :: K -> St -> St
freeze ᴋ s | Just (u, _) <- IS.minView (fr$wkls s) =
    let s0 = mvFrSimp u s in freezeMoves ᴋ u s0

{-# SCC freezeMoves #-}
freezeMoves :: K -> Int -> St -> St
freezeMoves ᴋ u st = thread (fmap g (S.toList$nodeMoves u st)) st where
    g m@(x, y) s =
        let y' = getAlias y s; v = if y' == getAlias u s then getAlias x s else y'
            st0 = mvActvFrz m s
        in if S.null (nodeMoves v st0) && v !* degs st0 < ᴋ
            then mvFrSimp v st0
            else st0

{-# SCC adj #-}
adj :: Int -> St -> [Int]
adj n s = aL s !. n ∖ (IS.fromList (stack s) <> coalN (ɴs s))

(∖) :: [Int] -> IS.IntSet -> [Int]
(∖) x yϵ = filter (`IS.notMember` yϵ) x

dSet :: Ord reg => [reg] -> [reg] -> [reg]
dSet x ys = filter (`S.notMember` yϵ) x where yϵ = S.fromList ys

{-# SCC coalesce #-}
coalesce :: K -> St -> St
coalesce ᴋ s | Just (m@(x,y), nWl) <- S.minView (wl$mvS s) =
    let y' = getAlias y s
        preS = pre (wkls s)
        (u, v) = if y' `IS.member` preS then (y',x') else (x',y') where x' = getAlias x s
        s0 = setWl nWl s
    in case () of
        _ | u == v -> addWkl ᴋ u $ iCoal m s0
          | v `IS.member` preS || (u,v) `S.member` aS s0 -> addWkl ᴋ v $ addWkl ᴋ u $ iConstr m s0
          | let av = adj v s0 in if u `IS.member` preS then all (\t -> ok ᴋ t u s0) av else conserv ᴋ (adj u s0 ++ av) s0 ->
              addWkl ᴋ u $ combine ᴋ u v $ iCoal m s0
          | otherwise -> actvIns m s0

sspill :: K -> St -> St
sspill ᴋ s | Just (m, nSp) <- IS.minView (sp$wkls s) = freezeMoves ᴋ m $ mapWk (iSimp m . \wk -> wk { sp = nSp }) s

{-# SCC assign #-}
assign :: (Ord reg) => IM.IntMap reg -> [reg] -> St -> (St, IM.IntMap reg)
assign iC colors s = snip $ go (s, colors, iC) where
    snip (x, _, z) = (x, z)
    go (sϵ@(St _ _ _ _ (Ns ns _ _) _ _ _ [] _), _, c) = (sϵ, undefined, thread [ IM.insert n (c IM.! getAlias n sϵ) | n <- IS.toList ns ] c)
    go (sϵ@(St _ _ al _ _ _ _ _ (n:ns) _), okϵ, cs) =
        let ok0 = okϵ `dSet` [ cs IM.! getAlias w sϵ | w <- al !. n, getAlias w sϵ `IS.member` (colN (ɴs sϵ) `IS.union` pre (wkls sϵ)) ]
            s0 = sϵ { stack = ns }
            (s1, cs0) =
                case ok0 of
                    c:_ -> (iColN n s0, IM.insert n c cs)
                    _   -> (iSpN n s0, cs)
        in go (s1, colors, cs0)
