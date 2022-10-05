-- | From Appel
module Asm.G ( build, mkWorklist, assign ) where

import           CF
import           Control.Monad.State.Strict (State)
import qualified Data.Array                 as A
import           Data.Copointed
import Data.Tuple.Extra (fst3, snd3, thd3)
import Data.Containers.ListUtils
import           Data.Graph                 (Bounds, Edge, Graph, Vertex, buildG)
import qualified Data.IntMap                as IM
import qualified Data.IntSet                as IS
import qualified Data.Set                   as S


-- move list: map from abstract registers (def ∪ used) to nodes
type Movs = IM.IntMap MS
type GS = S.Set (Int, Int)
type GL = IM.IntMap [Int]

data Memb = Pre | Init | Sp | Fr | Simp | Coal | Colored | Stack

-- TODO: might work as lazy lists idk (deletion)
-- difference would still be annoying though...
data Wk = Wk { pre :: IS.IntSet, sp :: IS.IntSet, fr :: IS.IntSet, simp :: IS.IntSet }

mapSp f w = w { sp = f (sp w) }
mapFr f w = w { fr = f (fr w) }
mapSimp f w = w { simp = f (simp w) }

type M = (Int, Int); type MS = S.Set M

-- TODO: appel says to make these doubly-linked lists
--
-- also these appear to be (Int, Int) idk
data Mv = Mv { coal :: MS, constr :: MS, frz :: MS, wl :: MS, actv :: MS }

mapWl f mv = mv { wl = f (wl mv) }
mapActv f mv = mv { actv = f (actv mv) }
mapCoal f mv = mv { coal = f (coal mv) }
mapFrz f mv = mv { frz = f (frz mv) }
mapConstr f mv = mv { constr = f (constr mv) }

data Ns = Ns { coalN :: IS.IntSet, colN :: IS.IntSet, spN :: IS.IntSet }

mapCoalN f ns = ns { coalN = f (coalN ns) }

data St = St { mvs :: Movs, aS :: GS, aL :: GL, mvS :: Mv, ɴs :: Ns, degs :: IM.IntMap Int, initial :: [Int], wkls :: Wk, stack :: [Int], alias :: IM.IntMap Int }

mapMv f st = st { mvS = f (mvS st) }
mapWk f st = st { wkls = f (wkls st) }
mapNs f st = st { ɴs = f (ɴs st) }

thread :: [a -> a] -> a -> a
thread = foldr (.) id

(!:) :: IM.Key -> Int -> GL -> GL
(!:) k i = IM.alter (\kϵ -> Just$case kϵ of {Nothing -> [i]; Just is -> i:is}) k

(@!) :: IM.Key -> M -> Movs -> Movs
(@!) k i = IM.alter (\kϵ -> Just$case kϵ of {Nothing -> S.singleton i; Just is -> S.insert i is}) k

dec :: IM.Key -> IM.IntMap Int -> IM.IntMap Int
dec = IM.alter (\k -> case k of {Nothing -> Nothing;Just d -> Just$d-1})

inc :: IM.Key -> IM.IntMap Int -> IM.IntMap Int
inc = IM.alter (\k -> case k of {Nothing -> Nothing;Just d -> Just$d+1})

-- | To be called in reverse order
build :: (Copointed p) 
      => IS.IntSet -- ^ Live-out for the block
      -> St 
      -> [p (ControlAnn, NLiveness, Maybe M)] 
      -> (IS.IntSet, St)
build l st [] = (l, st)
build l (St ml as al mv ns ds i wk s a) (isn:isns) | Just mIx <- thd3 (copoint isn) =
    let ca = fst3 (copoint isn)
        nl = snd3 (copoint isn)
        u = usesNode ca
        d = defsNode ca
        lm = l IS.\\ u
        nIx = nx nl
        ml' = thread [ kϵ @! mIx | kϵ <- IS.toList (u `IS.union` d) ] ml
        le = lm `IS.union` d
        st' = St ml' as al (mapWl (S.insert mIx) mv) ns ds i wk s a
        st'' = thread [ addEdge lϵ dϵ | lϵ <- IS.toList le, dϵ <- IS.toList d ] st'
        l' = u `IS.union` (lm IS.\\ d)
    in build l' st'' isns
                                  | otherwise =
    let ca = fst3 (copoint isn)
        u = usesNode ca
        d = defsNode ca
        le = l `IS.union` d
        st' = St ml as al mv ns ds i wk s a
        st'' = thread [ addEdge lϵ dϵ | lϵ <- IS.toList le, dϵ <- IS.toList d ] st'
        l' = u `IS.union` (l IS.\\ d)
    in build l' st'' isns

-- TODO: Memb?
addEdge :: Int -> Int -> St -> St
addEdge u v st@(St ml as al mv ns ds i wk s a) =
    if (u, v) `S.notMember` as && u /= v
        then
            let as' = as `S.union` S.fromList [(u,v), (v, u)]
                uC = u `IS.notMember` pre wk
                vC = v `IS.notMember` pre wk
                al' = (if uC then u !: v else id)$(if vC then v !: u else id) al
                ds' = (if uC then inc u else id)$(if vC then inc v else id) ds
            in St ml as' al' mv ns ds' i wk s a
        else st

mkWorklist :: St -> St
mkWorklist st@(St _ _ _ _ _ ds i wk _ _) =
    let i' = []
        wk' = thread [ (case () of { _ | ds IM.! n >= ᴋ -> mapSp; _ | isMR n st -> mapFr; _-> mapSimp}) (IS.insert n) | n <- i] wk
    in st { initial = i', wkls = wk' }

-- same for xmm0, r15
ᴋ = 16

isMR :: Int -> St -> Bool
isMR i st = nodeMoves i st /= S.empty

nodeMoves :: Int -> St -> MS
nodeMoves n (St ml _ _ mv _ _ _ _ _ _) = (ml IM.! n) `S.intersection` (actv mv `S.union` wl mv)

simplify :: St -> St
simplify s@(St _ _ al _ _ ds _ wk@(Wk _ _ _ stϵ) st _) | Just (n,ns) <- IS.minView stϵ =
    let ds' = thread [ dec m | m <- al IM.! n ] ds
    in s { wkls = wk { simp = ns }, stack = n:st, degs = ds' }
                                                   | otherwise = s

-- decrement degree
ddg :: Int -> St -> St
ddg m s =
    let d = degs s; s' = s { degs = dec m d }
    in if d IM.! m == ᴋ
        then let s'' = enaMv (m:(aL s IM.! m)) s'
             in mapWk (mapSp (IS.delete m).(if isMR m s'' then mapFr else mapSimp) (IS.insert m)) s''
        else s'

-- enable moves
enaMv :: [Int] -> St -> St
enaMv ns = thread (fmap g ns) where
    g n st = let ms = S.toList (nodeMoves n st) in thread (fmap h ms) st
        where h m stϵ | m `S.member` actv(mvS stϵ) = mapMv (mapWl (S.insert m) . mapActv (S.delete m)) st
                      | otherwise = st

addWkl :: Int -> St -> St
addWkl u st | u `IS.notMember` pre (wkls st) && not (isMR u st) && degs st IM.! u < ᴋ = mapWk (mapFr (IS.delete u) . mapSimp (IS.insert u)) st
            | otherwise = st

ok :: Int -> Int -> St -> Bool
ok t r s = degs s IM.! t < ᴋ || t `IS.member` pre (wkls s) || (t,r) `S.member` aS s

conserv :: [Int] -> St -> Bool
conserv is s =
    let d = degs s
        k = length (filter (\n -> (d IM.! n)>=ᴋ) is)
    in k<ᴋ

getAlias :: Int -> St -> Int
getAlias i s = case IM.lookup i (alias s) of {Just i' -> getAlias i' s; Nothing -> i}

combine :: Int -> Int -> St -> St
combine u v st =
    let st0 = mapWk (\(Wk p s f sm) -> if v `IS.member` f then Wk p s (IS.delete v f) sm else Wk p (IS.delete v s) f sm) st
        st1 = mapNs (mapCoalN (IS.insert v)) st0
        st2 = st1 { alias = IM.insert v u (alias st1) }
        -- https://github.com/sunchao/tiger/blob/d083a354987b7f1fe23f7065ab0c19c714e78cc4/color.sml#L265
        st3 = let m = mvs st2; mvu = m IM.! u; mvv = m IM.! v in st2 { mvs = IM.insert u (mvu `S.union` mvv) m }
        st4 = thread [ ddg t.addEdge t u | t <- aL st2 IM.! v ] st3
    in if degs st4 IM.! u >= ᴋ && u `IS.member` fr(wkls st3) then st4 else mapWk(\(Wk p s f sm) -> Wk p (IS.insert u s) (IS.delete u f) sm) st4

freezeMoves :: Int -> St -> St
freezeMoves u st = thread (fmap g (S.toList$nodeMoves u st)) st where
    g m@(x, y) st =
        let y' = getAlias y st; v = if y' == getAlias u st then getAlias x st else y'
            st0 = mapMv (mapActv (S.delete m).mapFrz (S.insert m)) st
        in if S.null (nodeMoves v st0) && degs st0 IM.! v < ᴋ
            then mapWk (mapFr (IS.delete v).mapSimp (IS.insert v)) st0
            else st0

adj :: Int -> St -> [Int]
adj n s = aL s IM.! n ∖ (IS.fromList (stack s) <> coalN (ɴs s))

(∖) :: [Int] -> IS.IntSet -> [Int]
(∖) x yϵ = filter (`IS.notMember` yϵ) x

coalesce :: St -> St
coalesce s | Just (m@(x,y), nWl) <- S.minView (wl$mvS s) =
    let x' = getAlias x s; y' = getAlias y s
        preS = pre (wkls s)
        (u, v) = if y' `IS.member` preS then (y',x') else (x',y')
        s0 = mapMv (\mv -> mv { wl = nWl }) s
    in case () of
        _ | u == v -> addWkl u $ mapMv (mapCoal (S.insert m)) s0
          | v `IS.member` preS || (u,v) `S.member` aS s0 -> addWkl v $ addWkl u $ mapMv (mapConstr (S.insert m)) s0
          | let av = adj v s0 in u `IS.member` preS && all (\t -> ok t u s0) av || u `IS.notMember` preS && conserv (adj u s0 ++ av) s0 ->
              addWkl u $ combine u v $ mapMv (mapCoal (S.insert m)) s0
          | otherwise -> mapMv (mapActv (S.insert m)) s0

sspill :: St -> St
sspill s | Just (m, nSp) <- IS.minView (sp$wkls s) = freezeMoves m $ mapWk (mapSimp (IS.insert m). \wk -> wk { sp = nSp }) s

assign :: Eq reg => [reg] -> St -> IM.IntMap reg
assign colors s = thd3 $ go (s, colors, initColors) where
    go (sϵ@(St _ _ _ _ (Ns ns _ _) _ _ _ [] _), _, c) = (undefined, undefined, thread [ IM.insert n (c IM.! getAlias n sϵ) | n <- IS.toList ns ] c)
    initColors = IM.empty
