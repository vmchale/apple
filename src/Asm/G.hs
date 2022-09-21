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
type GL = IM.IntMap [Int]

data Memb = Pre | Init | Sp | Fr | Simp | Coal | Colored | Stack

data Wk = Wk { pre :: IS.IntSet, sp :: IS.IntSet, fr :: IS.IntSet, simp :: IS.IntSet }

mapSp f w = w { sp = f (sp w) }
mapFr f w = w { fr = f (fr w) }
mapSimp f w = w { simp = f (simp w) }

-- TODO: appel says to make these doubly-linked lists
data Mv = Mv { coal :: IS.IntSet, constr :: IS.IntSet, frz :: IS.IntSet, wl :: IS.IntSet, actv :: IS.IntSet }

mapWl f mv = mv { wl = f (wl mv) }
mapActv f mv = mv { actv = f (actv mv) }

data St = St { mvs :: Movs, aS :: GS, aL :: GL, mvS :: Mv, degs :: IM.IntMap Int, initial :: [Int], wkls :: Wk, stack :: [Int], alias :: IM.IntMap Int }

mapMv f st = st { mvS = f (mvS st) }
mapWk f st = st { wkls = f (wkls st) }

thread :: [a -> a] -> a -> a
thread = foldr (.) id

(!:) :: IM.Key -> Int -> GL -> GL
(!:) k i = IM.alter (\kϵ -> Just$case kϵ of {Nothing -> [i]; Just is -> i:is}) k

(@!) :: IM.Key -> Int -> Movs -> Movs
(@!) k i = IM.alter (\kϵ -> Just$case kϵ of {Nothing -> IS.singleton i; Just is -> IS.insert i is}) k

dec :: IM.Key -> IM.IntMap Int -> IM.IntMap Int
dec = IM.alter (\k -> case k of {Nothing -> Nothing;Just d -> Just$d-1})

inc :: IM.Key -> IM.IntMap Int -> IM.IntMap Int
inc = IM.alter (\k -> case k of {Nothing -> Nothing;Just d -> Just$d+1})

-- | To be called in reverse order, init with liveOut for the block
build :: (Arch (p (ControlAnn, NLiveness)), Copointed p) => IS.IntSet -> St -> [p (ControlAnn, NLiveness)] -> (IS.IntSet, St)
build l st [] = (l, st)
build l (St ml as al mv ds i wk s a) (isn:isns) | isM isn =
    let ca = fst (copoint isn)
        nl = snd (copoint isn)
        u = usesNode ca
        d = defsNode ca
        lm = l IS.\\ u
        nIx = nx nl
        ml' = thread [ kϵ @! nIx | kϵ <- IS.toList (u `IS.union` d) ] ml
        le = lm `IS.union` d
        st' = St ml' as al (mapWl (IS.insert nIx) mv) ds i wk s a
        st'' = thread [ addEdge lϵ dϵ | lϵ <- IS.toList le, dϵ <- IS.toList d ] st'
        l' = u `IS.union` (lm IS.\\ d)
    in build l' st'' isns
                                  | otherwise =
    let ca = fst (copoint isn)
        u = usesNode ca
        d = defsNode ca
        le = l `IS.union` d
        st' = St ml as al mv ds i wk s a
        st'' = thread [ addEdge lϵ dϵ | lϵ <- IS.toList le, dϵ <- IS.toList d ] st'
        l' = u `IS.union` (l IS.\\ d)
    in build l' st'' isns

-- TODO: Memb?
addEdge :: Int -> Int -> St -> St
addEdge u v st@(St ml as al mv ds i wk s a) =
    if (u, v) `S.notMember` as && u /= v
        then
            let as' = as `S.union` S.fromList [(u,v), (v, u)]
                uC = u `IS.notMember` pre wk
                vC = v `IS.notMember` pre wk
                al' = (if uC then u !: v else id)$(if vC then v !: u else id) al
                ds' = (if uC then inc u else id)$(if vC then inc v else id) ds
            in St ml as' al' mv ds' i wk s a
        else st

mkWorklist :: St -> St
mkWorklist st@(St _ _ _ _ ds i wk _ _) =
    let i' = []
        wk' = thread [ (case () of { _ | ds IM.! n >= ᴋ -> mapSp; _ | isMR n st -> mapFr; _-> mapSimp}) (IS.insert n) | n <- i] wk
    in st { initial = i', wkls = wk' }

-- same for xmm0, r15
ᴋ = 16

isMR :: Int -> St -> Bool
isMR i st = nodeMoves i st /= IS.empty

nodeMoves :: Int -> St -> IS.IntSet
nodeMoves n (St ml _ _ mv _ _ _ _ _) = (ml IM.! n) `IS.intersection` (actv mv `IS.union` wl mv)

simplify :: St -> St
simplify s@(St _ _ al _ ds _ wk@(Wk _ _ _ stϵ) st _) | Just (n,ns) <- IS.minView stϵ =
    let ds' = thread [ dec m | m <- al IM.! n ] ds
    in s { wkls = wk { simp = ns }, stack = n:st, degs = ds' }
                                                   | otherwise = s

-- decrement degree
ddg :: Int -> St -> St
ddg m s =
    let d = degs s
        s' = s { degs = dec m d }
    in if d IM.! m == ᴋ
        then let s'' = enaMv (m:(aL s IM.! m)) s'
             in mapWk (mapSp (IS.delete m).(if isMR m s'' then mapFr else mapSimp) (IS.insert m)) s''
        else s'

-- enable moves
enaMv :: [Int] -> St -> St
enaMv ns = thread (fmap g ns) where
    g n st = let ms = IS.toList (nodeMoves n st) in thread (fmap h ms) st
        where h m stϵ | m `IS.member` actv(mvS stϵ) = mapMv (mapWl (IS.insert m) . mapActv (IS.delete m)) st
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
