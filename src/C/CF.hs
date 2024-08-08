module C.CF ( cfC ) where

import           C
import           CF
import           CF.AL
import           Control.Monad.Trans.State.Strict (State, evalState, gets, modify, state)
import           Data.Bifunctor             (first)
import           Data.Functor               (($>))
import qualified Data.IntMap                as IM
import qualified Data.IntSet                as IS
import           Data.List                  (uncons)
import qualified Data.Map                   as M
import           Data.Tuple.Extra           (second3, snd3, thd3, third3)
import           Data.Void                  (Void)
import           Q

type N=Int

-- map of labels by node
type FreshM = State (N, M.Map Label N, M.Map Label [N])

runFreshM :: FreshM a -> a
runFreshM = flip evalState (0, mempty, mempty)

cfC :: [CS ()] -> ([N], [CS ControlAnn], IM.IntMap (ControlAnn, Liveness))
cfC cs = let cfs = mkControlFlow cs in (inspectOrder cfs, cfs, initLiveness cfs)

mkControlFlow :: [CS ()] -> [CS ControlAnn]
mkControlFlow instrs = runFreshM (brs instrs *> addCF instrs)

getFresh :: FreshM N
getFresh = state (\(i,m0,m1) -> (i,(i+1,m0,m1)))

fm :: Label -> FreshM N
fm l = do {i <- getFresh; br i l $> i}

ll :: Label -> FreshM N
ll l = gets (M.findWithDefault (error "Internal error in control-flow graph: node label not in map.") l . snd3)

lC :: Label -> FreshM [N]
lC l = gets (M.findWithDefault (error "Internal error in CF graph: node label not in map.") l . thd3)

br :: N -> Label -> FreshM ()
br i l = modify (second3 (M.insert l i))

b3 :: N -> Label -> FreshM ()
b3 i l = modify (third3 (M.alter (\k -> Just$case k of {Nothing -> [i]; Just is -> i:is}) l))

mC :: ([N] -> [N]) -> ControlAnn -> ControlAnn
mC f (ControlAnn l ds udϵ) = ControlAnn l (f ds) udϵ

addH n = mC (n:)

unsnoc :: [a] -> ([a], a)
unsnoc [x]    = ([], x)
unsnoc (x:xs) = first (x:) $ unsnoc xs
unsnoc _      = error "Internal error: unsnoc called on empty list."

emptyL :: Liveness
emptyL = Liveness IS.empty IS.empty IS.empty IS.empty

initLiveness :: [CS ControlAnn] -> IM.IntMap (ControlAnn, Liveness)
initLiveness = IM.fromList . go where
    go []                          = []
    go (For ann _ _ _ _ ss:cs)     = (node ann, (ann, emptyL)):go ss++go cs
    go (F2or ann _ _ _ _ ss s1:cs) = (node ann, (ann, emptyL)):go s1++go ss++go cs
    go (For1 ann _ _ _ _ ss:cs)    = (node ann, (ann, emptyL)):go ss++go cs
    go (Rof ann _ _ ss:cs)      = (node ann, (ann, emptyL)):go ss++go cs
    go (Rof1 ann _ _ ss:cs)     = (node ann, (ann, emptyL)):go ss++go cs
    go (While ann _ _ _ ss:cs)     = (node ann, (ann, emptyL)):go ss++go cs
    go (WT ann _ ss:cs)         = (node ann, (ann, emptyL)):go ss++go cs
    go (If ann _ ss ss':cs)        = (node ann, (ann, emptyL)):go ss++go ss'++go cs
    go (Ifn't ann _ ss:cs)         = (node ann, (ann, emptyL)):go ss++go cs
    go (Def ann _ ss:cs)           = (node ann, (ann, emptyL)):go ss++go cs
    go (c:cs)                      = let x=lann c in (node x, (x, emptyL)):go cs

inspectOrder :: [CS ControlAnn] -> [N]
inspectOrder (For ann _ _ _ _ ss:cs)     = node ann:inspectOrder ss++inspectOrder cs
inspectOrder (F2or ann _ _ _ _ ss s1:cs) = node ann:inspectOrder s1++inspectOrder ss++inspectOrder cs
inspectOrder (Rof ann _ _ ss:cs)      = node ann:inspectOrder ss++inspectOrder cs
inspectOrder (Rof1 ann _ _ ss:cs)     = node ann:inspectOrder ss++inspectOrder cs
inspectOrder (For1 ann _ _ _ _ ss:cs)    = node ann:inspectOrder ss++inspectOrder cs
inspectOrder (While ann _ _ _ ss:cs)     = node ann:inspectOrder ss++inspectOrder cs
inspectOrder (WT ann _ ss:cs)         = node ann:inspectOrder ss++inspectOrder cs
inspectOrder (If ann _ ss ss':cs)        = node ann:inspectOrder ss++inspectOrder ss'++inspectOrder cs
inspectOrder (Ifn't ann _ ss:cs)         = node ann:inspectOrder ss++inspectOrder cs
inspectOrder (Def ann _ ss:cs)           = node ann:inspectOrder ss++inspectOrder cs
inspectOrder (c:cs)                      = node (lann c):inspectOrder cs
inspectOrder []                          = []

tieBranch :: N -> ([N] -> [N]) -> [CS ()] -> FreshM ([N] -> [N], [CS ControlAnn])
tieBranch i f ss = do
    preSs <- addCF ss
    pure $ case uncons preSs of
        Just (i1, _) ->
            let hi=node (lann i1)
                (ss',l) = unsnoc preSs
                l' = fmap (mC f) l
            in ((hi:),) $ case uncons ss' of
                Nothing       -> ss'++[l']
                Just (hh, bs) -> let h' = fmap (addH i) hh in h':bs++[l']
        Nothing -> (id, preSs)

tieBody :: N -> ([N] -> [N]) -> [CS ()] -> FreshM ([N] -> [N], [CS ControlAnn])
tieBody h f ss = do
    preSs <- addCF ss
    case uncons preSs of
        Just (i1, _) ->
            let hi=node (lann i1)
                (ss',l) = unsnoc preSs
                l'=fmap (mC ((h:).f)) l
                ss''=ss'++[l']
            in pure ((hi:), ss'')
        Nothing -> pure (id, [])

-- | Pair 'CS with a unique node name and a list of all possible
-- destinations.
addCF :: [CS ()] -> FreshM [CS ControlAnn]
addCF [] = pure []
addCF ((Def _ l ss):stmts) = do
    i <- ll l
    nextStmts <- addCF stmts
    preSs <- addCF ss
    case uncons preSs of
        Nothing -> undefined
        Just (h, _) ->
            let hi=node (lann h)
                (ss',lϵ) = unsnoc preSs
            in do
                l_is <- lC l
                let l'= fmap (mC (const l_is)) lϵ
                    ss''=ss'++[l']
                pure (Def (ControlAnn i [hi] (UD IS.empty IS.empty IS.empty IS.empty)) l ss'':nextStmts)
addCF (G _ l r:stmts) = do
    i <- ll r
    nextStmts <- addCF stmts
    l_i <- ll l
    pure (G (ControlAnn i [l_i] (UD IS.empty IS.empty IS.empty IS.empty)) l r:nextStmts)
addCF ((For _ t el c eu ss):stmts) = do
    i <- getFresh
    (f, stmts') <- next stmts
    (h, ss') <- tieBody i f ss
    pure $ For (ControlAnn i (f (h [])) udϵ) t el c eu ss':stmts'
  where
    udϵ = UD (uE el<>uE eu) IS.empty IS.empty IS.empty
addCF ((F2or _ t el c eu s1 ss):stmts) = do
    i <- getFresh
    (f, stmts') <- next stmts
    (h1, s1') <- tieBranch i f s1
    (h, ss') <- tieBody i f ss
    let ss'' = case uncons ss' of
            Nothing        -> []
            Just (hb, ssϵ) -> fmap (mC h1) hb:ssϵ
    pure $ F2or (ControlAnn i (f (h (h1 []))) udϵ) t el c eu s1' ss'':stmts'
  where
    udϵ = UD (uE el<>uE eu) IS.empty IS.empty IS.empty
addCF ((For1 _ t el c eu ss):stmts) = do
    i <- getFresh
    (f, stmts') <- next stmts
    (h, ss') <- tieBody i f ss
    pure $ For1 (ControlAnn i (f (h [])) udϵ) t el c eu ss':stmts'
  where
    udϵ = UD (uE el<>uE eu) IS.empty IS.empty IS.empty
addCF ((Rof _ t ec ss):stmts) = do
    i <- getFresh
    (f, stmts') <- next stmts
    (h, ss') <- tieBody i f ss
    pure $ Rof (ControlAnn i (f (h [])) udϵ) t ec ss':stmts'
  where
    udϵ = UD (uE ec) IS.empty IS.empty IS.empty
addCF ((Rof1 _ t ec ss):stmts) = do
    i <- getFresh
    (f, stmts') <- next stmts
    (h, ss') <- tieBody i f ss
    pure $ Rof1 (ControlAnn i (f (h [])) udϵ) t ec ss':stmts'
  where
    udϵ = UD (uE ec) IS.empty IS.empty IS.empty
addCF ((While _ t c ed ss):stmts) = do
    i <- getFresh
    (f, stmts') <- next stmts
    (h, ss') <- tieBody i f ss
    pure $ While (ControlAnn i (f (h [])) udϵ) t c ed ss':stmts'
  where
    udϵ = UD (uE ed) IS.empty IS.empty IS.empty
addCF ((WT _ t ss):stmts) = do
    i <- getFresh
    (f, stmts') <- next stmts
    (h, ss') <- tieBody i f ss
    pure $ WT (ControlAnn i (f (h [])) udϵ) t ss':stmts'
  where
    udϵ = UD IS.empty IS.empty IS.empty IS.empty
addCF (If _ p b0 b1:stmts) = do
    i <- getFresh
    (f, stmts') <- next stmts
    (h0, b0') <- tieBranch i f b0
    (h1, b1') <- tieBranch i f b1
    let fnext = if null b0 || null b1 then f else id
    pure $ If (ControlAnn i (fnext (h0 (h1 []))) udϵ) p b0' b1':stmts'
  where
    udϵ = UD (uB p) IS.empty IS.empty IS.empty
addCF (Ifn't _ p b:stmts) = do
    i <- getFresh
    (f, stmts') <- next stmts
    (h, b') <- tieBranch i f b
    pure $ Ifn't (ControlAnn i (f (h [])) udϵ) p b':stmts'
  where
    udϵ = UD (uB p) IS.empty IS.empty IS.empty
addCF (stmt:stmts) = do
    i <- getFresh
    (f, stmts') <- next stmts
    pure ((stmt $> ControlAnn i (f []) (UD (uses stmt) IS.empty (defs stmt) IS.empty)):stmts')

uE :: CE -> IS.IntSet
uE ConstI{}      = IS.empty
uE LA{}          = IS.empty
uE (EAt a)       = uA a
uE Tmp{}         = IS.empty
uE (Bin _ e0 e1) = uE e0<>uE e1
uE (CFloor e0)   = uF e0
uE (DP _ e)      = uE e

uF :: CFE FTemp x CE -> IS.IntSet
uF ConstF{}       = IS.empty
uF FTmp{}         = IS.empty
uF (FAt a)        = uA a
uF (FUn _ e)      = uF e
uF (FBin _ e0 e1) = uF e0<>uF e1
uF (IE e)         = uE e

uF2 :: CFE F2Temp x Void -> IS.IntSet
uF2 FTmp{}         = IS.empty
uF2 (FAt a)        = uA a
uF2 ConstF{}       = IS.empty
uF2 (FUn _ e)      = uF2 e
uF2 (FBin _ e0 e1) = uF2 e0<>uF2 e1

m'insert (Just l) a = sinsert l a
m'insert Nothing a  = a

uA (ARnk _ (Just l))  = singleton l
uA (ARnk _ Nothing)   = IS.empty
uA (ADim _ d l)       = m'insert l $ uE d
uA (TupM _ (Just l))  = singleton l
uA (TupM _ Nothing)   = IS.empty
uA (AElem _ r ei l _) = m'insert l (uE r<>uE ei)
uA (Raw _ e l _)      = m'insert l (uE e)
uA (At _ ss ixs l _)  = m'insert l (uE@<>ss<>uE@<>ixs)

uses :: CS a -> IS.IntSet
uses (Ma _ _ _ r n _)      = uE r<>uE n
uses MaΠ{}                 = IS.empty
uses (MX _ _ e)            = uF e
uses (MX2 _ _ e)           = uF2 e
uses (Wr _ a e)            = uA a <> uE e
uses (RA _ l)              = singleton l
uses (Cmov _ e0 _ e1)      = uB e0<>uE e1
uses (Fcmov _ e0 _ e1)     = uB e0<>uF e1
uses (WrF _ a e)           = uA a <> uF e
uses (Wr2F _ a e)          = uA a <> uF2 e
uses (Cset _ e _)          = uB e
uses (MT _ _ e)            = uE e
uses (MB _ _ e)            = uB e
uses (WrP _ a e)           = uA a<>uB e
uses Rnd{}                 = IS.empty
uses FRnd{}                = IS.empty
uses (PlProd _ _ es)       = uE@<>es
uses (SZ _ _ _ e (Just l)) = sinsert l (uE e)
uses (SZ _ _ _ e Nothing)  = uE e
uses (Pop8 _ e)            = uE e
uses (Sa8 _ _ e)           = uE e
uses (Pop _ e)             = uE e
uses (Sa _ _ e)            = uE e
uses (CpyD _ d s n)        = uA d<>uA s<>uE n
uses (CpyE _ d s n _)      = uA d<>uA s<>uE n

uB :: PE -> IS.IntSet
uB (PAt a)        = uA a
uB BConst{}       = IS.empty
uB (IRel _ e0 e1) = uE e0<>uE e1
uB (FRel _ e0 e1) = uF e0<>uF e1
uB (Boo _ e0 e1)  = uB e0<>uB e1
uB (IUn _ e)      = uE e
uB Is{}           = IS.empty
uB (BU _ e)       = uB e

defs :: CS a -> IS.IntSet
defs (Ma _ a _ _ _ _) = singleton a
defs (MaΠ _ a _ _)    = singleton a
defs _                = IS.empty

next :: [CS ()] -> FreshM ([N] -> [N], [CS ControlAnn])
next stmts = do
    nextStmts <- addCF stmts
    case nextStmts of
        []       -> pure (id, [])
        (stmt:_) -> pure ((node (lann stmt) :), nextStmts)

-- | Construct map assigning labels to their node name.
brs :: [CS ()] -> FreshM ()
brs []                           = pure ()
brs (G _ l retL:stmts)           = do {i <- fm retL; b3 i l; brs stmts}
brs (Def _ f b:stmts)            = fm f *> brs b *> brs stmts
brs (Rof _ _ _ ss:stmts)      = brs ss *> brs stmts
brs (Rof1 _ _ _ ss:stmts)     = brs ss *> brs stmts
brs (For _ _ _ _ _ ss:stmts)     = brs ss *> brs stmts
brs (F2or _ _ _ _ _ ss s1:stmts) = brs ss *> brs s1 *> brs stmts
brs (For1 _ _ _ _ _ ss:stmts)    = brs ss *> brs stmts
brs (While _ _ _ _ ss:stmts)     = brs ss *> brs stmts
brs (WT _ _ ss:stmts)         = brs ss *> brs stmts
brs (If _ _ ss ss':stmts)        = brs ss *> brs ss' *> brs stmts
brs (Ifn't _ _ ss:stmts)         = brs ss *> brs stmts
brs (_:asms)                     = brs asms
