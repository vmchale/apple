module C.CF ( cfC ) where

import           C
import           CF
import           CF.AL
-- seems to pretty clearly be faster
import           Control.Monad.State.Strict (State, evalState, gets, modify, state)
import           Data.Bifunctor             (first, second)
import           Data.Functor               (($>))
import qualified Data.IntSet                as IS
import           Data.List                  (uncons)
import qualified Data.Map                   as M
import           Data.Tuple.Extra           (second3, snd3, thd3, third3)

type N=Int

-- map of labels by node
type FreshM = State (N, M.Map Label N, M.Map Label [N])

runFreshM :: FreshM a -> a
runFreshM = flip evalState (0, mempty, mempty)

cfC :: [CS ()] -> ([N], [CS ControlAnn])
cfC cs = let cfs = mkControlFlow cs in (inspectOrder cfs, cfs)

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

addH :: Int -> ControlAnn -> ControlAnn
addH n = mC (n:)

mC :: ([N] -> [N]) -> ControlAnn -> ControlAnn
mC f (ControlAnn l ds udϵ) = ControlAnn l (f ds) udϵ

unsnoc :: [a] -> ([a], a)
unsnoc [x]    = ([], x)
unsnoc (x:xs) = first (x:) $ unsnoc xs
unsnoc _      = error "Internal error: unsnoc called on empty list."

inspectOrder :: [CS ControlAnn] -> [N]
inspectOrder (For ann _ _ _ _ ss:cs) = node ann:inspectOrder ss++inspectOrder cs
inspectOrder (For1{}:cs)             = undefined
inspectOrder (While{}:cs)            = undefined
inspectOrder (If{}:cs)               = undefined
inspectOrder (Ifn't{}:cs)            = undefined
inspectOrder (Def{}:cs)              = undefined
inspectOrder (G{}:cs)                = undefined
inspectOrder (c:cs)                  = node (lann c):inspectOrder cs
inspectOrder []                      = []

-- | Pair 'CS with a unique node name and a list of all possible
-- destinations.
addCF :: [CS ()] -> FreshM [CS ControlAnn]
addCF [] = pure []
addCF ((Def _ l ss):stmts) =
    case uncons ss of
        Nothing -> undefined
addCF (G _ l r:stmts) = undefined
addCF ((For _ t el c eu ss):stmts) = do
    i <- getFresh
    (f, stmts') <- next stmts
    preSs <- addCF ss
    case uncons preSs of
        Just (i1, _) ->
            let hi=node (lann i1)
                (ss',l) = unsnoc preSs
                l'=fmap (addH hi.mC f) l
                ss''=ss'++[l']
                ub=foldMap (usesNode.ud.lann) ss''; db=foldMap (defsNode.ud.lann) ss''
            in pure (For (ControlAnn i (f [hi]) (UD (uE el<>uE eu<>ub) IS.empty db IS.empty)) t el c eu ss'':stmts')
addCF (For1{}:_) = undefined
addCF (While{}:_) = undefined
addCF (If{}:_) = undefined
addCF (Ifn't{}:_) = undefined
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

uF :: CFE -> IS.IntSet
uF ConstF{}       = IS.empty
uF FTmp{}         = IS.empty
uF (FAt a)        = uA a
uF (FUn _ e)      = uF e
uF (FBin _ e0 e1) = uF e0<>uF e1
uF (IE e)         = uE e

m'insert (Just l) a = sinsert l a
m'insert Nothing a  = a

uA (ARnk _ (Just l))  = singleton l
uA (ARnk _ Nothing)   = IS.empty
uA (ADim _ d l)       = m'insert l $ uE d
uA (TupM _ (Just l))  = singleton l
uA (TupM _ Nothing)   = IS.empty
uA (AElem _ r ei l _) = m'insert l (uE r<>uE ei)
uA (Raw _ e l _)      = m'insert l (uE e)
uA (At _ ss ixs l _)  = m'insert l (foldMap uE ss<>foldMap uE ixs)

uses :: CS a -> IS.IntSet
uses (Ma _ _ _ r n _)      = uE r<>uE n
uses (MX _ _ e)            = uF e
uses (Wr _ a e)            = uA a <> uE e
uses (RA _ l)              = singleton l
uses (Cmov _ e0 _ e1)      = uB e0<>uE e1
uses (Fcmov _ e0 _ e1)     = uB e0<>uF e1
uses (WrF _ a e)           = uA a <> uF e
uses (Cset _ e _)          = uB e
uses (MT _ _ e)            = uE e
uses (MB _ _ e)            = uB e
uses (WrP _ a e)           = uA a<>uB e
uses Rnd{}                 = IS.empty
uses FRnd{}                = IS.empty
uses (PlProd _ _ es)       = foldMap uE es
uses (SZ _ _ _ e (Just l)) = sinsert l (uE e)
uses (SZ _ _ _ e Nothing)  = uE e
uses (Pop _ e)             = uE e
uses (Sa _ _ e)            = uE e
uses (CpyD _ d s n)        = uA d<>uA s<>uE n
uses (CpyE _ d s n _)      = uA d<>uA s<>uE n

uB :: PE -> IS.IntSet
uB (PAt a)        = uA a
uB BConst{}       = IS.empty
uB (IRel _ e0 e1) = uE e0<>uE e1
uB (FRel _ e0 e1) = uF e0 <> uF e1

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
brs []                        = pure ()
brs (G _ l retL:stmts)        = do {i <- fm retL; b3 i l; brs stmts}
brs (Def _ f b:stmts)         = fm f *> brs b *> brs stmts
brs (For _ _ _ _ _ ss:stmts)  = brs ss *> brs stmts
brs (For1 _ _ _ _ _ ss:stmts) = brs ss *> brs stmts
brs (While _ _ _ _ ss:stmts)  = brs ss *> brs stmts
brs (If _ _ ss ss':stmts)     = brs ss *> brs ss' *> brs stmts
brs (Ifn't _ _ ss:stmts)      = brs ss *> brs stmts
brs (_:asms)                  = brs asms
