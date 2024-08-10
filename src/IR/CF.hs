module IR.CF ( rToInt, fToInt
             , mkControlFlow
             ) where

import           CF
-- seems to pretty clearly be faster
import           Control.Monad.Trans.State.Strict (State, gets, modify, runState, state)
import           Data.Bifunctor             (second)
import           Data.Functor               (($>))
import qualified Data.IntSet                as IS
import qualified Data.Map                   as M
import           Data.Tuple.Extra           (fst3, second3, snd3, thd3, third3)
import           Data.Void                  (absurd)
import           IR

type N=Int

-- map of labels by node
type FreshM = State (Int, M.Map Label N, M.Map Label [N])

runFreshM :: FreshM a -> (a, Int)
runFreshM = second fst3.flip runState (0, mempty, mempty)

mkControlFlow :: [Stmt] -> ([(Stmt, ControlAnn)], Int)
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

-- | Pair 'Stmt's with a unique node name and a list of all possible
-- destinations.
addCF :: [Stmt] -> FreshM [(Stmt, ControlAnn)]
addCF [] = pure []
addCF ((L l):stmts) = do
    { i <- ll l
    ; (f, stmts') <- next stmts
    ; pure ((L l, ControlAnn i (f []) (UD IS.empty IS.empty IS.empty IS.empty)):stmts')
    }
addCF (J l:stmts) = do
    { i <- getFresh
    ; ns <- addCF stmts
    ; l_i <- ll l
    ; pure ((J l, ControlAnn i [l_i] (UD IS.empty IS.empty IS.empty IS.empty)):ns)
    }
addCF (C l:stmts) = do
    { i <- getFresh
    ; ns <- addCF stmts
    ; l_i <- ll l
    ; pure ((C l, ControlAnn i [l_i] (UD IS.empty IS.empty IS.empty IS.empty)):ns)
    }
addCF (R l:stmts) = do
    { i <- getFresh
    ; ns <- addCF stmts
    ; l_is <- lC l
    ; pure ((R l, ControlAnn i l_is (UD IS.empty IS.empty IS.empty IS.empty)):ns)
    }
addCF (MJ e l:stmts) = do
    { i <- getFresh
    ; (f, stmts') <- next stmts
    ; l_i <- ll l
    ; pure ((MJ e l, ControlAnn i (f [l_i]) (UD (uE e) IS.empty IS.empty IS.empty)):stmts')
    }
addCF (stmt:stmts) = do
    { i <- getFresh
    ; (f, stmts') <- next stmts
    ; pure ((stmt, ControlAnn i (f []) (UD (uses stmt) (usesF stmt) (defs stmt) (defsF stmt))):stmts')
    }

rToInt :: Temp -> Int
rToInt (ITemp i) = i; rToInt (ATemp i) = i
rToInt C0 = -1; rToInt C1 = -2; rToInt C2 = -3; rToInt C3 = -4
rToInt C4 = -5; rToInt C5 = -6; rToInt CRet = -7

fToInt :: FTemp -> Int
fToInt (FTemp i) = i
fToInt F0 = -8; fToInt F1 = -9; fToInt F2 = -10; fToInt F3 = -11
fToInt F4 = -12; fToInt F5 = -13; fToInt FRet = -14; fToInt FRet1 = -15

singleton :: Temp -> IS.IntSet
singleton = IS.singleton . rToInt

fsingleton :: FTemp -> IS.IntSet
fsingleton = IS.singleton . fToInt

f2is :: F2 -> IS.IntSet
f2is = IS.singleton . f2ToInt

uE :: Exp -> IS.IntSet
uE (Reg r)        = singleton r
uE ConstI{}       = IS.empty
uE (IB _ e0 e1)   = uE e0 <> uE e1
uE (IRel _ e0 e1) = uE e0 <> uE e1
uE (Is t)         = singleton t
uE (IU _ e)       = uE e
uE (BU _ e)       = uE e
uE LA{}           = IS.empty
uE (IRFloor x)    = uFR x
uE (EAt a)        = uA a
uE (BAt a)        = uA a
uE (FRel _ x0 x1) = uFR x0<>uFR x1

uFF :: Exp -> IS.IntSet
uFF (IRFloor e)    = uF e
uFF ConstI{}       = IS.empty
uFF Reg{}          = IS.empty
uFF (IB _ e0 e1)   = uFF e0<>uFF e1
uFF (IRel _ e0 e1) = uFF e0<>uFF e1
uFF (FRel _ e0 e1) = uF e0<>uF e1
uFF (IU _ e)       = uFF e
uFF (BU _ e)       = uFF e
uFF LA{}           = IS.empty
uFF Is{}           = IS.empty
uFF (EAt e)        = uAF e
uFF (BAt e)        = uAF e

uF2 :: F2E -> IS.IntSet
uF2 ConstF{}     = IS.empty
uF2 (FReg t)     = f2is t
uF2 (FB _ e0 e1) = uF2 e0<>uF2 e1
uF2 (FU _ e)     = uF2 e
uF2 (FAt e)      = uAF e
uF2 (FConv x)    = absurd x

uF :: FE -> IS.IntSet
uF ConstF{}     = IS.empty
uF (FB _ e0 e1) = uF e0 <> uF e1
uF (FConv e)    = uFF e
uF (FReg t)     = fsingleton t
uF (FU _ e)     = uF e
uF (FAt a)      = uAF a

uAF :: AE -> IS.IntSet
uAF (AP _ (Just e) _) = uFF e
uAF _                 = IS.empty

uA :: AE -> IS.IntSet
uA (AP t Nothing _)  = singleton t
uA (AP t (Just e) _) = IS.insert (rToInt t) (uE e)

uFR :: FE -> IS.IntSet
uFR (FAt a)      = uA a
uFR (FConv e)    = uE e
uFR (FB _ e0 e1) = uFR e0<>uFR e1
uFR FReg{}       = IS.empty
uFR (FU _ e)     = uFR e
uFR ConstF{}     = IS.empty

uFR2 :: F2E -> IS.IntSet
uFR2 (FAt a)      = uA a
uFR2 FReg{}       = IS.empty
uFR2 (FB _ e0 e1) = uFR2 e0<>uFR2 e1
uFR2 ConstF{}     = IS.empty
uFR2 (FU _ e)     = uFR2 e
uFR2 (FConv x)    = absurd x

uses, defs :: Stmt -> IS.IntSet
uses IRnd{}         = IS.empty
uses FRnd{}         = IS.empty
uses L{}            = IS.empty
uses J{}            = IS.empty
uses (MJ e _)       = uE e
uses (MT _ e)       = uE e
uses (MX _ e)       = uFR e
uses (MX2 _ e)      = uFR2 e
uses (Ma _ _ e)     = uE e
uses (Free t)       = singleton t
uses RA{}           = IS.empty
uses (Wr a e)       = uA a<>uE e
uses (WrF a e)      = uA a<>uFR e
uses (WrF2 a e)     = uA a<>uFR2 e
uses (WrB a e)      = uA a<>uE e
uses (Sa _ e)       = uE e
uses (Pop e)        = uE e
uses (Sa8 _ e)      = uE e
uses (Pop8 e)       = uE e
uses S2{}           = IS.empty
uses (Cmov e0 _ e1) = uE e0<>uE e1
uses (Fcmov e _ x)  = uE e<>uFR x
uses R{}            = IS.empty
uses C{}            = IS.empty
uses (Cset _ e)     = uE e
uses (Cpy a0 a1 e)  = uA a0<>uA a1<>uE e
uses (Cpy1 a0 a1 e) = uA a0<>uA a1<>uE e

defs (MT t _)     = singleton t
defs (IRnd t)     = singleton t
defs (Ma _ t _)   = singleton t
defs (Cmov _ t _) = singleton t
defs (Sa8 t _)    = singleton t
defs (Cset t _)   = singleton t
defs _            = IS.empty

usesF, defsF :: Stmt -> IS.IntSet
usesF IRnd{}         = IS.empty
usesF FRnd{}         = IS.empty
usesF (MX _ e)       = uF e
usesF (MX2 _ e)      = uF2 e
usesF (WrF2 a e)     = uAF a<>uF2 e
usesF L{}            = IS.empty
usesF J{}            = IS.empty
usesF (MJ e _)       = uFF e
usesF (MT _ e)       = uFF e
usesF (Ma _ _ e)     = uFF e
usesF Free{}         = IS.empty
usesF RA{}           = IS.empty
usesF (Cmov e _ e')  = uFF e<>uFF e'
usesF (Fcmov e _ x)  = uFF e<>uF x
usesF (Wr a e)       = uAF a<>uFF e
usesF (WrF a x)      = uAF a<>uF x
usesF (WrB a e)      = uAF a<>uFF e
usesF (Cset _ e)     = uFF e
usesF (Sa _ e)       = uFF e
usesF (Pop e)        = uFF e
usesF (Sa8 _ e)      = uFF e
usesF (Pop8 e)       = uFF e
usesF C{}            = IS.empty
usesF R{}            = IS.empty
usesF (Cpy a0 a1 e)  = uAF a0<>uAF a1<>uFF e
usesF (Cpy1 a0 a1 e) = uAF a0<>uAF a1<>uFF e
usesF (S2 _ _ r)     = f2is r

defsF (MX t _)      = fsingleton t
defsF (MX2 t _)     = f2is t
defsF (Fcmov _ x _) = fsingleton x
defsF (FRnd t)      = fsingleton t
defsF (S2 _ t _)    = fsingleton t
defsF _             = IS.empty

next :: [Stmt] -> FreshM ([N] -> [N], [(Stmt, ControlAnn)])
next stmts = do
    nextStmts <- addCF stmts
    case nextStmts of
        []       -> pure (id, [])
        (stmt:_) -> pure ((node (snd stmt) :), nextStmts)

-- | Construct map assigning labels to their node name.
brs :: [Stmt] -> FreshM ()
brs []                     = pure ()
brs ((C l):(L retL):stmts) = do {i <- fm retL; b3 i l; brs stmts}
brs ((L l):stmts)          = fm l *> brs stmts
brs (_:asms)               = brs asms
