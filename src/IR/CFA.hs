module IR.CFA ( mkControlFlow ) where

import           CF
import           CF.AL
-- seems to pretty clearly be faster
import           Control.Monad.State.Strict (State, evalState, gets, modify, state)
import           Data.Functor               (($>))
import qualified Data.IntSet                as IS
import qualified Data.Map                   as M
import           Data.Tuple.Extra           (second3, snd3, thd3, third3)
import           IR

type N=Int

-- map of labels by node
type FreshM = State (N, M.Map Label N, M.Map Label [N])

runFreshM :: FreshM a -> a
runFreshM = flip evalState (0, mempty, mempty)

mkControlFlow :: [Stmt] -> [(Stmt, ControlAnn)]
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
    ; nextStmts <- addCF stmts
    ; l_i <- ll l
    ; pure ((J l, ControlAnn i [l_i] (UD IS.empty IS.empty IS.empty IS.empty)):nextStmts)
    }
addCF (C l:stmts) = do
    { i <- getFresh
    ; nextStmts <- addCF stmts
    ; l_i <- ll l
    ; pure ((C l, ControlAnn i [l_i] (UD IS.empty IS.empty IS.empty IS.empty)):nextStmts)
    }
addCF (R l:stmts) = do
    { i <- getFresh
    ; nextStmts <- addCF stmts
    ; l_is <- lC l
    ; pure ((R l, ControlAnn i l_is (UD IS.empty IS.empty IS.empty IS.empty)):nextStmts)
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
    ; pure ((stmt, ControlAnn i (f []) (UD (uses stmt) IS.empty (defs stmt) IS.empty)):stmts')
    }

uE :: Exp -> IS.IntSet
uE (EAt (AP _ Nothing (Just m)))  = singleton m
uE (EAt (AP _ (Just e) (Just m))) = sinsert m$uE e
uE (EAt (AP _ Nothing Nothing))   = IS.empty
uE (EAt (AP _ (Just e) Nothing))  = uE e
uE (BAt (AP _ Nothing (Just m)))  = singleton m
uE (BAt (AP _ (Just e) (Just m))) = sinsert m$uE e
uE (BAt (AP _ Nothing Nothing))   = IS.empty
uE (BAt (AP _ (Just e) Nothing))  = uE e
uE (IRel _ e0 e1)                 = uE e0<>uE e1
uE Reg{}                          = IS.empty
uE ConstI{}                       = IS.empty
uE Is{}                           = IS.empty
uE (IB _ e0 e1)                   = uE e0<>uE e1
uE (FRel _ e0 e1)                 = uF e0 <> uF e1
uE (IRFloor e)                    = uF e
uE (IU _ e)                       = uE e
uE (BU _ e)                       = uE e
uE LA{}                           = IS.empty

uF :: FExp -> IS.IntSet
uF (FAt (AP _ Nothing (Just m)))  = singleton m
uF (FAt (AP _ (Just e) (Just m))) = sinsert m$uE e
uF (FAt (AP _ (Just e) Nothing))  = uE e
uF (FAt (AP _ Nothing Nothing))   = IS.empty
uF ConstF{}                       = IS.empty
uF FReg{}                         = IS.empty
uF (FU _ e)                       = uF e
uF (FConv e)                      = uE e
uF (FB _ e0 e1)                   = uF e0<>uF e1

uA (AP _ (Just e) (Just m)) = sinsert m $ uE e
uA (AP _ (Just e) Nothing)  = uE e
uA (AP _ Nothing (Just m))  = singleton m
uA _                        = IS.empty

uses :: Stmt -> IS.IntSet
uses (Ma _ _ e)      = uE e
uses (MX _ e)        = uF e
uses (MT _ e)        = uE e
uses (Wr a e)        = uA a <> uE e
uses (WrB a e)       = uA a <> uE e
uses (RA l)          = singleton l
uses (Cmov e0 _ e1)  = uE e0<>uE e1
uses (Fcmov e0 _ e1) = uE e0<>uF e1
uses Sa{}            = IS.empty
uses (WrF a e)       = uA a <> uF e
uses (Cpy d s e)     = uA d <> uA s <> uE e
uses Pop{}           = IS.empty
uses IRnd{}          = IS.empty
uses FRnd{}          = IS.empty
uses (Cset _ e)      = uE e
uses s               = error (show s)

defs :: Stmt -> IS.IntSet
defs (Ma a _ _) = singleton a
defs _          = IS.empty

next :: [Stmt] -> FreshM ([N] -> [N], [(Stmt, ControlAnn)])
next stmts = do
    nextStmts <- addCF stmts
    case nextStmts of
        []       -> pure (id, [])
        (stmt:_) -> pure ((node (snd stmt) :), nextStmts)

-- | Construct map assigning labels to their node name.
brs :: [Stmt] -> FreshM ()
brs []                 = pure ()
brs (C l:L retL:stmts) = do {i <- fm retL; b3 i l ; brs stmts}
brs (L l:stmts)        = fm l *> brs stmts
brs (_:asms)           = brs asms
