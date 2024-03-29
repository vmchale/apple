module IR.CF ( mkControlFlow
             ) where

import           CF
-- seems to pretty clearly be faster
import           Control.Monad.State.Strict (State, evalState, gets, modify)
import qualified Data.IntSet                as IS
import qualified Data.Map                   as M
import           Data.Tuple.Extra           (first3, fst3, second3, snd3, thd3, third3)
import           IR

-- map of labels by node
type FreshM = State (Int, M.Map Label Int, M.Map Label [Int])

runFreshM :: FreshM a -> a
runFreshM = flip evalState (0, mempty, mempty)

mkControlFlow :: [Stmt] -> [(Stmt, ControlAnn)]
mkControlFlow instrs = runFreshM (broadcasts instrs *> addControlFlow instrs)

getFresh :: FreshM Int
getFresh = gets fst3 <* modify (first3 (+1))

lookupLabel :: Label -> FreshM Int
lookupLabel l = gets (M.findWithDefault (error "Internal error in control-flow graph: node label not in map.") l . snd3)

lC :: Label -> FreshM [Int]
lC l = gets (M.findWithDefault (error "Internal error in CF graph: node label not in map.") l . thd3)

broadcast :: Int -> Label -> FreshM ()
broadcast i l = modify (second3 (M.insert l i))

b3 :: Int -> Label -> FreshM ()
b3 i l = modify (third3 (M.alter (\k -> Just$case k of {Nothing -> [i]; Just is -> i:is}) l))

-- | Pair 'Stmt's with a unique node name and a list of all possible
-- destinations.
addControlFlow :: [Stmt] -> FreshM [(Stmt, ControlAnn)]
addControlFlow [] = pure []
addControlFlow ((L l):stmts) = do
    { i <- lookupLabel l
    ; (f, stmts') <- next stmts
    ; pure ((L l, ControlAnn i (f []) (UD IS.empty IS.empty IS.empty IS.empty)):stmts')
    }
addControlFlow (J l:stmts) = do
    { i <- getFresh
    ; nextStmts <- addControlFlow stmts
    ; l_i <- lookupLabel l
    ; pure ((J l, ControlAnn i [l_i] (UD IS.empty IS.empty IS.empty IS.empty)):nextStmts)
    }
addControlFlow (C l:stmts) = do
    { i <- getFresh
    ; nextStmts <- addControlFlow stmts
    ; l_i <- lookupLabel l
    ; pure ((C l, ControlAnn i [l_i] (UD IS.empty IS.empty IS.empty IS.empty)):nextStmts)
    }
addControlFlow (R l:stmts) = do
    { i <- getFresh
    ; nextStmts <- addControlFlow stmts
    ; l_is <- lC l
    ; pure ((R l, ControlAnn i l_is (UD IS.empty IS.empty IS.empty IS.empty)):nextStmts)
    }
addControlFlow (MJ e l:stmts) = do
    { i <- getFresh
    ; (f, stmts') <- next stmts
    ; l_i <- lookupLabel l
    ; pure ((MJ e l, ControlAnn i (f [l_i]) (UD (uE e) IS.empty IS.empty IS.empty)):stmts')
    }
addControlFlow (stmt:stmts) = do
    { i <- getFresh
    ; (f, stmts') <- next stmts
    ; pure ((stmt, ControlAnn i (f []) (UD (uses stmt) IS.empty (defs stmt) IS.empty)):stmts')
    }

uE :: Exp -> IS.IntSet
uE (EAt (AP _ Nothing (Just m)))  = IS.singleton m
uE (EAt (AP _ (Just e) (Just m))) = IS.insert m$uE e
uE (EAt (AP _ Nothing Nothing))   = IS.empty
uE (EAt (AP _ (Just e) Nothing))  = uE e
uE (IRel _ e0 e1)                 = uE e0<>uE e1
uE Reg{}                          = IS.empty
uE ConstI{}                       = IS.empty
uE Is{}                           = IS.empty
uE (IB _ e0 e1)                   = uE e0<>uE e1
uE (FRel _ e0 e1)                 = uF e0 <> uF e1
uE (IRFloor e)                    = uF e
uE (IU _ e)                       = uE e
uE LA{}                           = IS.empty

uF :: FExp -> IS.IntSet
uF (FAt (AP _ Nothing (Just m)))  = IS.singleton m
uF (FAt (AP _ (Just e) (Just m))) = IS.insert m$uE e
uF (FAt (AP _ (Just e) Nothing))  = uE e
uF (FAt (AP _ Nothing Nothing))   = IS.empty
uF ConstF{}                       = IS.empty
uF FReg{}                         = IS.empty
uF (FU _ e)                       = uF e
uF (FConv e)                      = uE e
uF (FB _ e0 e1)                   = uF e0<>uF e1

uA (AP _ (Just e) (Just m)) = IS.insert m $ uE e
uA (AP _ (Just e) Nothing)  = uE e
uA (AP _ Nothing (Just m))  = IS.singleton m
uA _                        = IS.empty

uses :: Stmt -> IS.IntSet
uses (Ma _ _ e)      = uE e
uses (MX _ e)        = uF e
uses (MT _ e)        = uE e
uses (Wr a e)        = uA a <> uE e
uses (RA l)          = IS.singleton l
uses (Cmov e0 _ e1)  = uE e0<>uE e1
uses (Fcmov e0 _ e1) = uE e0<>uF e1
uses Sa{}            = IS.empty
uses (WrF a e)       = uA a <> uF e
uses (Cpy d s e)     = uA d <> uA s <> uE e
uses Pop{}           = IS.empty
uses IRnd{}          = IS.empty
uses (Cset _ e)      = uE e

defs :: Stmt -> IS.IntSet
defs (Ma a _ _) = IS.singleton a
defs _          = IS.empty

next :: [Stmt] -> FreshM ([Int] -> [Int], [(Stmt, ControlAnn)])
next stmts = do
    nextStmts <- addControlFlow stmts
    case nextStmts of
        []       -> pure (id, [])
        (stmt:_) -> pure ((node (snd stmt) :), nextStmts)

-- | Construct map assigning labels to their node name.
broadcasts :: [Stmt] -> FreshM [Stmt]
broadcasts [] = pure []
broadcasts (stmt@(C l):stmt'@(L retL):stmts) = do
    { i <- getFresh
    ; broadcast i retL; b3 i l
    ; (stmt:).(stmt':) <$> broadcasts stmts
    }
broadcasts (stmt@(L l):stmts) = do
    { i <- getFresh
    ; broadcast i l
    ; (stmt :) <$> broadcasts stmts
    }
broadcasts (asm:asms) = (asm :) <$> broadcasts asms
