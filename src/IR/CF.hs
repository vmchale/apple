module IR.CF ( mkControlFlow
             ) where

import           CF
-- seems to pretty clearly be faster
import           Control.Monad.State.Strict (State, evalState, gets, modify)
import           Data.Bifunctor             (first, second)
import qualified Data.IntSet                as IS
import qualified Data.Map                   as M
import           Data.Semigroup             ((<>))
import           IR

-- map of labels by node
type FreshM = State (Int, M.Map Label Int)

runFreshM :: FreshM a -> a
runFreshM = flip evalState (0, mempty)

mkControlFlow :: [Stmt] -> [(Stmt, ControlAnn)]
mkControlFlow instrs = runFreshM (broadcasts instrs *> addControlFlow instrs)

getFresh :: FreshM Int
getFresh = gets fst <* modify (first (+1))

lookupLabel :: Label -> FreshM Int
lookupLabel l = gets (M.findWithDefault (error "Internal error in control-flow graph: node label not in map.") l . snd)

broadcast :: Int -> Label -> FreshM ()
broadcast i l = modify (second (M.insert l i))

-- | Pair 'Stmt's with a unique node name and a list of all possible
-- destinations.
addControlFlow :: [Stmt] -> FreshM [(Stmt, ControlAnn)]
addControlFlow [] = pure []
addControlFlow ((L l):stmts) = do
    { i <- lookupLabel l
    ; (f, stmts') <- next stmts
    ; pure ((L l, ControlAnn i (f []) IS.empty IS.empty IS.empty IS.empty):stmts')
    }
addControlFlow (J l:stmts) = do
    { i <- getFresh
    ; nextStmts <- addControlFlow stmts
    ; l_i <- lookupLabel l
    ; pure ((J l, ControlAnn i [l_i] IS.empty IS.empty IS.empty IS.empty):nextStmts)
    }
addControlFlow (MJ e l:stmts) = do
    { i <- getFresh
    ; (f, stmts') <- next stmts
    ; l_i <- lookupLabel l
    ; pure ((MJ e l, ControlAnn i (f [l_i]) (uE e) IS.empty IS.empty IS.empty):stmts')
    }
addControlFlow (stmt:stmts) = do
    { i <- getFresh
    ; (f, stmts') <- next stmts
    ; pure ((stmt, ControlAnn i (f []) (uses stmt) IS.empty (defs stmt) IS.empty):stmts')
    }

uE :: Exp -> IS.IntSet
uE (EAt (AP _ Nothing (Just m)))  = IS.singleton m
uE (EAt (AP _ (Just e) (Just m))) = IS.insert m$uE e
uE (EAt (AP _ Nothing Nothing))   = IS.empty
uE (EAt (AP _ (Just e) Nothing))  = uE e
uE (IRel _ e0 e1)                 = uE e0<>uE e1
uE Reg{}                          = IS.empty
uE ConstI{}                       = IS.empty
uE (IB _ e0 e1)                   = uE e0<>uE e1
uE e                              = error(show e)

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

uses :: Stmt -> IS.IntSet
uses L{}                              = IS.empty
uses (Ma _ _ e)                       = uE e
uses (MX _ e)                         = uF e
uses (MT _ e)                         = uE e
uses (Wr (AP _ Nothing (Just m)) e)   = IS.insert m$uE e
uses (Wr (AP _ (Just eϵ) (Just m)) e) = IS.insert m$uE eϵ<>uE e
uses (RA l)                           = IS.singleton l

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
broadcasts (stmt@(L l):stmts) = do
    { i <- getFresh
    ; broadcast i l
    ; (stmt :) <$> broadcasts stmts
    }
broadcasts (asm:asms) = (asm :) <$> broadcasts asms
