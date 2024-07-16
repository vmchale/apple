module IR.CF ( mkControlFlow ) where

import           CF
-- seems to pretty clearly be faster
import           Control.Monad.State.Strict (State, gets, modify, runState, state)
import           Data.Bifunctor             (second)
import qualified Data.IntSet                as IS
import qualified Data.Map                   as M
import           Data.Tuple.Extra           (fst3, second3, snd3, thd3, third3)
import           IR

-- map of labels by node
type FreshM = State (Int, M.Map Label Int, M.Map Label [Int])

runFreshM :: FreshM a -> (a, Int)
runFreshM = second fst3.flip runState (0, mempty, mempty)

mkControlFlow :: [Stmt] -> ([(Stmt, ControlAnn)], Int)
mkControlFlow instrs = runFreshM (broadcasts instrs *> addControlFlow instrs)

getFresh :: FreshM Int
getFresh = state (\(i,m0,m1) -> (i,(i+1,m0,m1)))

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
    ; pure ((stmt, ControlAnn i (f []) (UD (uses stmt) (usesF stmt) (defs stmt) (defsF stmt))):stmts')
    }

rToInt :: Temp -> Int
rToInt (ITemp i) = i

singleton :: Temp -> IS.IntSet
singleton = IS.singleton . rToInt

uE :: Exp -> IS.IntSet
uE (Reg r)        = singleton r
uE ConstI{}       = IS.empty
uE (IB _ e0 e1)   = uE e0 <> uE e1
uE (IRel _ e0 e1) = uE e0 <> uE e1
uE (Is t)         = singleton t
uE (IU _ e)       = uE e
uE LA{}           = IS.empty

uF :: FExp -> IS.IntSet
uF ConstF{}     = IS.empty
uF (FB _ e0 e1) = uF e0 <> uF e1

uses, defs :: Stmt -> IS.IntSet
uses IRnd{} = IS.empty

defs (IRnd t) = singleton t

usesF, defsF :: Stmt -> IS.IntSet
usesF IRnd{} = IS.empty

defsF IRnd{} = IS.empty

next :: [Stmt] -> FreshM ([Int] -> [Int], [(Stmt, ControlAnn)])
next stmts = do
    nextStmts <- addControlFlow stmts
    case nextStmts of
        []       -> pure (id, [])
        (stmt:_) -> pure ((node (snd stmt) :), nextStmts)

-- | Construct map assigning labels to their node name.
broadcasts :: [Stmt] -> FreshM ()
broadcasts [] = pure ()
broadcasts (stmt@(C l):stmt'@(L retL):stmts) = do
    { i <- getFresh
    ; broadcast i retL; b3 i l
    ; broadcasts stmts
    }
broadcasts (stmt@(L l):stmts) = do
    { i <- getFresh
    ; broadcast i l
    ; broadcasts stmts
    }
broadcasts (_:asms) = broadcasts asms
