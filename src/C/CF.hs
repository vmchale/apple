module C.CF ( mkControlFlow ) where

import           CF
import           CF.AL
-- seems to pretty clearly be faster
import           C
import           Control.Monad.State.Strict (State, evalState, gets, modify, state)
import           Data.Bifunctor             (second)
import           Data.Functor               (($>))
import qualified Data.IntSet                as IS
import           Data.List                  (uncons, unsnoc)
import qualified Data.Map                   as M
import           Data.Tuple.Extra           (second3, snd3, thd3, third3)

type N=Int

-- map of labels by node
type FreshM = State (N, M.Map Label N, M.Map Label [N])

runFreshM :: FreshM a -> a
runFreshM = flip evalState (0, mempty, mempty)

mkControlFlow :: [CS ()] -> [(CS (), ControlAnn)]
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

-- | Pair 'CS with a unique node name and a list of all possible
-- destinations.
addCF :: [CS ()] -> FreshM [(CS (), ControlAnn)]
addCF [] = pure []
addCF ((Def l ss):stmts) =
    case uncons ss of
        Nothing -> undefined
addCF (G l r:stmts) = undefined
addCF (For{}:_) = undefined
addCF (While{}:_) = undefined
addCF (If{}:_) = undefined
addCF (Ifn't{}:_) = undefined
addCF (stmt:stmts) = do
    i <- getFresh
    (f, stmts') <- next stmts
    pure ((stmt, ControlAnn i (f []) (UD (uses stmt) IS.empty (defs stmt) IS.empty)):stmts')

uE :: CE -> IS.IntSet
uE ConstI{} = IS.empty
uE LA{}     = IS.empty
uE (EAt a)  = uA a

uF :: CFE -> IS.IntSet
uF ConstF{}                       = IS.empty

uA (ARnk _ (Just l)) = singleton l

uses :: CS () -> IS.IntSet
uses (Ma _ _ e _ _)  = uE e
uses (MX _ e)        = uF e
uses (Wr a e)        = uA a <> uE e
uses (RA l)          = singleton l
uses (Cmov e0 _ e1)  = uB e0<>uE e1
uses (Fcmov e0 _ e1) = uB e0<>uF e1
uses Sa{}            = IS.empty
uses (WrF a e)       = uA a <> uF e
uses Pop{}           = IS.empty
uses (Cset e _)      = uB e

uB :: PE -> IS.IntSet
uB (PAt a)        = uA a
uB BConst{}       = IS.empty
uB (IRel _ e0 e1) = uE e0<>uE e1
uB (FRel _ e0 e1) = uF e0 <> uF e1


defs :: CS () -> IS.IntSet
defs (Ma a _ _ _ _) = singleton a
defs (MaΠ a _ _)    = singleton a
defs _              = IS.empty

next :: [CS ()] -> FreshM ([N] -> [N], [(CS (), ControlAnn)])
next stmts = do
    nextStmts <- addCF stmts
    case nextStmts of
        []       -> pure (id, [])
        (stmt:_) -> pure ((node (snd stmt) :), nextStmts)

-- | Construct map assigning labels to their node name.
brs :: [CS ()] -> FreshM ()
brs []               = pure ()
brs (G l retL:stmts) = do {i <- fm retL; b3 i l; brs stmts}
brs (Def f b:stmts)  = fm f *> brs b *> brs stmts
brs (_:asms)         = brs asms
