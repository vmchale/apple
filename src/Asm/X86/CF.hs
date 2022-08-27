-- | From the [Kempe compiler](http://vmchale.com/original/compiler.pdf) with
-- improvements.
module Asm.X86.CF ( mkControlFlow
                  ) where

import           Asm.X86
import           CF
-- seems to pretty clearly be faster
import           Control.Monad.State.Strict (State, evalState, gets, modify)
import           Data.Bifunctor             (first, second)
import           Data.Functor               (($>))
import qualified Data.IntSet                as IS
import qualified Data.Map                   as M
import           Data.Semigroup             ((<>))

-- map of labels by node
type FreshM = State (Int, M.Map Label Int)

runFreshM :: FreshM a -> a
runFreshM = flip evalState (0, mempty)

mkControlFlow :: [X86 AbsReg ()] -> [X86 AbsReg ControlAnn]
mkControlFlow instrs = runFreshM (broadcasts instrs *> addControlFlow instrs)

getFresh :: FreshM Int
getFresh = gets fst <* modify (first (+1))

lookupLabel :: Label -> FreshM Int
lookupLabel l = gets (M.findWithDefault (error "Internal error in control-flow graph: node label not in map.") l . snd)

broadcast :: Int -> Label -> FreshM ()
broadcast i l = modify (second (M.insert l i))

singleton :: AbsReg -> IS.IntSet
singleton = IS.singleton . toInt

fromList :: [AbsReg] -> IS.IntSet
fromList = foldMap singleton

-- | Annotate instructions with a unique node name and a list of all possible
-- destinations.
addControlFlow :: [X86 AbsReg ()] -> FreshM [X86 AbsReg ControlAnn]
addControlFlow [] = pure []
addControlFlow ((Label _ l):asms) = do
    { i <- lookupLabel l
    ; (f, asms') <- next asms
    ; pure (Label (ControlAnn i (f []) IS.empty IS.empty) l : asms')
    }
addControlFlow ((Je _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l
    ; pure (Je (ControlAnn i (f [l_i]) IS.empty IS.empty) l : asms')
    }
addControlFlow ((Jl _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l
    ; pure (Jl (ControlAnn i (f [l_i]) IS.empty IS.empty) l : asms')
    }
addControlFlow ((Jle _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l
    ; pure (Jle (ControlAnn i (f [l_i]) IS.empty IS.empty) l : asms')
    }
addControlFlow ((Jne _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l
    ; pure (Jne (ControlAnn i (f [l_i]) IS.empty IS.empty) l : asms')
    }
addControlFlow ((Jge _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l
    ; pure (Jge (ControlAnn i (f [l_i]) IS.empty IS.empty) l : asms')
    }
addControlFlow ((Jg _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l
    ; pure (Jg (ControlAnn i (f [l_i]) IS.empty IS.empty) l : asms')
    }
addControlFlow ((J _ l):asms) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow asms
    ; l_i <- lookupLabel l
    ; pure (J (ControlAnn i [l_i] IS.empty IS.empty) l : nextAsms)
    }
addControlFlow (Ret{}:asms) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow asms
    ; pure (Ret (ControlAnn i [] IS.empty IS.empty) : nextAsms)
    }
addControlFlow (asm:asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; pure ((asm $> ControlAnn i (f []) (uses asm) (defs asm)) : asms')
    }

isM :: X86 AbsReg ann -> Bool
isM MovRR{}   = True
isM MovRA{}   = True
isM MovRI{}   = True
isM MovAR{}   = True
isM MovAI32{} = True

isMX :: X86 AbsReg ann -> Bool
isMX MovqXR{} = True
isMX MovqXA{} = True
isMX MovqAX{} = True

uA :: Addr AbsReg -> IS.IntSet
uA (R r)         = singleton r
uA (RC r _)      = singleton r
uA (RS b _ i)    = fromList [b,i]
uA (RSD b _ i _) = fromList [b,i]

uses :: X86 AbsReg ann -> IS.IntSet
uses (MovRR _ _ r)            = singleton r
uses (And _ r0 r1)            = fromList [r0, r1]
uses (IAddRR _ r0 r1)         = fromList [r0, r1]
uses (IAddRI _ r _)           = singleton r
uses (ISubRR _ r0 r1)         = fromList [r0, r1]
uses (ISubRI _ r _)           = singleton r
uses (IMulRR _ r0 r1)         = fromList [r0, r1]
uses (CmpRR _ r0 r1)          = fromList [r0, r1]
uses MovRI{}                  = IS.empty
uses (Movapd _ _ r)           = singleton r
uses (Roundsd _ _ r _)        = singleton r
uses (Cvttsd2si _ _ r)        = singleton r
uses (CmpRI _ r _)            = singleton r
uses (Vmulsd _ _ r0 r1)       = fromList [r0, r1]
uses (Vaddsd _ _ r0 r1)       = fromList [r0, r1]
uses (Vsubsd _ _ r0 r1)       = fromList [r0, r1]
uses (Vdivsd _ _ r0 r1)       = fromList [r0, r1]
uses (MovqXR _ _ r)           = singleton r
uses (Cvtsi2sd _ _ r)         = singleton r
uses (Mulsd _ r0 r1)          = fromList [r0, r1]
uses (Divsd _ r0 r1)          = fromList [r0, r1]
uses (Addsd _ r0 r1)          = fromList [r0, r1]
uses (Subsd _ r0 r1)          = fromList [r0, r1]
uses (MovqXA _ _ a)           = uA a
uses (MovqAX _ a x)           = uA a <> singleton x
uses Fldl2e{}                 = IS.empty
uses Fldln2{}                 = IS.empty
uses (Fld _ a)                = uA a
uses Fyl2x{}                  = IS.empty
uses F2xm1{}                  = IS.empty
uses Fmulp{}                  = IS.empty
uses (Fstp _ a)               = uA a
uses (MovRA _ _ a)            = uA a
uses (Vfmadd231sd _ r0 r1 r2) = fromList [r0, r1, r2]
uses (IDiv _ r)               = fromList [r, Quot, Rem]
uses (MovAR _ a r)            = uA a <> singleton r
uses (Sal _ r _)              = singleton r
uses (Sar _ r _)              = singleton r
uses (Call _ Malloc)          = fromList [CArg0]
uses (MovAI32 _ a _)          = uA a
uses (Sqrtsd _ _ r)           = singleton r
uses (Vmaxsd _ _ r0 r1)       = fromList [r0, r1]
uses (Vminsd _ _ r0 r1)       = fromList [r0, r1]
uses (Maxsd _ r0 r1)          = fromList [r0, r1]
uses (Minsd _ r0 r1)          = fromList [r0, r1]
uses Fld1{}                   = IS.empty
uses FldS{}                   = IS.empty
uses Fprem{}                  = IS.empty
uses Faddp{}                  = IS.empty
uses Fscale{}                 = IS.empty
uses Fxch{}                   = IS.empty
uses (Not _ r)                = singleton r
uses (Cmovnle _ _ r)          = singleton r
uses r                        = error (show r)

defs :: X86 AbsReg ann -> IS.IntSet
defs (MovRR _ r _)         = singleton r
defs (IAddRR _ r _)        = singleton r
defs (And _ r _)           = singleton r
defs (IAddRI _ r _)        = singleton r
defs (ISubRR _ r _)        = singleton r
defs (ISubRI _ r _)        = singleton r
defs (IMulRR _ r _)        = singleton r
defs CmpRR{}               = IS.empty
defs (MovRI _ r _)         = singleton r
defs (Movapd _ r _)        = singleton r
defs (Roundsd _ r _ _)     = singleton r
defs (Cvttsd2si _ r _)     = singleton r
defs CmpRI{}               = IS.empty
defs (Vmulsd _ r _ _)      = singleton r
defs (Vaddsd _ r _ _)      = singleton r
defs (Vsubsd _ r _ _)      = singleton r
defs (Vdivsd _ r _ _)      = singleton r
defs (MovqXR _ r _)        = singleton r
defs (Cvtsi2sd _ r _)      = singleton r
defs (Addsd _ r _)         = singleton r
defs (Subsd _ r _)         = singleton r
defs (Divsd _ r _)         = singleton r
defs (Mulsd _ r _)         = singleton r
defs (MovqXA _ r _)        = singleton r
defs MovqAX{}              = IS.empty
defs Fldl2e{}              = IS.empty
defs Fldln2{}              = IS.empty
defs Fyl2x{}               = IS.empty
defs Fstp{}                = IS.empty
defs Fld{}                 = IS.empty
defs F2xm1{}               = IS.empty
defs Fmulp{}               = IS.empty
defs (MovRA _ r _)         = singleton r
defs (Vfmadd231sd _ r _ _) = singleton r
defs (IDiv _ r)            = fromList [r, Quot, Rem]
defs MovAR{}               = IS.empty
defs (Sal _ r _)           = singleton r
defs (Sar _ r _)           = singleton r
defs (Call _ Malloc)       = singleton CRet
defs MovAI32{}             = IS.empty
defs (Sqrtsd _ r _)        = singleton r
defs (Vmaxsd _ r _ _)      = singleton r
defs (Vminsd _ r _ _)      = singleton r
defs (Minsd _ r _)         = singleton r
defs (Maxsd _ r _)         = singleton r
defs Fld1{}                = IS.empty
defs FldS{}                = IS.empty
defs Fprem{}               = IS.empty
defs Faddp{}               = IS.empty
defs Fscale{}              = IS.empty
defs Fxch{}                = IS.empty
defs (Not _ r)             = singleton r
defs (Cmovnle _ r _)       = singleton r

next :: [X86 AbsReg ()] -> FreshM ([Int] -> [Int], [X86 AbsReg ControlAnn])
next asms = do
    nextAsms <- addControlFlow asms
    case nextAsms of
        []      -> pure (id, [])
        (asm:_) -> pure ((node (ann asm) :), nextAsms)

-- | Construct map assigning labels to their node name.
broadcasts :: [X86 reg ()] -> FreshM [X86 reg ()]
broadcasts [] = pure []
broadcasts (asm@(Label _ l):asms) = do
    { i <- getFresh
    ; broadcast i l
    ; (asm :) <$> broadcasts asms
    }
broadcasts (asm:asms) = (asm :) <$> broadcasts asms
