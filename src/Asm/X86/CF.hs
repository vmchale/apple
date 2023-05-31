-- | From the [Kempe compiler](http://vmchale.com/original/compiler.pdf) with
-- improvements.
module Asm.X86.CF ( mkControlFlow
                  , uses, defs
                  , udX86
                  ) where

import           Asm.CF
import           Asm.X86        as X86
import           CF
-- seems to pretty clearly be faster
import           Class.E        as E
import           Data.Functor   (($>))
import qualified Data.IntSet    as IS
import           Data.Semigroup ((<>))

mkControlFlow :: (E reg, E freg) => [X86 reg freg ()] -> [X86 reg freg ControlAnn]
mkControlFlow instrs = runFreshM (broadcasts instrs *> addControlFlow instrs)

-- | Annotate instructions with a unique node name and a list of all possible
-- destinations.
addControlFlow :: (E reg, E freg) => [X86 reg freg ()] -> FreshM [X86 reg freg ControlAnn]
addControlFlow [] = pure []
addControlFlow ((Label _ l):asms) = do
    { i <- lookupLabel l
    ; (f, asms') <- next asms
    ; pure (Label (ControlAnn i (f []) (UD IS.empty IS.empty IS.empty IS.empty)) l : asms')
    }
addControlFlow ((Je _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l
    ; pure (Je (ControlAnn i (f [l_i]) (UD IS.empty IS.empty IS.empty IS.empty)) l : asms')
    }
addControlFlow ((Jl _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l
    ; pure (Jl (ControlAnn i (f [l_i]) (UD IS.empty IS.empty IS.empty IS.empty)) l : asms')
    }
addControlFlow ((Jle _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l
    ; pure (Jle (ControlAnn i (f [l_i]) (UD IS.empty IS.empty IS.empty IS.empty)) l : asms')
    }
addControlFlow ((Jne _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l
    ; pure (Jne (ControlAnn i (f [l_i]) (UD IS.empty IS.empty IS.empty IS.empty)) l : asms')
    }
addControlFlow ((Jge _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l
    ; pure (Jge (ControlAnn i (f [l_i]) (UD IS.empty IS.empty IS.empty IS.empty)) l : asms')
    }
addControlFlow ((Jg _ l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l
    ; pure (Jg (ControlAnn i (f [l_i]) (UD IS.empty IS.empty IS.empty IS.empty)) l : asms')
    }
addControlFlow ((J _ l):asms) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow asms
    ; l_i <- lookupLabel l
    ; pure (J (ControlAnn i [l_i] (UD IS.empty IS.empty IS.empty IS.empty)) l : nextAsms)
    }
addControlFlow ((C _ l):asms) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow asms
    ; l_i <- lookupLabel l
    ; pure (C (ControlAnn i [l_i] (UD IS.empty IS.empty IS.empty IS.empty)) l : nextAsms)
    }
addControlFlow (RetL _ l:asms) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow asms
    ; l_is <- lC l
    ; pure (RetL (ControlAnn i l_is (UD IS.empty IS.empty IS.empty IS.empty)) l:nextAsms)
    }
addControlFlow (Ret{}:asms) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow asms
    ; pure (Ret (ControlAnn i [] (UD (singleton CRet) (fromList [FRet0, FRet1]) IS.empty IS.empty)) : nextAsms) -- TODO: pass information down if redundant
    }
addControlFlow (asm:asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; pure ((asm $> ControlAnn i (f []) (udX86 asm)) : asms')
    }

udX86 asm = UD (uses asm) (usesF asm) (defs asm) (defsF asm)

uA :: E reg => Addr reg -> IS.IntSet
uA (R r)         = singleton r
uA (RC32 r _)    = singleton r
uA (RC r _)      = singleton r
uA (RS b _ i)    = fromList [b,i]
uA (RSD b _ i _) = fromList [b,i]

usesF :: E freg => X86 reg freg ann -> IS.IntSet
usesF (Movapd _ _ r)            = singleton r
usesF (Vmulsd _ _ r0 r1)        = fromList [r0, r1]
usesF (Vaddsd _ _ r0 r1)        = fromList [r0, r1]
usesF (Vsubsd _ _ r0 r1)        = fromList [r0, r1]
usesF (Vdivsd _ _ r0 r1)        = fromList [r0, r1]
usesF (VaddsdA _ _ r _)         = singleton r
usesF (Mulsd _ r0 r1)           = fromList [r0, r1]
usesF (Divsd _ r0 r1)           = fromList [r0, r1]
usesF (Addsd _ r0 r1)           = fromList [r0, r1]
usesF (Subsd _ r0 r1)           = fromList [r0, r1]
usesF (Vfmadd231sd _ r0 r1 r2)  = fromList [r0, r1, r2]
usesF (Vfmnadd231sd _ r0 r1 r2) = fromList [r0, r1, r2]
usesF (Vfmadd231sdA _ r0 r1 _)  = fromList [r0, r1]
usesF (Sqrtsd _ _ r)            = singleton r
usesF (Vmaxsd _ _ r0 r1)        = fromList [r0, r1]
usesF (Vminsd _ _ r0 r1)        = fromList [r0, r1]
usesF (Maxsd _ r0 r1)           = fromList [r0, r1]
usesF (Minsd _ r0 r1)           = fromList [r0, r1]
usesF (Roundsd _ _ r _)         = singleton r
usesF (Cvttsd2si _ _ r)         = singleton r
usesF (MovqAX _ _ x)            = singleton x
usesF MovqXR{}                  = IS.empty
usesF IAddRR{}                  = IS.empty
usesF IAddRI{}                  = IS.empty
usesF ISubRR{}                  = IS.empty
usesF MovRI{}                   = IS.empty
usesF MovRR{}                   = IS.empty
usesF CmpRR{}                   = IS.empty
usesF CmpRI{}                   = IS.empty
usesF Cvtsi2sd{}                = IS.empty
usesF MovRA{}                   = IS.empty
usesF MovAR{}                   = IS.empty
usesF Fldln2{}                  = IS.empty
usesF ISubRI{}                  = IS.empty
usesF IMulRR{}                  = IS.empty
usesF IMulRA{}                  = IS.empty
usesF Sal{}                     = IS.empty
usesF Sar{}                     = IS.empty
usesF Fscale{}                  = IS.empty
usesF Call{}                    = IS.empty
usesF Cmovnle{}                 = IS.empty
usesF Cmovnl{}                  = IS.empty
usesF Cmovne{}                  = IS.empty
usesF Cmove{}                   = IS.empty
usesF Cmovle{}                  = IS.empty
usesF Cmovl{}                   = IS.empty
usesF Fstp{}                    = IS.empty
usesF MovqXA{}                  = IS.empty
usesF Fyl2x{}                   = IS.empty
usesF Fld{}                     = IS.empty
usesF MovAI32{}                 = IS.empty
usesF Faddp{}                   = IS.empty
usesF F2xm1{}                   = IS.empty
usesF Fprem{}                   = IS.empty
usesF Fmulp{}                   = IS.empty
usesF FldS{}                    = IS.empty
usesF Fld1{}                    = IS.empty
usesF Fldl2e{}                  = IS.empty
usesF Fninit{}                  = IS.empty
usesF TestI{}                   = IS.empty
usesF (Vcmppd _ _ r0 r1 _)      = fromList [r0, r1]
usesF (MovqRX _ _ xr)           = singleton xr
usesF Fsin{}                    = IS.empty
usesF Fcos{}                    = IS.empty
usesF XorRR{}                   = IS.empty
usesF Fxch{}                    = IS.empty
usesF IDiv{}                    = IS.empty
usesF Test{}                    = IS.empty
usesF Push{}                    = IS.empty
usesF Pop{}                     = IS.empty
usesF Rdrand{}                  = IS.empty
usesF Neg{}                     = IS.empty

uses :: E reg => X86 reg freg ann -> IS.IntSet
uses (MovRR _ _ r)          = singleton r
uses (And _ r0 r1)          = fromList [r0, r1]
uses (IAddRR _ r0 r1)       = fromList [r0, r1]
uses (IAddRI _ r _)         = singleton r
uses (ISubRR _ r0 r1)       = fromList [r0, r1]
uses (ISubRI _ r _)         = singleton r
uses (IMulRR _ r0 r1)       = fromList [r0, r1]
uses (IMulRA _ r a)         = IS.insert (E.toInt r) $ uA a
uses (CmpRR _ r0 r1)        = fromList [r0, r1]
uses MovRI{}                = IS.empty
uses (CmpRI _ r _)          = singleton r
uses (MovqXR _ _ r)         = singleton r
uses (Cvtsi2sd _ _ r)       = singleton r
uses (MovqXA _ _ a)         = uA a
uses (MovqAX _ a _)         = uA a
uses Fldl2e{}               = IS.empty
uses Fldln2{}               = IS.empty
uses (Fld _ a)              = uA a
uses Fyl2x{}                = IS.empty
uses F2xm1{}                = IS.empty
uses Fmulp{}                = IS.empty
uses (Fstp _ a)             = uA a
uses (MovRA _ _ a)          = uA a
uses (IDiv _ r)             = IS.insert (E.toInt r) $ fromList [Quot, Rem]
uses (MovAR _ a r)          = uA a <> singleton r
uses (Sal _ r _)            = singleton r
uses (Sar _ r _)            = singleton r
uses (Call _ Malloc)        = fromList [CArg0]
uses (Call _ Free)          = fromList [CArg0]
uses (MovAI32 _ a _)        = uA a
uses Fld1{}                 = IS.empty
uses FldS{}                 = IS.empty
uses Fprem{}                = IS.empty
uses Faddp{}                = IS.empty
uses Fsin{}                 = IS.empty
uses Fcos{}                 = IS.empty
uses Fscale{}               = IS.empty
uses Fxch{}                 = IS.empty
uses (Not _ r)              = singleton r
uses (Cmovnle _ _ r)        = singleton r
uses (Cmovnl _ _ r)         = singleton r
uses (Cmovne _ _ r)         = singleton r
uses (Cmove _ _ r)          = singleton r
uses (Cmovle _ _ r)         = singleton r
uses (Cmovl _ _ r)          = singleton r
uses Vmulsd{}               = IS.empty
uses Vaddsd{}               = IS.empty
uses (VaddsdA _ _ _ a)      = uA a
uses Vdivsd{}               = IS.empty
uses Vsubsd{}               = IS.empty
uses Vmaxsd{}               = IS.empty
uses Vminsd{}               = IS.empty
uses Addsd{}                = IS.empty
uses Subsd{}                = IS.empty
uses Roundsd{}              = IS.empty
uses Mulsd{}                = IS.empty
uses Divsd{}                = IS.empty
uses Movapd{}               = IS.empty
uses Vfmadd231sd{}          = IS.empty
uses Vfmnadd231sd{}         = IS.empty
uses (Vfmadd231sdA _ _ _ a) = uA a
uses Cvttsd2si{}            = IS.empty
uses Sqrtsd{}               = IS.empty
uses Rdrand{}               = IS.empty
uses Fninit{}               = IS.empty
uses (TestI _ r _)          = singleton r
uses Vcmppd{}               = IS.empty
uses MovqRX{}               = IS.empty
uses (XorRR _ r0 r1)        = fromList [r0, r1]
uses (Test _ r0 r1)         = fromList [r0, r1]
uses (Push _ r)             = singleton r
uses Pop{}                  = IS.empty
uses (Neg _ r)              = singleton r

defsF :: E freg => X86 reg freg ann -> IS.IntSet
defsF (Movapd _ r _)         = singleton r
defsF (Vmulsd _ r _ _)       = singleton r
defsF (Vaddsd _ r _ _)       = singleton r
defsF (VaddsdA _ r _ _)      = singleton r
defsF (Vsubsd _ r _ _)       = singleton r
defsF (Vdivsd _ r _ _)       = singleton r
defsF (MovqXR _ r _)         = singleton r
defsF (Addsd _ r _)          = singleton r
defsF (Subsd _ r _)          = singleton r
defsF (Divsd _ r _)          = singleton r
defsF (Mulsd _ r _)          = singleton r
defsF (MovqXA _ r _)         = singleton r
defsF MovqAX{}               = IS.empty
defsF (Sqrtsd _ r _)         = singleton r
defsF (Vmaxsd _ r _ _)       = singleton r
defsF (Vminsd _ r _ _)       = singleton r
defsF (Minsd _ r _)          = singleton r
defsF (Maxsd _ r _)          = singleton r
defsF (Vfmadd231sd _ r _ _)  = singleton r
defsF (Vfmnadd231sd _ r _ _) = singleton r
defsF (Vfmadd231sdA _ r _ _) = singleton r
defsF (Roundsd _ r _ _)      = singleton r
defsF (Cvtsi2sd _ r _)       = singleton r
defsF IAddRR{}               = IS.empty
defsF IAddRI{}               = IS.empty
defsF ISubRR{}               = IS.empty
defsF ISubRI{}               = IS.empty
defsF IMulRR{}               = IS.empty
defsF IMulRA{}               = IS.empty
defsF MovRR{}                = IS.empty
defsF MovRA{}                = IS.empty
defsF MovAR{}                = IS.empty
defsF MovAI32{}              = IS.empty
defsF MovRI{}                = IS.empty
defsF Fld{}                  = IS.empty
defsF FldS{}                 = IS.empty
defsF Fldl2e{}               = IS.empty
defsF Fldln2{}               = IS.empty
defsF Fld1{}                 = IS.empty
defsF Fsin{}                 = IS.empty
defsF Fcos{}                 = IS.empty
defsF Fyl2x{}                = IS.empty
defsF Fstp{}                 = IS.empty
defsF F2xm1{}                = IS.empty
defsF Fmulp{}                = IS.empty
defsF Fprem{}                = IS.empty
defsF Faddp{}                = IS.empty
defsF Fscale{}               = IS.empty
defsF Fxch{}                 = IS.empty
defsF CmpRR{}                = IS.empty
defsF CmpRI{}                = IS.empty
defsF Sal{}                  = IS.empty
defsF Sar{}                  = IS.empty
defsF Cmovnle{}              = IS.empty
defsF Cmovnl{}               = IS.empty
defsF Cmovne{}               = IS.empty
defsF Cmove{}                = IS.empty
defsF Cmovle{}               = IS.empty
defsF Cmovl{}                = IS.empty
defsF Call{}                 = IS.empty
defsF Cvttsd2si{}            = IS.empty
defsF Fninit{}               = IS.empty
defsF TestI{}                = IS.empty
defsF (Vcmppd _ r _ _ _)     = singleton r
defsF MovqRX{}               = IS.empty
defsF XorRR{}                = IS.empty
defsF IDiv{}                 = IS.empty
defsF Test{}                 = IS.empty
defsF Pop{}                  = IS.empty
defsF Push{}                 = IS.empty
defsF Rdrand{}               = IS.empty
defsF Neg{}                  = IS.empty

defs :: (E reg) => X86 reg freg ann -> IS.IntSet
defs (MovRR _ r _)     = singleton r
defs MovqXR{}          = IS.empty
defs MovqXA{}          = IS.empty
defs MovqAX{}          = IS.empty
defs (IAddRR _ r _)    = singleton r
defs (And _ r _)       = singleton r
defs (IAddRI _ r _)    = singleton r
defs (ISubRR _ r _)    = singleton r
defs (ISubRI _ r _)    = singleton r
defs (IMulRR _ r _)    = singleton r
defs (IMulRA _ r _)    = singleton r
defs CmpRR{}           = IS.empty
defs (MovRI _ r _)     = singleton r
defs (Cvttsd2si _ r _) = singleton r
defs CmpRI{}           = IS.empty
defs Fldl2e{}          = IS.empty
defs Fldln2{}          = IS.empty
defs Fyl2x{}           = IS.empty
defs Fstp{}            = IS.empty
defs Fsin{}            = IS.empty
defs Fcos{}            = IS.empty
defs Fld{}             = IS.empty
defs F2xm1{}           = IS.empty
defs Fmulp{}           = IS.empty
defs (MovRA _ r _)     = singleton r
defs IDiv{}            = fromList [Quot, Rem]
defs MovAR{}           = IS.empty
defs (Sal _ r _)       = singleton r
defs (Sar _ r _)       = singleton r
defs (Call _ Malloc)   = IS.singleton (X86.toInt CRet)
defs (Call _ Free)     = IS.empty
defs MovAI32{}         = IS.empty
defs Fld1{}            = IS.empty
defs FldS{}            = IS.empty
defs Fprem{}           = IS.empty
defs Faddp{}           = IS.empty
defs Fscale{}          = IS.empty
defs Fxch{}            = IS.empty
defs (Not _ r)         = singleton r
defs (Cmovnle _ r _)   = singleton r
defs (Cmovnl _ r _)    = singleton r
defs (Cmovne _ r _)    = singleton r
defs (Cmove _ r _)     = singleton r
defs (Cmovl _ r _)     = singleton r
defs (Cmovle _ r _)    = singleton r
defs Vmulsd{}          = IS.empty
defs Vdivsd{}          = IS.empty
defs Vaddsd{}          = IS.empty
defs Vsubsd{}          = IS.empty
defs Vmaxsd{}          = IS.empty
defs Vminsd{}          = IS.empty
defs VaddsdA{}         = IS.empty
defs Addsd{}           = IS.empty
defs Subsd{}           = IS.empty
defs Mulsd{}           = IS.empty
defs Divsd{}           = IS.empty
defs Movapd{}          = IS.empty
defs Cvtsi2sd{}        = IS.empty
defs Vfmadd231sd{}     = IS.empty
defs Vfmadd231sdA{}    = IS.empty
defs Vfmnadd231sd{}    = IS.empty
defs Roundsd{}         = IS.empty
defs Sqrtsd{}          = IS.empty
defs (Rdrand _ r)      = singleton r
defs Fninit{}          = IS.empty
defs TestI{}           = IS.empty
defs Vcmppd{}          = IS.empty
defs (MovqRX _ r _)    = singleton r
defs (XorRR _ r _)     = singleton r
defs Test{}            = IS.empty
defs Push{}            = IS.empty
defs (Pop _ r)         = singleton r
defs (Neg _ r)         = singleton r

next :: (E reg, E freg) => [X86 reg freg ()] -> FreshM ([Int] -> [Int], [X86 reg freg ControlAnn])
next asms = do
    nextAsms <- addControlFlow asms
    case nextAsms of
        []      -> pure (id, [])
        (asm:_) -> pure ((node (ann asm) :), nextAsms)

-- | Construct map assigning labels to their node name.
broadcasts :: [X86 reg freg ()] -> FreshM [X86 reg freg ()]
broadcasts [] = pure []
broadcasts (asm@(C _ l):asm'@(Label _ retL):stmts) = do
    { i <- getFresh
    ; broadcast i retL; b3 i l
    ; (asm:).(asm':) <$> broadcasts stmts
    }
broadcasts (asm@(Label _ l):asms) = do
    { i <- getFresh
    ; broadcast i l
    ; (asm :) <$> broadcasts asms
    }
broadcasts (asm:asms) = (asm :) <$> broadcasts asms
