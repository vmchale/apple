module Asm.Aarch64.CF ( mkControlFlow
                      , uses
                      , defs
                      ) where

import           Asm.Aarch64
import           Asm.CF
import           CF
import           Class.E      as E
import           Data.Functor (($>))
import qualified Data.IntSet  as IS

mkControlFlow :: (E reg, E freg) => [AArch64 reg freg ()] -> [AArch64 reg freg ControlAnn]
mkControlFlow instrs = runFreshM (broadcasts instrs *> addControlFlow instrs)

addControlFlow :: (E reg, E freg) => [AArch64 reg freg ()] -> FreshM [AArch64 reg freg ControlAnn]
addControlFlow [] = pure []
addControlFlow ((Label _ l):asms) = do
    { i <- lookupLabel l
    ; (f, asms') <- next asms
    ; pure (Label (ControlAnn i (f []) IS.empty IS.empty IS.empty IS.empty) l : asms')
    }
addControlFlow ((Bc _ c l):asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; l_i <- lookupLabel l
    ; pure (Bc (ControlAnn i (f [l_i]) IS.empty IS.empty IS.empty IS.empty) c l : asms')
    }
addControlFlow ((B _ l):asms) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow asms
    ; l_i <- lookupLabel l
    ; pure (B (ControlAnn i [l_i] IS.empty IS.empty IS.empty IS.empty) l : nextAsms)
    }
addControlFlow (Ret{}:asms) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow asms
    ; pure (Ret (ControlAnn i [] (singleton CArg0) (fromList [FArg0, FArg1]) IS.empty IS.empty) : nextAsms)
    }
addControlFlow (asm:asms) = do
    { i <- getFresh
    ; (f, asms') <- next asms
    ; pure ((asm $> ControlAnn i (f []) (uses asm) (usesF asm) (defs asm) (defsF asm)) : asms')
    }

uA :: E reg => Addr reg -> IS.IntSet
uA (R r)      = singleton r
uA (RP r _)   = singleton r
uA (BI b i _) = fromList [b, i]

defs, uses :: E reg => AArch64 reg freg a -> IS.IntSet
uses Label{}           = IS.empty
uses B{}               = IS.empty
uses Bc{}              = IS.empty
uses Bl{}              = IS.empty
uses (MovRR _ _ r)     = singleton r
uses MovRC{}           = IS.empty
uses FMovXX{}          = IS.empty
uses FMovXC{}          = IS.empty
uses (Ldr _ _ a)       = uA a
uses (Str _ r a)       = IS.insert (E.toInt r) $ uA a
uses (Ldp _ _ _ a)     = uA a
uses (Stp _ r0 r1 a)   = fromList [r0, r1] <> uA a
uses (LdrD _ _ a)      = uA a
uses (SubRR _ _ r0 r1) = fromList [r0, r1]
uses (AddRR _ _ r0 r1) = fromList [r0, r1]
uses (AddRC _ _ r _)   = singleton r
uses (SubRC _ _ r _)   = singleton r
uses (Lsl _ _ r _)     = singleton r
uses (CmpRR _ r0 r1)   = fromList [r0, r1]
uses (CmpRC _ r _)     = singleton r
uses (Neg _ _ r)       = singleton r
uses Fmul{}            = IS.empty
uses Fadd{}            = IS.empty
uses Fsub{}            = IS.empty
uses FcmpZ{}           = IS.empty
uses (StrD _ _ a)      = uA a
uses (MulRR _ _ r0 r1) = fromList [r0, r1]
uses Fdiv{}            = IS.empty
uses (Scvtf _ _ r)     = singleton r
uses Fcvtms{}          = IS.empty
uses (FMovDR _ _ r)    = singleton r
uses MovK{}            = IS.empty
uses Fcmp{}            = IS.empty
uses (StpD _ _ _ a)    = uA a
uses (LdpD _ _ _ a)    = uA a
uses Fmadd{}           = IS.empty

defs Label{}         = IS.empty
defs B{}             = IS.empty
defs Bc{}            = IS.empty
defs Bl{}            = IS.empty
defs FMovXX{}        = IS.empty
defs FMovXC{}        = IS.empty
defs (MovRC _ r _)   = singleton r
defs (MovRR _ r _)   = singleton r
defs (Ldr _ r _)     = singleton r
defs Str{}           = IS.empty
defs LdrD{}          = IS.empty
defs LdpD{}          = IS.empty
defs Stp{}           = IS.empty
defs StpD{}          = IS.empty
defs (Ldp _ r0 r1 _) = fromList [r0, r1]
defs (SubRR _ r _ _) = singleton r
defs (AddRR _ r _ _) = singleton r
defs (AddRC _ r _ _) = singleton r
defs (SubRC _ r _ _) = singleton r
defs (Lsl _ r _ _)   = singleton r
defs CmpRC{}         = IS.empty
defs CmpRR{}         = IS.empty
defs (Neg _ r _)     = singleton r
defs Fmul{}          = IS.empty
defs Fadd{}          = IS.empty
defs Fsub{}          = IS.empty
defs FcmpZ{}         = IS.empty
defs StrD{}          = IS.empty
defs (MulRR _ r _ _) = singleton r
defs Fdiv{}          = IS.empty
defs Scvtf{}         = IS.empty
defs (Fcvtms _ r _)  = singleton r
defs FMovDR{}        = IS.empty
defs (MovK _ r _ _)  = singleton r
defs Fcmp{}          = IS.empty
defs Fmadd{}         = IS.empty

defsF, usesF :: E freg => AArch64 reg freg ann -> IS.IntSet
defsF Label{}            = IS.empty
defsF B{}                = IS.empty
defsF Bc{}               = IS.empty
defsF Bl{}               = IS.empty
defsF (FMovXX _ r _)     = singleton r
defsF (FMovXC _ r _)     = singleton r
defsF MovRR{}            = IS.empty
defsF MovRC{}            = IS.empty
defsF Ldr{}              = IS.empty
defsF Str{}              = IS.empty
defsF (LdrD _ r _)       = singleton r
defsF AddRR{}            = IS.empty
defsF SubRR{}            = IS.empty
defsF AddRC{}            = IS.empty
defsF SubRC{}            = IS.empty
defsF Lsl{}              = IS.empty
defsF CmpRR{}            = IS.empty
defsF CmpRC{}            = IS.empty
defsF Neg{}              = IS.empty
defsF (Fmul _ r _ _)     = singleton r
defsF (Fadd _ r _ _)     = singleton r
defsF (Fsub _ r _ _)     = singleton r
defsF FcmpZ{}            = IS.empty
defsF (Fdiv _ d _ _)     = singleton d
defsF StrD{}             = IS.empty
defsF MulRR{}            = IS.empty
defsF (Scvtf _ r _)      = singleton r
defsF Fcvtms{}           = IS.empty
defsF (FMovDR _ r _)     = singleton r
defsF MovK{}             = IS.empty
defsF Fcmp{}             = IS.empty
defsF (LdpD _ r0 r1 _)   = fromList [r0, r1]
defsF Ldp{}              = IS.empty
defsF Stp{}              = IS.empty
defsF StpD{}             = IS.empty
defsF (Fmadd _ d0 _ _ _) = singleton d0

usesF Label{}              = IS.empty
usesF B{}                  = IS.empty
usesF Bc{}                 = IS.empty
usesF Bl{}                 = IS.empty
usesF (FMovXX _ _ r)       = singleton r
usesF FMovXC{}             = IS.empty
usesF MovRR{}              = IS.empty
usesF MovRC{}              = IS.empty
usesF Ldr{}                = IS.empty
usesF LdrD{}               = IS.empty
usesF Str{}                = IS.empty
usesF AddRR{}              = IS.empty
usesF SubRR{}              = IS.empty
usesF AddRC{}              = IS.empty
usesF SubRC{}              = IS.empty
usesF Lsl{}                = IS.empty
usesF CmpRR{}              = IS.empty
usesF CmpRC{}              = IS.empty
usesF Neg{}                = IS.empty
usesF (Fadd _ _ r0 r1)     = fromList [r0, r1]
usesF (Fsub _ _ r0 r1)     = fromList [r0, r1]
usesF (Fmul _ _ r0 r1)     = fromList [r0, r1]
usesF (FcmpZ _ r)          = singleton r
usesF (Fdiv _ _ r0 r1)     = fromList [r0, r1]
usesF (StrD _ r _)         = singleton r
usesF MulRR{}              = IS.empty
usesF Scvtf{}              = IS.empty
usesF (Fcvtms _ _ r)       = singleton r
usesF MovK{}               = IS.empty
usesF FMovDR{}             = IS.empty
usesF (Fcmp _ r0 r1)       = fromList [r0, r1]
usesF (StpD _ r0 r1 _)     = fromList [r0, r1]
usesF Stp{}                = IS.empty
usesF Ldp{}                = IS.empty
usesF LdpD{}               = IS.empty
usesF (Fmadd _ _ d0 d1 d2) = fromList [d0, d1, d2]

next :: (E reg, E freg) => [AArch64 reg freg ()] -> FreshM ([Int] -> [Int], [AArch64 reg freg ControlAnn])
next asms = do
    nextAsms <- addControlFlow asms
    case nextAsms of
        []      -> pure (id, [])
        (asm:_) -> pure ((node (ann asm) :), nextAsms)

broadcasts :: [AArch64 reg freg ()] -> FreshM [AArch64 reg freg ()]
broadcasts [] = pure []
broadcasts (asm@(Label _ l):asms) = do
    { i <- getFresh
    ; broadcast i l
    ; (asm :) <$> broadcasts asms
    }
broadcasts (asm:asms) = (asm :) <$> broadcasts asms
