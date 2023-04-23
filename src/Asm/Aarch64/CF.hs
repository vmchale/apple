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
uses Label{}            = IS.empty
uses B{}                = IS.empty
uses Bc{}               = IS.empty
uses Bl{}               = IS.empty
uses (MovRR _ _ r)      = singleton r
uses MovRC{}            = IS.empty
uses FMovXX{}           = IS.empty
uses FMovXC{}           = IS.empty
uses (Ldr _ _ a)        = uA a
uses (Str _ r a)        = IS.insert (E.toInt r) $ uA a
uses (LdrD _ _ a)       = uA a
uses (SubRR _ _ r0 r1)  = fromList [r0, r1]
uses (AddRR _ _ r0 r1)  = fromList [r0, r1]
uses (AddRC _ _ r _)    = singleton r
uses (SubRC _ _ r _)    = singleton r
uses (Lsl _ _ r _)      = singleton r
uses (CmpRR _ r0 r1)    = fromList [r0, r1]
uses (CmpRC _ r _)      = singleton r
uses (Neg _ _ r)        = singleton r
uses Fmul{}             = IS.empty
uses Fadd{}             = IS.empty
uses Fsub{}             = IS.empty
uses FcmpZ{}            = IS.empty
uses (CpyfP _ r0 r1 r2) = fromList [r0, r1, r2]
uses (CpyfM _ r0 r1 r2) = fromList [r0, r1, r2]
uses (CpyfE _ r0 r1 r2) = fromList [r0, r1, r2]

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
defs CpyfP{}         = IS.empty
defs CpyfM{}         = IS.empty
defs CpyfE{}         = IS.empty

defsF, usesF :: E freg => AArch64 reg freg ann -> IS.IntSet
defsF Label{}        = IS.empty
defsF B{}            = IS.empty
defsF Bc{}           = IS.empty
defsF Bl{}           = IS.empty
defsF (FMovXX _ r _) = singleton r
defsF (FMovXC _ r _) = singleton r
defsF MovRR{}        = IS.empty
defsF MovRC{}        = IS.empty
defsF Ldr{}          = IS.empty
defsF Str{}          = IS.empty
defsF (LdrD _ r _)   = singleton r
defsF AddRR{}        = IS.empty
defsF SubRR{}        = IS.empty
defsF AddRC{}        = IS.empty
defsF SubRC{}        = IS.empty
defsF Lsl{}          = IS.empty
defsF CmpRR{}        = IS.empty
defsF CmpRC{}        = IS.empty
defsF Neg{}          = IS.empty
defsF CpyfP{}        = IS.empty
defsF CpyfM{}        = IS.empty
defsF CpyfE{}        = IS.empty
defsF (Fmul _ r _ _) = singleton r
defsF (Fadd _ r _ _) = singleton r
defsF (Fsub _ r _ _) = singleton r
defsF FcmpZ{}        = IS.empty

usesF Label{}          = IS.empty
usesF B{}              = IS.empty
usesF Bc{}             = IS.empty
usesF Bl{}             = IS.empty
usesF (FMovXX _ _ r)   = singleton r
usesF FMovXC{}         = IS.empty
usesF MovRR{}          = IS.empty
usesF MovRC{}          = IS.empty
usesF Ldr{}            = IS.empty
usesF LdrD{}           = IS.empty
usesF Str{}            = IS.empty
usesF AddRR{}          = IS.empty
usesF SubRR{}          = IS.empty
usesF AddRC{}          = IS.empty
usesF SubRC{}          = IS.empty
usesF Lsl{}            = IS.empty
usesF CmpRR{}          = IS.empty
usesF CmpRC{}          = IS.empty
usesF Neg{}            = IS.empty
usesF CpyfP{}          = IS.empty
usesF CpyfM{}          = IS.empty
usesF CpyfE{}          = IS.empty
usesF (Fadd _ _ r0 r1) = fromList [r0, r1]
usesF (Fsub _ _ r0 r1) = fromList [r0, r1]
usesF (Fmul _ _ r0 r1) = fromList [r0, r1]
usesF (FcmpZ _ r)      = singleton r

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
