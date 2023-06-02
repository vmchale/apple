module Asm.Aarch64.CF ( mkControlFlow
                      , expand, udd
                      ) where

import           Asm.Aarch64
import           Asm.BB
import           Asm.CF
import           Asm.M
import           CF
import           Class.E      as E
import           Data.Functor (($>))
import qualified Data.IntSet  as IS
import           Data.List    (foldl')

mkControlFlow :: (E reg, E freg) => [BB AArch64 reg freg () ()] -> [BB AArch64 reg freg () ControlAnn]
mkControlFlow instrs = runFreshM (broadcasts instrs *> addControlFlow instrs)

expand :: (E reg, E freg) => BB AArch64 reg freg () Liveness -> [AArch64 reg freg Liveness]
expand (BB asms@(_:_) li) = scanr (\n p -> lN n (ann p)) lS iasms
    where lN a s =
            let ai=uses a <> (ao IS.\\ defs a)
                ao=ins s
                aif=usesF a <> (aof IS.\\ defsF a)
                aof=fins s
            in a $> Liveness ai ao aif aof
          lS = let ao=out li
                   aof=fout li
               in asm $> Liveness (uses asm `IS.union` (ao `IS.difference` defs asm)) ao (usesF asm `IS.union` (aof `IS.difference` defsF asm)) aof
          (iasms, asm) = (init asms, last asms)
expand _ = []

addControlFlow :: (E reg, E freg) => [BB AArch64 reg freg () ()] -> FreshM [BB AArch64 reg freg () ControlAnn]
addControlFlow [] = pure []
addControlFlow (BB [] _:bbs) = addControlFlow bbs
addControlFlow (BB asms@(Label _ l:_) _:bbs) = do
    { i <- lookupLabel l
    ; (f, bbs') <- next bbs
    ; acc <- case last asms of
            Bc _ _ lϵ     -> do {l_i <- lookupLabel lϵ; pure (f [l_i])}
            B _ lϵ        -> do {l_i <- lookupLabel lϵ; pure [l_i]}
            Tbnz _ _ _ lϵ -> do {l_i <- lookupLabel lϵ; pure $ f [l_i]}
            Tbz _ _ _ lϵ  -> do {l_i <- lookupLabel lϵ; pure $ f [l_i]}
            Cbnz _ _ lϵ   -> do {l_i <- lookupLabel lϵ; pure $ f [l_i]}
            _             -> pure (f [])
    ; pure (BB asms (ControlAnn i acc (udb asms)) : bbs')
    }
addControlFlow (BB asms _:bbs) = do
    { i <- getFresh
    ; (f, bbs') <- next bbs
    ; acc <- case last asms of
            Bc _ _ lϵ     -> do {l_i <- lookupLabel lϵ; pure (f [l_i])}
            B _ lϵ        -> do {l_i <- lookupLabel lϵ; pure [l_i]}
            Tbnz _ _ _ lϵ -> do {l_i <- lookupLabel lϵ; pure $ f [l_i]}
            Tbz _ _ _ lϵ  -> do {l_i <- lookupLabel lϵ; pure $ f [l_i]}
            Cbnz _ _ lϵ   -> do {l_i <- lookupLabel lϵ; pure $ f [l_i]}
            _             -> pure (f [])
    ; pure (BB asms (ControlAnn i acc (udb asms)) : bbs')
    }
{-
addControlFlow (BB [Bc _ c l] _:bbs) = do
    { i <- getFresh
    ; (f, bbs') <- next bbs
    ; l_i <- lookupLabel l
    ; pure (BB [Bc () c l] (ControlAnn i (f [l_i]) (UD IS.empty IS.empty IS.empty IS.empty)) : bbs')
    }
addControlFlow (BB [B _ l] _:bbs) = do
    { i <- getFresh
    ; nextAsms <- addControlFlow bbs
    ; l_i <- lookupLabel l
    ; pure (BB [B () l] (ControlAnn i [l_i] (UD IS.empty IS.empty IS.empty IS.empty)) : nextAsms)
    }
addControlFlow (BB [Cbnz _ r l] _:bbs) = do
    { i <- getFresh
    ; (f, bbs') <- next bbs
    ; l_i <- lookupLabel l
    ; pure (BB [Cbnz () r l] (ControlAnn i (f [l_i]) (UD (singleton r) IS.empty IS.empty IS.empty)) : bbs')
    }
addControlFlow (BB [Tbnz _ r n l] _:bbs) = do
    { i <- getFresh
    ; (f, bbs') <- next bbs
    ; l_i <- lookupLabel l
    ; pure (BB [Tbnz () r n l] (ControlAnn i (f [l_i]) (UD (singleton r) IS.empty IS.empty IS.empty)) : bbs')
    }
addControlFlow (BB [Tbz _ r n l] _:bbs) = do
    { i <- getFresh
    ; (f, bbs') <- next bbs
    ; l_i <- lookupLabel l
    ; pure (BB [Tbz () r n l] (ControlAnn i (f [l_i]) (UD (singleton r) IS.empty IS.empty IS.empty)) : bbs')
    }
-}
-- addControlFlow (BB [Ret _] _:asms) = do
    -- { i <- getFresh
    -- ; nextAsms <- addControlFlow asms
    -- ; pure (BB [Ret ()] (ControlAnn i [] (UD (singleton CArg0) (fromList [FArg0, FArg1]) IS.empty IS.empty)) : nextAsms)
    -- }

uA :: E reg => Addr reg -> IS.IntSet
uA (R r)      = singleton r
uA (RP r _)   = singleton r
uA (BI b i _) = fromList [b, i]

udb asms = UD (uBB asms) (uBBF asms) (dBB asms) (dBBF asms)
udd asm = UD (uses asm) (usesF asm) (defs asm) (defsF asm)

uBB, dBB :: E reg => [AArch64 reg freg a] -> IS.IntSet
uBB = fst . foldl' (\(pU, pD) n -> (pU `IS.union` (uses n `IS.difference` pD), defs n `IS.union` pU)) (IS.empty, IS.empty)
dBB = foldMap defs

uBBF, dBBF :: E freg => [AArch64 reg freg a] -> IS.IntSet
uBBF = fst . foldl' (\(pU, pD) n -> (pU `IS.union` (usesF n `IS.difference` pD), defsF n `IS.union` pD)) (IS.empty, IS.empty)
dBBF = foldMap defsF

defs, uses :: E reg => AArch64 reg freg a -> IS.IntSet
uses (MovRR _ _ r)       = singleton r
uses MovRC{}             = IS.empty
uses FMovXX{}            = IS.empty
uses FMovXC{}            = IS.empty
uses (Ldr _ _ a)         = uA a
uses (Str _ r a)         = IS.insert (E.toInt r) $ uA a
uses (Ldp _ _ _ a)       = uA a
uses (Stp _ r0 r1 a)     = fromList [r0, r1] <> uA a
uses (LdrD _ _ a)        = uA a
uses (SubRR _ _ r0 r1)   = fromList [r0, r1]
uses (AddRR _ _ r0 r1)   = fromList [r0, r1]
uses (AddRC _ _ r _)     = singleton r
uses (SubRC _ _ r _)     = singleton r
uses (Lsl _ _ r _)       = singleton r
uses (Asr _ _ r _)       = singleton r
uses (CmpRR _ r0 r1)     = fromList [r0, r1]
uses (CmpRC _ r _)       = singleton r
uses (Neg _ _ r)         = singleton r
uses Fmul{}              = IS.empty
uses Fadd{}              = IS.empty
uses Fsub{}              = IS.empty
uses FcmpZ{}             = IS.empty
uses (StrD _ _ a)        = uA a
uses (MulRR _ _ r0 r1)   = fromList [r0, r1]
uses (Madd _ _ r0 r1 r2) = fromList [r0, r1, r2]
uses Fdiv{}              = IS.empty
uses (Scvtf _ _ r)       = singleton r
uses Fcvtms{}            = IS.empty
uses (FMovDR _ _ r)      = singleton r
uses MovK{}              = IS.empty
uses Fcmp{}              = IS.empty
uses (StpD _ _ _ a)      = uA a
uses (LdpD _ _ _ a)      = uA a
uses Fmadd{}             = IS.empty
uses Fmsub{}             = IS.empty
uses Fsqrt{}             = IS.empty
uses MrsR{}              = IS.empty
uses MovRCf{}            = singleton CArg0
uses (Blr _ r)           = singleton r
uses Fmax{}              = IS.empty
uses Fabs{}              = IS.empty
uses (Csel _ _ r1 r2 _)  = fromList [r1, r2]
uses Fcsel{}             = IS.empty
uses (TstI _ r _)        = singleton r
uses Label{}             = IS.empty
uses Bc{}                = IS.empty
uses B{}                 = IS.empty
uses (Cbnz _ r _)        = singleton r
uses (Tbnz _ r _ _)      = singleton r
uses (Tbz _ r _ _)       = singleton r
uses Ret{}               = singleton CArg0

defs FMovXX{}            = IS.empty
defs FMovXC{}            = IS.empty
defs (MovRC _ r _)       = singleton r
defs (MovRR _ r _)       = singleton r
defs (Ldr _ r _)         = singleton r
defs Str{}               = IS.empty
defs LdrD{}              = IS.empty
defs LdpD{}              = IS.empty
defs Stp{}               = IS.empty
defs StpD{}              = IS.empty
defs (Ldp _ r0 r1 _)     = fromList [r0, r1]
defs (SubRR _ r _ _)     = singleton r
defs (AddRR _ r _ _)     = singleton r
defs (AddRC _ r _ _)     = singleton r
defs (SubRC _ r _ _)     = singleton r
defs (Lsl _ r _ _)       = singleton r
defs (Asr _ r _ _)       = singleton r
defs CmpRC{}             = IS.empty
defs CmpRR{}             = IS.empty
defs (Neg _ r _)         = singleton r
defs Fmul{}              = IS.empty
defs Fadd{}              = IS.empty
defs Fsub{}              = IS.empty
defs FcmpZ{}             = IS.empty
defs StrD{}              = IS.empty
defs (MulRR _ r _ _)     = singleton r
defs (Madd _ r _ _ _)    = singleton r
defs Fdiv{}              = IS.empty
defs Scvtf{}             = IS.empty
defs (Fcvtms _ r _)      = singleton r
defs FMovDR{}            = IS.empty
defs (MovK _ r _ _)      = singleton r
defs Fcmp{}              = IS.empty
defs Fmadd{}             = IS.empty
defs Fmsub{}             = IS.empty
defs Fsqrt{}             = IS.empty
defs (MrsR _ r)          = singleton r
defs Blr{}               = IS.empty
defs (MovRCf _ _ Free{}) = IS.empty
defs (MovRCf _ r Malloc) = singleton r <> singleton CArg0
defs Fmax{}              = IS.empty
defs Fabs{}              = IS.empty
defs (Csel _ r _ _ _)    = singleton r
defs Fcsel{}             = IS.empty
defs TstI{}              = IS.empty
defs Label{}             = IS.empty
defs Bc{}                = IS.empty
defs B{}                 = IS.empty
defs Cbnz{}              = IS.empty
defs Tbnz{}              = IS.empty
defs Tbz{}               = IS.empty
defs Ret{}               = IS.empty

defsF, usesF :: E freg => AArch64 reg freg ann -> IS.IntSet
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
defsF Asr{}              = IS.empty
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
defsF Madd{}             = IS.empty
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
defsF (Fmsub _ d0 _ _ _) = singleton d0
defsF (Fsqrt _ d _)      = singleton d
defsF MrsR{}             = IS.empty
defsF Blr{}              = IS.empty
defsF MovRCf{}           = IS.empty
defsF (Fmax _ d _ _)     = singleton d
defsF (Fabs _ d _)       = singleton d
defsF Csel{}             = IS.empty
defsF TstI{}             = IS.empty
defsF (Fcsel _ d0 _ _ _) = singleton d0
defsF Label{}            = IS.empty
defsF Bc{}               = IS.empty
defsF B{}                = IS.empty
defsF Cbnz{}             = IS.empty
defsF Tbnz{}             = IS.empty
defsF Tbz{}              = IS.empty
defsF Ret{}              = fromList [FArg0, FArg1]

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
usesF Asr{}                = IS.empty
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
usesF Madd{}               = IS.empty
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
usesF (Fmsub _ _ d0 d1 d2) = fromList [d0, d1, d2]
usesF (Fsqrt _ _ d)        = singleton d
usesF MrsR{}               = IS.empty
usesF Blr{}                = IS.empty
usesF MovRCf{}             = IS.empty
usesF (Fmax _ _ d0 d1)     = fromList [d0, d1]
usesF (Fabs _ _ d)         = singleton d
usesF Csel{}               = IS.empty
usesF TstI{}               = IS.empty
usesF (Fcsel _ _ d0 d1 _)  = fromList [d0, d1]
usesF Label{}              = IS.empty
usesF Bc{}                 = IS.empty
usesF B{}                  = IS.empty
usesF Cbnz{}               = IS.empty
usesF Tbnz{}               = IS.empty
usesF Tbz{}                = IS.empty
usesF Ret{}                = IS.empty

next :: (E reg, E freg) => [BB AArch64 reg freg () ()] -> FreshM ([Int] -> [Int], [BB AArch64 reg freg () ControlAnn])
next bbs = do
    nextBs <- addControlFlow bbs
    case nextBs of
        []    -> pure (id, [])
        (b:_) -> pure ((node (caBB b) :), nextBs)

broadcasts :: [BB AArch64 reg freg a ()] -> FreshM [BB AArch64 reg freg a ()]
broadcasts [] = pure []
broadcasts (b@(BB (Label _ l:_) _):asms) = do
    { i <- getFresh
    ; broadcast i l
    ; (b :) <$> broadcasts asms
    }
broadcasts (asm:asms) = (asm :) <$> broadcasts asms
