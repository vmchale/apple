module Asm.Aarch64.CF ( mkControlFlow
                      , expand
                      , udd
                      ) where

import           Asm.Aarch64
import           Asm.BB
import           Asm.CF
import           Asm.M
import           CF
import           Class.E      as E
import           Data.Functor (void, ($>))
import qualified Data.IntSet  as IS

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
addControlFlow (BB asms _:bbs) = do
    { i <- case asms of
        (Label _ l:_) -> lookupLabel l
        _             -> getFresh
    ; (f, bbs') <- next bbs
    ; acc <- case last asms of
            B _ lϵ        -> do {l_i <- lookupLabel lϵ; pure [l_i]}
            Bc _ _ lϵ     -> do {l_i <- lookupLabel lϵ; pure (f [l_i])}
            Tbnz _ _ _ lϵ -> do {l_i <- lookupLabel lϵ; pure $ f [l_i]}
            Tbz _ _ _ lϵ  -> do {l_i <- lookupLabel lϵ; pure $ f [l_i]}
            Cbnz _ _ lϵ   -> do {l_i <- lookupLabel lϵ; pure $ f [l_i]}
            C _ lϵ        -> do {l_i <- lookupLabel lϵ; pure $ f [l_i]}
            RetL _ lϵ     -> lC lϵ
            _             -> pure (f [])
    ; pure (BB asms (ControlAnn i acc (udb asms)) : bbs')
    }

uA :: E reg => Addr reg -> IS.IntSet
uA (R r)      = singleton r
uA (RP r _)   = singleton r
uA (BI b i _) = fromList [b, i]
uA (Po r _)   = singleton r
uA (Pr r _)   = singleton r

dA :: E reg => Addr reg -> IS.IntSet
dA (Po _ 0) = IS.empty
dA (Pr _ 0) = IS.empty
dA (Po r _) = singleton r
dA (Pr r _) = singleton r
dA _        = IS.empty

udb asms = UD (uBB asms) (uBBF asms) (dBB asms) (dBBF asms)
udd asm = UD (uses asm) (usesF asm) (defs asm) (defsF asm)

uBB, dBB :: E reg => [AArch64 reg freg a] -> IS.IntSet
uBB = foldr (\p n -> uses p `IS.union` (n IS.\\ defs p)) IS.empty
dBB = foldMap defs

uBBF, dBBF :: (E freg) => [AArch64 reg freg a] -> IS.IntSet
uBBF = foldr (\p n -> usesF p `IS.union` (n IS.\\ defsF p)) IS.empty
dBBF = foldMap defsF

defs, uses :: E reg => AArch64 reg freg a -> IS.IntSet
uses (MovRR _ _ r)        = singleton r
uses MovRC{}              = IS.empty
uses FMovXX{}             = IS.empty
uses (Ldr _ _ a)          = uA a
uses (LdrS _ _ a)         = uA a
uses (StrS _ _ a)         = uA a
uses (LdrB _ _ a)         = uA a
uses (Str _ r a)          = insert r (uA a)
uses (StrB _ r a)         = insert r (uA a)
uses (Ldp _ _ _ a)        = uA a
uses (Stp _ r0 r1 a)      = fromList [r0, r1] <> uA a
uses (LdrD _ _ a)         = uA a
uses (SubRR _ _ r0 r1)    = fromList [r0, r1]
uses (AddRR _ _ r0 r1)    = fromList [r0, r1]
uses (AddRRS _ _ r0 r1 _) = fromList [r0, r1]
uses (AndRR _ _ r0 r1)    = fromList [r0, r1]
uses (OrRR _ _ r0 r1)     = fromList [r0, r1]
uses (Eor _ _ r0 r1)      = fromList [r0, r1]
uses (Eon _ _ r0 r1)      = fromList [r0, r1]
uses (EorI _ _ r0 _)      = singleton r0
uses ZeroR{}              = IS.empty
uses (AddRC _ _ r _)      = singleton r
uses (SubRC _ _ r _)      = singleton r
uses (SubsRC _ r0 r1 _)   = fromList [r0,r1]
uses (Lsl _ _ r _)        = singleton r
uses (Asr _ _ r _)        = singleton r
uses (CmpRR _ r0 r1)      = fromList [r0, r1]
uses (CmpRC _ r _)        = singleton r
uses (Neg _ _ r)          = singleton r
uses Fmul{}               = IS.empty
uses Fadd{}               = IS.empty
uses Fsub{}               = IS.empty
uses FcmpZ{}              = IS.empty
uses (StrD _ _ a)         = uA a
uses (MulRR _ _ r0 r1)    = fromList [r0, r1]
uses (Madd _ _ r0 r1 r2)  = fromList [r0, r1, r2]
uses (Msub _ _ r0 r1 r2)  = fromList [r0, r1, r2]
uses (Sdiv _ _ r0 r1)     = fromList [r0, r1]
uses Fdiv{}               = IS.empty
uses (Scvtf _ _ r)        = singleton r
uses Fcvtms{}             = IS.empty
uses Fcvtas{}             = IS.empty
uses (FMovDR _ _ r)       = singleton r
uses (MovK _ r _ _)       = singleton r
uses MovZ{}               = IS.empty
uses Fcmp{}               = IS.empty
uses (StpD _ _ _ a)       = uA a
uses (Stp2 _ _ _ a)       = uA a
uses (LdpD _ _ _ a)       = uA a
uses (Ldp2 _ _ _ a)       = uA a
uses Fadd2{}              = IS.empty
uses Fsub2{}              = IS.empty
uses Fmul2{}              = IS.empty
uses Fdiv2{}              = IS.empty
uses Fmadd{}              = IS.empty
uses Fmsub{}              = IS.empty
uses Fsqrt{}              = IS.empty
uses Faddp{}              = IS.empty
uses Fneg{}               = IS.empty
uses Fsqrt2{}             = IS.empty
uses Frintm{}             = IS.empty
uses MrsR{}               = IS.empty
uses (MovRCf _ _ Free)    = singleton CArg0
uses (MovRCf _ _ Malloc)  = singleton CArg0
uses MovRCf{}             = IS.empty
uses LdrRL{}              = IS.empty
uses (Blr _ r)            = singleton r
uses Fmax{}               = IS.empty
uses Fmin{}               = IS.empty
uses Fabs{}               = IS.empty
uses (Csel _ _ r1 r2 _)   = fromList [r1, r2]
uses Fcsel{}              = IS.empty
uses (TstI _ r _)         = singleton r
uses Label{}              = IS.empty
uses Bc{}                 = IS.empty
uses B{}                  = IS.empty
uses (Cbnz _ r _)         = singleton r
uses (Cbz _ r _)          = singleton r
uses (Tbnz _ r _ _)       = singleton r
uses (Tbz _ r _ _)        = singleton r
uses Ret{}                = singleton CArg0
uses Cset{}               = IS.empty
uses ZeroS{}              = IS.empty
uses EorS{}               = IS.empty
uses Bl{}                 = IS.empty
uses C{}                  = singleton ASP
uses RetL{}               = singleton LR
uses (Bfc _ r _ _)        = singleton r
uses MovQQ{}              = IS.empty
uses Fmla{}               = IS.empty
uses Fmls{}               = IS.empty
uses (Dup _ _ r)          = singleton r

defs FMovXX{}            = IS.empty
defs (MovRC _ r _)       = singleton r
defs (MovRR _ r _)       = singleton r
defs (Ldr _ r a)         = insert r (dA a)
defs (LdrB _ r a)        = insert r (dA a)
defs (Str _ _ a)         = dA a
defs (StrB _ _ a)        = dA a
defs (LdrD _ _ a)        = dA a
defs (LdpD _ _ _ a)      = dA a
defs (Stp _ _ _ a)       = dA a
defs (StpD _ _ _ a)      = dA a
defs (Stp2 _ _ _ a)      = dA a
defs (Ldp2 _ _ _ a)      = dA a
defs (Ldp _ r0 r1 a)     = fromList [r0, r1] <> dA a
defs (LdrS _ _ a)        = dA a
defs (StrS _ _ a)        = dA a
defs (SubRR _ r _ _)     = singleton r
defs (AddRR _ r _ _)     = singleton r
defs (AddRRS _ r _ _ _)  = singleton r
defs (AndRR _ r _ _)     = singleton r
defs (OrRR _ r _ _)      = singleton r
defs (Eor _ r _ _)       = singleton r
defs (Eon _ r _ _)       = singleton r
defs (EorI _ r _ _)      = singleton r
defs (ZeroR _ r)         = singleton r
defs (AddRC _ r _ _)     = singleton r
defs (SubRC _ r _ _)     = singleton r
defs (SubsRC _ r _ _)    = singleton r
defs (Lsl _ r _ _)       = singleton r
defs (Asr _ r _ _)       = singleton r
defs CmpRC{}             = IS.empty
defs CmpRR{}             = IS.empty
defs (Neg _ r _)         = singleton r
defs Fmul{}              = IS.empty
defs Fadd{}              = IS.empty
defs Fsub{}              = IS.empty
defs FcmpZ{}             = IS.empty
defs (StrD _ _ a)        = dA a
defs (MulRR _ r _ _)     = singleton r
defs (Madd _ r _ _ _)    = singleton r
defs (Msub _ r _ _ _)    = singleton r
defs (Sdiv _ r _ _)      = singleton r
defs Fdiv{}              = IS.empty
defs Scvtf{}             = IS.empty
defs (Fcvtms _ r _)      = singleton r
defs (Fcvtas _ r _)      = singleton r
defs FMovDR{}            = IS.empty
defs (MovK _ r _ _)      = singleton r
defs (MovZ _ r _ _)      = singleton r
defs Fadd2{}             = IS.empty
defs Fsub2{}             = IS.empty
defs Fmul2{}             = IS.empty
defs Fdiv2{}             = IS.empty
defs Fcmp{}              = IS.empty
defs Fmadd{}             = IS.empty
defs Fmsub{}             = IS.empty
defs Fsqrt{}             = IS.empty
defs Fsqrt2{}            = IS.empty
defs Fneg{}              = IS.empty
defs Faddp{}             = IS.empty
defs Frintm{}            = IS.empty
defs (MrsR _ r)          = singleton r
defs Blr{}               = singleton LR
defs (MovRCf _ r Malloc) = singleton r <> singleton CArg0
defs (MovRCf _ r JR)     = singleton r <> singleton CArg0
defs (LdrRL _ r _)       = singleton r
defs MovRCf{}            = IS.empty
defs Fmax{}              = IS.empty
defs Fmin{}              = IS.empty
defs Fabs{}              = IS.empty
defs (Csel _ r _ _ _)    = singleton r
defs Fcsel{}             = IS.empty
defs TstI{}              = IS.empty
defs Label{}             = IS.empty
defs Bc{}                = IS.empty
defs B{}                 = IS.empty
defs Cbnz{}              = IS.empty
defs Cbz{}               = IS.empty
defs Tbnz{}              = IS.empty
defs Tbz{}               = IS.empty
defs Ret{}               = IS.empty
defs (Cset _ r _)        = singleton r
defs EorS{}              = IS.empty
defs ZeroS{}             = IS.empty
defs Bl{}                = singleton LR
defs C{}                 = fromList [LR, FP]
defs RetL{}              = IS.empty
defs (Bfc _ r _ _)       = singleton r
defs MovQQ{}             = IS.empty
defs Fmla{}              = IS.empty
defs Fmls{}              = IS.empty
defs Dup{}               = IS.empty

defsF, usesF :: (E freg) => AArch64 reg freg ann -> IS.IntSet
defsF (FMovXX _ r _)     = singleton r
defsF MovRR{}            = IS.empty
defsF MovRC{}            = IS.empty
defsF Ldr{}              = IS.empty
defsF LdrB{}             = IS.empty
defsF Str{}              = IS.empty
defsF StrB{}             = IS.empty
defsF (LdrD _ r _)       = singleton r
defsF (Ldp2 _ q0 q1 _)   = fromList [q0, q1]
defsF (LdrS _ q _)       = singleton q
defsF AddRR{}            = IS.empty
defsF AddRRS{}           = IS.empty
defsF SubRR{}            = IS.empty
defsF AndRR{}            = IS.empty
defsF OrRR{}             = IS.empty
defsF Eor{}              = IS.empty
defsF Eon{}              = IS.empty
defsF EorI{}             = IS.empty
defsF ZeroR{}            = IS.empty
defsF AddRC{}            = IS.empty
defsF SubRC{}            = IS.empty
defsF SubsRC{}           = IS.empty
defsF Lsl{}              = IS.empty
defsF Asr{}              = IS.empty
defsF CmpRR{}            = IS.empty
defsF CmpRC{}            = IS.empty
defsF Neg{}              = IS.empty
defsF (Fmul _ r _ _)     = singleton r
defsF (Fadd _ r _ _)     = singleton r
defsF (Fsub _ r _ _)     = singleton r
defsF (Fadd2 _ v _ _)    = singleton v
defsF (Fsub2 _ v _ _)    = singleton v
defsF (Fmul2 _ v _ _)    = singleton v
defsF (Fdiv2 _ v _ _)    = singleton v
defsF (Fsqrt2 _ v _)     = singleton v
defsF FcmpZ{}            = IS.empty
defsF (Fdiv _ d _ _)     = singleton d
defsF (Faddp _ d _)      = singleton d
defsF StrD{}             = IS.empty
defsF MulRR{}            = IS.empty
defsF Madd{}             = IS.empty
defsF Msub{}             = IS.empty
defsF Sdiv{}             = IS.empty
defsF (Scvtf _ r _)      = singleton r
defsF Fcvtms{}           = IS.empty
defsF Fcvtas{}           = IS.empty
defsF (FMovDR _ r _)     = singleton r
defsF MovK{}             = IS.empty
defsF MovZ{}             = IS.empty
defsF Fcmp{}             = IS.empty
defsF (LdpD _ r0 r1 _)   = fromList [r0, r1]
defsF Ldp{}              = IS.empty
defsF Stp{}              = IS.empty
defsF StpD{}             = IS.empty
defsF Stp2{}             = IS.empty
defsF (Fmadd _ d0 _ _ _) = singleton d0
defsF (Fmsub _ d0 _ _ _) = singleton d0
defsF (Fsqrt _ d _)      = singleton d
defsF (Fneg _ d _)       = singleton d
defsF (Frintm _ d _)     = singleton d
defsF MrsR{}             = IS.empty
defsF Blr{}              = IS.empty
defsF (MovRCf _ _ Exp)   = singleton FArg0
defsF (MovRCf _ _ Log)   = singleton FArg0
defsF (MovRCf _ _ Pow)   = singleton FArg0
defsF (MovRCf _ _ DR)    = singleton FArg0
defsF MovRCf{}           = IS.empty
defsF LdrRL{}            = IS.empty
defsF (Fmax _ d _ _)     = singleton d
defsF (Fmin _ d _ _)     = singleton d
defsF (Fabs _ d _)       = singleton d
defsF Csel{}             = IS.empty
defsF TstI{}             = IS.empty
defsF (Fcsel _ d0 _ _ _) = singleton d0
defsF Label{}            = IS.empty
defsF Bc{}               = IS.empty
defsF B{}                = IS.empty
defsF Cbnz{}             = IS.empty
defsF Cbz{}              = IS.empty
defsF Tbnz{}             = IS.empty
defsF Tbz{}              = IS.empty
defsF Ret{}              = IS.empty
defsF RetL{}             = IS.empty
defsF (ZeroS _ v)        = singleton v
defsF (EorS _ v _ _)     = singleton v
defsF Cset{}             = IS.empty
defsF Bl{}               = IS.empty
defsF C{}                = IS.empty
defsF Bfc{}              = IS.empty
defsF (MovQQ _ v _)      = singleton v
defsF (Fmla _ v _ _)     = singleton v
defsF (Fmls _ v _ _)     = singleton v
defsF (Dup _ v _)        = singleton v
defsF StrS{}             = IS.empty

usesF (FMovXX _ _ r)       = singleton r
usesF MovRR{}              = IS.empty
usesF MovRC{}              = IS.empty
usesF Ldr{}                = IS.empty
usesF LdrB{}               = IS.empty
usesF LdrD{}               = IS.empty
usesF LdrS{}               = IS.empty
usesF Str{}                = IS.empty
usesF StrB{}               = IS.empty
usesF AddRR{}              = IS.empty
usesF AddRRS{}             = IS.empty
usesF SubRR{}              = IS.empty
usesF ZeroR{}              = IS.empty
usesF AndRR{}              = IS.empty
usesF OrRR{}               = IS.empty
usesF Eor{}                = IS.empty
usesF Eon{}                = IS.empty
usesF EorI{}               = IS.empty
usesF AddRC{}              = IS.empty
usesF SubRC{}              = IS.empty
usesF SubsRC{}             = IS.empty
usesF Lsl{}                = IS.empty
usesF Asr{}                = IS.empty
usesF CmpRR{}              = IS.empty
usesF CmpRC{}              = IS.empty
usesF Neg{}                = IS.empty
usesF (Fadd _ _ r0 r1)     = fromList [r0, r1]
usesF (Fsub _ _ r0 r1)     = fromList [r0, r1]
usesF (Fmul _ _ r0 r1)     = fromList [r0, r1]
usesF (Fdiv _ _ r0 r1)     = fromList [r0, r1]
usesF (Fadd2 _ _ q0 q1)    = fromList [q0,q1]
usesF (Fsub2 _ _ q0 q1)    = fromList [q0,q1]
usesF (Fmul2 _ _ q0 q1)    = fromList [q0,q1]
usesF (Fdiv2 _ _ q0 q1)    = fromList [q0,q1]
usesF (Fsqrt2 _ _ v)       = singleton v
usesF (FcmpZ _ r)          = singleton r
usesF (StrD _ r _)         = singleton r
usesF MulRR{}              = IS.empty
usesF Madd{}               = IS.empty
usesF Msub{}               = IS.empty
usesF Sdiv{}               = IS.empty
usesF (Faddp _ _ v)        = singleton v
usesF Scvtf{}              = IS.empty
usesF (Fcvtms _ _ r)       = singleton r
usesF (Fcvtas _ _ r)       = singleton r
usesF MovK{}               = IS.empty
usesF MovZ{}               = IS.empty
usesF FMovDR{}             = IS.empty
usesF (Fcmp _ r0 r1)       = fromList [r0, r1]
usesF (StpD _ r0 r1 _)     = fromList [r0, r1]
usesF (Stp2 _ q0 q1 _)     = fromList [q0, q1]
usesF Ldp2{}               = IS.empty
usesF Stp{}                = IS.empty
usesF Ldp{}                = IS.empty
usesF LdpD{}               = IS.empty
usesF (Fmadd _ _ d0 d1 d2) = fromList [d0, d1, d2]
usesF (Fmsub _ _ d0 d1 d2) = fromList [d0, d1, d2]
usesF (Fsqrt _ _ d)        = singleton d
usesF (Fneg _ _ d)         = singleton d
usesF (Frintm _ _ d)       = singleton d
usesF MrsR{}               = IS.empty
usesF Blr{}                = IS.empty
usesF (MovRCf _ _ Exp)     = singleton FArg0
usesF (MovRCf _ _ Log)     = singleton FArg0
usesF (MovRCf _ _ Pow)     = fromList [FArg0, FArg1]
usesF MovRCf{}             = IS.empty
usesF LdrRL{}              = IS.empty
usesF (Fmax _ _ d0 d1)     = fromList [d0, d1]
usesF (Fmin _ _ d0 d1)     = fromList [d0, d1]
usesF (Fabs _ _ d)         = singleton d
usesF Csel{}               = IS.empty
usesF TstI{}               = IS.empty
usesF (Fcsel _ _ d0 d1 _)  = fromList [d0, d1]
usesF Label{}              = IS.empty
usesF Bc{}                 = IS.empty
usesF B{}                  = IS.empty
usesF Cbnz{}               = IS.empty
usesF Cbz{}                = IS.empty
usesF Tbnz{}               = IS.empty
usesF Tbz{}                = IS.empty
usesF Ret{}                = fromList [FArg0, FArg1]
usesF Cset{}               = IS.empty
usesF Bl{}                 = IS.empty
usesF C{}                  = IS.empty
usesF RetL{}               = IS.empty
usesF Bfc{}                = IS.empty
usesF ZeroS{}              = IS.empty
usesF (Fmla _ v0 v1 v2)    = fromList [v0,v1,v2]
usesF (Fmls _ v0 v1 v2)    = fromList [v0,v1,v2]
usesF (MovQQ _ _ vS)       = singleton vS
usesF (StrS _ v _)         = singleton v
usesF Dup{}                = IS.empty

next :: (E reg, E freg) => [BB AArch64 reg freg () ()] -> FreshM ([N] -> [N], [BB AArch64 reg freg () ControlAnn])
next bbs = do
    nextBs <- addControlFlow bbs
    case nextBs of
        []    -> pure (id, [])
        (b:_) -> pure ((node (caBB b) :), nextBs)

broadcasts :: [BB AArch64 reg freg a ()] -> FreshM ()
broadcasts [] = pure ()
broadcasts ((BB asms@(asm:_) _):bbs@((BB (Label _ retL:_) _):_)) | C _ l <- last asms = do
    { i <- fm retL; b3 i l
    ; case asm of {Label _ lϵ -> void $ fm lϵ; _ -> pure ()}
    ; broadcasts bbs
    }
broadcasts ((BB (Label _ l:_) _):asms) = fm l *> broadcasts asms
broadcasts (_:asms) = broadcasts asms
