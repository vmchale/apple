module Asm.Aarch64.T ( irToAarch64 ) where

import           Asm.Aarch64
import           Asm.M
import           Control.Monad.Trans.State.Strict (runState)
import           Data.Bifunctor                   (second)
import           Data.Bits                        (rotateR, shiftR, (.&.))
import           Data.Tuple                       (swap)
import           Data.Word                        (Word16, Word64, Word8)
import           GHC.Float                        (castDoubleToWord64)
import qualified IR
import qualified Op

absReg :: IR.Temp -> AbsReg
absReg (IR.ITemp i) = IReg i; absReg (IR.ATemp i) = IReg i
absReg IR.C0 = CArg0; absReg IR.C1 = CArg1; absReg IR.C2 = CArg2
absReg IR.C3 = CArg3; absReg IR.C4 = CArg4; absReg IR.C5 = CArg5
absReg IR.CRet = CArg0

fabsReg :: IR.FTemp -> FAbsReg
fabsReg (IR.FTemp i) = FReg i
fabsReg IR.F0 = FArg0; fabsReg IR.F1 = FArg1; fabsReg IR.F2 = FArg2
fabsReg IR.F3 = FArg3; fabsReg IR.F4 = FArg4; fabsReg IR.F5 = FArg5
fabsReg IR.FRet = FArg0; fabsReg IR.FRet1 = FArg1

mB Op.AndB = Just AndRR
mB Op.OrB  = Just OrRR
mB Op.XorB = Just Eor
mB Op.BEq  = Nothing

f2absReg :: IR.F2 -> V2Reg FAbsReg
f2absReg (IR.F2Temp i) = V2Reg (FReg i)

mIop Op.IPlus  = Just AddRR
mIop Op.IMinus = Just SubRR
mIop Op.ITimes = Just MulRR
mIop Op.IDiv   = Just Sdiv
mIop (Op.BI b) = mB b
mIop _         = Nothing

mFop Op.FPlus  = Just Fadd
mFop Op.FMinus = Just Fsub
mFop Op.FTimes = Just Fmul
mFop Op.FDiv   = Just Fdiv
mFop Op.FMax   = Just Fmax
mFop Op.FMin   = Just Fmin
mFop _         = Nothing

frel :: Op.FRel -> Cond
frel Op.FGeq = Geq
frel Op.FLeq = Leq
frel Op.FGt  = Gt
frel Op.FLt  = Lt
frel Op.FEq  = Eq
frel Op.FNeq = Neq

iop :: Op.IRel -> Cond
iop Op.IEq  = Eq
iop Op.INeq = Neq
iop Op.IGeq = Geq
iop Op.ILeq = Leq
iop Op.IGt  = Gt
iop Op.ILt  = Lt

nR :: WM AbsReg
nR = IReg <$> nextI

nextF :: WM FAbsReg
nextF = FReg <$> nextI

nQ :: WM (V2Reg FAbsReg)
nQ = V2Reg<$>nextF

irToAarch64 :: IR.WSt -> [IR.Stmt] -> (Int, [AArch64 AbsReg FAbsReg ()])
irToAarch64 st = swap . second IR.wtemps . flip runState st . foldMapA ir

aR8 :: AbsReg -> WM [AArch64 AbsReg FAbsReg ()]
aR8 t = do
    tϵ <- nR
    pure [AddRC () tϵ t 8 IZero, TstI () t (BM 1 3), Csel () t tϵ t Neq]

aR :: IR.Temp -> WM [AArch64 AbsReg FAbsReg ()]
aR t = do
    tϵ <- nextI
    pl <- eval (32-IR.IB Op.IRem (IR.Reg t) 16) (IR.ITemp tϵ)
    pure $ pl ++ [AddRR () t' t' (IReg tϵ)]
  where t'=absReg t

plF :: IR.FE -> WM ([AArch64 AbsReg FAbsReg ()] -> [AArch64 AbsReg FAbsReg ()], FAbsReg)
plF (IR.FReg t) = pure (id, fabsReg t)
plF e           = do {i <- nextI; pl <- feval e (IR.FTemp i); pure ((pl++), FReg i)}

plF2 :: IR.F2E -> WM ([AArch64 AbsReg FAbsReg ()] -> [AArch64 AbsReg FAbsReg ()], V2Reg FAbsReg)
plF2 (IR.FReg t) = pure (id, f2absReg t)
plF2 e           = do {i <- nextI; pl <- f2eval e (IR.F2Temp i); pure ((pl++), V2Reg (FReg i))}

plI :: IR.Exp -> WM ([AArch64 AbsReg FAbsReg ()] -> [AArch64 AbsReg FAbsReg ()], AbsReg)
plI (IR.Reg t) = pure (id, absReg t)
plI e          = do {i <- nextI; pl <- eval e (IR.ITemp i); pure ((pl++), IReg i)}

ir :: IR.Stmt -> WM [AArch64 AbsReg FAbsReg ()]
ir (IR.R l)      = pure [RetL () l]
ir (IR.L l)      = pure [Label () l]
ir (IR.J l)      = pure [B () l]
ir (IR.C l)      = pure [C () l]
ir (IR.MX t e)   = feval e t
ir (IR.MX2 t e)  = f2eval e t
ir (IR.MT t e)   = eval e t
ir (IR.Ins (IR.F2Temp i) t) = pure [FMovXX () (FReg i) (fabsReg t)]
ir (IR.S2 Op.FPlus t r) = pure [Faddp () (fabsReg t) (f2absReg r)]
ir (IR.S2 Op.FMax t r) = pure [Fmaxp () (fabsReg t) (f2absReg r)]
ir (IR.S2 Op.FMin t r) = pure [Fminp () (fabsReg t) (f2absReg r)]
ir (IR.Fill2 r t) = pure [DupD () (f2absReg r) (fabsReg t)]
ir (IR.Ma _ t e) = do {r <- nR; plE <- eval e IR.C0; pure $ plE ++ [puL, AddRC () FP ASP 16 IZero, MovRCf () r Malloc, Blr () r, MovRR () (absReg t) CArg0, poL]}
ir (IR.Free t) = do {r <- nR; pure [puL, MovRR () CArg0 (absReg t), AddRC () FP ASP 16 IZero, MovRCf () r Free, Blr () r, poL]}
ir (IR.Sa8 t (IR.ConstI i)) | Just u <- mu16 (sai i) = pure [SubRC () ASP ASP u IZero, MovRR () (absReg t) ASP]
ir (IR.Sa8 t (IR.Reg r)) = let r'=absReg r in do {plR <- aR8 r'; pure $ plR++[SubRR () ASP ASP r', MovRR () (absReg t) ASP]}
ir (IR.Sa8 t e) = do
    r <- nextI; plE <- eval e (IR.ITemp r)
    let r'=IReg r
    plR <- aR8 r'
    pure $ plE ++ plR ++ [SubRR () ASP ASP r', MovRR () (absReg t) ASP]
ir (IR.Sa t e) = do
    r <- nextI
    let irr=IR.ITemp r
    plE <- eval e irr
    let r'=IReg r
    plR <- aR irr
    pure $ plE ++ plR ++ [SubRR () ASP ASP r', MovRR () (absReg t) ASP]
ir (IR.Pop8 (IR.ConstI i)) | Just u <- mu16 (sai i) = pure [AddRC () ASP ASP u IZero]
ir (IR.Pop8 (IR.Reg r)) = let r'=absReg r in do {plR <- aR8 r'; pure $ plR ++ [AddRR () ASP ASP r']}
ir (IR.Pop8 e) = do
    r <- nextI; plE <- eval e (IR.ITemp r)
    let r'=IReg r
    plR <- aR8 r'
    pure $ plE ++ plR ++ [AddRR () ASP ASP r']
ir (IR.Pop e) = do
    r <- nextI
    let irr=IR.ITemp r
    plE <- eval e irr
    let r'=IReg r
    plR <- aR irr
    pure $ plE ++ plR ++ [AddRR () ASP ASP r']
ir (IR.Wr (IR.AP t Nothing _) e) = do
    (plE,r) <- plI e
    pure $ plE [Str () r (R $ absReg t)]
ir (IR.Wr (IR.AP t (Just (IR.ConstI i)) _) e) | Just p <- mp i = do
    (plE,r) <- plI e
    pure $ plE [Str () r (RP (absReg t) p)]
ir (IR.Wr (IR.AP t (Just (IR.IB Op.IAsl eI (IR.ConstI 3))) _) e) = do
    (plE,r) <- plI e; (plEI,rI) <- plI eI
    pure $ plE $ plEI [Str () r (BI (absReg t) rI Three)]
ir (IR.Wr (IR.AP t (Just (IR.IB Op.IPlus (IR.IB Op.IAsl eI (IR.ConstI 3)) (IR.ConstI i))) _) e) | (ix, 0) <- i `quotRem` 8 = do
    rI <- nextI
    plEI <- eval (eI+IR.ConstI ix) (IR.ITemp rI)
    (plE,r) <- plI e
    pure $ plE $ plEI ++ [Str () r (BI (absReg t) (IReg rI) Three)]
ir (IR.Wr (IR.AP t (Just eI) _) e) = do
    (plE,r) <- plI e; (plEI,rI) <- plI eI
    pure $ plE $ plEI [Str () r (BI (absReg t) rI Zero)]
ir (IR.WrF2 (IR.AP t Nothing _) e) = do
    (plE,v) <- plF2 e
    pure$plE [StrS () v (R (absReg t))]
ir (IR.WrF2 (IR.AP t (Just eI) _) e) = do
    (plEI, rI) <- plI eI
    (plE,v) <- plF2 e
    pure$plE$plEI [StrS () v (BI (absReg t) rI Zero)]
ir (IR.WrB (IR.AP t (Just (IR.ConstI i)) _) (IR.ConstI n)) | Just iu <- mu16 i, Just u <- mu16 n = do
    r <- nR
    pure [MovRC () r u, StrB () r (RP (absReg t) iu)]
ir (IR.WrB (IR.AP t (Just eI) _) (IR.ConstI n)) | Just u <- mu16 n = do
    i <- nR
    (plEI,rI) <- plI eI
    pure $ plEI [MovRC () i u, StrB () i (BI (absReg t) rI Zero)]
ir (IR.WrB (IR.AP t (Just (IR.ConstI ix)) _) (IR.Is i)) | Just iu <- mu16 ix = do
    pure [StrB () (absReg i) (RP (absReg t) iu)]
ir (IR.WrB (IR.AP t Nothing _) (IR.Is i)) =
    pure [StrB () (absReg i) (R (absReg t))]
ir (IR.WrB (IR.AP t (Just eI) _) (IR.Is i)) = do
    (plEI,rI) <- plI eI
    pure $ plEI [StrB () (absReg i) (BI (absReg t) rI Zero)]
ir (IR.WrF (IR.AP tB (Just (IR.IB Op.IAsl eI (IR.ConstI 3))) _) e) = do
    (plEI,iI) <- plI eI
    (plE,i) <- plF e
    pure $ plE $ plEI [StrD () i (BI (absReg tB) iI Three)]
ir (IR.WrF (IR.AP tB (Just (IR.IB Op.IPlus (IR.IB Op.IAsl eI (IR.ConstI 3)) (IR.ConstI ix8))) _) e) | (ix, 0) <- ix8 `quotRem` 8 = do
    iI <- nextI
    plEI <- eval (eI+IR.ConstI ix) (IR.ITemp iI)
    (plE,i) <- plF e
    pure $ plE $ plEI ++ [StrD () i (BI (absReg tB) (IReg iI) Three)]
ir (IR.WrF (IR.AP t (Just eI) _) e) = do
    (plE,i) <- plF e; (plEI,iI) <- plI eI
    pure $ plE $ plEI [StrD () i (BI (absReg t) iI Zero)]
ir (IR.WrF (IR.AP t Nothing _) e) = do
    (plE,i) <- plF e
    pure $ plE [StrD () i (R (absReg t))]
ir (IR.MJ (IR.IRel Op.INeq e (IR.ConstI 0)) l) = do
    (plE,r) <- plI e
    pure $ plE [Cbnz () r l]
ir (IR.MJ (IR.Is r) l) =
    pure [Cbnz () (absReg r) l]
ir (IR.MJ (IR.IU Op.IEven e) l) = do
    (plE,r) <- plI e
    pure $ plE [Tbz () r 0 l]
ir (IR.MJ (IR.IRel op e (IR.ConstI i)) l) | c <- iop op, Just u <- m12 i = do
    (plE,r) <- plI e
    pure $ plE [CmpRC () r u, Bc () c l]
ir (IR.MJ (IR.IRel op e0 e1) l) | c <- iop op = do
    (plE0,r0) <- plI e0; (plE1,r1) <- plI e1
    pure $ plE0 $ plE1 [CmpRR () r0 r1, Bc () c l]
ir (IR.MJ (IR.FRel op e (IR.ConstF 0)) l) | c <- frel op = do
    (plE,i) <- plF e
    pure $ plE [FcmpZ () i, Bc () c l]
ir (IR.MJ (IR.FRel op e0 e1) l) | c <- frel op = do
    (plE0,r0) <- plF e0; (plE1,r1) <- plF e1
    pure $ plE0 $ plE1 [Fcmp () r0 r1, Bc () c l]
ir (IR.MJ (IR.BU Op.BNeg p) l) = do
    (plE,r) <- plI p
    pure $ plE [Cbz () r l]
ir (IR.MJ p l) = do
    (plE,r) <- plI p
    pure $ plE [Cbnz () r l]
ir (IR.Cmov (IR.IRel op e0 e1) t e) | c <- iop op = do
    (plE0,r0) <- plI e0; (plE1,r1) <- plI e1
    (plE,r) <- plI e
    pure $ plE $ plE0 $ plE1 [CmpRR () r0 r1, Csel () (absReg t) r (absReg t) c]
ir (IR.Cset t (IR.IRel op e0 (IR.ConstI i))) | c <- iop op, Just u <- m12 i = do
    (plE0,r0) <- plI e0
    pure $ plE0 [CmpRC () r0 u, Cset () (absReg t) c]
ir (IR.Cset t (IR.IRel op e0 e1)) | c <- iop op = do
    (plE0,r0) <- plI e0; (plE1,r1) <- plI e1
    pure $ plE0 $ plE1 [CmpRR () r0 r1, Cset () (absReg t) c]
ir (IR.Cset t (IR.FRel op e0 e1)) | c <- frel op = do
    (plE0,r0) <- plF e0; (plE1,r1) <- plF e1
    pure $ plE0 $ plE1 [Fcmp () r0 r1, Cset () (absReg t) c]
ir (IR.Cset t (IR.IU Op.IOdd e0)) = do
    (plE0,r0) <- plI e0
    pure $ plE0 [TstI () r0 (BM 1 0), Cset () (absReg t) Neq]
ir (IR.Cset t (IR.IU Op.IEven e0)) = do
    (plE0,r0) <- plI e0
    pure $ plE0 [TstI () r0 (BM 1 0), Cset () (absReg t) Eq]
ir (IR.Cset t b) = do
    (plE0,r0) <- plI b
    pure $ plE0 [MovRR () (absReg t) r0]
ir (IR.Fcmov (IR.IRel op e0 (IR.ConstI i64)) t e) | c <- iop op, Just u <- m12 i64 = do
    (plE0,r0) <- plI e0; (plE,i) <- plF e
    pure $ plE $ plE0 [CmpRC () r0 u, Fcsel () (fabsReg t) i (fabsReg t) c]
ir (IR.Fcmov (IR.IRel op e0 e1) t e) | c <- iop op = do
    (plE0,r0) <- plI e0; (plE1,r1) <- plI e1
    (plE,i) <- plF e
    pure $ plE $ plE0 $ plE1 [CmpRR () r0 r1, Fcsel () (fabsReg t) i (fabsReg t) c]
ir (IR.Fcmov (IR.FRel op e0 e1) t e) | c <- frel op = do
    (plE0,r0) <- plF e0; (plE1,r1) <- plF e1
    (plE,i) <- plF e
    pure $ plE $ plE0 $ plE1 [Fcmp () r0 r1, Fcsel () (fabsReg t) i (fabsReg t) c]
ir (IR.Fcmov (IR.IU Op.IOdd e0) t e) = do
    (plE0,r0) <- plI e0; (plE,i) <- plF e
    pure $ plE $ plE0 [TstI () r0 (BM 1 0), Fcsel () (fabsReg t) i (fabsReg t) Neq]
ir (IR.Fcmov (IR.IU Op.IEven e0) t e) = do
    (plE0,r0) <- plI e0; (plE,i) <- plF e
    pure $ plE $ plE0 [TstI () r0 (BM 1 0), Fcsel () (fabsReg t) i (fabsReg t) Eq]
ir (IR.Cpy (IR.AP tD Nothing _) (IR.AP tS Nothing _) (IR.ConstI n)) | (n', 0) <- n `quotRem` 4, n' <= 4 = do
    q0 <- nQ; q1 <- nQ
    pure $ concat [ [Ldp2 () q0 q1 (RP (absReg tS) (i*32)), Stp2 () q0 q1 (RP (absReg tD) (i*32))] | i <- fromIntegral<$>[0..(n'-1)] ]
ir (IR.Cpy (IR.AP tD Nothing _) (IR.AP tS Nothing _) (IR.ConstI n)) | (n', 0) <- n `quotRem` 2, n' <= 4 = do
    t0 <- nR; t1 <- nR
    pure $ concat [ [Ldp () t0 t1 (RP (absReg tS) (i*16)), Stp () t0 t1 (RP (absReg tD) (i*16))] | i <- fromIntegral<$>[0..(n'-1)] ]
ir (IR.Cpy (IR.AP tD Nothing _) (IR.AP tS Nothing _) (IR.ConstI n)) | n <= 4 = do
    t <- nR
    pure $ concat [ [Ldr () t (RP (absReg tS) (i*8)), Str () t (RP (absReg tD) (i*8))] | i <- fromIntegral<$>[0..(n-1)] ]
ir (IR.Cpy (IR.AP tD Nothing _) (IR.AP tS (Just eS) _) (IR.ConstI n)) | (n', 0) <- n `quotRem` 2, n' <= 4 = do
    rD <- nextI; rS <- nextI
    t0 <- nR; t1 <- nR
    plED <- eval (IR.Reg tD) (IR.ITemp rD)
    plES <- eval (IR.Reg tS+eS) (IR.ITemp rS)
    pure $ plED ++ plES ++ concat [ [Ldp () t0 t1 (RP (IReg rS) (i*16)), Stp () t0 t1 (RP (IReg rD) (i*16))] | i <- fromIntegral<$>[0..(n'-1)] ]
ir (IR.Cpy (IR.AP tD (Just eD) _) (IR.AP tS Nothing _) (IR.ConstI n)) | (n', 0) <- n `quotRem` 2, n' <= 4 = do
    rD <- nextI; rS <- nextI
    t0 <- nR; t1 <- nR
    plED <- eval (IR.Reg tD+eD) (IR.ITemp rD)
    plES <- eval (IR.Reg tS) (IR.ITemp rS)
    pure $ plED ++ plES ++ concat [ [Ldp () t0 t1 (RP (IReg rS) (i*16)), Stp () t0 t1 (RP (IReg rD) (i*16))] | i <- fromIntegral<$>[0..(n'-1)] ]
ir (IR.Cpy (IR.AP tD (Just eD) _) (IR.AP tS (Just eS) _) (IR.ConstI n)) | (n', 0) <- n `quotRem` 2, n' <= 4 = do
    rD <- nextI; rS <- nextI
    t0 <- nR; t1 <- nR
    plED <- eval (IR.Reg tD+eD) (IR.ITemp rD)
    plES <- eval (IR.Reg tS+eS) (IR.ITemp rS)
    pure $ plED ++ plES ++ concat [ [Ldp () t0 t1 (RP (IReg rS) (i*16)), Stp () t0 t1 (RP (IReg rD) (i*16))] | i <- fromIntegral<$>[0..(n'-1)] ]
ir (IR.Cpy (IR.AP tD (Just eD) _) (IR.AP tS (Just eS) _) (IR.ConstI n)) | (n', 1) <- n `quotRem` 4, n' <= 4 = do
    rD <- nextI; rS <- nextI
    t <- nR; q0 <- nQ; q1 <- nQ
    plED <- eval (IR.Reg tD+eD) (IR.ITemp rD)
    plES <- eval (IR.Reg tS+eS) (IR.ITemp rS)
    let li=fromIntegral$(n-1)*8
    pure $ plED ++ plES ++ concat [ [Ldp2 () q0 q1 (RP (IReg rS) (i*32)), Stp2 () q0 q1 (RP (IReg rD) (i*32))] | i <- fromIntegral<$>[0..(n'-1)] ] ++ [Ldr () t (RP (IReg rS) li), Str () t (RP (IReg rD) li)]
ir (IR.Cpy (IR.AP tD (Just eD) _) (IR.AP tS (Just eS) _) (IR.ConstI n)) | (n', 3) <- n `quotRem` 4, n' <= 4 = do
    rD <- nextI; rS <- nextI
    t0 <- nR; t1 <- nR
    q0 <- nQ; q1 <- nQ
    plED <- eval (IR.Reg tD+eD) (IR.ITemp rD)
    plES <- eval (IR.Reg tS+eS) (IR.ITemp rS)
    let pix=fromIntegral$(n-3)*8; li=pix+16
    pure $ plED ++ plES ++ concat [ [Ldp2 () q0 q1 (RP (IReg rS) (i*32)), Stp2 () q0 q1 (RP (IReg rD) (i*32))] | i <- fromIntegral<$>[0..(n'-1)] ] ++ [Ldp () t0 t1 (RP (IReg rS) pix), Stp () t0 t1 (RP (IReg rD) pix), Ldr () t0 (RP (IReg rS) li), Str () t0 (RP (IReg rD) li)]
ir (IR.Cpy (IR.AP tD (Just eD) _) (IR.AP tS (Just eS) _) (IR.ConstI n)) | (n', 1) <- n `quotRem` 2, n' <= 4 = do
    rD <- nextI; rS <- nextI
    t0 <- nR; t1 <- nR
    plED <- eval (IR.Reg tD+eD) (IR.ITemp rD)
    plES <- eval (IR.Reg tS+eS) (IR.ITemp rS)
    let li=fromIntegral$(n-1)*8
    pure $ plED ++ plES ++ concat [ [Ldp () t0 t1 (RP (IReg rS) (i*16)), Stp () t0 t1 (RP (IReg rD) (i*16))] | i <- fromIntegral<$>[0..(n'-1)] ] ++ [Ldr () t0 (RP (IReg rS) li), Str () t0 (RP (IReg rD) li)]
ir (IR.Cpy (IR.AP tD (Just (IR.ConstI oD)) _) (IR.AP tS (Just (IR.ConstI oS)) _) eN) | Just uS <- mu16 oS, Just uD <- mu16 oD, oD `rem` 16 == 0 && oS `rem` 16 == 0 = do
    rD <- nextI; rS <- nextI; i <- nR
    t0 <- nR; t1 <- nR
    q0 <- nQ; q1 <- nQ
    plED <- eval (IR.Reg tD) (IR.ITemp rD)
    plES <- eval (IR.Reg tS) (IR.ITemp rS)
    (plEN, rN) <- plI eN
    let rDA=IReg rD; rSA=IReg rS
    l <- nextL; l2 <- nextL; eL <- nextL
    pure $ plED ++ plES ++ plEN [Cbz () rN eL, MovRR () i rN, Tbz () rN 0 l2, Ldr () t0 (RP rSA uS), Str () t0 (RP rDA uD), SubsRC () i i 1, Bc () Eq eL, AddRC () rSA rSA 8 IZero, AddRC () rDA rDA 8 IZero, Label () l2, Tbz () rN 1 l, Ldp () t0 t1 (RP rSA uS), Stp () t0 t1 (RP rDA uD), SubsRC () i i 2, Bc () Eq eL, AddRC () rSA rSA 16 IZero, AddRC () rDA rDA 16 IZero, Label () l, Ldp2 () q0 q1 (RP rSA uS), Stp2 () q0 q1 (RP rDA uD), AddRC () rSA rSA 32 IZero, AddRC () rDA rDA 32 IZero, SubsRC () i i 4, Bc () Neq l, Label () eL]
ir (IR.Cpy (IR.AP tD (Just (IR.ConstI oD)) _) (IR.AP tS eS _) eN) | Just uD <- mu16 oD, oD `rem` 16 == 0 = do
    rD <- nextI; rS <- nextI; i <- nR
    t0 <- nR; t1 <- nR
    q0 <- nQ; q1 <- nQ
    plED <- eval (IR.Reg tD) (IR.ITemp rD)
    plES <- eval (maybe id (+) eS$IR.Reg tS) (IR.ITemp rS)
    (plEN, rN) <- plI eN
    let rDA=IReg rD; rSA=IReg rS
    l <- nextL; l2 <- nextL; eL <- nextL
    pure $ plED ++ plES ++ plEN [Cbz () rN eL, MovRR () i rN, Tbz () rN 0 l2, Ldr () t0 (R rSA), Str () t0 (RP rDA uD), SubsRC () i i 1, Bc () Eq eL, AddRC () rSA rSA 8 IZero, AddRC () rDA rDA 8 IZero, Label () l2, Tbz () rN 1 l, Ldp () t0 t1 (R rSA), Stp () t0 t1 (RP rDA uD), SubsRC () i i 2, Bc () Eq eL, AddRC () rSA rSA 16 IZero, AddRC () rDA rDA 16 IZero, Label () l, Ldp2 () q0 q1 (R rSA), Stp2 () q0 q1 (RP rDA uD), AddRC () rSA rSA 32 IZero, AddRC () rDA rDA 32 IZero, SubsRC () i i 4, Bc () Neq l, Label () eL]
ir (IR.Cpy (IR.AP tD eD _) (IR.AP tS (Just (IR.ConstI oS)) _) eN) | Just uS <- mu16 oS, oS `rem` 16 == 0 = do
    rD <- nextI; rS <- nextI; i <- nR
    t0 <- nR; t1 <- nR
    q0 <- nQ; q1 <- nQ
    plED <- eval (maybe id (+) eD$IR.Reg tD) (IR.ITemp rD)
    plES <- eval (IR.Reg tS) (IR.ITemp rS)
    (plEN, rN) <- plI eN
    let rDA=IReg rD; rSA=IReg rS
    l <- nextL; l2 <- nextL; eL <- nextL
    pure $ plED ++ plES ++ plEN [Cbz () rN eL, MovRR () i rN, Tbz () rN 0 l2, Ldr () t0 (RP rSA uS), Str () t0 (Po rDA 8), SubsRC () i i 1, Bc () Eq eL, AddRC () rSA rSA 8 IZero, Label () l2, Tbz () rN 1 l, Ldp () t0 t1 (RP rSA uS), Stp () t0 t1 (Po rDA 16), SubsRC () i i 2, Bc () Eq eL, AddRC () rSA rSA 16 IZero, Label () l, Ldp2 () q0 q1 (RP rSA uS), Stp2 () q0 q1 (Po rDA 32), AddRC () rSA rSA 32 IZero, SubsRC () i i 4, Bc () Neq l, Label () eL]
ir (IR.Cpy (IR.AP tD eD _) (IR.AP tS eS _) eN) = do
    rD <- nextI; rS <- nextI; i <- nR
    q0 <- nQ; q1 <- nQ; t0 <- nR; t1 <- nR
    plED <- eval (maybe id (+) eD$IR.Reg tD) (IR.ITemp rD)
    plES <- eval (maybe id (+) eS$IR.Reg tS) (IR.ITemp rS)
    (plEN, rN) <- plI eN
    let rDA=IReg rD; rSA=IReg rS
    l <- nextL; l2 <- nextL; eL <- nextL
    pure $ plED ++ plES ++ plEN [Cbz () rN eL, MovRR () i rN, Tbz () i 0 l2, Ldr () t0 (Po rSA 8), Str () t0 (Po rDA 8), SubsRC () i i 1, Bc () Eq eL, Label () l2, Tbz () rN 1 l, Ldp () t0 t1 (Po rSA 16), Stp () t0 t1 (Po rDA 16), SubsRC () i i 2, Bc () Eq eL, Label () l, Ldp2 () q0 q1 (Po rSA 32), Stp2 () q0 q1 (Po rDA 32), SubsRC () i i 4, Bc () Neq l, Label () eL]
ir (IR.Cpy1 (IR.AP tD (Just (IR.ConstI di)) _) (IR.AP tS (Just (IR.ConstI si)) _) eN) | Just du <- mu16 di, Just su <- mu16 si = do
    rD <- nextI; rS <- nextI; i <- nR; t <- nR
    (plEN, rN) <- plI eN
    l <- nextL; eL <- nextL
    let rDA=IReg rD; rSA=IReg rS
    pure $ plEN [Cbz () rN eL, MovRR () rDA (absReg tD), MovRR () rSA (absReg tS), MovRR () i rN, Label () l, LdrB () t (RP rSA du), StrB () t (RP rDA su), AddRC () rSA rSA 1 IZero, AddRC () rDA rDA 1 IZero, SubsRC () i i 1, Bc () Neq l, Label () eL]
ir (IR.Cpy1 (IR.AP tD eD _) (IR.AP tS eS _) (IR.ConstI 1)) = do
    rD <- nextI; rS <- nextI
    t <- nR
    plED <- eval (maybe id (+) eD$IR.Reg tD) (IR.ITemp rD)
    plES <- eval (maybe id (+) eS$IR.Reg tS) (IR.ITemp rS)
    let rDA=IReg rD; rSA=IReg rS
    pure $ plED ++ plES ++ [LdrB () t (R rSA), StrB () t (R rDA)]
ir (IR.Cpy1 (IR.AP tD eD _) (IR.AP tS eS _) eN) = do
    rD <- nextI; rS <- nextI; i <- nR; t <- nR
    plED <- eval (maybe id (+) eD$IR.Reg tD) (IR.ITemp rD)
    plES <- eval (maybe id (+) eS$IR.Reg tS) (IR.ITemp rS)
    (plEN, rN) <- plI eN
    l <- nextL; eL <- nextL
    let rDA=IReg rD; rSA=IReg rS
    pure $ plED ++ plES ++ plEN [Cbz () rN eL, MovRR () i rN, Label () l, LdrB () t (BI rSA i Zero), StrB () t (BI rDA i Zero), SubsRC () i i 1, Bc () Neq l, Label () eL]
-- ir (IR.IRnd t) = pure [MrsR () (absReg t)]
ir (IR.IRnd t) = do
    r <- nR
    pure [puL, AddRC () FP ASP 16 IZero, MovRCf () r JR, Blr () r, MovRR () (absReg t) CArg0, poL]
ir (IR.FRnd t) = do
    r <- nR
    pure [puL, AddRC () FP ASP 16 IZero, MovRCf () r DR, Blr () r, FMovXX () (fabsReg t) FArg0, poL]
ir s             = error (show s)

puL, poL :: AArch64 AbsReg freg ()
puL = Stp () FP LR (Pr ASP (-16))
poL = Ldp () FP LR (Po ASP 16)

sai i | i `rem` 16 == 0 = i+16 | otherwise = i+16+(16-r) where r = i `rem` 16

mw64 :: Word64 -> AbsReg -> [AArch64 AbsReg freg ()]
mw64 w r =
    let w0=w .&. 0xffff; w1=(w .&. 0xffff0000) `rotateR` 16; w2=(w .&. 0xFFFF00000000) `rotateR` 32; w3=(w .&. 0xFFFF000000000000) `rotateR` 48
    in MovRC () r (fromIntegral w0):[MovK () r (fromIntegral wi) s | (wi, s) <- [(w1, 16), (w2, 32), (w3, 48)], wi /= 0 ]

ssin :: IR.FTemp -> WM [AArch64 AbsReg FAbsReg ()]
ssin t = do
    d1 <- nextF; d2 <- nextF; d3 <- nextF
    tsI <- nextI
    let tsIR=IR.FTemp tsI; tsC=FReg tsI
    pl3 <- feval (IR.ConstF$ -(1/6)) tsIR; pl5 <- feval (IR.ConstF$1/120) tsIR; pl7 <- feval (IR.ConstF$ -(1/5040)) tsIR
    pure $ [Fmul () d1 d0 d0, Fmul () d2 d1 d0, Fmul () d3 d2 d1, Fmul () d1 d1 d3] ++ pl3 ++ Fmadd () d0 d2 tsC d0 : pl5 ++ Fmadd () d0 d3 tsC d0 : pl7 ++ [Fmadd () d0 d1 tsC d0]
  where
    d0 = fabsReg t

cosϵ :: IR.FTemp -> WM [AArch64 AbsReg FAbsReg ()]
cosϵ t = do
    d1 <- nextF; d2 <- nextF; d3 <- nextF
    tsI <- nextI
    let tsIR=IR.FTemp tsI; tsC=FReg tsI
    pl0 <- feval 1 tsIR; pl2 <- feval (IR.ConstF$ -(1/2)) tsIR; pl4 <- feval (IR.ConstF$1/24) tsIR; pl6 <- feval (IR.ConstF$ -(1/720)) tsIR
    pure $ [Fmul () d1 d0 d0, Fmul () d2 d1 d1, Fmul () d3 d2 d1] ++ pl0 ++ FMovXX () d0 tsC : pl2 ++ Fmadd () d0 d1 tsC d0 : pl4 ++ Fmadd () d0 d2 tsC d0 : pl6 ++ [Fmadd () d0 d3 tsC d0]
  where
    d0 = fabsReg t

f2eval :: IR.F2E -> IR.F2 -> WM [AArch64 AbsReg FAbsReg ()]
f2eval (IR.FAt (IR.AP tB Nothing _)) tD =
    pure [LdrS () (f2absReg tD) (R (absReg tB))]
f2eval (IR.FAt (IR.AP tB (Just e) _)) tD = do
    i <- nextI; plE <- eval e (IR.ITemp i)
    pure $ plE ++ [LdrS () (f2absReg tD) (BI (absReg tB) (IReg i) Zero)]
f2eval (IR.FB Op.FPlus e0 (IR.FB Op.FTimes e1 e2)) t = do
    (plE0,x0) <- plF2 e0; (plE1,x1) <- plF2 e1; (plE2,x2) <- plF2 e2
    let va=f2absReg t
    pure$plE0$plE1$plE2 [MovQQ () va x0, Fmla () va x1 x2]
f2eval (IR.FB Op.FMinus e0 (IR.FB Op.FTimes e1 e2)) t = do
    (plE0,x0) <- plF2 e0; (plE1,x1) <- plF2 e1; (plE2,x2) <- plF2 e2
    let va=f2absReg t
    pure$plE0$plE1$plE2 [MovQQ () va x0, Fmls () va x1 x2]
f2eval (IR.FB Op.FPlus e0 e1) t = do
    (plE0,x0) <- plF2 e0; (plE1,x1) <- plF2 e1
    pure$plE0$plE1 [Fadd2 () (f2absReg t) x0 x1]
f2eval (IR.FB Op.FMinus (IR.ConstF (0,0)) e1) t = do
    (plE1,x1) <- plF2 e1
    pure$plE1 [Fneg2 () (f2absReg t) x1]
f2eval (IR.FB Op.FTimes (IR.ConstF (-1,-1)) e1) t = do
    (plE1,x1) <- plF2 e1
    pure$plE1 [Fneg2 () (f2absReg t) x1]
f2eval (IR.FB Op.FTimes e0 (IR.ConstF (-1,-1))) t = do
    (plE,x) <- plF2 e0
    pure$plE [Fneg2 () (f2absReg t) x]
f2eval (IR.FB Op.FMinus e0 e1) t = do
    (plE0,x0) <- plF2 e0; (plE1,x1) <- plF2 e1
    pure$plE0$plE1 [Fsub2 () (f2absReg t) x0 x1]
f2eval (IR.FB Op.FTimes e0 e1) t = do
    (plE0,x0) <- plF2 e0; (plE1,x1) <- plF2 e1
    pure$plE0$plE1 [Fmul2 () (f2absReg t) x0 x1]
f2eval (IR.FB Op.FDiv e0 e1) t = do
    (plE0,x0) <- plF2 e0; (plE1,x1) <- plF2 e1
    pure$plE0$plE1 [Fdiv2 () (f2absReg t) x0 x1]
f2eval (IR.FB Op.FMax e0 e1) t = do
    (plE0,x0) <- plF2 e0; (plE1,x1) <- plF2 e1
    pure$plE0$plE1 [Fmax2 () (f2absReg t) x0 x1]
f2eval (IR.FB Op.FMin e0 e1) t = do
    (plE0,x0) <- plF2 e0; (plE1,x1) <- plF2 e1
    pure$plE0$plE1 [Fmin2 () (f2absReg t) x0 x1]
f2eval (IR.FU Op.FSqrt e) t = do
    (plE,x) <- plF2 e
    pure$plE[Fsqrt2 () (f2absReg t) x]
f2eval (IR.FU Op.FNeg e) t = do
    (plE,x) <- plF2 e
    pure$plE[Fneg2 () (f2absReg t) x]
f2eval (IR.ConstF (0,0)) t =
    let q=f2absReg t
    in pure [ZeroS () q]
f2eval (IR.ConstF (x,y)) t | x==y = do
    i <- nextI
    let r=IReg i
        w=castDoubleToWord64 x
    pure $ mw64 w r ++ [Dup () (f2absReg t) r]
f2eval (IR.ConstF (0,y)) t = do
    i <- nextI
    let q=f2absReg t; r=IReg i
        w=castDoubleToWord64 y
    pure $ ZeroS () q:mw64 w r++[Ins () q 1 r]
f2eval (IR.ConstF (x,y)) t = do
    i0 <- nextI; i1 <- nextI
    let r0=IReg i0; r1=IReg i1
        w0=castDoubleToWord64 x; w1=castDoubleToWord64 y
    pure $ mw64 w0 r0 ++ Ins () (f2absReg t) 0 r0:mw64 w1 r1++[Ins () (f2absReg t) 1 r1]
f2eval e _ = error (show e)

feval :: IR.FE -> IR.FTemp -> WM [AArch64 AbsReg FAbsReg ()]
feval (IR.FReg tS) tD = pure [FMovXX () (fabsReg tD) (fabsReg tS)]
feval (IR.ConstF 0) t = pure [ZeroD () (fabsReg t)]
feval (IR.ConstF d) t = do
    i <- nextI
    let r=IReg i
        w=castDoubleToWord64 d
    pure $ mw64 w r ++ [FMovDR () (fabsReg t) r]
-- https://litchie.com/2020/04/sine
feval (IR.FU Op.FSin e) t = do
    plE <- feval e t
    let d0=fabsReg t
    s <- ssin t; c <- cosϵ t
    lc <- nextL; endL <- nextL
    i <- nR; d2 <- nextF; i7 <- nextI
    π4i<-nextI; plπ4 <- feval (IR.ConstF$pi/4) (IR.FTemp π4i); pl7 <- eval (IR.ConstI 7) (IR.ITemp i7)
    let π4=FReg π4i
    dRot <- nextF; nres <- nextF
    pure $
        plE
        ++plπ4
        ++[Fdiv () d2 d0 π4, Frintm () d2 d2, Fmsub () d0 π4 d2 d0, Fcvtas () i d2]
        ++pl7
        ++[AndRR () i i (IReg i7), TstI () i (BM 1 0), Fsub () dRot π4 d0, Fcsel () d0 dRot d0 Neq]
        ++[ CmpRC () i 1, Bc () Eq lc, CmpRC () i 2, Bc () Eq lc
          , CmpRC () i 5, Bc () Eq lc, CmpRC () i 6, Bc () Eq lc
          ]
        ++s++B () endL
        :Label () lc:c
        ++[Label () endL]
        ++[Fneg () nres d0, TstI () i (BM 1 2), Fcsel () d0 nres d0 Neq]
feval (IR.FU Op.FCos e) t = feval (IR.FU Op.FSin (IR.ConstF(pi/2)-e)) t
feval (IR.FB Op.FExp (IR.ConstF 2.718281828459045) e) t = do
    r <- nR
    plE <- feval e IR.F0
    pure $ plE ++ [puL, AddRC () FP ASP 16 IZero, MovRCf () r Exp, Blr () r, FMovXX () (fabsReg t) FArg0, poL]
feval (IR.FB Op.FExp e0 e1) t = do
    r <- nR
    plE0 <- feval e0 IR.F0; plE1 <- feval e1 IR.F1
    pure $ plE0 ++ plE1 ++ [puL, AddRC () FP ASP 16 IZero, MovRCf () r Pow, Blr () r, FMovXX () (fabsReg t) FArg0, poL]
feval (IR.FU Op.FLog e) t = do
    r <- nR
    plE <- feval e IR.F0
    pure $ plE ++ [puL, AddRC () FP ASP 16 IZero, MovRCf () r Log, Blr () r, FMovXX () (fabsReg t) FArg0, poL]
feval (IR.FB Op.FPlus e0 (IR.FB Op.FTimes e1 e2)) t = do
    (plE0,i0) <- plF e0; (plE1,i1) <- plF e1; (plE2,i2) <- plF e2
    pure $ plE0 $ plE1 $ plE2 [Fmadd () (fabsReg t) i1 i2 i0]
feval (IR.FB Op.FPlus (IR.FB Op.FTimes e0 e1) e2) t = do
    (plE0,i0) <- plF e0; (plE1,i1) <- plF e1; (plE2,i2) <- plF e2
    pure $ plE0 $ plE1 $ plE2 [Fmadd () (fabsReg t) i0 i1 i2]
feval (IR.FB Op.FMinus e0 (IR.FB Op.FTimes e1 e2)) t = do
    (plE0,i0) <- plF e0; (plE1,i1) <- plF e1; (plE2,i2) <- plF e2
    pure $ plE0 $ plE1 $ plE2 [Fmsub () (fabsReg t) i1 i2 i0]
feval (IR.FB Op.FMinus (IR.ConstF 0) e) t = do
    (plE,i) <- plF e
    pure $ plE [Fneg () (fabsReg t) i]
feval (IR.FB Op.FTimes (IR.ConstF (-1)) e) t = do
    (plE,i) <- plF e
    pure $ plE [Fneg () (fabsReg t) i]
feval (IR.FB Op.FTimes e (IR.ConstF (-1))) t = do
    (plE,i) <- plF e
    pure $ plE [Fneg () (fabsReg t) i]
feval (IR.FB fop e0 e1) t | Just isn <- mFop fop = do
    (plE0,r0) <- plF e0; (plE1,r1) <- plF e1
    pure $ plE0 $ plE1 [isn () (fabsReg t) r0 r1]
feval (IR.FU Op.FAbs e) t = do
    (plE,i) <- plF e
    pure $ plE [Fabs () (fabsReg t) i]
feval (IR.FAt (IR.AP tS (Just (IR.ConstI i)) _)) tD | Just i8 <- mp i = pure [LdrD () (fabsReg tD) (RP (absReg tS) i8)]
feval (IR.FAt (IR.AP tB (Just (IR.IB Op.IAsl eI (IR.ConstI 3))) _)) tD = do
    (plE,i) <- plI eI
    pure $ plE [LdrD () (fabsReg tD) (BI (absReg tB) i Three)]
feval (IR.FAt (IR.AP tB (Just (IR.IB Op.IPlus (IR.IB Op.IAsl eI (IR.ConstI 3)) (IR.ConstI ix8))) _)) tD | (ix, 0) <- ix8 `quotRem` 8 = do
    i <- nextI; plE <- eval (eI+IR.ConstI ix) (IR.ITemp i)
    pure $ plE ++ [LdrD () (fabsReg tD) (BI (absReg tB) (IReg i) Three)]
feval (IR.FAt (IR.AP tB (Just e) _)) tD = do
    i <- nextI; plE <- eval e (IR.ITemp i)
    pure $ plE ++ [LdrD () (fabsReg tD) (BI (absReg tB) (IReg i) Zero)]
feval (IR.FAt (IR.AP tB Nothing _)) tD =
    pure [LdrD () (fabsReg tD) (R (absReg tB))]
feval (IR.FConv e) tD = do
    (plE,r) <- plI e
    pure $ plE [Scvtf () (fabsReg tD) r]
feval (IR.FU Op.FSqrt e) t = do
    (plE,r) <- plF e
    pure $ plE [Fsqrt () (fabsReg t) r]
feval (IR.FU Op.FNeg e) t = do
    (plE,r) <- plF e
    pure $ plE [Fneg () (fabsReg t) r]
feval e _             = error (show e)

eval :: IR.Exp -> IR.Temp -> WM [AArch64 AbsReg FAbsReg ()]
eval (IR.Reg tS) tD = pure [MovRR () (absReg tD) (absReg tS)]
eval (IR.ConstI 0) tD = pure [ZeroR () (absReg tD)]
eval (IR.ConstI i) tD | Just u <- mu16 i = pure [MovRC () (absReg tD) u]
eval (IR.ConstI i) tD | Just u <- mu16 (-i) = let t=absReg tD in pure [MovRC () t u, Neg () t t]
eval (IR.ConstI i) tD = pure $ mw64 (fromIntegral i) (absReg tD)
eval (IR.Is p) tD = pure [MovRR () (absReg tD) (absReg p)]
eval (IR.IB Op.IPlus (IR.IB Op.IAsl e0 (IR.ConstI i)) e1) t | Just u <- ms i = do
    r0 <- nextI; r1 <- nextI
    plE0 <- eval e0 (IR.ITemp r0); plE1 <- eval e1 (IR.ITemp r1)
    pure $ plE0 ++ plE1 ++ [AddRRS () (absReg t) (IReg r1) (IReg r0) u]
eval (IR.IB Op.IPlus e (IR.ConstI i)) t | Just u <- m12 i = do
    r <- nextI; plE <- eval e (IR.ITemp r)
    pure $ plE ++ [AddRC () (absReg t) (IReg r) u IZero]
eval (IR.IB Op.IPlus e (IR.ConstI i)) t | 0 <- i .&. 4095, Just u <- m12 (i `shiftR` 12) = do
    r <- nextI; plE <- eval e (IR.ITemp r)
    pure $ plE ++ [AddRC () (absReg t) (IReg r) u Twelve]
eval (IR.IB Op.IMinus e (IR.ConstI i)) t | Just u <- m12 i = do
    (plE,r) <- plI e
    pure $ plE [SubRC () (absReg t) r u IZero]
eval (IR.IB Op.IMinus e (IR.ConstI i)) t | 0 <- i .&. 4095, Just u <- m12 (i `shiftR` 12) = do
    (plE,r) <- plI e
    pure $ plE [SubRC () (absReg t) r u Twelve]
eval (IR.IB Op.IAsl e (IR.ConstI i)) t = do
    (plE,r) <- plI e
    pure $ plE [Lsl () (absReg t) r (fromIntegral (i `mod` 64))]
eval (IR.IB Op.IMinus (IR.ConstI 0) e) t = do
    (plE,r) <- plI e
    pure $ plE [Neg () (absReg t) r]
eval (IR.IB Op.ITimes (IR.ConstI (-1)) e) t = do
    (plE,r) <- plI e
    pure $ plE [Neg () (absReg t) r]
eval (IR.IB Op.ITimes e (IR.ConstI (-1))) t = do
    (plE,r) <- plI e
    pure $ plE [Neg () (absReg t) r]
eval (IR.IB Op.IRem e0 e1) t = do
    r2 <- nR
    (plE0,r0) <- plI e0; (plE1,r1) <- plI e1
    pure $ plE0 $ plE1 [Sdiv () r2 r0 r1, Msub () (absReg t) r2 r1 r0]
eval (IR.IB Op.IPlus (IR.IB Op.ITimes e0 e1) e2) t = do
    (plE0,r0) <- plI e0; (plE1,r1) <- plI e1; (plE2,r2) <- plI e2
    pure $ plE0 $ plE1 $ plE2 [Madd () (absReg t) r0 r1 r2]
eval (IR.IB Op.IMin e0 e1) t = do
    (plE0,r0) <- plI e0; (plE1,r1) <- plI e1
    pure $ plE0 $ plE1 [CmpRR () r0 r1, Csel () (absReg t) r0 r1 Leq]
eval (IR.IB Op.IMax e0 e1) t = do
    (plE0,r0) <- plI e0; (plE1,r1) <- plI e1
    pure $ plE0 $ plE1 [CmpRR () r0 r1, Csel () (absReg t) r0 r1 Geq]
eval (IR.IB (Op.BI Op.BEq) e0 e1) t = do
    (plE0,r0) <- plI e0; (plE1,r1) <- plI e1
    pure $ plE0 $ plE1 [Eon () (absReg t) r0 r1, Bfc () (absReg t) 1 63]
eval (IR.IB op e0 e1) t | Just isn <- mIop op = do
    (plE0,r0) <- plI e0; (plE1,r1) <- plI e1
    pure $ plE0 $ plE1 [isn () (absReg t) r0 r1]
eval (IR.IRel rel e0 e1) t | c <- iop rel = do
    (plE0,r0) <- plI e0; (plE1,r1) <- plI e1
    pure $ plE0 $ plE1 [CmpRR () r0 r1, Cset () (absReg t) c]
eval (IR.IRFloor e) t = do
    (plE,r) <- plF e
    pure $ plE [Fcvtms () (absReg t) r]
eval (IR.EAt (IR.AP tB (Just (IR.ConstI i)) _)) tD | Just p <- mp i = pure [Ldr () (absReg tD) (RP (absReg tB) p)]
eval (IR.BAt (IR.AP tB Nothing _)) tD = pure [LdrB () (absReg tD) (R (absReg tB))]
eval (IR.BAt (IR.AP tB (Just (IR.ConstI i)) _)) tD | Just u <- mu16 i = pure [LdrB () (absReg tD) (RP (absReg tB) u)]
eval (IR.EAt (IR.AP rB (Just (IR.IB Op.IAsl eI (IR.ConstI 3))) _)) t = do
    (plE,i) <- plI eI
    pure $ plE [Ldr () (absReg t) (BI (absReg rB) i Three)]
eval (IR.EAt (IR.AP rB Nothing _)) t = do
    pure [Ldr () (absReg t) (R (absReg rB))]
eval (IR.EAt (IR.AP rB (Just e) _)) t = do
    (plE,i) <- plI e
    pure $ plE [Ldr () (absReg t) (BI (absReg rB) i Zero)]
eval (IR.BAt (IR.AP rB (Just e) _)) t = do
    (plE,i) <- plI e
    pure $ plE [LdrB () (absReg t) (BI (absReg rB) i Zero)]
eval (IR.IB Op.IAsr e (IR.ConstI i)) t | Just s <- ms i = do
    (plE,r) <- plI e
    pure $ plE [Asr () (absReg t) r s]
eval (IR.LA n) t = pure [LdrRL () (absReg t) n]
eval (IR.BU Op.BNeg (IR.Is r)) t = pure [EorI () (absReg t) (absReg r) (BM 1 0)]
eval e _            = error (show e)

ms :: Integral a => a -> Maybe Word8
ms i | i >=0 && i <= 63 = Just (fromIntegral i) | otherwise = Nothing

m12, mu16 :: Integral a => a -> Maybe Word16
m12 i | i >= 0 && i < 4096 = Just (fromIntegral i) | otherwise = Nothing

mu16 i | i > fromIntegral (maxBound :: Word16) || i < fromIntegral (minBound :: Word16) = Nothing
       | otherwise = Just (fromIntegral i)

mp :: Integral a => a -> Maybe Word16
mp i | i `rem` 8 == 0 && i >= 0 && i <= 32760 = Just (fromIntegral i) | otherwise = Nothing
