module Asm.Aarch64.T ( irToAarch64 ) where

import           Asm.Aarch64
import           Asm.M
import           Control.Monad.State.Strict (runState)
import           Data.Bifunctor             (second)
import           Data.Bits                  (rotateR, (.&.))
import           Data.Tuple                 (swap)
import           Data.Word                  (Word16, Word64, Word8)
import           GHC.Float                  (castDoubleToWord64)
import qualified IR

absReg :: IR.Temp -> AbsReg
absReg (IR.ITemp i) = IReg i
absReg (IR.ATemp i) = IReg i
absReg IR.C0        = CArg0
absReg IR.C1        = CArg1
absReg IR.C2        = CArg2
absReg IR.C3        = CArg3
absReg IR.C4        = CArg4
absReg IR.C5        = CArg5
absReg IR.CRet      = CArg0

fabsReg :: IR.Temp -> FAbsReg
fabsReg (IR.FTemp i) = FReg i
fabsReg IR.F0        = FArg0
fabsReg IR.F1        = FArg1
fabsReg IR.F2        = FArg2
fabsReg IR.F3        = FArg3
fabsReg IR.F4        = FArg4
fabsReg IR.F5        = FArg5
fabsReg IR.FRet      = FArg0
fabsReg IR.FRet1     = FArg1

nextR :: WM AbsReg
nextR = IReg <$> nextI

irToAarch64 :: IR.WSt -> [IR.Stmt] -> (Int, [AArch64 AbsReg FAbsReg ()])
irToAarch64 st = swap . second (head.IR.wtemps) . flip runState st . foldMapA ir

ir :: IR.Stmt -> WM [AArch64 AbsReg FAbsReg ()]
ir (IR.L l)      = pure [Label () l]
ir (IR.J l)      = pure [B () l]
ir (IR.MX t e)   = feval e t
ir (IR.MT t e)   = eval e t
ir (IR.Ma _ t e) = do {r <- nextR; plE <- eval e IR.C0; pure $ plE ++ puL ++ [AddRC () FP ASP 16, MovRCf () r Malloc, Blr () r, MovRR () (absReg t) CArg0] ++ poL}
ir (IR.Free t) = do {r <- nextR; pure $ puL ++ [MovRR () CArg0 (absReg t), AddRC () FP ASP 16, MovRCf () r Free, Blr () r] ++ poL}
ir (IR.Sa t (IR.ConstI i)) | Just u <- mu16 (sai i) = pure [SubRC () ASP ASP u, MovRR () (absReg t) ASP]
ir (IR.Pop (IR.ConstI i)) | Just u <- mu16 (sai i) = pure [AddRC () ASP ASP u]
ir (IR.Wr (IR.AP t Nothing _) e) = do
    r <- nextI; plE <- eval e (IR.ITemp r)
    pure $ plE ++ [Str () (IReg r) (R $ absReg t)]
ir (IR.Wr (IR.AP t (Just (IR.ConstI i)) _) e) | Just p <- mp i = do
    r <- nextI; plE <- eval e (IR.ITemp r)
    pure $ plE ++ [Str () (IReg r) (RP (absReg t) p)]
ir (IR.Wr (IR.AP t (Just (IR.IB IR.IAsl eI (IR.ConstI 3))) _) e) = do
    r <- nextI; rI <- nextI
    plE <- eval e (IR.ITemp r); plEI <- eval eI (IR.ITemp rI)
    pure $ plE ++ plEI ++ [Str () (IReg r) (BI (absReg t) (IReg rI) Three)]
ir (IR.Wr (IR.AP t (Just (IR.IB IR.IPlus (IR.IB IR.IAsl eI (IR.ConstI 3)) (IR.ConstI i))) _) e) | (ix, 0) <- i `quotRem` 8 = do
    r <- nextI; rI <- nextI
    plE <- eval e (IR.ITemp r); plEI <- eval (eI+IR.ConstI ix) (IR.ITemp rI)
    pure $ plE ++ plEI ++ [Str () (IReg r) (BI (absReg t) (IReg rI) Three)]
ir (IR.Wr (IR.AP t (Just eI) _) e) = do
    r <- nextI; rI <- nextI
    plE <- eval e (IR.ITemp r); plEI <- eval eI (IR.ITemp rI)
    pure $ plE ++ plEI ++ [Str () (IReg r) (BI (absReg t) (IReg rI) Zero)]
ir (IR.WrF (IR.AP tB (Just (IR.IB IR.IAsl eI (IR.ConstI 3))) _) e) = do
    iI <- nextI; i <- nextI
    plE <- feval e (IR.FTemp i); plEI <- eval eI (IR.ITemp iI)
    pure $ plE ++ plEI ++ [StrD () (FReg i) (BI (absReg tB) (IReg iI) Three)]
ir (IR.WrF (IR.AP tB (Just (IR.IB IR.IPlus (IR.IB IR.IAsl eI (IR.ConstI 3)) (IR.ConstI ix8))) _) e) | (ix, 0) <- ix8 `quotRem` 8 = do
    iI <- nextI; i <- nextI
    plE <- feval e (IR.FTemp i); plEI <- eval (eI+IR.ConstI ix) (IR.ITemp iI)
    pure $ plE ++ plEI ++ [StrD () (FReg i) (BI (absReg tB) (IReg iI) Three)]
ir (IR.WrF (IR.AP t (Just eI) _) e) = do
    i <- nextI; iI <- nextI
    plE <- feval e (IR.FTemp i); plEI <- eval eI (IR.ITemp iI)
    pure $ plE ++ plEI ++ [StrD () (FReg i) (BI (absReg t) (IReg iI) Zero)]
ir (IR.MJ (IR.IRel IR.INeq (IR.Reg r) (IR.ConstI 0)) l) =
    pure [Cbnz () (absReg r) l]
ir (IR.MJ (IR.IU IR.IEven (IR.Reg r)) l) =
    pure [Tbz () (absReg r) 0 l]
ir (IR.MJ (IR.IRel IR.IGeq e (IR.ConstI i)) l) | Just u <- m12 i = do
    r <- nextI; plE <- eval e (IR.ITemp r)
    pure $ plE ++ [CmpRC () (IReg r) u, Bc () Geq l]
ir (IR.MJ (IR.IRel IR.IEq e (IR.ConstI i)) l) | Just u <- m12 i = do
    r <- nextI; plE <- eval e (IR.ITemp r)
    pure $ plE ++ [CmpRC () (IReg r) u, Bc () Eq l]
ir (IR.MJ (IR.IRel IR.IGeq e0 e1) l) = do
    r0 <- nextI; r1 <- nextI
    plE0 <- eval e0 (IR.ITemp r0); plE1 <- eval e1 (IR.ITemp r1)
    pure $ plE0 ++ plE1 ++ [CmpRR () (IReg r0) (IReg r1), Bc () Geq l]
ir (IR.MJ (IR.IRel IR.IGt e0 e1) l) = do
    r0 <- nextI; r1 <- nextI
    plE0 <- eval e0 (IR.ITemp r0); plE1 <- eval e1 (IR.ITemp r1)
    pure $ plE0 ++ plE1 ++ [CmpRR () (IReg r0) (IReg r1), Bc () Gt l]
ir (IR.MJ (IR.IRel IR.ILt e0 e1) l) = do
    r0 <- nextI; r1 <- nextI
    plE0 <- eval e0 (IR.ITemp r0); plE1 <- eval e1 (IR.ITemp r1)
    pure $ plE0 ++ plE1 ++ [CmpRR () (IReg r0) (IReg r1), Bc () Lt l]
ir (IR.MJ (IR.IRel IR.ILeq e0 e1) l) = do
    r0 <- nextI; r1 <- nextI
    plE0 <- eval e0 (IR.ITemp r0); plE1 <- eval e1 (IR.ITemp r1)
    pure $ plE0 ++ plE1 ++ [CmpRR () (IReg r0) (IReg r1), Bc () Leq l]
ir (IR.MJ (IR.FRel IR.FGeq e (IR.ConstF 0)) l) = do
    i <- nextI; plE <- feval e (IR.FTemp i)
    pure $ plE ++ [FcmpZ () (FReg i), Bc () Geq l]
ir (IR.MJ (IR.FRel IR.FGeq e0 e1) l) = do
    r0 <- nextI; r1 <- nextI
    plE0 <- feval e0 (IR.FTemp r0); plE1 <- feval e1 (IR.FTemp r1)
    pure $ plE0 ++ plE1 ++ [Fcmp () (FReg r0) (FReg r1), Bc () Geq l]
ir (IR.Cmov (IR.IRel IR.IGt (IR.Reg r0) (IR.Reg r1)) t (IR.Reg r)) = do
    pure [CmpRR () (absReg r0) (absReg r1), Csel () (absReg t) (absReg r) (absReg t) Gt]
ir (IR.Cset t (IR.IU IR.IOdd (IR.Reg r))) = do
    pure [TstI () (absReg r) 1, Cset () (absReg t) Eq]
ir (IR.Cset t (IR.IU IR.IEven (IR.Reg r))) = do
    pure [TstI () (absReg r) 1, Cset () (absReg t) Neq]
ir (IR.Fcmov (IR.IU IR.IOdd (IR.Reg r0)) t e) = do
    i <- nextI; plE <- feval e (IR.FTemp i)
    pure $ plE ++ [TstI () (absReg r0) 1, Fcsel () (fabsReg t) (FReg i) (fabsReg t) Neq]
ir (IR.Cpy (IR.AP tD Nothing _) (IR.AP tS Nothing _) (IR.ConstI n)) | (n', 0) <- n `quotRem` 2, n' <= 4 = do
    t0 <- nextR; t1 <- nextR
    pure $ concat [ [Ldp () t0 t1 (RP (absReg tS) (i*16)), Stp () t0 t1 (RP (absReg tD) (i*16))] | i <- [0..fromIntegral (n'-1)] ]
ir (IR.Cpy (IR.AP tD Nothing _) (IR.AP tS Nothing _) (IR.ConstI n)) | n <= 4 = do
    t <- nextR
    pure $ concat [ [Ldr () t (RP (absReg tS) (i*8)), Str () t (RP (absReg tD) (i*8))] | i <- [0..fromIntegral (n-1)] ]
ir (IR.Cpy (IR.AP tD Nothing _) (IR.AP tS (Just eS) _) (IR.ConstI n)) | (n', 0) <- n `quotRem` 2, n' <= 4 = do
    rD <- nextI; rS <- nextI
    t0 <- nextR; t1 <- nextR
    plED <- eval (IR.Reg tD) (IR.ITemp rD)
    plES <- eval (IR.Reg tS+eS) (IR.ITemp rS)
    pure $ plED ++ plES ++ concat [ [Ldp () t0 t1 (RP (IReg rS) (i*16)), Stp () t0 t1 (RP (IReg rD) (i*16))] | i <- [0..fromIntegral (n'-1)] ]
ir (IR.Cpy (IR.AP tD (Just eD) _) (IR.AP tS Nothing _) (IR.ConstI n)) | (n', 0) <- n `quotRem` 2, n' <= 4 = do
    rD <- nextI; rS <- nextI
    t0 <- nextR; t1 <- nextR
    plED <- eval (IR.Reg tD+eD) (IR.ITemp rD)
    plES <- eval (IR.Reg tS) (IR.ITemp rS)
    pure $ plED ++ plES ++ concat [ [Ldp () t0 t1 (RP (IReg rS) (i*16)), Stp () t0 t1 (RP (IReg rD) (i*16))] | i <- [0..fromIntegral (n'-1)] ]
ir (IR.Cpy (IR.AP tD (Just eD) _) (IR.AP tS (Just eS) _) (IR.ConstI n)) | (n', 0) <- n `quotRem` 2, n' <= 4 = do
    rD <- nextI; rS <- nextI
    t0 <- nextR; t1 <- nextR
    plED <- eval (IR.Reg tD+eD) (IR.ITemp rD)
    plES <- eval (IR.Reg tS+eS) (IR.ITemp rS)
    pure $ plED ++ plES ++ concat [ [Ldp () t0 t1 (RP (IReg rS) (i*16)), Stp () t0 t1 (RP (IReg rD) (i*16))] | i <- [0..fromIntegral (n'-1)] ]
ir (IR.Cpy (IR.AP tD (Just eD) _) (IR.AP tS (Just eS) _) (IR.ConstI n)) | (n', 1) <- n `quotRem` 2, n' <= 4 = do
    rD <- nextI; rS <- nextI
    t0 <- nextR; t1 <- nextR
    plED <- eval (IR.Reg tD+eD) (IR.ITemp rD)
    plES <- eval (IR.Reg tS+eS) (IR.ITemp rS)
    let li=fromIntegral$(n-1)*8
    pure $ plED ++ plES ++ concat [ [Ldp () t0 t1 (RP (IReg rS) (i*16)), Stp () t0 t1 (RP (IReg rD) (i*16))] | i <- [0..fromIntegral (n'-1)] ] ++ [Ldr () t0 (RP (IReg rS) li), Str () t0 (RP (IReg rD) li)]
ir (IR.Cpy (IR.AP tD eD _) (IR.AP tS eS _) eN) = do
    rD <- nextI; rS <- nextI; rN <- nextI; i <- nextR
    t0 <- nextR; t1 <- nextR
    plED <- eval (maybe id (+) eD$IR.Reg tD) (IR.ITemp rD)
    plES <- eval (maybe id (+) eS$IR.Reg tS) (IR.ITemp rS)
    plEN <- eval eN (IR.ITemp rN)
    let rDA=IReg rD; rSA=IReg rS; rNA=IReg rN
    l <- nextL; eL <- nextL
    pure $ plED ++ plES ++ plEN ++ [MovRC () i 0, CmpRR () i rNA, Bc () Geq eL, Tbz () rNA 0 l, Ldr () t0 (R rSA), Str () t0 (R rDA), MovRC () i 1, AddRC () rSA rSA 8, AddRC () rDA rDA 8, Label () l, Ldp () t0 t1 (R rSA), Stp () t0 t1 (R rDA), AddRC () rSA rSA 16, AddRC () rDA rDA 16, AddRC () i i 2, CmpRR () i rNA, Bc () Lt l, Label () eL]
ir (IR.IRnd t) = pure [MrsR () (absReg t)]
ir s             = error (show s)

sai i | i `rem` 16 == 0 = i+16 | otherwise = i+24
-- FIXME: only do +16 when necessary

mw64 :: Word64 -> AbsReg -> [AArch64 AbsReg freg ()]
mw64 w r =
    let w0=w .&. 0xffff; w1=(w .&. 0xffff0000) `rotateR` 16; w2=(w .&. 0xFFFF00000000) `rotateR` 32; w3=(w .&. 0xFFFF000000000000) `rotateR` 48
    in MovRC () r (fromIntegral w0):[MovK () r (fromIntegral wi) s | (wi, s) <- [(w1, 16), (w2, 32), (w3, 48)], wi /= 0 ]

feval :: IR.FExp -> IR.Temp -> WM [AArch64 AbsReg FAbsReg ()]
feval (IR.FReg tS) tD = pure [FMovXX () (fabsReg tD) (fabsReg tS)]
feval (IR.ConstF d) t = do
    i <- nextI
    let r=IReg i
        w=castDoubleToWord64 d
    pure $ mw64 w r ++ [FMovDR () (fabsReg t) r]
feval (IR.FAt (IR.AP tS (Just (IR.ConstI i)) _)) tD | Just i8 <- mp i = pure [LdrD () (fabsReg tD) (RP (absReg tS) i8)]
feval (IR.FB IR.FPlus e0 (IR.FB IR.FTimes e1 e2)) t = do
    i0 <- nextI; i1 <- nextI; i2 <- nextI
    plE0 <- feval e0 (IR.FTemp i0); plE1 <- feval e1 (IR.FTemp i1); plE2 <- feval e2 (IR.FTemp i2)
    pure $ plE0 ++ plE1 ++ plE2 ++ [Fmadd () (fabsReg t) (FReg i1) (FReg i2) (FReg i0)]
feval (IR.FB IR.FPlus (IR.FB IR.FTimes e0 e1) e2) t = do
    i0 <- nextI; i1 <- nextI; i2 <- nextI
    plE0 <- feval e0 (IR.FTemp i0); plE1 <- feval e1 (IR.FTemp i1); plE2 <- feval e2 (IR.FTemp i2)
    pure $ plE0 ++ plE1 ++ plE2 ++ [Fmadd () (fabsReg t) (FReg i0) (FReg i1) (FReg i2)]
feval (IR.FB IR.FMinus e0 (IR.FB IR.FTimes e1 e2)) t = do
    i0 <- nextI; i1 <- nextI; i2 <- nextI
    plE0 <- feval e0 (IR.FTemp i0); plE1 <- feval e1 (IR.FTemp i1); plE2 <- feval e2 (IR.FTemp i2)
    pure $ plE0 ++ plE1 ++ plE2 ++ [Fmsub () (fabsReg t) (FReg i1) (FReg i2) (FReg i0)]
feval (IR.FB IR.FPlus e0 e1) t = do
    i1 <- nextI; i2 <- nextI
    plE0 <- feval e0 (IR.FTemp i1); plE1 <- feval e1 (IR.FTemp i2)
    pure $ plE0 ++ plE1 ++ [Fadd () (fabsReg t) (FReg i1) (FReg i2)]
feval (IR.FB IR.FTimes e0 e1) t = do
    i1 <- nextI; i2 <- nextI
    plE0 <- feval e0 (IR.FTemp i1); plE1 <- feval e1 (IR.FTemp i2)
    pure $ plE0 ++ plE1 ++ [Fmul () (fabsReg t) (FReg i1) (FReg i2)]
feval (IR.FB IR.FMinus e0 e1) t = do
    i1 <- nextI; i2 <- nextI
    plE0 <- feval e0 (IR.FTemp i1); plE1 <- feval e1 (IR.FTemp i2)
    pure $ plE0 ++ plE1 ++ [Fsub () (fabsReg t) (FReg i1) (FReg i2)]
feval (IR.FB IR.FDiv e0 e1) t = do
    i1 <- nextI; i2 <- nextI
    plE0 <- feval e0 (IR.FTemp i1); plE1 <- feval e1 (IR.FTemp i2)
    pure $ plE0 ++ plE1 ++ [Fdiv () (fabsReg t) (FReg i1) (FReg i2)]
feval (IR.FB IR.FMax e0 e1) t = do
    i1 <- nextI; i2 <- nextI
    plE0 <- feval e0 (IR.FTemp i1); plE1 <- feval e1 (IR.FTemp i2)
    pure $ plE0 ++ plE1 ++ [Fmax () (fabsReg t) (FReg i1) (FReg i2)]
feval (IR.FU IR.FAbs e) t = do
    i <- nextI; plE <- feval e (IR.FTemp i)
    pure $ plE ++ [Fabs () (fabsReg t) (FReg i)]
feval (IR.FAt (IR.AP tB (Just (IR.IB IR.IAsl eI (IR.ConstI 3))) _)) tD = do
    i <- nextI; plE <- eval eI (IR.ITemp i)
    pure $ plE ++ [LdrD () (fabsReg tD) (BI (absReg tB) (IReg i) Three)]
feval (IR.FAt (IR.AP tB (Just (IR.IB IR.IPlus (IR.IB IR.IAsl eI (IR.ConstI 3)) (IR.ConstI ix8))) _)) tD | (ix, 0) <- ix8 `quotRem` 8 = do
    i <- nextI; plE <- eval (eI+IR.ConstI ix) (IR.ITemp i)
    pure $ plE ++ [LdrD () (fabsReg tD) (BI (absReg tB) (IReg i) Three)]
feval (IR.FAt (IR.AP tB (Just e) _)) tD = do
    i <- nextI; plE <- eval e (IR.ITemp i)
    pure $ plE ++ [LdrD () (fabsReg tD) (BI (absReg tB) (IReg i) Zero)]
feval (IR.FAt (IR.AP tB Nothing _)) tD =
    pure [LdrD () (fabsReg tD) (R (absReg tB))]
feval (IR.FConv (IR.Reg r)) tD = pure [Scvtf () (fabsReg tD) (absReg r)]
feval (IR.FU IR.FSqrt (IR.FReg r)) t = pure [Fsqrt () (fabsReg t) (fabsReg r)]
feval e _             = error (show e)

eval :: IR.Exp -> IR.Temp -> WM [AArch64 AbsReg FAbsReg ()]
eval (IR.Reg tS) tD = pure [MovRR () (absReg tD) (absReg tS)]
eval (IR.ConstI i) tD | Just u <- mu16 i = pure [MovRC () (absReg tD) u]
eval (IR.ConstI i) tD = pure $ mw64 (fromIntegral i) (absReg tD)
eval (IR.EAt (IR.AP tB (Just (IR.ConstI i)) _)) tD | Just p <- mp i = pure [Ldr () (absReg tD) (RP (absReg tB) p)]
eval (IR.IB IR.IPlus e (IR.ConstI i)) t | Just u <- m12 i = do
    r <- nextI; plE <- eval e (IR.ITemp r)
    pure $ plE ++ [AddRC () (absReg t) (IReg r) u]
eval (IR.IB IR.IAsl e (IR.ConstI i)) t = do
    r <- nextI; plE <- eval e (IR.ITemp r)
    pure $ plE ++ [Lsl () (absReg t) (IReg r) (fromIntegral (i `mod` 64))]
eval (IR.IB IR.IMinus (IR.ConstI 0) e) t = do
    r <- nextI; plE <- eval e (IR.ITemp r)
    pure $ plE ++ [Neg () (absReg t) (IReg r)]
eval (IR.IB IR.IMinus e0 e1) t = do
    r0 <- nextI; r1 <- nextI
    plE0 <- eval e0 (IR.ITemp r0); plE1 <- eval e1 (IR.ITemp r1)
    pure $ plE0 ++ plE1 ++ [SubRR () (absReg t) (IReg r0) (IReg r1)]
eval (IR.IB IR.IPlus (IR.IB IR.ITimes e0 e1) e2) t = do
    r0 <- nextI; r1 <- nextI; r2 <- nextI
    plE0 <- eval e0 (IR.ITemp r0); plE1 <- eval e1 (IR.ITemp r1); plE2 <- eval e2 (IR.ITemp r2)
    pure $ plE0 ++ plE1 ++ plE2 ++ [Madd () (absReg t) (IReg r0) (IReg r1) (IReg r2)]
eval (IR.IB IR.IPlus e0 e1) t = do
    r0 <- nextI; r1 <- nextI
    plE0 <- eval e0 (IR.ITemp r0); plE1 <- eval e1 (IR.ITemp r1)
    pure $ plE0 ++ plE1 ++ [AddRR () (absReg t) (IReg r0) (IReg r1)]
eval (IR.IB IR.ITimes e0 e1) t = do
    r0 <- nextI; r1 <- nextI
    plE0 <- eval e0 (IR.ITemp r0); plE1 <- eval e1 (IR.ITemp r1)
    pure $ plE0 ++ plE1 ++ [MulRR () (absReg t) (IReg r0) (IReg r1)]
eval (IR.IRFloor (IR.FReg r)) t = pure [Fcvtms () (absReg t) (fabsReg r)]
eval (IR.EAt (IR.AP rB (Just (IR.IB IR.IAsl eI (IR.ConstI 3))) _)) t = do
    i <- nextI; plE <- eval eI (IR.ITemp i)
    pure $ plE ++ [Ldr () (absReg t) (BI (absReg rB) (IReg i) Three)]
eval (IR.EAt (IR.AP rB Nothing _)) t = do
    pure [Ldr () (absReg t) (R (absReg rB))]
eval (IR.EAt (IR.AP rB (Just e) _)) t = do
    i <- nextI; plE <- eval e (IR.ITemp i)
    pure $ plE ++ [Ldr () (absReg t) (BI (absReg rB) (IReg i) Zero)]
eval (IR.IB IR.IAsr (IR.Reg r) (IR.ConstI i)) t | Just s <- ms i = pure [Asr () (absReg t) (absReg r) s]
eval e _            = error (show e)

puL, poL :: [AArch64 AbsReg freg ()]
puL = [SubRC () ASP ASP 16, Stp () FP LR (R ASP)]
poL = [Ldp () FP LR (R ASP), AddRC () ASP ASP 16]

ms :: Integral a => a -> Maybe Word8
ms i | i >=0 && i <= 63 = Just (fromIntegral i) | otherwise = Nothing

m12, mu16 :: Integral a => a -> Maybe Word16
m12 i | i >= 0 && i < 4096 = Just (fromIntegral i) | otherwise = Nothing

mu16 i | i > fromIntegral (maxBound :: Word16) || i < fromIntegral (minBound :: Word16) = Nothing
       | otherwise = Just (fromIntegral i)

mp :: Integral a => a -> Maybe Word16
mp i | i `rem` 8 == 0 && i >= 0 && i <= 32760 = Just (fromIntegral i) | otherwise = Nothing
