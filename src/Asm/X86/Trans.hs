module Asm.X86.Trans ( irToX86 ) where

import           Asm.X86
import           Control.Monad.State.Strict (State, evalState, gets, modify)
import           Data.ByteString.Internal   (accursedUnutterablePerformIO)
import           Data.Foldable              (fold)
import           Data.Functor               (($>))
import           Data.Int                   (Int32, Int64, Int8)
import           Foreign.Marshal.Alloc      (alloca)
import           Foreign.Ptr                (castPtr)
import           Foreign.Storable           (peek, poke)
import qualified IR

type WM = State IR.WSt

absReg :: IR.Temp -> AbsReg
absReg (IR.ITemp i) = IReg i
absReg (IR.ATemp i) = IReg i
absReg IR.C0        = CArg0
absReg IR.C1        = CArg1
absReg IR.C2        = CArg2
absReg IR.C3        = CArg3
absReg IR.C4        = CArg4
absReg IR.C5        = CArg5
absReg IR.CRet      = CRet

fabsReg :: IR.Temp -> FAbsReg
fabsReg (IR.FTemp i) = FReg i
fabsReg IR.F0        = FArg0
fabsReg IR.F1        = FArg1
fabsReg IR.F2        = FArg2
fabsReg IR.F3        = FArg3
fabsReg IR.F4        = FArg4
fabsReg IR.F5        = FArg5
fabsReg IR.FRet      = FRet0
fabsReg IR.FRet1     = FRet1

foldMapA :: (Applicative f, Traversable t, Monoid m) => (a -> f m) -> t a -> f m
foldMapA = (fmap fold .) . traverse

irToX86 :: IR.WSt -> [IR.Stmt] -> [X86 AbsReg FAbsReg ()]
irToX86 st = flip evalState st . foldMapA ir

nextI :: WM Int
nextI = do { i <- gets (head.IR.wtemps); modify (\(IR.WSt l (_:t)) -> IR.WSt l t) $> i }

nextL :: WM Label
nextL = do { i <- gets (head.IR.wlabels); modify (\(IR.WSt (_:l) t) -> IR.WSt l t) $> i }

nextR :: WM AbsReg
nextR = IReg <$> nextI; nextF = FReg <$> nextI

mi8 :: Int64 -> Maybe Int8
mi8 i | i <= fromIntegral (maxBound :: Int8) && i >= fromIntegral (minBound :: Int8) = Just $ fromIntegral i
       | otherwise = Nothing

mi32 :: Int64 -> Maybe Int32
mi32 i | i <= fromIntegral (maxBound :: Int32) && i >= fromIntegral (minBound :: Int32) = Just $ fromIntegral i
       | otherwise = Nothing

fI64 :: Double -> Int64
fI64 x = accursedUnutterablePerformIO $ alloca $ \bytes -> poke (castPtr bytes) x *> peek bytes

ir :: IR.Stmt -> WM [X86 AbsReg FAbsReg ()]
ir (IR.MT t (IR.EAt (IR.AP m (Just (IR.ConstI i)) _))) | Just i8 <- mi8 i = pure [MovRA () (absReg t) (RC (absReg m) i8)]
ir (IR.MT t (IR.EAt (IR.AP m Nothing _)))               = pure [MovRA () (absReg t) (R$absReg m)]
ir (IR.MX t (IR.FAt (IR.AP m Nothing _ )))              = pure [MovqXA () (fabsReg t) (R (absReg m))]
ir (IR.MX t (IR.FAt (IR.AP m (Just (IR.IB IR.IAsl (IR.Reg i) (IR.ConstI 3))) _))) = pure [MovqXA () (fabsReg t) (RS (absReg m) Eight (absReg i))]
ir (IR.MX t (IR.FAt (IR.AP m (Just (IR.ConstI i)) _))) | Just i8 <- mi8 i = pure [MovqXA () (fabsReg t) (RC (absReg m) i8)]
ir (IR.L l)                                             = pure [Label () l]
ir (IR.MT t e)                                          = evalE e t
ir (IR.MJ (IR.IRel IR.ILeq (IR.Reg r0) (IR.Reg r1)) l)  = pure [CmpRR () (absReg r0) (absReg r1), Jle () l]
ir (IR.MJ (IR.IRel IR.INeq (IR.Reg r0) (IR.Reg r1)) l)  = pure [Test () (absReg r0) (absReg r1), Jne () l]
ir (IR.MJ (IR.IRel IR.IEq (IR.Reg r0) (IR.Reg r1)) l)   = pure [Test () (absReg r0) (absReg r1), Je () l]
ir (IR.MJ (IR.IRel IR.IEq (IR.Reg r0) (IR.ConstI i)) l) | Just i32 <- mi32 i = pure [CmpRI () (absReg r0) i32, Je () l]
ir (IR.MJ (IR.IRel IR.IGt (IR.Reg r0) (IR.Reg r1)) l)   = pure [CmpRR () (absReg r0) (absReg r1), Jg () l]
ir (IR.MJ (IR.IRel IR.IGeq (IR.Reg r0) (IR.Reg r1)) l)  = pure [CmpRR () (absReg r0) (absReg r1), Jge () l]
ir (IR.MJ (IR.IRel IR.IGt (IR.Reg r0) (IR.ConstI i)) l) | Just i32 <- mi32 i = pure [CmpRI () (absReg r0) i32, Jg () l]
ir (IR.MJ (IR.IRel IR.IGeq (IR.Reg r0) (IR.ConstI i)) l) | Just i32 <- mi32 i = pure [CmpRI () (absReg r0) i32, Jge () l]
ir (IR.MJ (IR.IRel IR.ILt (IR.Reg r0) (IR.Reg r1)) l)   = pure [CmpRR () (absReg r0) (absReg r1), Jl () l]
ir (IR.MJ (IR.IRel IR.ILt (IR.Reg r0) (IR.ConstI i)) l) | Just i32 <- mi32 i = pure [CmpRI () (absReg r0) i32, Jl () l]
ir (IR.MJ (IR.FRel IR.FGeq (IR.FReg r0) (IR.FReg r1)) l) = do
    f <- nextF; r <- nextR
    pure [Vcmppd () f (fabsReg r0) (fabsReg r1) Nltus, MovqRX () r f, TestI () r maxBound, Jne () l]
ir (IR.J l)                                             = pure [J () l]
-- see https://www.agner.org/optimize/optimizing_assembly.pdf, p. 125
ir (IR.MX t e)                                          = feval e t
ir IR.RA{}                                              = pure[]
ir (IR.Ma _ t e)                                        = do
    plE <- evalE e IR.C0
    pure $ plE ++ [Call () Malloc, MovRR () (absReg t) CRet]
ir (IR.Free t)                                          =
    pure [MovRR () CArg0 (absReg t), Call () Free]
ir (IR.Wr (IR.AP m Nothing _) (IR.ConstI i)) | Just i32 <- mi32 i = pure [MovAI32 () (R$absReg m) i32]
ir (IR.Wr (IR.AP m Nothing _) (IR.Reg r))               = pure [MovAR () (R$absReg m) (absReg r)]
ir (IR.Wr (IR.AP m (Just (IR.ConstI i)) _) (IR.Reg r)) | Just i8 <- mi8 i = pure [MovAR () (RC (absReg m) i8) (absReg r)]
ir (IR.Wr (IR.AP m (Just (IR.ConstI i)) _) e) | Just i8 <- mi8 i = do
    iT <- nextI
    plE <- evalE e (IR.ITemp iT)
    pure $ plE ++ [MovAR () (RC (absReg m) i8) (IReg iT)]
ir (IR.Wr (IR.AP m (Just (IR.Reg ix)) _) (IR.Reg r)) = pure [MovAR () (RS (absReg m) One (absReg ix)) (absReg r)]
ir (IR.Wr (IR.AP m (Just (IR.IB IR.IAsl (IR.Reg i) (IR.ConstI 3))) _) (IR.Reg r)) = pure [MovAR () (RS (absReg m) Eight (absReg i)) (absReg r)]
ir (IR.Wr (IR.AP m (Just (IR.IB IR.IPlus (IR.IB IR.IAsl (IR.Reg i) (IR.ConstI 3)) (IR.ConstI j))) _) (IR.Reg r)) | Just i8 <- mi8 j = pure [MovAR () (RSD (absReg m) Eight (absReg i) i8) (absReg r)]
ir (IR.WrF (IR.AP m (Just (IR.IB IR.IPlus (IR.IB IR.IAsl (IR.Reg i) (IR.ConstI 3)) (IR.ConstI d))) _) (IR.FReg fr)) | Just d8 <- mi8 d = pure [MovqAX () (RSD (absReg m) Eight (absReg i) d8) (fabsReg fr)]
ir (IR.WrF (IR.AP m (Just (IR.ConstI i)) _) (IR.FReg r)) | Just d8 <- mi8 i = pure [MovqAX () (RC (absReg m) d8) (fabsReg r)]
ir (IR.Cmov (IR.IRel IR.IGt (IR.Reg r0) (IR.Reg r1)) rD (IR.Reg rS)) = pure [CmpRR () (absReg r0) (absReg r1), Cmovnle () (absReg rD) (absReg rS)]
ir (IR.Cpy (IR.AP tD (Just (IR.ConstI sD)) _) (IR.AP tS (Just eI) _) (IR.ConstI n)) | Just n32 <- mi32 n, Just sd8 <- mi8 sD = do
    iT <- nextI
    plE <- evalE (IR.IB IR.IPlus (IR.Reg tS) eI) (IR.ITemp iT)
    i <- nextR; t <- nextR
    l <- nextL; endL <- nextL
    pure $ plE ++ [MovRI () i 0, Label () l, CmpRI () i (n32-1), Jg () endL, MovRA () t (RS (IReg iT) Eight i), MovAR () (RSD (absReg tD) Eight i sd8) t, IAddRI () i 1, J () l, Label () endL]
ir (IR.Cpy (IR.AP tD (Just (IR.ConstI sD)) _) (IR.AP tS (Just (IR.ConstI sI)) _) (IR.ConstI n)) | Just sd8 <- mi8 sD, Just si8 <- mi8 sI = do
    i <- nextR; t <- nextR
    l <- nextL; endL <- nextL
    pure [MovRI () i (n-1), Label () l, CmpRI () i 0, Jl () endL, MovRA () t (RSD (absReg tS) Eight i si8), MovAR () (RSD (absReg tD) Eight i sd8) t, ISubRI () i 1, J () l, Label () endL]
ir (IR.Cpy (IR.AP tD (Just (IR.ConstI sD)) _) (IR.AP tS (Just (IR.ConstI sI)) _) e) | Just sd8 <- mi8 sD, Just si8 <- mi8 sI = do
    ii <- nextI; t <- nextR
    plE <- evalE e (IR.ITemp ii)
    let i = IReg ii
    l <- nextL; endL <- nextL
    pure $ plE ++ [ISubRI () i 1, Label () l, CmpRI () i 0, Jl () endL, MovRA () t (RSD (absReg tS) Eight i si8), MovAR () (RSD (absReg tD) Eight i sd8) t, ISubRI () i 1, J () l, Label () endL]
ir (IR.Cpy (IR.AP tD (Just e) _) (IR.AP tS Nothing _) (IR.ConstI n)) | n <= 4 = do
    iR <- nextI; plE <- evalE e (IR.ITemp iR)
    t <- nextR
    pure $ plE ++ IAddRR () (IReg iR) (absReg tD):concat [ [MovRA () t (RC (absReg tS) (i*8)), MovAR () (RC (IReg iR) (i*8)) t ] | i <- [0..(fromIntegral n-1)] ]
ir (IR.Cpy (IR.AP tD (Just e) _) (IR.AP tS Nothing _) (IR.ConstI n)) | Just n32 <- mi32 n = do
    iR <- nextI; plE <- evalE e (IR.ITemp iR)
    i <- nextR; t <- nextR
    l <- nextL; endL <- nextL
    pure $ plE ++ [IAddRR () (IReg iR) (absReg tD), MovRI () i 0, Label () l, CmpRI () i (n32-1), Jg () endL, MovRA () t (RS (absReg tS) Eight i), MovAR () (RS (IReg iR) Eight i) t, IAddRI () i 1, J () l, Label () endL]
ir (IR.Cpy (IR.AP tD Nothing _) (IR.AP tS (Just e) _) (IR.ConstI n)) | Just n32 <- mi32 n = do
    iR <- nextI; plE <- evalE e (IR.ITemp iR)
    i <- nextR; t <- nextR
    l <- nextL; endL <- nextL
    pure $ plE ++ [IAddRR () (IReg iR) (absReg tS), MovRI () i 0, Label () l, CmpRI () i (n32-1), Jg () endL, MovRA () t (RS (IReg iR) Eight i), MovAR () (RS (absReg tD) Eight i) t, IAddRI () i 1, J () l, Label () endL]
ir (IR.Cpy (IR.AP tD (Just e) _) (IR.AP tS (Just (IR.ConstI d)) _) (IR.ConstI n)) | Just n32 <- mi32 n, Just d8 <- mi8 d = do
    iR <- nextI; plE <- evalE e (IR.ITemp iR)
    i <- nextR; t <- nextR
    l <- nextL; endL <- nextL
    pure $ plE ++ [IAddRR () (IReg iR) (absReg tD), MovRI () i 0, Label () l, CmpRI () i (n32-1), Jg () endL, MovRA () t (RSD (absReg tS) Eight i d8), MovAR () (RS (IReg iR) Eight i) t, IAddRI () i 1, J () l, Label () endL]
-- https://www.cs.uaf.edu/2015/fall/cs301/lecture/09_23_allocation.html
ir (IR.Sa t i)                                          = pure [ISubRI () SP (saI i+8), MovRR () (absReg t) SP]
ir (IR.Pop i)                                           = pure [IAddRI () SP (saI i+8)]
ir (IR.R l)                                             = pure [RetL () l]
ir (IR.C l)                                             = pure [C () l]
ir s                                                    = error (show s)

saI i | i+8 `rem` 16 == 0 = fromIntegral i | otherwise = fromIntegral i+8

feval :: IR.FExp -> IR.Temp -> WM [X86 AbsReg FAbsReg ()] -- TODO: feval 0 (xor?)
feval (IR.FB IR.FDiv (IR.FReg r0) (IR.FReg r1)) t   | t == r0 = pure [Divsd () (fabsReg t) (fabsReg r1)]
feval (IR.FB IR.FTimes (IR.FReg r0) (IR.FReg r1)) t | t == r0 = pure [Mulsd () (fabsReg t) (fabsReg r1)]
feval (IR.FB IR.FMinus (IR.FReg r0) (IR.FReg r1)) t | t == r0 = pure [Subsd () (fabsReg t) (fabsReg r1)]
feval (IR.FB IR.FPlus (IR.FReg r0) (IR.FReg r1)) t  | t == r0 = pure [Addsd () (fabsReg t) (fabsReg r1)]
feval (IR.FB IR.FDiv (IR.FReg r0) (IR.FReg r1)) t   = pure [Vdivsd () (fabsReg t) (fabsReg r0) (fabsReg r1)]
feval (IR.FB IR.FTimes (IR.FReg r0) (IR.FReg r1)) t = pure [Vmulsd () (fabsReg t) (fabsReg r0) (fabsReg r1)]
feval (IR.FB IR.FPlus (IR.FReg r0) (IR.FReg r1)) t  = pure [Vaddsd () (fabsReg t) (fabsReg r0) (fabsReg r1)]
feval (IR.FB IR.FMinus (IR.FReg r0) (IR.FReg r1)) t = pure [Vsubsd () (fabsReg t) (fabsReg r0) (fabsReg r1)]
feval (IR.FConv (IR.Reg r)) t                       = pure [Cvtsi2sd () (fabsReg t) (absReg r)]
feval (IR.FReg r) t                                 = pure [Movapd () (fabsReg t) (fabsReg r)]
feval (IR.FB IR.FMinus (IR.FReg r0) e) t            = do
    i <- nextI
    putR <- feval e (IR.FTemp i)
    pure $ putR ++ [Vsubsd () (fabsReg t) (fabsReg r0) (FReg i)]
feval (IR.FB IR.FMinus e (IR.FReg r)) t            = do
    i <- nextI
    putR <- feval e (IR.FTemp i)
    pure $ putR ++ [Vsubsd () (fabsReg t) (FReg i) (fabsReg r)]
feval (IR.FB IR.FPlus (IR.FReg r0) (IR.FB IR.FTimes (IR.FReg r1) (IR.FReg r2))) t =
    pure [Movapd () (fabsReg t) (fabsReg r0), Vfmadd231sd () (fabsReg t) (fabsReg r1) (fabsReg r2)]
feval (IR.FB IR.FPlus (IR.FReg r0) e) t            = do
    i <- nextI
    putR <- feval e (IR.FTemp i)
    pure $ putR ++ [Vaddsd () (fabsReg t) (fabsReg r0) (FReg i)]
feval (IR.ConstF x) t = do
    iR <- nextR
    pure [MovRI () iR (fI64 x), MovqXR () (fabsReg t) iR]
feval (IR.FU IR.FLog (IR.FReg r0)) t =
    let sa = RC SP (-8) in
    pure [Fldln2 (), MovqAX () sa (fabsReg r0), Fld () sa, Fyl2x (), Fstp () sa, MovqXA () (fabsReg t) sa]
feval (IR.FU IR.FSin (IR.FReg r)) t =
    let sa = RC SP (-8) in
    pure [MovqAX () sa (fabsReg r), Fld () sa, Fsin (), Fstp () sa, MovqXA () (fabsReg t) sa]
feval (IR.FB IR.FExp (IR.ConstF 2.718281828459045) e) t = do
    i <- nextI
    putE <- feval e (IR.FTemp i)
    let sa = RC SP (-8)
    -- https://www.madwizard.org/programming/snippets?id=36
    pure $ putE ++ [MovqAX () sa (FReg i), Fld () sa, Fldl2e (), Fmulp (), Fld1 (), FldS () (ST 1), Fprem (), F2xm1 (), Faddp (), Fscale (), Fstp () sa, MovqXA () (fabsReg t) sa]
feval (IR.FB IR.FExp e0 e1) t = do
    i0 <- nextI
    i1 <- nextI
    putE0 <- feval e0 (IR.FTemp i0)
    putE1 <- feval e1 (IR.FTemp i1)
    let sa0 = RC SP (-8)
        sa1 = RC SP (-16)
    -- https://www.madwizard.org/programming/snippets?id=36
    pure $ putE0 ++ putE1 ++ [MovqAX () sa0 (FReg i0), MovqAX () sa1 (FReg i1), Fld () sa1, Fld () sa0, Fyl2x (), Fld1 (), FldS () (ST 1), Fprem (), F2xm1 (), Faddp (), Fscale (), Fstp () sa0, MovqXA () (fabsReg t) sa0]
feval (IR.FU IR.FSqrt (IR.FReg r)) t =
    pure [Sqrtsd () (fabsReg t) (fabsReg r)]
feval (IR.FAt (IR.AP m (Just (IR.IB IR.IPlus (IR.IB IR.IAsl (IR.Reg i) (IR.ConstI 3)) (IR.ConstI d))) _)) rD | Just i8 <- mi8 d = pure [MovqXA () (fabsReg rD) (RSD (absReg m) Eight (absReg i) i8)]
feval (IR.FAt (IR.AP m (Just (IR.IB IR.IPlus (IR.IB IR.IAsl e (IR.ConstI 3)) (IR.ConstI d))) _)) rD | Just i8 <- mi8 d = do
    i <- nextI; plE <- evalE e (IR.ITemp i)
    pure $ plE ++ [MovqXA () (fabsReg rD) (RSD (absReg m) Eight (IReg i) i8)]
feval e _                                           = error (show e)

evalE :: IR.Exp -> IR.Temp -> WM [X86 AbsReg FAbsReg ()]
evalE (IR.Reg r) rD                                  = pure [MovRR () (absReg rD) (absReg r)]
evalE (IR.ConstI 0) rD                               = pure [XorRR () (absReg rD) (absReg rD)]
evalE (IR.ConstI i) rD                               = pure [MovRI () (absReg rD) i]
evalE (IR.IB IR.IPlus (IR.Reg r0) (IR.ConstI i)) rD  = let rD' = absReg rD in pure [MovRR () rD' (absReg r0), IAddRI () rD' i]
evalE (IR.IB IR.ITimes (IR.Reg r0) (IR.Reg r1)) rD   = let rD' = absReg rD in pure [MovRR () rD' (absReg r0), IMulRR () rD' (absReg r1)]
evalE (IR.IB IR.IAsl e (IR.ConstI i)) rD | Just i8 <- mi8 i = do
    let rD' = absReg rD
    eR <- nextI; plE <- evalE e (IR.ITemp eR)
    pure $ plE ++ [MovRR () rD' (IReg eR), Sal () rD' i8]
evalE (IR.IB IR.IAsr e (IR.ConstI i)) rD | Just i8 <- mi8 i = do
    let rD' = absReg rD
    eR <- nextI; plE <- evalE e (IR.ITemp eR)
    pure $ plE ++ [MovRR () rD' (IReg eR), Sar () rD' i8]
evalE (IR.IB IR.IMinus e (IR.ConstI i)) rD           = do
    let rD' = absReg rD
    eR <- nextI
    plE <- evalE e (IR.ITemp eR)
    pure $ plE ++ [MovRR () rD' (IReg eR), ISubRI () rD' i]
evalE (IR.IB IR.IMinus e (IR.Reg r)) rD              = do
    let rD' = absReg rD
    eR <- nextI
    plE <- evalE e (IR.ITemp eR)
    pure $ plE ++ [MovRR () rD' (IReg eR), ISubRR () rD' (absReg r)]
evalE (IR.IB IR.IPlus (IR.Reg r) e) rD               = do
    let rD' = absReg rD
    eR <- nextI
    plE <- evalE e (IR.ITemp eR)
    pure $ plE ++ [MovRR () rD' (absReg r), IAddRR () rD' (IReg eR)]
evalE (IR.IB IR.ITimes e0 e1) rD = do
    let rD' = absReg rD
    e0R <- nextI; plE0 <- evalE e0 (IR.ITemp e0R)
    e1R <- nextI; plE1 <- evalE e1 (IR.ITemp e1R)
    pure $ plE0 ++ plE1 ++ [MovRR () rD' (IReg e0R), IMulRR () rD' (IReg e1R)]
evalE (IR.IB IR.IDiv e0 e1) rD                       = do
    let rD' = absReg rD
    e0R <- nextI
    e1R <- nextI
    plE0 <- evalE e0 (IR.ITemp e0R)
    plE1 <- evalE e1 (IR.ITemp e1R)
    pure $ plE0 ++ plE1 ++ [MovRR () Quot (IReg e0R), IDiv () (IReg e1R), MovRR () rD' Quot]
evalE (IR.IB IR.IPlus e (IR.ConstI i)) rD            = do
    let rD' = absReg rD
    eR <- nextI
    plE <- evalE e (IR.ITemp eR)
    pure $ plE ++ [MovRR () rD' (IReg eR), IAddRI () rD' i]
    -- = let rD' = absReg rD in pure [MovRR () rD' (absReg r0), IAddRI () rD' i]
evalE (IR.IRFloor (IR.FReg r)) t                     = let r' = fabsReg r in pure [Roundsd () r' r' RDown, Cvttsd2si () (absReg t) r']
evalE (IR.EAt (IR.AP m (Just (IR.ConstI i)) _)) rD | Just i8 <- mi8 i = pure [MovRA () (absReg rD) (RC (absReg m) i8)]
evalE (IR.EAt (IR.AP m (Just (IR.IB IR.IAsl (IR.Reg i) (IR.ConstI 3))) _)) rD = pure [MovRA () (absReg rD) (RS (absReg m) Eight (absReg i))]
evalE (IR.IU IR.INot (IR.Reg r0)) rD | r0 == rD      = pure [Not () (absReg rD)]
                                     | otherwise     = let rD' = absReg rD in pure [MovRR () rD' (absReg r0), Not () rD']
evalE (IR.IB IR.IAnd (IR.Reg r0) (IR.Reg r1)) rD     = let rD' = absReg rD in pure [MovRR () rD' (absReg r0), And () rD' (absReg r1)]
evalE (IR.EAt (IR.AP m (Just (IR.IB IR.IPlus (IR.IB IR.IAsl (IR.Reg i) (IR.ConstI 3)) (IR.ConstI d))) _)) rD | Just i8 <- mi8 d = pure [MovRA () (absReg rD) (RSD (absReg m) Eight (absReg i) i8)]
evalE (IR.EAt (IR.AP m (Just (IR.IB IR.IPlus (IR.IB IR.IAsl e (IR.ConstI 3)) (IR.ConstI d))) _)) rD | Just i8 <- mi8 d = do
    i <- nextI; plE <- evalE e (IR.ITemp i)
    pure $ plE ++ [MovRA () (absReg rD) (RSD (absReg m) Eight (IReg i) i8)]
evalE e _                                            = error (show e)
