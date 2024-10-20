module Asm.X86.Trans ( irToX86 ) where

import           Asm.M
import           Asm.X86
import           Control.Monad.Trans.State.Strict (runState)
import           Data.Bifunctor                   (second)
import           Data.ByteString.Internal         (accursedUnutterablePerformIO)
import           Data.Int                         (Int32, Int64, Int8)
import           Data.Tuple                       (swap)
import           Foreign.Marshal.Alloc            (alloca)
import           Foreign.Ptr                      (castPtr)
import           Foreign.Storable                 (peek, poke)
import qualified IR
import qualified Op

plF :: IR.FE -> WM ([X86 AbsReg FAbsReg ()] -> [X86 AbsReg FAbsReg ()], FAbsReg)
plF (IR.FReg t) = pure (id, fabsReg t)
plF e           = do {i <- nextI; pl <- feval e (IR.FTemp i); pure ((pl++), FReg i)}

plI :: IR.Exp -> WM ([X86 AbsReg FAbsReg ()] -> [X86 AbsReg FAbsReg ()], AbsReg)
plI (IR.Reg t) = pure (id, absReg t)
plI e          = do {i <- nextI; pl <- evalE e (IR.ITemp i); pure ((pl++), IReg i)}

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

fabsReg :: IR.FTemp -> FAbsReg
fabsReg (IR.FTemp i) = FReg i
fabsReg IR.F0        = FArg0
fabsReg IR.F1        = FArg1
fabsReg IR.F2        = FArg2
fabsReg IR.F3        = FArg3
fabsReg IR.F4        = FArg4
fabsReg IR.F5        = FArg5
fabsReg IR.FRet      = FRet0
fabsReg IR.FRet1     = FRet1

irToX86 :: IR.WSt -> [IR.Stmt] -> (Int, [X86 AbsReg FAbsReg ()])
irToX86 st = swap . second IR.wtemps . flip runState st . foldMapA ir

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

opPred :: Op.FRel -> Pred
opPred Op.FGeq = Nltus
opPred Op.FGt  = Nleus
opPred Op.FEq  = Eqoq
opPred Op.FNeq = Nequq
opPred Op.FLeq = Leos
opPred Op.FLt  = Ltos

nopPred :: Op.FRel -> Pred
nopPred Op.FGeq = Ltos
nopPred Op.FGt  = Leos
nopPred Op.FEq  = Nequq
nopPred Op.FNeq = Eqoq
nopPred Op.FLt  = Nltus
nopPred Op.FLeq = Nleus

ir :: IR.Stmt -> WM [X86 AbsReg FAbsReg ()]
ir (IR.MT t (IR.EAt (IR.AP m (Just (IR.ConstI i)) _))) | Just i8 <- mi8 i = pure [MovRA () (absReg t) (RC (absReg m) i8)]
ir (IR.MT t (IR.EAt (IR.AP m Nothing _)))               = pure [MovRA () (absReg t) (R$absReg m)]
ir (IR.MX t (IR.FAt (IR.AP m Nothing _ )))              = pure [MovqXA () (fabsReg t) (R (absReg m))]
ir (IR.MX t (IR.FAt (IR.AP m (Just (IR.IB Op.IAsl (IR.Reg i) (IR.ConstI 3))) _))) = pure [MovqXA () (fabsReg t) (RS (absReg m) Eight (absReg i))]
ir (IR.MX t (IR.FAt (IR.AP m (Just (IR.ConstI i)) _))) | Just i8 <- mi8 i = pure [MovqXA () (fabsReg t) (RC (absReg m) i8)]
ir (IR.L l)                                             = pure [Label () l]
ir IR.CD{}                                              = pure []
ir (IR.MT t e)                                          = evalE e t
ir (IR.MJ (IR.IRel Op.ILeq (IR.Reg r0) (IR.Reg r1)) l)  = pure [CmpRR () (absReg r0) (absReg r1), Jle () l]
ir (IR.MJ (IR.IRel Op.ILeq (IR.Reg r0) (IR.ConstI i)) l) | Just i32 <- mi32 i = pure [CmpRI () (absReg r0) i32, Jle () l]
ir (IR.MJ (IR.IRel Op.INeq (IR.Reg r0) (IR.ConstI 0)) l) = pure [TestI () (absReg r0) maxBound, Jne () l]
ir (IR.MJ (IR.IRel Op.INeq (IR.Reg r0) (IR.Reg r1)) l)  = pure [Test () (absReg r0) (absReg r1), Jne () l]
ir (IR.MJ (IR.IRel Op.IEq (IR.Reg r0) (IR.Reg r1)) l)   = pure [Test () (absReg r0) (absReg r1), Je () l]
ir (IR.MJ (IR.IRel Op.IEq (IR.Reg r0) (IR.ConstI i)) l) | Just i32 <- mi32 i = pure [CmpRI () (absReg r0) i32, Je () l]
ir (IR.MJ (IR.IRel Op.IGeq (IR.Reg r0) (IR.ConstI i)) l) | Just i32 <- mi32 i = pure [CmpRI () (absReg r0) i32, Jge () l]
ir (IR.MJ (IR.IRel Op.IGt (IR.Reg r0) (IR.Reg r1)) l)   = pure [CmpRR () (absReg r0) (absReg r1), Jg () l]
ir (IR.MJ (IR.IRel Op.IGeq (IR.Reg r0) e1) l) = do
    i1 <- nextI; plE1 <- evalE e1 (IR.ITemp i1)
    pure $ plE1 ++ [CmpRR () (absReg r0) (IReg i1), Jge () l]
ir (IR.MJ (IR.IRel Op.IGt (IR.Reg r0) (IR.ConstI i)) l) | Just i32 <- mi32 i = pure [CmpRI () (absReg r0) i32, Jg () l]
ir (IR.MJ (IR.IRel Op.ILt (IR.Reg r0) (IR.Reg r1)) l)   = pure [CmpRR () (absReg r0) (absReg r1), Jl () l]
ir (IR.MJ (IR.IRel Op.ILt (IR.Reg r0) (IR.ConstI i)) l) | Just i32 <- mi32 i = pure [CmpRI () (absReg r0) i32, Jl () l]
ir (IR.MJ (IR.IRel Op.ILt (IR.Reg r0) e1) l) = do
    i1 <- nextI; plE1 <- evalE e1 (IR.ITemp i1)
    pure $ plE1 ++ [CmpRR () (absReg r0) (IReg i1), Jl () l]
ir (IR.MJ (IR.FRel fop (IR.FReg r0) e1) l) = do
    (plE1,i1) <- plF e1
    f <- nextF; r <- nextR
    pure $ plE1 [Vcmppd () f (fabsReg r0) i1 (opPred fop), MovqRX () r f, TestI () r maxBound, Jne () l]
ir (IR.MJ (IR.Is p) l) = pure [TestI () (absReg p) 1, Jne () l]
ir (IR.MJ (IR.IU Op.IOdd e) l) = do
    i <- nextI; plE <- evalE e (IR.ITemp i)
    pure $ plE ++ [TestI () (IReg i) 1, Jne () l]
ir (IR.MJ (IR.IU Op.IEven e) l) = do
    i <- nextI; plE <- evalE e (IR.ITemp i)
    pure $ plE ++ [TestI () (IReg i) 1, Je () l]
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
ir (IR.Wr (IR.AP m (Just (IR.IB Op.IAsl (IR.Reg i) (IR.ConstI 3))) _) (IR.Reg r)) = pure [MovAR () (RS (absReg m) Eight (absReg i)) (absReg r)]
ir (IR.Wr (IR.AP m (Just (IR.IB Op.IPlus (IR.IB Op.IAsl (IR.Reg i) (IR.ConstI 3)) (IR.ConstI j))) _) (IR.Reg r)) | Just i8 <- mi8 j = pure [MovAR () (RSD (absReg m) Eight (absReg i) i8) (absReg r)]
ir (IR.Wr (IR.AP m Nothing _) e) = do
    eR <- nextI
    plE <- evalE e (IR.ITemp eR)
    pure $ plE ++ [MovAR () (R (absReg m)) (IReg eR)]
ir (IR.Wr (IR.AP m (Just ei) _) e) = do
    eR <- nextI; eiR <- nextI
    plE <- evalE e (IR.ITemp eR); plE' <- evalE ei (IR.ITemp eiR)
    pure $ plE ++ plE' ++ [MovAR () (RS (absReg m) One (IReg eiR)) (IReg eR)]
ir (IR.WrF (IR.AP m (Just (IR.IB Op.IPlus (IR.IB Op.IAsl ei (IR.ConstI 3)) (IR.ConstI d))) _) (IR.FReg fr)) | Just d8 <- mi8 d = do
    i <- nextI; plEI <- evalE ei (IR.ITemp i)
    pure $ plEI ++ [MovqAX () (RSD (absReg m) Eight (IReg i) d8) (fabsReg fr)]
ir (IR.WrF (IR.AP m (Just (IR.ConstI i)) _) (IR.FReg r)) | Just d8 <- mi8 i = pure [MovqAX () (RC (absReg m) d8) (fabsReg r)]
ir (IR.WrF (IR.AP m (Just ei) _) (IR.FReg r)) = do
    let m' = absReg m
    eR <- nextI
    plE <- evalE ei (IR.ITemp eR)
    pure $ plE ++ [IAddRR () (IReg eR) m', MovqAX () (R (IReg eR)) (fabsReg r)]
ir (IR.WrF (IR.AP m Nothing _) (IR.FReg r)) =
    pure [MovqAX () (R (absReg m)) (fabsReg r)]
ir (IR.WrF (IR.AP m Nothing _) (IR.ConstF x)) = do
    iR <- nextR
    pure [MovRI () iR (fI64 x), MovAR () (R (absReg m)) iR]
ir (IR.Cset t p) = foldMapA ir [IR.MT t 0, IR.Cmov p t 1]
ir (IR.Fcmov (IR.IU Op.IOdd (IR.Reg r0)) t e) = do
    plE <- feval e t; l <- nextL
    pure $ [TestI () (absReg r0) 1, Je () l] ++ plE ++ [Label () l]
ir (IR.Cmov (IR.IU Op.IEven (IR.Reg r)) rD e) = do
    i <- nextI; plE <- evalE e (IR.ITemp i)
    pure $ plE ++ [TestI () (absReg r) 1, Cmove () (absReg rD) (IReg i)]
ir (IR.Cmov (IR.IU Op.IOdd (IR.Reg r)) rD e) = do
    i <- nextI; plE <- evalE e (IR.ITemp i)
    pure $ plE ++ [TestI () (absReg r) 1, Cmovne () (absReg rD) (IReg i)]
ir (IR.Fcmov (IR.IRel Op.IEq (IR.Reg r0) (IR.Reg r1)) t e) = do
    plE <- feval e t; l <- nextL
    pure $ [CmpRR () (absReg r0) (absReg r1), Jne () l] ++ plE ++ [Label () l]
ir (IR.Cmov (IR.IRel Op.IGt (IR.Reg r0) (IR.Reg r1)) rD eS) = do
    iS <- nextI; plES <- evalE eS (IR.ITemp iS)
    pure $ plES ++ [CmpRR () (absReg r0) (absReg r1), Cmovnle () (absReg rD) (IReg iS)]
ir (IR.Cmov (IR.IRel Op.IGeq (IR.Reg r0) (IR.Reg r1)) rD eS) = do
    iS <- nextI; plES <- evalE eS (IR.ITemp iS)
    pure $ plES ++ [CmpRR () (absReg r0) (absReg r1), Cmovnl () (absReg rD) (IReg iS)]
ir (IR.Cmov (IR.IRel Op.INeq (IR.Reg r0) (IR.Reg r1)) rD eS) = do
    iS <- nextI; plES <- evalE eS (IR.ITemp iS)
    pure $ plES ++ [CmpRR () (absReg r0) (absReg r1), Cmovne () (absReg rD) (IReg iS)]
ir (IR.Cmov (IR.IRel Op.IEq (IR.Reg r0) (IR.Reg r1)) rD eS) = do
    iS <- nextI; plES <- evalE eS (IR.ITemp iS)
    pure $ plES ++ [CmpRR () (absReg r0) (absReg r1), Cmove () (absReg rD) (IReg iS)]
ir (IR.Cmov (IR.IRel Op.ILeq (IR.Reg r0) (IR.Reg r1)) rD eS) = do
    iS <- nextI; plES <- evalE eS (IR.ITemp iS)
    pure $ plES ++ [CmpRR () (absReg r0) (absReg r1), Cmovle () (absReg rD) (IReg iS)]
ir (IR.Cmov (IR.IRel Op.ILt (IR.Reg r0) (IR.Reg r1)) rD eS) = do
    iS <- nextI; plES <- evalE eS (IR.ITemp iS)
    pure $ plES ++ [CmpRR () (absReg r0) (absReg r1), Cmovl () (absReg rD) (IReg iS)]
ir (IR.Cmov (IR.FRel fop (IR.FReg xr0) (IR.FReg xr1)) rD e) = do
    i1 <- nextI; plE <- evalE e (IR.ITemp i1)
    f <- nextF; r <- nextR
    pure $ plE ++ [Vcmppd () f (fabsReg xr0) (fabsReg xr1) (opPred fop), MovqRX () r f, TestI () r maxBound, Cmovne () (absReg rD) (IReg i1)]
ir (IR.Fcmov (IR.FRel fop (IR.FReg xr0) (IR.FReg xr1)) t e) = do
    plE <- feval e t; l <- nextL
    f <- nextF; r <- nextR
    pure $ [Vcmppd () f (fabsReg xr0) (fabsReg xr1) (nopPred fop), MovqRX () r f, TestI () r maxBound, Jne () l] ++ plE ++ [Label () l]
ir (IR.Fcmov (IR.IRel Op.IEq (IR.Reg r0) (IR.ConstI n)) t e) | Just i32 <- mi32 n = do
    plE <- feval e t; l <- nextL
    pure $ [CmpRI () (absReg r0) i32, Jne () l] ++ plE ++ [Label () l]
ir (IR.Cpy (IR.AP tD (Just (IR.ConstI sD)) _) (IR.AP tS (Just eI) _) (IR.ConstI n)) | Just n32 <- mi32 n, Just sd8 <- mi8 sD = do
    iT <- nextI
    plE <- evalE (IR.IB Op.IPlus (IR.Reg tS) eI) (IR.ITemp iT)
    i <- nextR; t <- nextR
    l <- nextL; eL <- nextL
    pure $ plE ++ [MovRI () i 0, CmpRI () i (n32-1), Jg () eL, Label () l, MovRA () t (RS (IReg iT) Eight i), MovAR () (RSD (absReg tD) Eight i sd8) t, IAddRI () i 1, CmpRI () i (n32-1), Jle () l, Label () eL]
ir (IR.Cpy (IR.AP tD (Just (IR.ConstI sD)) _) (IR.AP tS (Just (IR.ConstI sI)) _) (IR.ConstI n)) | Just sd8 <- mi8 sD, Just si8 <- mi8 sI = do
    i <- nextR; t <- nextR
    l <- nextL; eL <- nextL
    pure [MovRI () i (n-1), CmpRI () i 0, Jl () eL, Label () l, MovRA () t (RSD (absReg tS) Eight i si8), MovAR () (RSD (absReg tD) Eight i sd8) t, ISubRI () i 1, CmpRI () i 0, Jge () l, Label () eL]
ir (IR.Cpy (IR.AP tD (Just (IR.ConstI sD)) _) (IR.AP tS (Just (IR.ConstI sI)) _) e) | Just sd8 <- mi8 sD, Just si8 <- mi8 sI = do
    ii <- nextI; t <- nextR
    plE <- evalE e (IR.ITemp ii)
    let i = IReg ii
    l <- nextL; endL <- nextL
    pure $ plE ++ [ISubRI () i 1, Label () l, CmpRI () i 0, Jl () endL, MovRA () t (RSD (absReg tS) Eight i si8), MovAR () (RSD (absReg tD) Eight i sd8) t, ISubRI () i 1, J () l, Label () endL]
ir (IR.Cpy (IR.AP tD Nothing _ ) (IR.AP tS Nothing _) (IR.ConstI n)) | n <= 4 = do
    t <- nextR
    pure $ concat [ [ MovRA () t (RC (absReg tS) (i*8)), MovAR () (RC (absReg tD) (i*8)) t ] | i <- [0..(fromIntegral n-1)] ]
ir (IR.Cpy (IR.AP tD (Just e) _) (IR.AP tS Nothing _) (IR.ConstI n)) | n <= 4 = do
    iR <- nextI; plE <- evalE e (IR.ITemp iR)
    t <- nextR
    pure $ plE ++ IAddRR () (IReg iR) (absReg tD):concat [ [MovRA () t (RC (absReg tS) (i*8)), MovAR () (RC (IReg iR) (i*8)) t ] | i <- [0..(fromIntegral n-1)] ]
ir (IR.Cpy (IR.AP tD Nothing _) (IR.AP tS (Just e) _) (IR.ConstI n)) | n <= 4 = do
    iR <- nextI; plE <- evalE e (IR.ITemp iR)
    t <- nextR
    pure $ plE ++ IAddRR () (IReg iR) (absReg tS):concat [ [MovRA () t (RC (IReg iR) (i*8)), MovAR () (RC (absReg tD) (i*8)) t ] | i <- [0..(fromIntegral n-1)] ]
ir (IR.Cpy (IR.AP tD (Just (IR.IB Op.IPlus (IR.IB Op.IAsl eid (IR.ConstI 3)) (IR.ConstI sS))) _) (IR.AP tS (Just (IR.IB Op.IPlus (IR.IB Op.IAsl eis (IR.ConstI 3)) (IR.ConstI dS))) _) n) | Just ds8 <- mi8 dS, Just sS8 <- mi8 sS = do
    (plD,diR) <- plI eid; (plS,siR) <- plI eis
    (plN,nR) <- plI n
    t <- nextR; i <- nextR
    l <- nextL; eL <- nextL
    pure $ plN $ plD $ plS [IAddRR () diR (absReg tD), IAddRR () siR (absReg tS), MovRI () i 0, CmpRR () i nR, Jge () eL, Label () l, MovRA () t (RSD siR Eight i ds8), MovAR () (RSD diR Eight i sS8) t, IAddRI () i 1, CmpRR () i nR, Jl () l, Label () eL]
ir (IR.Cpy (IR.AP tD (Just (IR.IB Op.IAsl eid (IR.ConstI 3))) _) (IR.AP tS (Just (IR.IB Op.IAsl eis (IR.ConstI 3))) _) (IR.ConstI n)) | n <= 4 = do
    (plD,diR) <- plI eid; (plS,siR) <- plI eis
    t <- nextR
    pure $ plD $ plS $ concat [ [ MovRA () t (RSD (absReg tS) Eight siR (i*8)), MovAR () (RSD (absReg tD) Eight diR (i*8)) t ] | i <- [0..(fromIntegral n-1)] ]
ir (IR.Cpy (IR.AP tD (Just ed) _) (IR.AP tS (Just es) _) (IR.ConstI n)) | n <= 4 = do
    dR <- nextI; sR <- nextI
    plD <- evalE ed (IR.ITemp dR); plS <- evalE es (IR.ITemp sR)
    t <- nextR
    pure $ plD ++ plS ++ IAddRR () (IReg dR) (absReg tD):IAddRR () (IReg sR) (absReg tS):concat [ [MovRA () t (RC (IReg sR) (i*8)), MovAR () (RC (IReg dR) (i*8)) t ] | i <- [0..(fromIntegral n-1)] ]
ir (IR.Cpy (IR.AP tD (Just (IR.IB Op.IPlus ed (IR.ConstI sS))) _) (IR.AP tS (Just (IR.ConstI dS)) _) k) | Just dS8 <- mi8 dS, Just sS8 <- mi8 sS = do
    dR <- nextI; kR <- nextI
    plK <- evalE k (IR.ITemp kR); plD <- evalE ed (IR.ITemp dR)
    i <- nextR; t <- nextR
    l <- nextL; eL <- nextL
    pure $ plD ++ plK ++ IAddRR () (IReg dR) (absReg tD):[MovRI () i 0, CmpRR () i (IReg kR), Jge () eL, Label () l, MovRA () t (RSD (absReg tS) Eight i dS8), MovAR () (RSD (IReg dR) Eight i sS8) t, IAddRI () i 1, CmpRR () i (IReg kR), Jl () l, Label () eL]
ir (IR.Cpy (IR.AP tD (Just ed) _) (IR.AP tS (Just (IR.ConstI dS)) _) k) | Just dS8 <- mi8 dS = do
    dR <- nextI; kR <- nextI
    plK <- evalE k (IR.ITemp kR); plD <- evalE ed (IR.ITemp dR)
    i <- nextR; t <- nextR
    l <- nextL; eL <- nextL
    pure $ plD ++ plK ++ IAddRR () (IReg dR) (absReg tD):[MovRI () i 0, CmpRR () i (IReg kR), Jge () eL, Label () l, MovRA () t (RSD (absReg tS) Eight i dS8), MovAR () (RS (IReg dR) Eight i) t, IAddRI () i 1, CmpRR () i (IReg kR), Jl () l, Label () eL]
ir (IR.Cpy (IR.AP tD (Just e) _) (IR.AP tS Nothing _) (IR.ConstI n)) | Just n32 <- mi32 n = do
    iR <- nextI; plE <- evalE e (IR.ITemp iR)
    i <- nextR; t <- nextR
    l <- nextL; eL <- nextL
    pure $ plE ++ [IAddRR () (IReg iR) (absReg tD), MovRI () i 0, CmpRI () i (n32-1), Jg () eL, Label () l, MovRA () t (RS (absReg tS) Eight i), MovAR () (RS (IReg iR) Eight i) t, IAddRI () i 1, CmpRI () i (n32-1), Jle () l, Label () eL]
ir (IR.Cpy (IR.AP tD Nothing _) (IR.AP tS (Just e) _) (IR.ConstI n)) | Just n32 <- mi32 n = do
    iR <- nextI; plE <- evalE e (IR.ITemp iR)
    i <- nextR; t <- nextR
    l <- nextL; eL <- nextL
    pure $ plE ++ [IAddRR () (IReg iR) (absReg tS), MovRI () i 0, CmpRI () i (n32-1), Jg () eL, Label () l, MovRA () t (RS (IReg iR) Eight i), MovAR () (RS (absReg tD) Eight i) t, IAddRI () i 1, CmpRI () i (n32-1), Jle () l, Label () eL]
ir (IR.Cpy (IR.AP tD (Just e) _) (IR.AP tS (Just (IR.ConstI d)) _) (IR.ConstI n)) | Just n32 <- mi32 n, Just d8 <- mi8 d = do
    iR <- nextI; plE <- evalE e (IR.ITemp iR)
    i <- nextR; t <- nextR
    l <- nextL; eL <- nextL
    pure $ plE ++ [IAddRR () (IReg iR) (absReg tD), MovRI () i 0, CmpRI () i (n32-1), Jg () eL, Label () l, MovRA () t (RSD (absReg tS) Eight i d8), MovAR () (RS (IReg iR) Eight i) t, IAddRI () i 1, CmpRI () i (n32-1), Jle () l, Label () eL]
ir (IR.Cpy (IR.AP tD Nothing _) (IR.AP tS (Just e) _) ne) = do
    iR <- nextI; plE <- evalE e (IR.ITemp iR)
    (plN,nR) <- plI ne
    i <- nextR; t <- nextR
    l <- nextL; eL <- nextL
    pure $ plE ++ plN [IAddRR () (IReg iR) (absReg tS), MovRI () i 0, CmpRR () i nR, Jge () eL, Label () l, MovRA () t (RS (IReg iR) Eight i), MovAR () (RS (absReg tD) Eight i) t, IAddRI () i 1, CmpRR () i nR, Jl () l, Label () eL]
ir (IR.Cpy (IR.AP tD (Just e) _) (IR.AP tS Nothing _) ne) = do
    iR <- nextI; plE <- evalE e (IR.ITemp iR)
    (plN,nR) <- plI ne
    i <- nextR; t <- nextR
    l <- nextL; eL <- nextL
    pure $ plE ++ plN [IAddRR () (IReg iR) (absReg tD), MovRI () i 0, CmpRR () i nR, Jge () eL, Label () l, MovRA () t (RS (IReg iR) Eight i), MovAR () (RS (absReg tS) Eight i) t, IAddRI () i 1, CmpRR () i nR, Jl () l, Label () eL]
ir (IR.Cpy (IR.AP tD Nothing _) (IR.AP tS Nothing _) ne) = do
    (plN,nR) <- plI ne
    i <- nextR; t <- nextR
    l <- nextL; eL <- nextL
    pure $ plN [MovRI () i 0, CmpRR () i nR, Jge () eL, Label () l, MovRA () t (RS (absReg tS) Eight i), MovAR () (RS (absReg tD) Eight i) t, IAddRI () i 1, CmpRR () i nR, Jl () l, Label () eL]
ir (IR.Cpy (IR.AP tD (Just (IR.ConstI n)) _) (IR.AP tS (Just e) _) ne) | Just n8 <- mi8 n = do
    iR <- nextI; plE <- evalE e (IR.ITemp iR)
    (plN,nR) <- plI ne
    i <- nextR; t <- nextR
    l <- nextL; eL <- nextL
    pure $ plE ++ plN [IAddRR () (IReg iR) (absReg tS), MovRI () i 0, CmpRR () i nR, Jge () eL, Label () l, MovRA () t (RS (IReg iR) Eight i), MovAR () (RSD (absReg tD) Eight i n8) t, IAddRI () i 1, CmpRR () i nR, Jl () l, Label () eL]
-- https://www.cs.uaf.edu/2015/fall/cs301/lecture/09_23_allocation.html
ir (IR.Sa8 t (IR.ConstI i))                              = pure [ISubRI () SP (saI$i+8), MovRR () (absReg t) SP]
ir (IR.Pop8 (IR.ConstI i))                               = pure [IAddRI () SP (saI$i+8)]
ir (IR.Sa8 t e)                                          = do
    iR <- nextI; plE <- evalE e (IR.ITemp iR)
    l <- nextL
    pure $ plE ++ [TestI () (IReg iR) 0x8, Je () l, IAddRI () (IReg iR) 8, Label () l, ISubRR () SP (IReg iR), MovRR () (absReg t) SP]
ir (IR.Pop8 e)                                           = do
    iR <- nextI; plE <- evalE e (IR.ITemp iR)
    l <- nextL
    pure $ plE ++ [TestI () (IReg iR) 0x8, Je () l, IAddRI () (IReg iR) 8, Label () l, IAddRR () SP (IReg iR)]
ir (IR.FRnd t) =
    pure [Call () DR, Movapd () (fabsReg t) FRet0]
ir (IR.IRnd t)                                          = pure [Rdrand () (absReg t)]
ir (IR.R l)                                             = pure [RetL () l]
ir (IR.C l)                                             = pure [C () l]
ir s                                                    = error (show s)

saI i | i`rem`16 == 0 = fromIntegral i | otherwise = fromIntegral i+8

mSse Op.FMinus = Just Vsubsd
mSse Op.FPlus  = Just Vaddsd
mSse Op.FDiv   = Just Vdivsd
mSse Op.FMax   = Just Vmaxsd
mSse Op.FMin   = Just Vminsd
mSse Op.FTimes = Just Vmulsd
mSse Op.FExp   = Nothing

feval :: IR.FE -> IR.FTemp -> WM [X86 AbsReg FAbsReg ()] -- TODO: feval 0 (xor?)
feval (IR.FB Op.FDiv (IR.FReg r0) (IR.FReg r1)) t   | t == r0 = pure [Divsd () (fabsReg t) (fabsReg r1)]
feval (IR.FB Op.FTimes (IR.FReg r0) (IR.FReg r1)) t | t == r0 = pure [Mulsd () (fabsReg t) (fabsReg r1)]
feval (IR.FB Op.FMinus (IR.FReg r0) (IR.FReg r1)) t | t == r0 = pure [Subsd () (fabsReg t) (fabsReg r1)]
feval (IR.FB Op.FPlus (IR.FReg r0) (IR.FReg r1)) t  | t == r0 = pure [Addsd () (fabsReg t) (fabsReg r1)]
feval (IR.FConv (IR.Reg r)) t                       = pure [Cvtsi2sd () (fabsReg t) (absReg r)]
feval (IR.FReg r) t                                 = pure [Movapd () (fabsReg t) (fabsReg r)]
feval (IR.FB Op.FPlus (IR.FReg r0) (IR.FB Op.FTimes (IR.FReg r1) (IR.FReg r2))) t =
    pure [Movapd () (fabsReg t) (fabsReg r0), Vfmadd231sd () (fabsReg t) (fabsReg r1) (fabsReg r2)]
feval (IR.FB Op.FPlus (IR.FReg r0) (IR.FB Op.FTimes e (IR.FAt (IR.AP b (Just (IR.IB Op.IPlus (IR.IB Op.IAsl eI (IR.ConstI 3)) (IR.ConstI s))) _)))) t | Just i8 <- mi8 s = do
    (plE,i) <- plF e; (plEI,iI) <- plI eI
    pure $ plE $ plEI [Movapd () (fabsReg t) (fabsReg r0), Vfmadd231sdA () (fabsReg t) i (RSD (absReg b) Eight iI i8)]
feval (IR.FB Op.FPlus (IR.FReg r0) (IR.FB Op.FTimes e0 e1)) t = do
    (plE0,i0) <- plF e0; (plE1,i1) <- plF e1
    pure $ plE0 $ plE1 [Movapd () (fabsReg t) (fabsReg r0), Vfmadd231sd () (fabsReg t) i0 i1]
feval (IR.FB Op.FMinus (IR.FReg r0) (IR.FB Op.FTimes (IR.FReg r1) (IR.FReg r2))) t =
    pure [Movapd () (fabsReg t) (fabsReg r0), Vfmnadd231sd () (fabsReg t) (fabsReg r1) (fabsReg r2)]
feval (IR.FB fop e0 e1) t | Just isn <- mSse fop = do
    (plR0,i0) <- plF e0; (plR1,i1) <- plF e1
    pure $ plR0 $ plR1 [isn () (fabsReg t) i0 i1]
feval (IR.ConstF x) t = do
    iR <- nextR
    pure [MovRI () iR (fI64 x), MovqXR () (fabsReg t) iR]
feval (IR.FU Op.FAbs e) t = do
    plE <- feval e t
    l <- nextL
    let nextIR=[IR.MJ (IR.FRel Op.FGeq (IR.FReg t) 0) l, IR.MX t (negate (IR.FReg t)), IR.L l]
    nextX86 <- foldMapA ir nextIR
    pure $ plE ++ nextX86
feval (IR.FU Op.FLog (IR.FReg r0)) t =
    let sa = RC SP (-8) in
    pure [Fldln2 (), MovqAX () sa (fabsReg r0), Fld () sa, Fyl2x (), Fstp () sa, MovqXA () (fabsReg t) sa]
feval (IR.FU Op.FSin (IR.FReg r)) t =
    let sa = RC SP (-8) in
    pure [MovqAX () sa (fabsReg r), Fld () sa, Fsin (), Fstp () sa, MovqXA () (fabsReg t) sa]
feval (IR.FU Op.FCos (IR.FReg r)) t =
    let sa = RC SP (-8) in
    pure [MovqAX () sa (fabsReg r), Fld () sa, Fcos (), Fstp () sa, MovqXA () (fabsReg t) sa]
feval (IR.FB Op.FExp (IR.ConstF 2.718281828459045) e) t = do
    (plE,i) <- plF e
    let sa = RC SP (-8)
    -- https://www.madwizard.org/programming/snippets?id=36
    pure $ plE [MovqAX () sa i, Fninit (), Fld () sa, Fldl2e (), Fmulp (), Fld1 (), FldS () (ST 1), Fprem (), F2xm1 (), Faddp (), Fscale (), Fstp () sa, MovqXA () (fabsReg t) sa]
feval (IR.FB Op.FExp e0 e1) t = do
    (plE0,i0) <- plF e0
    (plE1,i1) <- plF e1
    let sa0 = RC SP (-8)
        sa1 = RC SP (-16)
    -- https://www.madwizard.org/programming/snippets?id=36
    pure $ plE0 $ plE1 [MovqAX () sa0 i0, MovqAX () sa1 i1, Fld () sa1, Fld () sa0, Fyl2x (), Fld1 (), FldS () (ST 1), Fprem (), F2xm1 (), Faddp (), Fscale (), Fstp () sa0, MovqXA () (fabsReg t) sa0]
feval (IR.FU Op.FSqrt (IR.FReg r)) t =
    pure [Sqrtsd () (fabsReg t) (fabsReg r)]
feval (IR.FAt (IR.AP m (Just (IR.IB Op.IPlus (IR.IB Op.IAsl (IR.Reg i) (IR.ConstI 3)) (IR.ConstI d))) _)) rD | Just i8 <- mi8 d = pure [MovqXA () (fabsReg rD) (RSD (absReg m) Eight (absReg i) i8)]
feval (IR.FAt (IR.AP m (Just (IR.IB Op.IPlus (IR.IB Op.IAsl e (IR.ConstI 3)) (IR.ConstI d))) _)) rD | Just i8 <- mi8 d = do
    i <- nextI; plE <- evalE e (IR.ITemp i)
    pure $ plE ++ [MovqXA () (fabsReg rD) (RSD (absReg m) Eight (IReg i) i8)]
feval (IR.FAt (IR.AP m (Just (IR.IB Op.IAsl e (IR.ConstI 3))) _)) rD = do
    i <- nextI; plE <- evalE e (IR.ITemp i)
    pure $ plE ++ [MovqXA () (fabsReg rD) (RS (absReg m) Eight (IReg i))]
feval e _                                           = error (show e)

evalE :: IR.Exp -> IR.Temp -> WM [X86 AbsReg FAbsReg ()]
evalE (IR.Reg r) rD                                  = pure [MovRR () (absReg rD) (absReg r)]
evalE (IR.ConstI 0) rD                               = pure [XorRR () (absReg rD) (absReg rD)]
evalE (IR.ConstI i) rD                               = pure [MovRI () (absReg rD) i]
evalE (IR.IB Op.IPlus (IR.IB Op.ITimes (IR.Reg r0) (IR.Reg r1)) (IR.Reg r2)) rD = let rD' = absReg rD in pure [MovRR () rD' (absReg r0), IMulRR () rD' (absReg r1), IAddRR () rD' (absReg r2)]
evalE (IR.IB Op.ITimes (IR.Reg r0) (IR.Reg r1)) rD | r1 /= rD = let rD' = absReg rD in pure [MovRR () rD' (absReg r0), IMulRR () rD' (absReg r1)]
                                                   | otherwise = pure [IMulRR () (absReg rD) (absReg r0)]
evalE (IR.IB Op.IAsl e (IR.ConstI i)) rD | Just i8 <- mi8 i = do
    let rD' = absReg rD
    eR <- nextI; plE <- evalE e (IR.ITemp eR)
    pure $ plE ++ [MovRR () rD' (IReg eR), Sal () rD' i8]
evalE (IR.IB Op.IAsr e (IR.ConstI i)) rD | Just i8 <- mi8 i = do
    let rD' = absReg rD
    eR <- nextI; plE <- evalE e (IR.ITemp eR)
    pure $ plE ++ [MovRR () rD' (IReg eR), Sar () rD' i8]
evalE (IR.IB Op.IMinus (IR.ConstI 0) e) rD           = do
    let rD' = absReg rD
    plE <- evalE e rD
    pure $ plE ++ [Neg () rD']
evalE (IR.IB Op.IMinus e (IR.ConstI i)) rD           = do
    let rD' = absReg rD
    eR <- nextI
    plE <- evalE e (IR.ITemp eR)
    pure $ plE ++ [MovRR () rD' (IReg eR), ISubRI () rD' i]
evalE (IR.IB Op.IMinus e e') rD                      = do
    let rD' = absReg rD
    (plE,eR) <- plI e; (plE',e'R) <- plI e'
    pure $ plE $ plE' [MovRR () rD' eR, ISubRR () rD' e'R]
evalE (IR.IB Op.IPlus e (IR.ConstI i)) rD            = do
    let rD' = absReg rD
    (plE,eR) <- plI e
    pure $ plE [MovRR () rD' eR, IAddRI () rD' i]
evalE (IR.IB Op.IPlus e e') rD                       = do
    let rD' = absReg rD
    (plE,eR) <- plI e; (plE',e'R) <- plI e'
    pure $ plE $ plE' [MovRR () rD' eR, IAddRR () rD' e'R]
evalE (IR.IB Op.ITimes e (IR.EAt (IR.AP m (Just (IR.IB Op.IPlus (IR.IB Op.IAsl (IR.Reg i) (IR.ConstI 3)) (IR.ConstI d))) _))) rD | Just d8 <- mi8 d = do
    let rD'=absReg rD
    eR <- nextI; plE <- evalE e (IR.ITemp eR)
    pure $ plE ++ [MovRR () rD' (IReg eR), IMulRA () rD' (RSD (absReg m) Eight (absReg i) d8)]
evalE (IR.IB Op.ITimes e0 e1) rD = do
    let rD' = absReg rD
    e0R <- nextI; plE0 <- evalE e0 (IR.ITemp e0R)
    e1R <- nextI; plE1 <- evalE e1 (IR.ITemp e1R)
    pure $ plE0 ++ plE1 ++ [MovRR () rD' (IReg e0R), IMulRR () rD' (IReg e1R)]
evalE (IR.IB Op.IDiv e0 e1) rD                       = do
    let rD' = absReg rD
    e0R <- nextI; e1R <- nextI
    plE0 <- evalE e0 (IR.ITemp e0R)
    plE1 <- evalE e1 (IR.ITemp e1R)
    pure $ plE0 ++ plE1 ++ [XorRR () Rem Rem, MovRR () Quot (IReg e0R), IDiv () (IReg e1R), MovRR () rD' Quot]
evalE (IR.IB Op.IRem e0 e1) rD                       = do
    let rD' = absReg rD
    e0R <- nextI; e1R <- nextI
    plE0 <- evalE e0 (IR.ITemp e0R); plE1 <- evalE e1 (IR.ITemp e1R)
    pure $ plE0 ++ plE1 ++ [XorRR () Rem Rem, MovRR () Quot (IReg e0R), IDiv () (IReg e1R), MovRR () rD' Rem]
evalE (IR.IRFloor (IR.FReg r)) t                     = let r' = fabsReg r in pure [Roundsd () r' r' RDown, Cvttsd2si () (absReg t) r']
evalE (IR.EAt (IR.AP m (Just (IR.ConstI i)) _)) rD | Just i8 <- mi8 i = pure [MovRA () (absReg rD) (RC (absReg m) i8)]
evalE (IR.EAt (IR.AP m (Just (IR.Reg i)) _)) rD = pure [MovRA () (absReg rD) (RS (absReg m) One (absReg i))]
evalE (IR.EAt (IR.AP m (Just (IR.IB Op.IAsl (IR.Reg i) (IR.ConstI 3))) _)) rD = pure [MovRA () (absReg rD) (RS (absReg m) Eight (absReg i))]
evalE (IR.IB (Op.BI Op.AndB) (IR.Reg r0) (IR.Reg r1)) rD     = let rD' = absReg rD in pure [MovRR () rD' (absReg r0), And () rD' (absReg r1)]
evalE (IR.EAt (IR.AP m (Just (IR.IB Op.IPlus (IR.IB Op.IAsl (IR.Reg i) (IR.ConstI 3)) (IR.ConstI d))) _)) rD | Just i8 <- mi8 d = pure [MovRA () (absReg rD) (RSD (absReg m) Eight (absReg i) i8)]
evalE (IR.EAt (IR.AP m (Just (IR.IB Op.IPlus (IR.IB Op.IAsl e (IR.ConstI 3)) (IR.ConstI d))) _)) rD | Just i8 <- mi8 d = do
    i <- nextI; plE <- evalE e (IR.ITemp i)
    pure $ plE ++ [MovRA () (absReg rD) (RSD (absReg m) Eight (IReg i) i8)]
evalE (IR.EAt (IR.AP m Nothing _)) rD =
    let rD'=absReg rD in pure [MovRA () rD' (R$absReg m)]
evalE (IR.EAt (IR.AP m (Just e) _)) rD = do
    let rD'=absReg rD;m'=absReg m
    r <- nextR; eR <- nextI; plE <- evalE e (IR.ITemp eR)
    pure $ plE ++ [MovRR () r m', IAddRR () r (IReg eR), MovRA () rD' (R r)]
evalE (IR.LA i) rD                                   = pure [MovRL () (absReg rD) i]
evalE e _                                            = error (show e)
