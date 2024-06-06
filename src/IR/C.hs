module IR.C ( ctemp
            , cToIR
            ) where

import           C
import           Control.Monad              (foldM)
import           Control.Monad.State.Strict (State, runState, state)
import           Data.Bits                  (FiniteBits, countTrailingZeros, popCount)
import           Data.Coerce                (coerce)
import           Data.Int                   (Int64)
import           IR
import           Op

type IRM = State WSt

nextI :: IRM C.Temp
nextI = C.ITemp <$> state (\(WSt ls (t:ts)) -> (t, WSt ls ts))

nextL :: IRM IR.Label
nextL = state (\(WSt (l:ls) ts) -> (l, WSt ls ts))

ctemp :: C.Temp -> IR.Temp
ctemp (C.ATemp i) = IR.ATemp i
ctemp (C.ITemp i) = IR.ITemp i
ctemp C.C0        = IR.C0
ctemp C.C1        = IR.C1
ctemp C.C2        = IR.C2
ctemp C.C3        = IR.C3
ctemp C.C4        = IR.C4
ctemp C.C5        = IR.C5
ctemp C.CRet      = IR.CRet

fx :: FTemp -> IR.Temp
fx (C.FTemp i) = IR.FTemp i
fx FRet0       = FRet
fx C.FRet1     = IR.FRet1
fx C.F0        = IR.F0
fx C.F1        = IR.F1
fx C.F2        = IR.F2
fx C.F3        = IR.F3
fx C.F4        = IR.F4
fx C.F5        = IR.F5

cToIR :: LSt -> [CS] -> ([Stmt], WSt)
cToIR (LSt ls ts) cs = runState (foldMapM cToIRM cs) (WSt ls ts)

tick reg = IR.MT reg (Reg reg+1)

nr IGeq=ILt; nr IGt=ILeq; nr ILt=IGeq; nr ILeq=IGt; nr IEq=INeq; nr INeq=IEq

cLog :: FiniteBits a => a -> Maybe Int64
cLog n | popCount n == 1 = Just (fromIntegral$countTrailingZeros n) | otherwise = Nothing

cToIRM :: CS -> IRM [Stmt]
cToIRM (C.MT t e)          = pure [IR.MT (ctemp t) (irE e)]
cToIRM (C.MX t e)          = pure [IR.MX (fx t) (irX e)]
cToIRM (C.Ma l t (C.ConstI rnkI) n sz) | Just s <- cLog sz = let t'=ctemp t in pure [IR.Ma (al l) t' (IR.IB IAsl (irE n) (IR.ConstI s)+IR.ConstI (8+8*rnkI)), IR.Wr (AP t' Nothing (Just$al l)) (IR.ConstI rnkI)]
cToIRM (C.Ma l t rnk n sz) | Just s <- cLog sz = let t'=ctemp t in pure [IR.Ma (al l) t' (IR.IB IAsl (irE rnk+irE n) (IR.ConstI s)+8), IR.Wr (AP t' Nothing (Just$al l)) (irE rnk)]
cToIRM (C.MaΠ l t sz)      = pure [IR.Ma (al l) (ctemp t) (irE sz)]
cToIRM (C.Wr a e)          = pure [IR.Wr (irAt a) (irE e)]
cToIRM (C.WrF a x)         = pure [IR.WrF (irAt a) (irX x)]
cToIRM (For t el rel eu s) = do
    l <- nextL; eL <- nextL
    irs <- foldMapM cToIRM s
    pure $ IR.MT t' (irE el):MJ (IR.IRel (nr rel) (Reg t') (irE eu)) eL:L l:irs++[tick t', MJ (IR.IRel rel (Reg t') (irE eu)) l, L eL]
  where
    t'=ctemp t
cToIRM (While t rel eb s) = do
    l <- nextL; eL <- nextL
    s' <- foldMapM cToIRM s
    pure $ MJ (IR.IRel (nr rel) (Reg t') (irE eb)) eL:L l:s'++[MJ (IR.IRel rel (Reg t') (irE eb)) l, L eL]
  where t'=ctemp t
cToIRM (C.RA i) = pure [IR.RA (al i)]
cToIRM (CpyD a0 a1 e) = pure [Cpy (irAt a0) (irAt a1) (irE e)]
cToIRM (CpyE a0 a1 e 8) = pure [Cpy (irAt a0) (irAt a1) (irE e)]
cToIRM (CpyE a0 a1 e sz) | (s,0) <- sz `quotRem` 8 = pure [Cpy (irAt a0) (irAt a1) (IB ITimes (irE e) (IR.ConstI s))]
cToIRM (C.Sa t e) = pure [IR.Sa (ctemp t) (irE e)]
cToIRM (C.Pop e) = pure [IR.Pop (irE e)]
cToIRM (Ifn't p s) = do
    l <- nextL
    s' <- foldMapM cToIRM s
    pure $ MJ (irp p) l:s'++[L l]
cToIRM (If p s0 s1) = do
    l <- nextL; l' <- nextL
    s0' <- foldMapM cToIRM s0; s1' <- foldMapM cToIRM s1
    pure $ MJ (irp p) l:s1'++J l':L l:s0'++[L l']
cToIRM (C.Cmov p t e) = pure [IR.Cmov (irp p) (ctemp t) (irE e)]
cToIRM (C.Fcmov p t e) = pure [IR.Fcmov (irp p) (fx t) (irX e)]
cToIRM (C.Cset p t) = pure [IR.Cset (ctemp t) (irp p)]
cToIRM (SZ td t rnk l) = do
    i <- nextI
    foldMapM cToIRM
        [C.MT td (C.EAt (ADim t 0 l)), For i 1 ILt rnk [C.MT td (Tmp td*C.EAt (ADim t (Tmp i) l))]]

irAt :: ArrAcc -> AE
irAt (ARnk t l)                                                = AP (ctemp t) Nothing (coerce l)
irAt (ADim t (C.ConstI n) l)                                   = AP (ctemp t) (Just$IR.ConstI (8+8*n)) (coerce l)
irAt (ADim t e l)                                              = AP (ctemp t) (Just$IR.IB IAsl (irE e) 3+8) (coerce l)
irAt (AElem t (C.ConstI 1) (C.ConstI 0) l 8)                   = AP (ctemp t) (Just 16) (coerce l)
irAt (AElem t (C.ConstI rnkI) (Bin IPlus e (C.ConstI n)) l 8)  = AP (ctemp t) (Just$IR.IB IAsl (irE e) 3+IR.ConstI (8+8*(rnkI+n))) (coerce l)
irAt (AElem t (C.ConstI rnkI) (Bin IMinus e (C.ConstI n)) l 8) = AP (ctemp t) (Just$IR.IB IAsl (irE e) 3+IR.ConstI (8+8*(rnkI-n))) (coerce l)
irAt (AElem t (C.ConstI rnkI) e l sz) | Just s <- cLog sz      = AP (ctemp t) (Just$IR.IB IAsl (irE e) (IR.ConstI s)+IR.ConstI (8+8*rnkI)) (coerce l)
irAt (AElem t rnk e l 8)                                       = AP (ctemp t) (Just$IR.IB IAsl (irE rnk+irE e) 3+8) (coerce l)
irAt (Raw t (C.ConstI 0) l _)                                  = AP (ctemp t) Nothing (coerce l)
irAt (Raw t (C.ConstI i) l sz)                                 = AP (ctemp t) (Just (IR.ConstI$i*sz)) (coerce l)
irAt (Raw t o l 8)                                             = AP (ctemp t) (Just$IR.IB IAsl (irE o) 3) (coerce l)
irAt (Raw t o l 1)                                             = AP (ctemp t) (Just$irE o) (coerce l)
irAt (At dt s ix l sz) | Just sϵ <- cLog sz =
    let offs=foldl1 (IB IPlus) $ zipWith (\d i -> irE i*irE d) s ix
    in AP (ctemp dt) (Just$IR.IB IAsl offs (IR.ConstI sϵ)) (coerce l)

irE :: CE -> Exp
irE (Tmp t)               = Reg (ctemp t)
irE (C.EAt a)             = IR.EAt (irAt a)
irE (C.ConstI i)          = IR.ConstI i
irE (Bin op e0 e1)        = IB op (irE e0) (irE e1)
irE (C.LA i)              = IR.LA i
irE (DP t (C.ConstI rnk)) = Reg (ctemp t)+IR.ConstI (8*(1+rnk))
irE (DP t e)              = Reg (ctemp t)+IB IAsl (irE e) 3+8
irE (CFloor e)            = IRFloor (irX e)

irp :: PE -> Exp
irp (C.IRel rel e0 e1) = IR.IRel rel (irE e0) (irE e1)
irp (C.FRel rel x0 x1) = IR.FRel rel (irX x0) (irX x1)
irp (C.IUn p e)        = IR.IU p (irE e)
irp (C.Is t)           = IR.Is (ctemp t)

irX :: CFE -> FExp
irX (C.ConstF x)    = IR.ConstF x
irX (FTmp t)        = FReg (fx t)
irX (C.FAt a)       = IR.FAt (irAt a)
irX (FBin op x0 x1) = FB op (irX x0) (irX x1)
irX (IE e)          = FConv (irE e)
irX (FUn f e)       = FU f (irX e)

foldMapM f = foldM (\x y -> (x `mappend`) <$> f y) mempty
