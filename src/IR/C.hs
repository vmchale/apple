module IR.C ( ctemp, cToIR ) where

import           Bits
import           C
import           Control.Monad                    (foldM)
import           Control.Monad.Trans.State.Strict (State, runState, state)
import           IR
import           Op

type IRM = State WSt

nextI :: IRM C.Temp
nextI = C.ITemp <$> state (\(WSt ls t) -> (t, WSt ls (t+1)))

nextL :: IRM IR.Label
nextL = state (\(WSt l ts) -> (l, WSt (l+1) ts))

cbtemp :: C.BTemp -> IR.Temp
cbtemp (C.BTemp i) = IR.ITemp i; cbtemp C.CBRet = IR.CRet

ctemp :: C.Temp -> IR.Temp
ctemp (C.ATemp i) = IR.ATemp i; ctemp (C.ITemp i) = IR.ITemp i
ctemp C.C0 = IR.C0; ctemp C.C1 = IR.C1; ctemp C.C2 = IR.C2
ctemp C.C3 = IR.C3; ctemp C.C4 = IR.C4; ctemp C.C5 = IR.C5
ctemp C.CRet = IR.CRet

f2x :: C.F2Temp -> IR.F2
f2x (C.F2Temp i) = IR.F2Temp i

fx :: C.FTemp -> IR.FTemp
fx (C.FTemp i) = IR.FTemp i
fx FRet0 = FRet; fx C.FRet1 = IR.FRet1
fx C.F0 = IR.F0; fx C.F1 = IR.F1; fx C.F2 = IR.F2
fx C.F3 = IR.F3; fx C.F4 = IR.F4; fx C.F5 = IR.F5

cToIR :: LSt -> [CS a] -> ([Stmt], WSt)
cToIR (LSt ls ts) cs = runState (foldMapM cToIRM cs) (WSt ls ts)

tick reg = IR.MT reg (Reg reg+1); untick r=IR.MT r (Reg r-1)

nr IGeq=ILt; nr IGt=ILeq; nr ILt=IGeq; nr ILeq=IGt; nr IEq=INeq; nr INeq=IEq

cToIRM :: CS a -> IRM [Stmt]
cToIRM (G _ l retL)          = pure [IR.C l, IR.L retL]
-- FIXME: put this at the end so it doesn't have to be skipped
cToIRM (Def _ l cs)          = do
    endL <- nextL
    irs <- foldMapM cToIRM cs
    pure (J endL:L l:irs++[IR.R l, L endL])
cToIRM (C.PlProd _ t (e:es)) = let t' = ctemp t in pure (IR.MT t' (irE e):[IR.MT t' (IR.Reg t'*irE eϵ) | eϵ <- es])
cToIRM (C.MT _ t e)        = pure [IR.MT (ctemp t) (irE e)]
cToIRM (C.MX _ t e)        = pure [IR.MX (fx t) (irX e)]
cToIRM (C.MX2 _ t e)       = pure [IR.MX2 (f2x t) (irX2 e)]
cToIRM (C.MB _ t e)        = pure [IR.MT (cbtemp t) (irp e)]
cToIRM (Rnd _ t)           = pure [IR.IRnd (ctemp t)]
cToIRM (C.FRnd _ t)        = pure [IR.FRnd (fx t)]
cToIRM (C.Ma _ l t (C.ConstI rnkI) n sz) | Just s <- cLog sz = let t'=ctemp t in pure [IR.Ma l t' (IR.IB IAsl (irE n) (IR.ConstI s)+IR.ConstI (8+8*rnkI)), IR.Wr (AP t' Nothing (Just l)) (IR.ConstI rnkI)]
cToIRM (C.Ma _ l t rnk n sz) | Just s <- cLog sz = let t'=ctemp t in pure [IR.Ma l t' (IR.IB IAsl (irE n) (IR.ConstI s)+IR.IB IAsl (irE rnk) 3+8), IR.Wr (AP t' Nothing (Just l)) (irE rnk)]
cToIRM (C.Ma _ l t rnk n sz) = let t'=ctemp t in pure [IR.Ma l t' (irE n*IR.ConstI sz+IR.IB IAsl (irE rnk) 3+8), IR.Wr (AP t' Nothing (Just l)) (irE rnk)]
cToIRM (C.MaΠ _ l t sz)      = pure [IR.Ma l (ctemp t) (IR.ConstI sz)]
cToIRM (C.Free t)            = pure [IR.Free (ctemp t)]
cToIRM (C.Wr _ a e)          = pure [IR.Wr (irAt a) (irE e)]
cToIRM (C.WrF _ a x)         = pure [IR.WrF (irAt a) (irX x)]
cToIRM (C.WrP _ a b)         = pure [IR.WrB (irAt a) (irp b)]
cToIRM (Rof _ t ec s)        = do
    l <- nextL; eL <- nextL
    irs <- foldMapM cToIRM s
    pure $ IR.MT t' (irE ec):MJ (IR.IRel IEq (Reg t') 0) eL:L l:irs++[untick t', MJ (IR.IRel IGeq (Reg t') 0) l, L eL]
  where
    t'=ctemp t
cToIRM (Rof1 _ t ec s)        = do
    l <- nextL; eL <- nextL
    irs <- foldMapM cToIRM s
    pure $ IR.MT t' (irE ec):L l:irs++[untick t', MJ (IR.IRel IGeq (Reg t') 0) l, L eL]
  where
    t'=ctemp t
cToIRM (For _ t el rel eu s) = do
    l <- nextL; eL <- nextL
    irs <- foldMapM cToIRM s
    pure $ IR.MT t' (irE el):MJ (IR.IRel (nr rel) (Reg t') (irE eu)) eL:IR.L l:irs++[tick t', MJ (IR.IRel rel (Reg t') (irE eu)) l, L eL]
  where
    t'=ctemp t
cToIRM (F2or _ t el rel eu s s1) = do
    l <- nextL; eL <- nextL
    irs <- foldMapM cToIRM s; ir1 <- foldMapM cToIRM s1
    pure $ IR.MT t' (irE el):MJ (IR.IRel (nr rel) (Reg t') (irE eu)) eL:MJ (IR.IU IEven (irE eu-irE el)) l:ir1++tick t':IR.L l:irs++[IR.MT t' (Reg t'+2), MJ (IR.IRel rel (Reg t') (irE eu)) l, L eL]
  where
    t'=ctemp t
cToIRM (For1 _ t el rel eu s) = do
    l <- nextL
    irs <- foldMapM cToIRM s
    pure $ IR.MT t' (irE el):L l:irs++[tick t', MJ (IR.IRel rel (Reg t') (irE eu)) l]
  where
    t'=ctemp t
cToIRM (While _ t rel eb s) = do
    l <- nextL; eL <- nextL
    s' <- foldMapM cToIRM s
    pure $ MJ (IR.IRel (nr rel) (Reg t') (irE eb)) eL:L l:s'++[MJ (IR.IRel rel (Reg t') (irE eb)) l, L eL]
  where t'=ctemp t
cToIRM (WT _ p s) = do
    l <- nextL; eL <- nextL
    s' <- foldMapM cToIRM s
    pure $ MJ (IR.BU BNeg p') eL:L l:s'++[MJ p' l, L eL]
  where p'=irp p
cToIRM (C.RA _ i) = pure [IR.RA i]
cToIRM (CpyD _ a0 a1 e) = pure [Cpy (irAt a0) (irAt a1) (irE e)]
cToIRM (CpyE _ a0 a1 e 8) = pure [Cpy (irAt a0) (irAt a1) (irE e)]
cToIRM (CpyE _ a0 a1 e sz) | (s,0) <- sz `quotRem` 8 = pure [Cpy (irAt a0) (irAt a1) (irE e*IR.ConstI s)]
cToIRM (CpyE _ a0 a1 e sz) = pure [Cpy1 (irAt a0) (irAt a1) (irE e*IR.ConstI sz)]
cToIRM (C.Sa _ t e) = pure [IR.Sa (ctemp t) (irE e)]
cToIRM (C.Pop _ e) = pure [IR.Pop (irE e)]
cToIRM (C.Sa8 _ t e) = pure [IR.Sa8 (ctemp t) (irE e)]
cToIRM (C.Pop8 _ e) = pure [IR.Pop8 (irE e)]
cToIRM (Ifn't _ p s) = do
    l <- nextL
    s' <- foldMapM cToIRM s
    pure $ MJ (irp p) l:s'++[L l]
cToIRM (If _ p s0 s1) = do
    l <- nextL; l' <- nextL
    s0' <- foldMapM cToIRM s0; s1' <- foldMapM cToIRM s1
    pure $ MJ (irp p) l:s1'++J l':L l:s0'++[L l']
cToIRM (C.Cmov _ p t e) = pure [IR.Cmov (irp p) (ctemp t) (irE e)]
cToIRM (C.Fcmov _ p t e) = pure [IR.Fcmov (irp p) (fx t) (irX e)]
cToIRM (C.Cset _ p t) = pure [IR.Cset (cbtemp t) (irp p)]
cToIRM (SZ _ td t rnk l) = do
    i <- nextI
    foldMapM cToIRM
        [td =: C.EAt (ADim t 0 l), For () i 1 ILt rnk [td =: (Tmp td*C.EAt (ADim t (Tmp i) l))]]

irAt :: ArrAcc -> AE
irAt (ARnk t l)                                                = AP (ctemp t) Nothing l
irAt (ADim t (C.ConstI n) l)                                   = AP (ctemp t) (Just$IR.ConstI (8+8*n)) l
irAt (ADim t e l)                                              = AP (ctemp t) (Just$IR.IB IAsl (irE e) 3+8) l
irAt (AElem t (C.ConstI 1) (C.ConstI 0) l _)                   = AP (ctemp t) (Just 16) l
irAt (AElem t (C.ConstI rnkI) (Bin IPlus e (C.ConstI n)) l 8)  = AP (ctemp t) (Just$IR.IB IAsl (irE e) 3+IR.ConstI (8+8*(rnkI+n))) l
irAt (AElem t (C.ConstI rnkI) (Bin IMinus e (C.ConstI n)) l 8) = AP (ctemp t) (Just$IR.IB IAsl (irE e) 3+IR.ConstI (8+8*(rnkI-n))) l
irAt (AElem t (C.ConstI rnkI) e l sz) | Just s <- cLog sz      = AP (ctemp t) (Just$IR.IB IAsl (irE e) (IR.ConstI s)+IR.ConstI (8+8*rnkI)) l
                                      | otherwise              = AP (ctemp t) (Just$(irE e*IR.ConstI sz)+IR.ConstI (8+8*rnkI)) l
irAt (AElem t rnk e l 8)                                       = AP (ctemp t) (Just$IR.IB IAsl (irE rnk+irE e) 3+8) l
irAt (AElem t rnk e l sz) | Just s <- cLog sz                  = AP (ctemp t) (Just$IR.IB IAsl (irE rnk) 3+IR.IB IAsl (irE e) (IR.ConstI s)+8) l
                          | otherwise                          = AP (ctemp t) (Just$IR.IB IAsl (irE rnk) 3+(irE e*IR.ConstI sz)+8) l
irAt (TupM t l)                                                = AP (ctemp t) Nothing l
irAt (Raw t (C.ConstI 0) l _)                                  = AP (ctemp t) Nothing l
irAt (Raw t (C.ConstI i) l sz)                                 = AP (ctemp t) (Just (IR.ConstI$i*sz)) l
irAt (Raw t o l 1)                                             = AP (ctemp t) (Just$irE o) l
irAt (Raw t o l sz) | Just n <- cLog sz                        = AP (ctemp t) (Just$IR.IB IAsl (irE o) (IR.ConstI n)) l
                    | otherwise                                = AP (ctemp t) (Just$irE o*IR.ConstI sz) l
irAt (At dt s ix l sz) | Just sϵ <- cLog sz =
    let offs=foldl1 (IB IPlus) $ zipWith (\d i -> sm (irE i) (irE d)) s ix
    in AP (ctemp dt) (Just$IR.IB IAsl offs (IR.ConstI sϵ)) l
  where
    sm i (IR.ConstI 1) = i
    sm i (IR.ConstI d) | Just sϵ <- cLog d = IR.IB IAsl i (IR.ConstI sϵ)
    sm d i = i*d

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
irp (C.BU op e)        = IR.BU op (irp e)
irp (C.Is t)           = IR.Is (cbtemp t)
irp (C.PAt a)          = IR.BAt (irAt a)
irp (C.BConst True)    = IR.ConstI 1
irp (C.BConst False)   = IR.ConstI 0
irp (C.Boo op e0 e1)   = IB (BI op) (irp e0) (irp e1)

irX2 :: C.F2E -> IR.F2E
irX2 (C.ConstF x)    = IR.ConstF x
irX2 (C.FAt a)       = IR.FAt (irAt a)
irX2 (FTmp t)        = FReg (f2x t)
irX2 (FBin op x0 x1) = FB op (irX2 x0) (irX2 x1)

irX :: F1E -> FE
irX (C.ConstF x)    = IR.ConstF x
irX (FTmp t)        = FReg (fx t)
irX (C.FAt a)       = IR.FAt (irAt a)
irX (FBin op x0 x1) = FB op (irX x0) (irX x1)
irX (IE e)          = FConv (irE e)
irX (FUn f e)       = FU f (irX e)

foldMapM f = foldM (\x y -> (x `mappend`) <$> f y) mempty
