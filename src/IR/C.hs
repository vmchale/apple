module IR.C ( ctemp
            , cToIR
            ) where

import           C
import           Control.Monad              (foldM)
import           Control.Monad.State.Strict (State, runState, state)
import           IR
import           Op

type IRM = State WSt

nextL :: IRM IR.Label
nextL = state (\(WSt (l:ls) ts) -> (l, WSt ls ts))

nextI :: IRM Int
nextI = state (\(WSt ls (t:ts)) -> (t, WSt ls ts))

newITemp, newFTemp :: IRM IR.Temp
newITemp = IR.ITemp<$>nextI
newFTemp = IR.FTemp<$>nextI

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

cToIRM :: CS -> IRM [Stmt]
cToIRM (C.MT t e)          = pure [IR.MT (ctemp t) (irE e)]
cToIRM (C.MX t e)          = (:[]) . IR.MX (fx t) <$> irX e
cToIRM (C.Ma l t (C.ConstI rnkI) n 8) = let t'=ctemp t in pure [IR.Ma l t' (IR.IB IAsl (irE n) 3+IR.ConstI (8+8*rnkI)), IR.Wr (AP t' Nothing (Just l)) (IR.ConstI rnkI)]
cToIRM (C.Ma l t rnk n 8)  = let t'=ctemp t in pure [IR.Ma l t' (IR.IB IAsl (irE rnk+irE n) 3+8), IR.Wr (AP t' Nothing (Just l)) (irE rnk)]
cToIRM (C.Wr a e)          = pure [IR.Wr (irAt a) (irE e)]
cToIRM (C.WrF a x)         = (:[]) . IR.WrF (irAt a) <$> irX x
cToIRM (For t el rel eu s) = do
    l <- nextL; eL <- nextL
    irs <- foldMapM cToIRM s
    pure $ IR.MT t' (irE el):MJ (IRel (nr rel) (Reg t') (irE eu)) eL:L l:irs++[tick t', MJ (IRel rel (Reg t') (irE eu)) l, L eL]
  where
    t'=ctemp t
cToIRM (C.RA i) = pure [IR.RA i]
cToIRM (CpyE a0 a1 e 8) = pure [Cpy (irAt a0) (irAt a1) (irE e)]

irAt :: ArrAcc -> AE
irAt (ADim t (C.ConstI 0) l)                 = AP (ctemp t) (Just 8) l
irAt (ADim t e l)                            = AP (ctemp t) (Just$IR.IB IAsl (irE e) 3+8) l
irAt (AElem t (C.ConstI 1) (C.ConstI 0) l 8) = AP (ctemp t) (Just 16) l
irAt (AElem t (C.ConstI rnkI) e l 8)         = AP (ctemp t) (Just$IR.IB IAsl (irE e) 3+IR.ConstI (8+8*rnkI)) l
irAt (AElem t rnk e l 8)                     = AP (ctemp t) (Just$IR.IB IAsl (irE rnk+irE e) 3+8) l

irE :: CE -> Exp
irE (Tmp t)        = Reg (ctemp t)
irE (C.EAt a)      = IR.EAt (irAt a)
irE (C.ConstI i)   = IR.ConstI i
irE (Bin op e0 e1) = IB op (irE e0) (irE e1)

irX :: CFE -> IRM FExp
irX (C.ConstF x)    = pure $ IR.ConstF x
irX (FTmp t)        = pure $ FReg (fx t)
irX (C.FAt a)       = pure $ IR.FAt (irAt a)
irX (FBin op x0 x1) = FB op <$> irX x0 <*> irX x1

foldMapM f = foldM (\x y -> (x `mappend`) <$> f y) mempty
