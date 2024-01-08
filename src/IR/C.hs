module IR.C ( ctemp
            , cToIR
            ) where

import           C
import           Control.Monad              (foldM)
import           Control.Monad.State.Strict (State, runState)
import           IR

type IRM = State WSt

ctemp :: C.Temp -> IR.Temp
ctemp (C.ATemp i) = IR.ATemp i
ctemp (C.ITemp i) = IR.ITemp i
ctemp C.C0        = IR.C0
ctemp C.C1        = IR.C1
ctemp C.C2        = IR.C2
ctemp C.C3        = IR.C3
ctemp C.C4        = IR.C4
ctemp C.C5        = IR.C5

fx :: FTemp -> IR.Temp
fx (C.FTemp i) = IR.FTemp i

cToIR :: LSt -> [CS] -> ([Stmt], WSt)
cToIR (LSt ls ts) cs = runState (foldMapM cToIRM cs) (WSt ls ts)

cToIRM :: CS -> IRM [Stmt]
cToIRM (C.MT t e)         = pure [IR.MT (ctemp t) (irE e)]
cToIRM (C.MX t e)         = pure [IR.MX (fx t) (irX e)]
cToIRM (C.Ma l t rnk n 8) = pure [IR.Ma l (ctemp t) (IR.IB IR.IAsl (irE rnk+irE n) 3)]
cToIRM (C.Wr a e)         = pure [IR.Wr (irAt a) (irE e)]
cToIRM c                  = error (show c)

irAt :: ArrAcc -> AE
irAt (ADim t rnk e l) = AP (ctemp t) (Just$IR.IB IR.IAsl (irE rnk+irE e) 3) l

irE :: CE -> Exp
irE (Tmp t) = Reg (ctemp t)
irE e       = error (show e)

irX :: CFE -> FExp
irX x = error (show x)

foldMapM f = foldM (\x y -> (x `mappend`) <$> f y) mempty
