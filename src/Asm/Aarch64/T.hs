module Asm.Aarch64.T ( irToAarch64 ) where

import           Asm.Aarch64
import           Asm.M
import           Control.Monad.State.Strict (runState)
import           Data.Bifunctor             (second)
import           Data.Tuple                 (swap)
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

irToAarch64 :: IR.WSt -> [IR.Stmt] -> (Int, [AArch64 AbsReg FAbsReg ()])
irToAarch64 st = swap . second (head.IR.wtemps) . flip runState st . foldMapA ir

ir :: IR.Stmt -> WM [AArch64 AbsReg FAbsReg ()]
ir (IR.L l)    = pure [Label () l]
ir (IR.J l)    = pure [B () l]
ir (IR.MX t e) = feval e t
ir (IR.MT t e) = eval e t
ir s           = error (show s)

feval :: IR.FExp -> IR.Temp -> WM [AArch64 AbsReg FAbsReg ()]
feval (IR.FReg tS) tD = pure [FMovXX () (fabsReg tD) (fabsReg tS)]
feval (IR.ConstF d) t = pure [FMovXC () (fabsReg t) d]
feval e _             = error (show e)

eval :: IR.Exp -> IR.Temp -> WM [AArch64 AbsReg FAbsReg ()]
eval (IR.Reg tS) tD = pure [MovRR () (absReg tD) (absReg tS)]
eval e _            = error (show e)
