module Asm.Aarch64.T ( irToAarch64 ) where

import           Asm.Aarch64
import           Asm.M
import           Control.Monad.State.Strict (runState)
import           Data.Bifunctor             (second)
import           Data.Tuple                 (swap)
import           IR

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

irToAarch64 :: IR.WSt -> [IR.Stmt] -> (Int, [AArch64 AbsReg FAbsReg ()])
irToAarch64 st = swap . second (head.IR.wtemps) . flip runState st . foldMapA ir

ir :: IR.Stmt -> WM [AArch64 AbsReg FAbsReg ()]
ir (IR.L l) = pure [Label () l]
ir (IR.J l) = pure [B () l]
