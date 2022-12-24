module Class.E ( E (..) ) where

import qualified Asm.X86 as X86

class E a where
    toInt :: a -> Int

instance E X86.X86Reg where
    toInt X86.Rdi = 0
    toInt X86.Rsi = 1
    toInt X86.Rdx = 2
    toInt X86.Rcx = 3
    toInt X86.R8  = 4
    toInt X86.R9  = 5
    toInt X86.Rax = 6
    toInt X86.Rsp = 7
    toInt X86.R10 = -1
    toInt X86.R11 = -2
    toInt X86.R12 = -3
    toInt X86.R13 = -4
    toInt X86.R14 = -13
    toInt X86.R15 = -14
    toInt X86.Rbx = -15
    toInt X86.Rbp = -16

instance E X86.FX86Reg where
    toInt X86.XMM0  = 8
    toInt X86.XMM1  = 9
    toInt X86.XMM2  = 10
    toInt X86.XMM3  = 11
    toInt X86.XMM4  = 12
    toInt X86.XMM5  = 13
    toInt X86.XMM6  = 14
    toInt X86.XMM7  = 15
    toInt X86.XMM8  = -5
    toInt X86.XMM9  = -6
    toInt X86.XMM10 = -7
    toInt X86.XMM11 = -8
    toInt X86.XMM12 = -9
    toInt X86.XMM13 = -10
    toInt X86.XMM14 = -11
    toInt X86.XMM15 = -12

instance E X86.AbsReg where
    toInt = X86.toInt

instance E X86.FAbsReg where
    toInt = X86.fToInt
