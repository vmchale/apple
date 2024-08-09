module Class.E ( E (..) ) where

import qualified Asm.Aarch64 as AArch64
import qualified Asm.X86     as X86

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

instance E AArch64.AReg where
    toInt AArch64.X0  = 0
    toInt AArch64.X1  = 1
    toInt AArch64.X2  = 2
    toInt AArch64.X3  = 3
    toInt AArch64.X4  = 4
    toInt AArch64.X5  = 5
    toInt AArch64.X6  = 6
    toInt AArch64.X7  = 7
    toInt AArch64.X8  = -1
    toInt AArch64.X9  = -2
    toInt AArch64.X10 = -3
    toInt AArch64.X11 = -4
    toInt AArch64.X12 = -5
    toInt AArch64.X13 = -6
    toInt AArch64.X14 = -7
    toInt AArch64.X15 = -8
    toInt AArch64.X16 = -9
    toInt AArch64.X17 = -10
    toInt AArch64.X18 = -11
    toInt AArch64.X19 = -12
    toInt AArch64.X20 = -13
    toInt AArch64.X21 = -14
    toInt AArch64.X22 = -15
    toInt AArch64.X23 = -16
    toInt AArch64.X24 = -17
    toInt AArch64.X25 = -18
    toInt AArch64.X26 = -19
    toInt AArch64.X27 = -20
    toInt AArch64.X28 = -21
    toInt AArch64.X29 = 18
    toInt AArch64.X30 = 8
    toInt AArch64.SP  = 9

instance E AArch64.FAReg where
    toInt AArch64.D0  = 10
    toInt AArch64.D1  = 11
    toInt AArch64.D2  = 12
    toInt AArch64.D3  = 13
    toInt AArch64.D4  = 14
    toInt AArch64.D5  = 15
    toInt AArch64.D6  = 16
    toInt AArch64.D7  = 17
    toInt AArch64.D8  = -23
    toInt AArch64.D9  = -24
    toInt AArch64.D10 = -25
    toInt AArch64.D11 = -26
    toInt AArch64.D12 = -27
    toInt AArch64.D13 = -28
    toInt AArch64.D14 = -29
    toInt AArch64.D15 = -30
    toInt AArch64.D16 = -31
    toInt AArch64.D17 = -32
    toInt AArch64.D18 = -33
    toInt AArch64.D19 = -34
    toInt AArch64.D20 = -35
    toInt AArch64.D21 = -36
    toInt AArch64.D22 = -37
    toInt AArch64.D23 = -38
    toInt AArch64.D24 = -39
    toInt AArch64.D25 = -40
    toInt AArch64.D26 = -41
    toInt AArch64.D27 = -42
    toInt AArch64.D28 = -43
    toInt AArch64.D29 = -44
    toInt AArch64.D30 = -45
    toInt AArch64.D31 = -46

instance E AArch64.AbsReg where
    toInt = AArch64.toInt

instance E AArch64.FAbsReg where
    toInt = AArch64.fToInt

instance E freg => E (AArch64.V2Reg freg) where
    toInt = toInt . AArch64.simd2
