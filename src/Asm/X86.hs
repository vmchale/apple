{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Asm.X86 ( X86 (..)
               , AbsReg (..), FAbsReg (..)
               , X86Reg (..), FX86Reg (..)
               , Addr (..)
               , ST (..)
               , Scale (..)
               , Pred (..)
               , RoundMode (..)
               , Label
               , CFunc (..)
               , prettyDebugX86
               , toInt, fToInt
               , imm8, simd2
               , roundMode
               , mapR, mapFR
               , fR, fF
               , hasMa
               ) where

import           Asm.M
import           Control.DeepSeq   (NFData (..))
import           Data.Copointed
import           Data.Int          (Int32, Int64, Int8)
import           Data.Word         (Word8)
import           GHC.Generics      (Generic)
import           Prettyprinter     (Doc, Pretty (..), brackets, colon, (<+>))
import           Prettyprinter.Ext
import           Q

data X86Reg = Rcx | Rdx | Rsi | Rdi | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 | Rbx | Rax | Rbp | Rsp
            deriving (Eq, Ord, Enum, Generic)

data FX86Reg = XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7 | XMM8 | XMM9 | XMM10 | XMM11 | XMM12 | XMM13 | XMM14 | XMM15 | XMM0
             deriving (Eq, Ord, Enum, Generic)

data F2X86 = YMM1 | YMM2 | YMM3 | YMM4 | YMM5 | YMM6 | YMM7 | YMM8 | YMM9 | YMM10 | YMM11 | YMM12 | YMM13 | YMM14 | YMM15 | YMM0
           deriving (Eq, Ord, Enum, Generic)

instance NFData X86Reg where
instance NFData FX86Reg where
instance NFData F2X86 where

simd2 :: FX86Reg -> F2X86
simd2 = toEnum.fromEnum

instance Pretty X86Reg where
    pretty Rax = "rax"
    pretty Rbx = "rbx"
    pretty Rcx = "rcx"
    pretty Rdx = "rdx"
    pretty Rsi = "rsi"
    pretty Rdi = "rdi"
    pretty R8  = "r8"
    pretty R9  = "r9"
    pretty R10 = "r10"
    pretty R11 = "r11"
    pretty R12 = "r12"
    pretty R13 = "r13"
    pretty R14 = "r14"
    pretty R15 = "r15"
    pretty Rsp = "rsp"
    pretty Rbp = "rbp"

instance Pretty FX86Reg where
    pretty XMM0  = "xmm0"
    pretty XMM1  = "xmm1"
    pretty XMM2  = "xmm2"
    pretty XMM3  = "xmm3"
    pretty XMM4  = "xmm4"
    pretty XMM5  = "xmm5"
    pretty XMM6  = "xmm6"
    pretty XMM7  = "xmm7"
    pretty XMM8  = "xmm8"
    pretty XMM9  = "xmm9"
    pretty XMM10 = "xmm10"
    pretty XMM11 = "xmm11"
    pretty XMM12 = "xmm12"
    pretty XMM13 = "xmm13"
    pretty XMM14 = "xmm14"
    pretty XMM15 = "xmm15"

instance Pretty F2X86 where
    pretty YMM0  = "ymm0"
    pretty YMM1  = "ymm1"
    pretty YMM2  = "ymm2"
    pretty YMM3  = "ymm3"
    pretty YMM4  = "ymm4"
    pretty YMM5  = "ymm5"
    pretty YMM6  = "ymm6"
    pretty YMM7  = "ymm7"
    pretty YMM8  = "ymm8"
    pretty YMM9  = "ymm9"
    pretty YMM10 = "ymm10"
    pretty YMM11 = "ymm11"
    pretty YMM12 = "ymm12"
    pretty YMM13 = "ymm13"
    pretty YMM14 = "ymm14"
    pretty YMM15 = "ymm15"

instance Show X86Reg where show = show . pretty
instance Show FX86Reg where show = show . pretty
instance Show F2X86 where show = show . pretty

data AbsReg = IReg !Int
            | CArg0 | CArg1 | CArg2 | CArg3 | CArg4 | CArg5
            | CRet
            | SP | BP
            | Quot | Rem
            deriving (Eq, Ord)

data FAbsReg = FReg !Int
             | FArg0 | FArg1 | FArg2 | FArg3 | FArg4 | FArg5 | FArg6 | FArg7
             | FRet0 | FRet1
             deriving (Eq, Ord)

data X2Abs = F2Reg !Int deriving (Eq, Ord)

instance Pretty AbsReg where
    pretty CArg0    = "rdi"
    pretty CArg1    = "rsi"
    pretty CArg2    = "rdx"
    pretty CArg3    = "rcx"
    pretty CArg4    = "r8"
    pretty CArg5    = "r9"
    pretty CRet     = "rax"
    pretty SP       = "rsp"
    pretty Quot     = "rax"
    pretty Rem      = "rdx"
    pretty (IReg i) = "^r" <> pretty i
    pretty BP       = "rbp"

instance Pretty FAbsReg where
    pretty FArg0    = "xmm0"
    pretty FArg1    = "xmm1"
    pretty FArg2    = "xmm2"
    pretty FArg3    = "xmm3"
    pretty FArg4    = "xmm4"
    pretty FArg5    = "xmm5"
    pretty FArg6    = "xmm6"
    pretty FArg7    = "xmm7"
    pretty FRet0    = "xmm0"
    pretty FRet1    = "xmm1"
    pretty (FReg i) = "^xmm" <> pretty i

instance Pretty X2Abs where
    pretty (F2Reg i) = "ymm" <> pretty i

toInt :: AbsReg -> Int
toInt CArg0    = 0
toInt CArg1    = 1
toInt CArg2    = 2
toInt CArg3    = 3
toInt CArg4    = 4
toInt CArg5    = 5
toInt CRet     = 6
toInt SP       = 7
toInt Quot     = 6 -- FIXME: I think this is wrong, graph approach would precolor both...?
toInt Rem      = 2
toInt (IReg i) = 16+i
toInt BP       = -16

fToInt :: FAbsReg -> Int
fToInt FArg0    = 8
fToInt FArg1    = 9
fToInt FArg2    = 10
fToInt FArg3    = 11
fToInt FArg4    = 12
fToInt FArg5    = 13
fToInt FArg6    = 14
fToInt FArg7    = 15
fToInt FRet0    = 8 -- xmm0
fToInt FRet1    = 9 -- xmm1
fToInt (FReg i) = 16+i

newtype ST = ST Int8 deriving (NFData)

instance Pretty ST where
    pretty (ST i) = "st" <> pretty i

data RoundMode = RNearest | RDown | RUp | RZero deriving Generic

instance NFData RoundMode where

-- 3 bits, stored as Word8 for ease of manipulation
roundMode :: RoundMode -> Word8
roundMode RNearest = 0x0
roundMode RDown    = 0x1
roundMode RUp      = 0x2
roundMode RZero    = 0x3

instance Pretty RoundMode where
    pretty = pretty . roundMode

data Scale = One | Two | Four | Eight deriving (Eq, Generic)

instance Pretty Scale where
    pretty One   = "1"
    pretty Two   = "2"
    pretty Four  = "4"
    pretty Eight = "8"

data Pred = Eqoq | Ltos | Leos | Unordq | Nequq | Nltus | Nleus | Ordq deriving (Generic)

instance Pretty Pred where
    pretty Eqoq   = "EQ_OQ"
    pretty Ltos   = "LT_OS"
    pretty Leos   = "LE_OS"
    pretty Unordq = "UNORD_Q"
    pretty Nequq  = "NEQ_UQ"
    pretty Nltus  = "NLT_US"
    pretty Nleus  = "NLE_US"
    pretty Ordq   = "ORD_Q"

hasMa :: [X86 reg freg a] -> Bool
hasMa = any g where g Call{} = True; g _ = False

-- https://www.felixcloutier.com/x86/cmppd
imm8 :: Pred -> Int8
imm8 Eqoq   = 0
imm8 Ltos   = 1
imm8 Leos   = 2
imm8 Unordq = 3
imm8 Nequq  = 4
imm8 Nltus  = 5
imm8 Nleus  = 6
imm8 Ordq   = 7

instance NFData Pred where

data Addr reg = R reg | RC reg Int8 | RC32 reg Int32 | RS reg Scale reg | RSD reg Scale reg Int8 deriving (Eq, Generic, Functor, Foldable)

instance NFData Scale where

instance NFData reg => NFData (Addr reg) where

pix c | c < 0 = pretty c | otherwise = "+" <> pretty c

instance Pretty reg => Pretty (Addr reg) where
    pretty (R r)           = brackets (pretty r)
    pretty (RC r c)        = brackets (pretty r <> pix c)
    pretty (RC32 r c)      = brackets (pretty r <> pix c)
    pretty (RS b One i)    = brackets (pretty b <> "+" <> pretty i)
    pretty (RS b s i)      = brackets (pretty b <> "+" <> pretty s <> "*" <> pretty i)
    pretty (RSD b One i d) = brackets (pretty b <> pretty i <> pix d)
    pretty (RSD b s i d)   = brackets (pretty b <> "+" <> pretty s <> "*" <> pretty i <> pix d)

data X86 reg freg a = Label { ann :: a, label :: Label }
                    | IAddRR { ann :: a, rAdd1, rAdd2 :: reg }
                    | IAddRI { ann :: a, rAdd1 :: reg, rAddI :: Int64 }
                    | ISubRR { ann :: a, rSub1, rSub2 :: reg }
                    | ISubRI { ann :: a, rSub :: reg, rSubI :: Int64 }
                    | IMulRR { ann :: a, rMul1, rMul2 :: reg }
                    | IMulRA { ann :: a, rMul :: reg, aSrc :: Addr reg }
                    | XorRR { ann :: a, rXor1, rXor2 :: reg }
                    | MovRR { ann :: a, rDest, rSrc :: reg }
                    | MovRA { ann :: a, rDest :: reg, aSrc :: Addr reg }
                    | MovAR { ann :: a, aDest :: Addr reg, rSrc :: reg }
                    | MovRL { ann :: a, rDest :: reg, lSrc :: Int }
                    | MovAI32 { ann :: a, aDest :: Addr reg, i32Src :: Int32 }
                    | MovRI { ann :: a, rDest :: reg, iSrc :: Int64 }
                    | MovqXR { ann :: a, fDest :: freg, rSrc :: reg }
                    | MovqXA { ann :: a, fDest :: freg, aSrc :: Addr reg }
                    | MovqAX { ann :: a, aDest :: Addr reg, fSrc :: freg }
                    | MovqRX { ann :: a, rDest :: reg, fSrc :: freg }
                    | Fld { ann :: a, a87 :: Addr reg }
                    | FldS { ann :: a, stIsn :: ST }
                    | Fldl2e { ann :: a }
                    | Fldln2 { ann :: a }
                    | Fld1 { ann :: a }
                    | Fyl2x { ann :: a }
                    | Fsin { ann :: a }
                    | Fcos { ann :: a }
                    | Fstp { ann :: a, a87 :: Addr reg }
                    | F2xm1 { ann :: a }
                    | Fmulp { ann :: a }
                    | Fprem { ann :: a }
                    | Faddp { ann :: a }
                    | Fscale { ann :: a }
                    | Fninit { ann :: a }
                    | Fxch { ann :: a, stIsn :: ST }
                    | J { ann :: a, label :: Label }
                    | Je { ann :: a, jLabel :: Label }
                    | Jne { ann :: a, jLabel :: Label }
                    | Jg { ann :: a, jLabel :: Label }
                    | Jge { ann :: a, jLabel :: Label }
                    | Jl { ann :: a, jLabel :: Label }
                    | Jle { ann :: a, jLabel :: Label }
                    | C { ann :: a, label :: Label }
                    | CmpRR { ann :: a, rCmp, rCmp' :: reg }
                    | CmpRI { ann :: a, rCmp :: reg, cmpI32 :: Int32 }
                    | Vcmppd { ann :: a, fDest, fCmp, fCmp' :: freg, cpred :: Pred }
                    | Test { ann :: a, rCmp, rCmp' :: reg }
                    | TestI { ann :: a, rCmp :: reg, cmpI32 :: Int32 }
                    | Ret { ann :: a } | RetL { ann :: a, label :: Label }
                    | Vdivsd { ann :: a, fDest, fSrc1, fSrc2 :: freg }
                    | Movapd { ann :: a, fDest, fSrc :: freg }
                    | Roundsd { ann :: a, fDest, fSrc :: freg, mode :: RoundMode }
                    | Cvttsd2si { ann :: a, rDest :: reg, fSrc :: freg }
                    | Mulsd { ann :: a, fDest, fSrc :: freg }
                    | Addsd { ann :: a, fDest, fSrc :: freg }
                    | Subsd { ann :: a, fDest, fSrc :: freg }
                    | Divsd { ann :: a, fDest, fSrc :: freg }
                    | Vmulsd { ann :: a, fDest, fSrc1, fSrc2 :: freg }
                    | Vaddsd { ann :: a, fDest, fSrc1, fSrc2 :: freg }
                    | Vsubsd { ann :: a, fDest, fSrc1, fSrc2 :: freg }
                    | VaddsdA { ann :: a, fDest, fSrc :: freg, aSrc :: Addr reg }
                    | Cvtsi2sd { ann :: a, fDest :: freg, rSrc :: reg }
                    | Vfmadd231sd { ann :: a, fDest, fSrc1, fSrc2 :: freg }
                    | Vfmadd213sd { ann :: a, fDest, fSrc1, fSrc2 :: freg }
                    | Vfmsub231sd { ann :: a, fDest, fSrc1, fSrc2 :: freg }
                    | Vfmsub213sd { ann :: a, fDest, fSrc1, fSrc2 :: freg }
                    | Vfmsub132sd { ann :: a, fDest, fSrc1, fSrc2 :: freg }
                    | Vfmnadd231sd { ann :: a, fDest, fSrc1, fSrc2 :: freg }
                    | Vfmadd231sdA { ann :: a, fDest, fSrc :: freg, aSrc :: Addr reg }
                    | Push { ann :: a, rSrc :: reg }
                    | Pop { ann :: a, rDest :: reg }
                    | Call { ann :: a, cfunc :: CFunc }
                    | IDiv { ann :: a, rSrc :: reg }
                    | Sal { ann :: a, rSrc :: reg, iExp :: Int8 }
                    | Sar { ann :: a, rSrc :: reg, iExp :: Int8 }
                    | Sqrtsd { ann :: a, fDest, fSrc :: freg }
                    | Maxsd { ann :: a, fDest, fSrc :: freg }
                    | Vmaxsd { ann :: a, fDest, fSrc1, fSrc2 :: freg }
                    | VmaxsdA { ann :: a, fDest, fSrc :: freg, aSrc :: Addr reg }
                    | Minsd { ann :: a, fDest, fSrc :: freg }
                    | Vminsd { ann :: a, fDest, rSrc1, rSrc2 :: freg }
                    | Not { ann :: a, rSrc :: reg }
                    | And { ann :: a, rDest, rSrc :: reg }
                    | Cmovnle { ann :: a, rDest, rSrc :: reg }
                    | Cmovnl { ann :: a, rDest, rSrc :: reg }
                    | Cmovne { ann :: a, rDest , rSrc :: reg }
                    | Cmove { ann :: a, rDest, rSrc :: reg }
                    | Cmovl { ann :: a, rDest, rSrc :: reg }
                    | Cmovle { ann :: a, rDest, rSrc :: reg }
                    | Rdrand { ann :: a, rDest :: reg }
                    | Neg { ann :: a, rDest :: reg }
                    deriving (Functor, Generic)

instance (NFData a, NFData reg, NFData freg) => NFData (X86 reg freg a) where

instance Copointed (X86 reg freg) where copoint = ann

instance (Pretty reg, Pretty freg) => Pretty (X86 reg freg a) where
    pretty (J _ l)                       = i4 ("jmp" <+> prettyLabel l)
    pretty (Label _ l)                   = prettyLabel l <> colon
    pretty (CmpRR _ r0 r1)               = i4 ("cmp" <+> pretty r0 <> "," <+> pretty r1)
    pretty (MovRR _ r0 r1)               = i4 ("mov" <+> pretty r0 <> "," <+> pretty r1)
    pretty (MovRL _ r l)                 = i4 ("mov" <+> pretty r <> "," <+> "arr_" <> pretty l)
    pretty (MovRI _ r i)                 = i4 ("mov" <+> pretty r <> "," <+> pretty i)
    pretty (XorRR _ r0 r1)               = i4 ("xor" <+> pretty r0 <> "," <+> pretty r1)
    pretty (MovqXR _ r0 r1)              = i4 ("movq" <+> pretty r0 <> "," <+> pretty r1)
    pretty (IAddRR _ r0 r1)              = i4 ("add" <+> pretty r0 <> "," <+> pretty r1)
    pretty (IAddRI _ r i)                = i4 ("add" <+> pretty r <> "," <+> pretty i)
    pretty (ISubRR _ r0 r1)              = i4 ("sub" <+> pretty r0 <> "," <+> pretty r1)
    pretty (ISubRI _ r i)                = i4 ("sub" <+> pretty r <> "," <+> pretty i)
    pretty (IMulRR _ r0 r1)              = i4 ("imul" <+> pretty r0 <> "," <+> pretty r1)
    pretty (IMulRA _ r a)                = i4 ("imul" <+> pretty r <> "," <+> pretty a)
    pretty (Jne _ l)                     = i4 ("jne" <+> prettyLabel l)
    pretty (Jle _ l)                     = i4 ("jle" <+> prettyLabel l)
    pretty (Je _ l)                      = i4 ("je" <+> prettyLabel l)
    pretty (Jge _ l)                     = i4 ("jge" <+> prettyLabel l)
    pretty (Jg _ l)                      = i4 ("jg" <+> prettyLabel l)
    pretty (Jl _ l)                      = i4 ("jl" <+> prettyLabel l)
    pretty Ret{}                         = i4 "ret"
    pretty (Vdivsd _ rD r0 r1)           = i4 ("vdivsd" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (Movapd _ r0 r1)              = i4 ("movapd" <+> pretty r0 <> "," <+> pretty r1)
    pretty (Cvttsd2si _ r0 r1)           = i4 ("cvttsd2si" <+> pretty r0 <> "," <+> pretty r1)
    pretty (Vmulsd _ rD r0 r1)           = i4 ("vmulsd" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (Vaddsd _ rD r0 r1)           = i4 ("vaddsd" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (VaddsdA _ rD r a)            = i4 ("vaddsd" <+> pretty rD <> "," <+> pretty r <> "," <+> pretty a)
    pretty (Vsubsd _ rD r0 r1)           = i4 ("vsubsd" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (Cvtsi2sd _ r0 r1)            = i4 ("cvtsi2sd" <+> pretty r0 <> "," <+> pretty r1)
    pretty (Roundsd _ r0 r1 m)           = i4 ("roundsd" <+> pretty r0 <> "," <+> pretty r1 <> "," <+> pretty m)
    pretty (CmpRI _ r i)                 = i4 ("cmp" <+> pretty r <> "," <+> pretty i)
    pretty (Divsd _ r0 r1)               = i4 ("divsd" <+> pretty r0 <> "," <+> pretty r1)
    pretty (Mulsd _ r0 r1)               = i4 ("mulsd" <+> pretty r0 <> "," <+> pretty r1)
    pretty (Addsd _ r0 r1)               = i4 ("addsd" <+> pretty r0 <> "," <+> pretty r1)
    pretty (Subsd _ r0 r1)               = i4 ("subsd" <+> pretty r0 <> "," <+> pretty r1)
    pretty (MovRA _ r a)                 = i4 ("mov" <+> pretty r <> "," <+> pretty a)
    pretty (MovAR _ a r)                 = i4 ("mov" <+> pretty a <> "," <+> pretty r)
    pretty (MovAI32 _ a i)               = i4 ("mov qword" <+> pretty a <> "," <+> pretty i)
    pretty (MovqXA _ x a)                = i4 ("movq" <+> pretty x <> "," <+> pretty a)
    pretty (MovqAX _ a x)                = i4 ("movq" <+> pretty a <> "," <+> pretty x)
    pretty (Fld _ a)                     = i4 ("fld qword" <+> pretty a)
    pretty Fyl2x{}                       = i4 "fyl2x"
    pretty (Fstp _ a)                    = i4 ("fstp qword" <+> pretty a)
    pretty F2xm1{}                       = i4 "f2xm1"
    pretty Fldl2e{}                      = i4 "fldl2e"
    pretty Fldln2{}                      = i4 "fldln2"
    pretty Fld1{}                        = i4 "fld1"
    pretty Fsin{}                        = i4 "fsin"
    pretty Fcos{}                        = i4 "fcos"
    pretty Fprem{}                       = i4 "fprem"
    pretty Faddp{}                       = i4 "faddp"
    pretty Fscale{}                      = i4 "fscale"
    pretty Fninit{}                      = i4 "fninit"
    pretty (Fxch _ st)                   = i4 ("fxch" <+> pretty st)
    pretty (FldS _ st)                   = i4 ("fld" <+> pretty st)
    pretty Fmulp{}                       = i4 "fmulp"
    pretty (Vfmadd231sd _ rD r0 r1)      = i4 ("vfmadd231sd" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (Vfmnadd231sd _ rD r0 r1)     = i4 ("vfmnadd231sd" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (Vfmadd213sd _ rD r0 r1)      = i4 ("vfmadd213sd" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (Vfmsub231sd _ rD r0 r1)      = i4 ("vfsubd231sd" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (Vfmsub213sd _ rD r0 r1)      = i4 ("vfsubd213sd" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (Vfmsub132sd _ rD r0 r1)      = i4 ("vfsubd132sd" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (Vfmadd231sdA _ rD r a)       = i4 ("vfmadd231sd" <+> pretty rD <> "," <+> pretty r <> "," <+> pretty a)
    pretty (Push _ r)                    = i4 ("push" <+> pretty r)
    pretty (Pop _ r)                     = i4 ("pop" <+> pretty r)
    pretty (IDiv _ r)                    = i4 ("idiv" <+> pretty r)
    pretty (Call _ f)                    = i4 ("call" <+> pretty f <+> "wrt ..plt")
    pretty (Sal _ r i)                   = i4 ("sal" <+> pretty r <> "," <+> pretty i)
    pretty (Sar _ r i)                   = i4 ("sar" <+> pretty r <> "," <+> pretty i)
    pretty (Sqrtsd _ r0 r1)              = i4 ("sqrtsd" <+> pretty r0 <> "," <+> pretty r1)
    pretty (Maxsd _ r0 r1)               = i4 ("maxsd" <+> pretty r0 <> "," <+> pretty r1)
    pretty (Vmaxsd _ r0 r1 r2)           = i4 ("vmaxsd" <+> pretty r0 <> "," <+> pretty r1 <> "," <+> pretty r2)
    pretty (VmaxsdA _ r0 r1 a)           = i4 ("vmaxsd" <+> pretty r0 <> "," <+> pretty r1 <> "," <+> pretty a)
    pretty (Minsd _ r0 r1)               = i4 ("minsd" <+> pretty r0 <> "," <+> pretty r1)
    pretty (Vminsd _ r0 r1 r2)           = i4 ("vminsd" <+> pretty r0 <> "," <+> pretty r1 <> "," <+> pretty r2)
    pretty (Not _ r)                     = i4 ("not" <+> pretty r)
    pretty (And _ r0 r1)                 = i4 ("and" <+> pretty r0 <> "," <+> pretty r1)
    pretty (Cmovnle _ r0 r1)             = i4 ("cmovnle" <+> pretty r0 <> "," <+> pretty r1)
    pretty (Cmovnl _ r0 r1)              = i4 ("cmovnl" <+> pretty r0 <> "," <+> pretty r1)
    pretty (Cmovne _ r0 r1)              = i4 ("cmovne" <+> pretty r0 <> "," <+> pretty r1)
    pretty (Cmove _ r0 r1)               = i4 ("cmove" <+> pretty r0 <> "," <+> pretty r1)
    pretty (Cmovl _ r0 r1)               = i4 ("cmovl" <+> pretty r0 <> "," <+> pretty r1)
    pretty (Cmovle _ r0 r1)              = i4 ("cmovle" <+> pretty r0 <> "," <+> pretty r1)
    pretty (Rdrand _ r)                  = i4 ("rdrand" <+> pretty r)
    pretty (Test _ r0 r1)                = i4 ("test" <+> pretty r0 <> "," <+> pretty r1)
    pretty (TestI _ r0 i)                = i4 ("test" <+> pretty r0 <> "," <+> pretty i)
    pretty (MovqRX _ r xr)               = i4 ("movq" <+> pretty r <> "," <+> pretty xr)
    pretty (Vcmppd _ xr0 xr1 xr2 Eqoq)   = i4 ("vcmpeqpd" <+> pretty xr0 <> "," <+> pretty xr1 <> "," <+> pretty xr2)
    pretty (Vcmppd _ xr0 xr1 xr2 Ltos)   = i4 ("vcmpltpd" <+> pretty xr0 <> "," <+> pretty xr1 <> "," <+> pretty xr2)
    pretty (Vcmppd _ xr0 xr1 xr2 Leos)   = i4 ("vcmplepd" <+> pretty xr0 <> "," <+> pretty xr1 <> "," <+> pretty xr2)
    pretty (Vcmppd _ xr0 xr1 xr2 Unordq) = i4 ("vcmpunordpd" <+> pretty xr0 <> "," <+> pretty xr1 <> "," <+> pretty xr2)
    pretty (Vcmppd _ xr0 xr1 xr2 Nequq)  = i4 ("vcmpneqpd" <+> pretty xr0 <> "," <+> pretty xr1 <> "," <+> pretty xr2)
    pretty (Vcmppd _ xr0 xr1 xr2 Nltus)  = i4 ("vcmpnltpd" <+> pretty xr0 <> "," <+> pretty xr1 <> "," <+> pretty xr2)
    pretty (Vcmppd _ xr0 xr1 xr2 Nleus)  = i4 ("vcmpnlepd" <+> pretty xr0 <> "," <+> pretty xr1 <> "," <+> pretty xr2)
    pretty (Vcmppd _ xr0 xr1 xr2 Ordq)   = i4 ("vcmpordpd" <+> pretty xr0 <> "," <+> pretty xr1 <> "," <+> pretty xr2)
    pretty (C _ l)                       = i4 ("call" <+> prettyLabel l)
    pretty RetL{}                        = i4 "ret"
    pretty (Neg _ r)                     = i4 ("neg" <+> pretty r)

instance (Pretty reg, Pretty freg) => Show (X86 reg freg a) where show = show . pretty

prettyLive :: (Pretty reg, Pretty freg, Pretty o) => X86 reg freg o -> Doc ann
prettyLive r = pretty r <+> pretty (ann r)

prettyDebugX86 :: (Pretty freg, Pretty reg, Pretty o) => [X86 reg freg o] -> Doc ann
prettyDebugX86 = prettyLines . fmap prettyLive

mapR :: (areg -> reg) -> X86 areg afreg a -> X86 reg afreg a
mapR f (MovRR l r0 r1)              = MovRR l (f r0) (f r1)
mapR f (MovRL x r l)                = MovRL x (f r) l
mapR _ (Jg x l)                     = Jg x l
mapR _ (Je x l)                     = Je x l
mapR _ (Jge x l)                    = Jge x l
mapR _ (Jne x l)                    = Jne x l
mapR _ (J x l)                      = J x l
mapR _ (Jl x l)                     = Jl x l
mapR _ (Jle x l)                    = Jle x l
mapR _ (Label x l)                  = Label x l
mapR f (IMulRR l r0 r1)             = IMulRR l (f r0) (f r1)
mapR f (IMulRA l r a)               = IMulRA l (f r) (f<$>a)
mapR f (IAddRR l r0 r1)             = IAddRR l (f r0) (f r1)
mapR f (ISubRR l r0 r1)             = ISubRR l (f r0) (f r1)
mapR f (CmpRR l r0 r1)              = CmpRR l (f r0) (f r1)
mapR f (ISubRI l r0 i)              = ISubRI l (f r0) i
mapR f (IAddRI l r0 i)              = IAddRI l (f r0) i
mapR f (MovRI l r0 i)               = MovRI l (f r0) i
mapR f (MovRA l r a)                = MovRA l (f r) (f<$>a)
mapR f (MovAR l a r)                = MovAR l (f<$>a) (f r)
mapR f (MovAI32 l a i)              = MovAI32 l (f<$>a) i

mapR f (MovqXR l xr r)              = MovqXR l xr (f r)
mapR f (MovqXA l xr a)              = MovqXA l xr (f<$>a)
mapR f (MovqAX l a xr)              = MovqAX l (f<$>a) xr
mapR f (Fld l a)                    = Fld l (f<$>a)
mapR _ (Fldl2e l)                   = Fldl2e l
mapR _ (Fldln2 l)                   = Fldln2 l
mapR _ (Fld1 l)                     = Fld1 l
mapR _ (Fyl2x l)                    = Fyl2x l
mapR f (Fstp l a)                   = Fstp l (f<$>a)
mapR _ (F2xm1 l)                    = F2xm1 l
mapR _ (Fmulp l)                    = Fmulp l
mapR _ (Fprem l)                    = Fprem l
mapR _ (Faddp l)                    = Faddp l
mapR _ (Fscale l)                   = Fscale l
mapR f (CmpRI l r i)                = CmpRI l (f r) i
mapR _ (Ret l)                      = Ret l
mapR _ (Vdivsd l xr0 xr1 xr2)       = Vdivsd l xr0 xr1 xr2
mapR _ (Movapd l xr0 xr1)           = Movapd l xr0 xr1
mapR _ (FldS l s)                   = FldS l s
mapR _ (Fxch l s)                   = Fxch l s
mapR _ (Mulsd l xr0 xr1)            = Mulsd l xr0 xr1
mapR _ (Addsd l xr0 xr1)            = Addsd l xr0 xr1
mapR _ (Subsd l xr0 xr1)            = Subsd l xr0 xr1
mapR _ (Divsd l xr0 xr1)            = Divsd l xr0 xr1
mapR _ (Vmulsd l xr0 xr1 xr2)       = Vmulsd l xr0 xr1 xr2
mapR _ (Vaddsd l xr0 xr1 xr2)       = Vaddsd l xr0 xr1 xr2
mapR f (VaddsdA l xr0 xr1 a)        = VaddsdA l xr0 xr1 (f<$>a)
mapR _ (Vsubsd l xr0 xr1 xr2)       = Vsubsd l xr0 xr1 xr2
mapR f (Cvttsd2si l r xr)           = Cvttsd2si l (f r) xr
mapR f (Push l r)                   = Push l (f r)
mapR f (Pop l r)                    = Pop l (f r)
mapR _ (Call l f)                   = Call l f
mapR f (IDiv l r)                   = IDiv l (f r)
mapR _ (Roundsd l xr0 xr1 m)        = Roundsd l xr0 xr1 m
mapR f (Cvtsi2sd l xr r)            = Cvtsi2sd l xr (f r)
mapR _ (Vfmadd231sd l xr0 xr1 xr2)  = Vfmadd231sd l xr0 xr1 xr2
mapR _ (Vfmnadd231sd l xr0 xr1 xr2) = Vfmnadd231sd l xr0 xr1 xr2
mapR _ (Vfmadd213sd l xr0 xr1 xr2)  = Vfmadd213sd l xr0 xr1 xr2
mapR _ (Vfmsub213sd l xr0 xr1 xr2)  = Vfmsub213sd l xr0 xr1 xr2
mapR _ (Vfmsub231sd l xr0 xr1 xr2)  = Vfmsub231sd l xr0 xr1 xr2
mapR _ (Vfmsub132sd l xr0 xr1 xr2)  = Vfmsub132sd l xr0 xr1 xr2
mapR f (Vfmadd231sdA l xr0 xr a)    = Vfmadd231sdA l xr0 xr (f<$>a)
mapR f (Sal l r i)                  = Sal l (f r) i
mapR f (Sar l r i)                  = Sar l (f r) i
mapR _ (Sqrtsd l xr0 xr1)           = Sqrtsd l xr0 xr1
mapR _ (Maxsd l xr0 xr1)            = Maxsd l xr0 xr1
mapR _ (Minsd l xr0 xr1)            = Minsd l xr0 xr1
mapR _ (Vmaxsd l xr0 xr1 xr2)       = Vmaxsd l xr0 xr1 xr2
mapR _ (Vminsd l xr0 xr1 xr2)       = Vminsd l xr0 xr1 xr2
mapR f (VmaxsdA l xr0 xr1 a)        = VmaxsdA l xr0 xr1 (f<$>a)
mapR f (Not l r)                    = Not l (f r)
mapR f (And l r0 r1)                = And l (f r0) (f r1)
mapR f (Rdrand l r)                 = Rdrand l (f r)
mapR f (Cmovnle l r0 r1)            = Cmovnle l (f r0) (f r1)
mapR f (Cmovnl l r0 r1)             = Cmovnl l (f r0) (f r1)
mapR f (Cmovne l r0 r1)             = Cmovne l (f r0) (f r1)
mapR f (Cmove l r0 r1)              = Cmove l (f r0) (f r1)
mapR f (Cmovl l r0 r1)              = Cmovl l (f r0) (f r1)
mapR f (Cmovle l r0 r1)             = Cmovle l (f r0) (f r1)
mapR _ (Fninit l)                   = Fninit l
mapR f (Test l r0 r1)               = Test l (f r0) (f r1)
mapR f (TestI l r i)                = TestI l (f r) i
mapR _ (Vcmppd l xr0 xr1 xr2 p)     = Vcmppd l xr0 xr1 xr2 p
mapR f (MovqRX l r xr)              = MovqRX l (f r) xr
mapR _ (Fsin l)                     = Fsin l
mapR _ (Fcos l)                     = Fcos l
mapR f (XorRR l r0 r1)              = XorRR l (f r0) (f r1)
mapR _ (C a l)                      = C a l
mapR _ (RetL a l)                   = RetL a l
mapR f (Neg a r)                    = Neg a (f r)

fF :: (Monoid m) => (freg -> m) -> X86 reg freg a -> m
fF _ J{}                       = mempty
fF _ Jg{}                      = mempty
fF _ Jge{}                     = mempty
fF _ Je{}                      = mempty
fF _ Jne{}                     = mempty
fF _ Jle{}                     = mempty
fF _ Jl{}                      = mempty
fF _ Label{}                   = mempty
fF _ IAddRR{}                  = mempty
fF _ IAddRI{}                  = mempty
fF _ ISubRR{}                  = mempty
fF _ ISubRI{}                  = mempty
fF _ IMulRR{}                  = mempty
fF _ IMulRA{}                  = mempty
fF _ XorRR{}                   = mempty
fF _ MovRR{}                   = mempty
fF _ MovRA{}                   = mempty
fF _ MovAR{}                   = mempty
fF _ MovRL{}                   = mempty
fF _ MovAI32{}                 = mempty
fF _ MovRI{}                   = mempty
fF f (MovqXR _ x _)            = f x
fF f (MovqXA _ x _)            = f x
fF f (MovqAX _ _ x)            = f x
fF f (MovqRX _ _ x)            = f x
fF _ Fld{}                     = mempty
fF _ FldS{}                    = mempty
fF _ Fldl2e{}                  = mempty
fF _ Fldln2{}                  = mempty
fF _ Fld1{}                    = mempty
fF _ Fyl2x{}                   = mempty
fF _ Fsin{}                    = mempty
fF _ Fcos{}                    = mempty
fF _ Fstp{}                    = mempty
fF _ F2xm1{}                   = mempty
fF _ Fmulp{}                   = mempty
fF _ Fprem{}                   = mempty
fF _ Faddp{}                   = mempty
fF _ Fscale{}                  = mempty
fF _ Fninit{}                  = mempty
fF _ Fxch{}                    = mempty
fF _ C{}                       = mempty
fF _ CmpRR{}                   = mempty
fF _ CmpRI{}                   = mempty
fF f (Vcmppd _ r0 r1 r2 _)     = f r0<>f r1<>f r2
fF _ Test{}                    = mempty
fF _ TestI{}                   = mempty
fF _ Ret{}                     = mempty
fF _ RetL{}                    = mempty
fF f (Vdivsd _ r0 r1 r2)       = f r0<>f r1<>f r2
fF f (Movapd _ r0 r1)          = f r0<>f r1
fF f (Roundsd _ r0 r1 _)       = f r0<>f r1
fF f (Cvttsd2si _ _ r)         = f r
fF f (Mulsd _ r0 r1)           = f r0<>f r1
fF f (Addsd _ r0 r1)           = f r0<>f r1
fF f (Subsd _ r0 r1)           = f r0<>f r1
fF f (Divsd _ r0 r1)           = f r0<>f r1
fF f (Maxsd _ r0 r1)           = f r0<>f r1
fF f (Minsd _ r0 r1)           = f r0<>f r1
fF f (Vaddsd _ r0 r1 r2)       = f r0<>f r1<>f r2
fF f (Vsubsd _ r0 r1 r2)       = f r0<>f r1<>f r2
fF f (Vmulsd _ r0 r1 r2)       = f r0<>f r1<>f r2
fF f (Vmaxsd _ r0 r1 r2)       = f r0<>f r1<>f r2
fF f (Vminsd _ r0 r1 r2)       = f r0<>f r1<>f r2
fF f (VaddsdA _ r0 r1 _)       = f r0<>f r1
fF f (VmaxsdA _ r0 r1 _)       = f r0<>f r1
fF f (Cvtsi2sd _ r0 _)         = f r0
fF f (Vfmadd231sd _ r0 r1 r2)  = f r0<>f r1<>f r2
fF f (Vfmadd213sd _ r0 r1 r2)  = f r0<>f r1<>f r2
fF f (Vfmsub231sd _ r0 r1 r2)  = f r0<>f r1<>f r2
fF f (Vfmsub213sd _ r0 r1 r2)  = f r0<>f r1<>f r2
fF f (Vfmsub132sd _ r0 r1 r2)  = f r0<>f r1<>f r2
fF f (Vfmnadd231sd _ r0 r1 r2) = f r0<>f r1<>f r2
fF f (Vfmadd231sdA _ r0 r1 _)  = f r0<>f r1
fF _ Push{}                    = mempty
fF _ Pop{}                     = mempty
fF _ Call{}                    = mempty
fF _ IDiv{}                    = mempty
fF _ Sal{}                     = mempty
fF _ Sar{}                     = mempty
fF _ Not{}                     = mempty
fF _ And{}                     = mempty
fF _ Neg{}                     = mempty
fF f (Sqrtsd _ r0 r1)          = f r0<>f r1
fF _ Cmovne{}                  = mempty
fF _ Cmovnle{}                 = mempty
fF _ Cmove{}                   = mempty
fF _ Cmovl{}                   = mempty
fF _ Cmovle{}                  = mempty
fF _ Cmovnl{}                  = mempty
fF _ Rdrand{}                  = mempty

fR :: (Monoid m) => (reg -> m) -> X86 reg freg a -> m
fR _ Jg{}                   = mempty
fR _ J{}                    = mempty
fR f (MovAR _ a r)          = f @<> a <> f r
fR f (MovRA _ r a)          = f r <> f @<> a
fR f (MovRR _ r0 r1)        = f r0 <> f r1
fR f (MovRL _ r _)          = f r
fR _ Label{}                = mempty
fR f (IAddRR _ r0 r1)       = f r0 <> f r1
fR f (IAddRI _ r _)         = f r
fR f (ISubRR _ r0 r1)       = f r0 <> f r1
fR f (ISubRI _ r _)         = f r
fR f (IMulRR _ r0 r1)       = f r0 <> f r1
fR f (IMulRA _ r a)         = f r <> f @<> a
fR f (XorRR _ r0 r1)        = f r0 <> f r1
fR f (MovAI32 _ a _)        = f @<> a
fR f (MovRI _ r _)          = f r
fR f (MovqXR _ _ r)         = f r
fR f (MovqXA _ _ a)         = f @<> a
fR f (MovqAX _ a _)         = f @<> a
fR f (MovqRX _ r _)         = f r
fR f (Fld _ a)              = f @<> a
fR _ FldS{}                 = mempty
fR _ Fldl2e{}               = mempty
fR _ Fldln2{}               = mempty
fR _ Fld1{}                 = mempty
fR _ Fyl2x{}                = mempty
fR _ Fsin{}                 = mempty
fR _ Fcos{}                 = mempty
fR f (Fstp _ a)             = f @<> a
fR _ F2xm1{}                = mempty
fR _ Fmulp{}                = mempty
fR _ Fprem{}                = mempty
fR _ Faddp{}                = mempty
fR _ Fscale{}               = mempty
fR _ Fninit{}               = mempty
fR _ Fxch{}                 = mempty
fR _ Je{}                   = mempty
fR _ Jne{}                  = mempty
fR _ Jge{}                  = mempty
fR _ Jl{}                   = mempty
fR _ Jle{}                  = mempty
fR _ C{}                    = mempty
fR f (CmpRR _ r0 r1)        = f r0 <> f r1
fR f (CmpRI _ r _)          = f r
fR f (Test _ r0 r1)         = f r0 <> f r1
fR _ Vcmppd{}               = mempty
fR f (TestI _ r _)          = f r
fR _ Ret{}                  = mempty
fR _ RetL{}                 = mempty
fR _ Vdivsd{}               = mempty
fR _ Movapd{}               = mempty
fR _ Roundsd{}              = mempty
fR f (Cvttsd2si _ r _)      = f r
fR _ Mulsd{}                = mempty
fR _ Addsd{}                = mempty
fR _ Subsd{}                = mempty
fR _ Divsd{}                = mempty
fR _ Vmulsd{}               = mempty
fR _ Vaddsd{}               = mempty
fR f (VaddsdA _ _ _ a)      = f @<> a
fR _ Vsubsd{}               = mempty
fR f (Cvtsi2sd _ _ r)       = f r
fR _ Vfmadd231sd{}          = mempty
fR _ Vfmnadd231sd{}         = mempty
fR _ Vfmadd213sd{}          = mempty
fR _ Vfmsub213sd{}          = mempty
fR _ Vfmsub231sd{}          = mempty
fR _ Vfmsub132sd{}          = mempty
fR f (Vfmadd231sdA _ _ _ a) = f @<> a
fR f (Push _ r)             = f r
fR f (Pop _ r)              = f r
fR _ Call{}                 = mempty
fR f (IDiv _ r)             = f r
fR f (Sal _ r _)            = f r
fR f (Sar _ r _)            = f r
fR _ Sqrtsd{}               = mempty
fR _ Maxsd{}                = mempty
fR _ Vmaxsd{}               = mempty
fR f (VmaxsdA _ _ _ a)      = f @<> a
fR _ Minsd{}                = mempty
fR _ Vminsd{}               = mempty
fR f (Not _ r)              = f r
fR f (And _ r0 r1)          = f r0 <> f r1
fR f (Cmovnle _ r0 r1)      = f r0 <> f r1
fR f (Cmovnl _ r0 r1)       = f r0 <> f r1
fR f (Cmovne _ r0 r1)       = f r0 <> f r1
fR f (Cmove _ r0 r1)        = f r0 <> f r1
fR f (Cmovl _ r0 r1)        = f r0 <> f r1
fR f (Cmovle _ r0 r1)       = f r0 <> f r1
fR f (Rdrand _ r)           = f r
fR f (Neg _ r)              = f r

mapFR :: (afreg -> freg) -> X86 areg afreg a -> X86 areg freg a
mapFR _ (Jg x l)                     = Jg x l
mapFR _ (J x l)                      = J x l
mapFR _ (Label x l)                  = Label x l
mapFR _ (MovRI l r i)                = MovRI l r i
mapFR _ (MovRR l r0 r1)              = MovRR l r0 r1
mapFR _ (MovRL x r l)                = MovRL x r l
mapFR _ (IAddRI l r i)               = IAddRI l r i
mapFR f (Movapd l r0 r1)             = Movapd l (f r0) (f r1)
mapFR f (Mulsd l xr0 xr1)            = Mulsd l (f xr0) (f xr1)
mapFR f (MovqXR l xr r)              = MovqXR l (f xr) r
mapFR f (Roundsd l xr0 xr1 s)        = Roundsd l (f xr0) (f xr1) s
mapFR f (Cvttsd2si l r xr)           = Cvttsd2si l r (f xr)
mapFR f (Vsubsd l xr0 xr1 xr2)       = Vsubsd l (f xr0) (f xr1) (f xr2)
mapFR f (Vaddsd l xr0 xr1 xr2)       = Vaddsd l (f xr0) (f xr1) (f xr2)
mapFR f (VaddsdA l xr0 xr1 r)        = VaddsdA l (f xr0) (f xr1) r
mapFR f (Vdivsd l xr0 xr1 xr2)       = Vdivsd l (f xr0) (f xr1) (f xr2)
mapFR _ (CmpRR l r0 r1)              = CmpRR l r0 r1
mapFR f (Addsd l xr0 xr1)            = Addsd l (f xr0) (f xr1)
mapFR _ (IAddRR l r0 r1)             = IAddRR l r0 r1
mapFR _ (ISubRR l r0 r1)             = ISubRR l r0 r1
mapFR _ (IMulRR l r0 r1)             = IMulRR l r0 r1
mapFR _ (IMulRA l r a)               = IMulRA l r a
mapFR _ (ISubRI l r i)               = ISubRI l r i
mapFR _ (MovRA l r a)                = MovRA l r a
mapFR _ (MovAI32 l r i)              = MovAI32 l r i
mapFR _ (MovAR l a r)                = MovAR l a r
mapFR f (MovqXA l xr a)              = MovqXA l (f xr) a
mapFR f (MovqAX l a xr)              = MovqAX l a (f xr)
mapFR _ (Fld l a)                    = Fld l a
mapFR _ (FldS l s)                   = FldS l s
mapFR _ (Fldl2e l)                   = Fldl2e l
mapFR _ (Fldln2 l)                   = Fldln2 l
mapFR _ (Fld1 l)                     = Fld1 l
mapFR _ (Fyl2x l)                    = Fyl2x l
mapFR _ (Fstp l a)                   = Fstp l a
mapFR _ (F2xm1 l)                    = F2xm1 l
mapFR _ (Fmulp l)                    = Fmulp l
mapFR _ (Fprem l)                    = Fprem l
mapFR _ (Faddp l)                    = Faddp l
mapFR _ (Fscale l)                   = Fscale l
mapFR _ (Fninit l)                   = Fninit l
mapFR _ (Fxch l s)                   = Fxch l s
mapFR _ (Je x l)                     = Je x l
mapFR _ (Jge x l)                    = Jge x l
mapFR _ (Jne x l)                    = Jne x l
mapFR _ (Jl x l)                     = Jl x l
mapFR _ (Jle x l)                    = Jle x l
mapFR _ (CmpRI l r i)                = CmpRI l r i
mapFR _ (Ret l)                      = Ret l
mapFR f (Subsd l xr0 xr1)            = Subsd l (f xr0) (f xr1)
mapFR f (Divsd l xr0 xr1)            = Divsd l (f xr0) (f xr1)
mapFR f (Vmulsd l xr0 xr1 xr2)       = Vmulsd l (f xr0) (f xr1) (f xr2)
mapFR _ (Push l r)                   = Push l r
mapFR _ (Pop l r)                    = Pop l r
mapFR _ (IDiv l r)                   = IDiv l r
mapFR _ (Call l f)                   = Call l f
mapFR _ (Sal l r i)                  = Sal l r i
mapFR _ (Sar l r i)                  = Sar l r i
mapFR f (Maxsd l xr0 xr1)            = Maxsd l (f xr0) (f xr1)
mapFR f (Vmaxsd l xr0 xr1 xr2)       = Vmaxsd l (f xr0) (f xr1) (f xr2)
mapFR f (VmaxsdA l xr0 xr1 a)        = VmaxsdA l (f xr0) (f xr1) a
mapFR f (Minsd l xr0 xr1)            = Minsd l (f xr0) (f xr1)
mapFR f (Vminsd l xr0 xr1 xr2)       = Vminsd l (f xr0) (f xr1) (f xr2)
mapFR _ (Not l r)                    = Not l r
mapFR f (Cvtsi2sd l xr r)            = Cvtsi2sd l (f xr) r
mapFR f (Vfmadd231sd l xr0 xr1 xr2)  = Vfmadd231sd l (f xr0) (f xr1) (f xr2)
mapFR f (Vfmnadd231sd l xr0 xr1 xr2) = Vfmnadd231sd l (f xr0) (f xr1) (f xr2)
mapFR f (Vfmadd213sd l xr0 xr1 xr2)  = Vfmadd213sd l (f xr0) (f xr1) (f xr2)
mapFR f (Vfmsub213sd l xr0 xr1 xr2)  = Vfmsub213sd l (f xr0) (f xr1) (f xr2)
mapFR f (Vfmsub231sd l xr0 xr1 xr2)  = Vfmsub231sd l (f xr0) (f xr1) (f xr2)
mapFR f (Vfmsub132sd l xr0 xr1 xr2)  = Vfmsub132sd l (f xr0) (f xr1) (f xr2)
mapFR f (Vfmadd231sdA l xr0 xr1 a)   = Vfmadd231sdA l (f xr0) (f xr1) a
mapFR f (Sqrtsd l xr0 xr1)           = Sqrtsd l (f xr0) (f xr1)
mapFR _ (And l r0 r1)                = And l r0 r1
mapFR _ (Cmovnle l r0 r1)            = Cmovnle l r0 r1
mapFR _ (Cmovnl l r0 r1)             = Cmovnl l r0 r1
mapFR _ (Cmovne l r0 r1)             = Cmovne l r0 r1
mapFR _ (Cmove l r0 r1)              = Cmove l r0 r1
mapFR _ (Cmovl l r0 r1)              = Cmovl l r0 r1
mapFR _ (Cmovle l r0 r1)             = Cmovle l r0 r1
mapFR _ (Rdrand l r)                 = Rdrand l r
mapFR _ (TestI l r i)                = TestI l r i
mapFR _ (Test l r0 r1)               = Test l r0 r1
mapFR f (Vcmppd l xr0 xr1 xr2 p)     = Vcmppd l (f xr0) (f xr1) (f xr2) p
mapFR f (MovqRX l r xr)              = MovqRX l r (f xr)
mapFR _ (Fsin l)                     = Fsin l
mapFR _ (Fcos l)                     = Fcos l
mapFR _ (XorRR l r0 r1)              = XorRR l r0 r1
mapFR _ (C a l)                      = C a l
mapFR _ (RetL a l)                   = RetL a l
mapFR _ (Neg a r)                    = Neg a r
