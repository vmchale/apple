{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Asm.X86 ( X86 (..)
               , AbsReg (..)
               , FAbsReg (..)
               , X86Reg (..)
               , FX86Reg (..)
               , Addr (..)
               , ST (..)
               , Scale (..)
               , RoundMode (..)
               , Label
               , CFunc (..)
               , prettyX86
               , prettyDebugX86
               , toInt
               , fToInt
               , roundMode
               ) where

import           Control.DeepSeq   (NFData (..))
import           Data.Copointed
import           Data.Int          (Int32, Int64, Int8)
import           Data.Semigroup    ((<>))
import           Data.Word         (Word8)
import           GHC.Generics      (Generic)
import           Prettyprinter     (Doc, Pretty (..), brackets, colon, indent, (<+>))
import           Prettyprinter.Ext

type Label = Word

-- TODO: consider separate FX86Reg etc. type
data X86Reg = Rcx | Rdx | Rsi | Rdi | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 | Rbx | Rax | Rsp
            deriving (Eq, Ord, Enum, Generic)

data FX86Reg = XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7 | XMM8 | XMM9 | XMM10 | XMM11 | XMM12 | XMM13 | XMM14 | XMM15 | XMM0
             deriving (Eq, Ord, Enum, Generic)

instance NFData X86Reg where

instance NFData FX86Reg where

instance Pretty X86Reg where
    pretty Rax   = "rax"
    pretty Rbx   = "rbx"
    pretty Rcx   = "rcx"
    pretty Rdx   = "rdx"
    pretty Rsi   = "rsi"
    pretty Rdi   = "rdi"
    pretty R8    = "r8"
    pretty R9    = "r9"
    pretty R10   = "r10"
    pretty R11   = "r11"
    pretty R12   = "r12"
    pretty R13   = "r13"
    pretty R14   = "r14"
    pretty R15   = "r15"
    pretty Rsp   = "rsp"

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

instance Show X86Reg where show = show . pretty

instance Show FX86Reg where show = show . pretty

-- TODO: FAbsReg
data AbsReg = IReg !Int
            | CArg0 | CArg1 | CArg2 | CArg3 | CArg4 | CArg5
            | CRet
            | SP
            | Quot | Rem
            deriving (Eq, Ord)

data FAbsReg = FReg !Int
             | FArg0 | FArg1 | FArg2 | FArg3 | FArg4 | FArg5 | FArg6 | FArg7
             | FRet0 | FRet1
             deriving (Eq, Ord)

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

fToInt :: FAbsReg -> Int
fToInt FArg0   = 8
fToInt FArg1   = 9
fToInt FArg2   = 10
fToInt FArg3   = 11
fToInt FArg4   = 12
fToInt FArg5   = 13
fToInt FArg6   = 14
fToInt FArg7   = 15
fToInt FRet0   = 8 -- xmm0
fToInt FRet1   = 9 -- xmm1
fToInt (FReg i) = 16+i

newtype ST = ST Int8 deriving (NFData)

instance Pretty ST where
    pretty (ST i) = "ST(" <> pretty i <> ")"

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

data Scale = One | Two | Four | Eight deriving (Generic)

instance Pretty Scale where
    pretty One   = "1"
    pretty Two   = "2"
    pretty Four  = "4"
    pretty Eight = "8"

data Addr reg = R reg | RC reg Int8 | RS reg Scale reg | RSD reg Scale reg Int8 deriving (Generic, Functor, Foldable, Traversable)

instance NFData Scale where

instance NFData reg => NFData (Addr reg) where

instance Pretty reg => Pretty (Addr reg) where
    pretty (R r)           = brackets (pretty r)
    pretty (RC r c)        = brackets (pretty r <> "+" <> pretty c)
    pretty (RS b One i)    = brackets (pretty b <> "+" <> pretty i)
    pretty (RS b s i)      = brackets (pretty b <> "+" <> pretty s <> "*" <> pretty i)
    pretty (RSD b One i d) = brackets (pretty b <> "+" <> pretty i <> pretty d)
    pretty (RSD b s i d)   = brackets (pretty b <> "+" <> pretty s <> "*" <> pretty i <> "+" <> pretty d)

data CFunc = Malloc | Free deriving (Generic)

instance NFData CFunc where

instance Pretty CFunc where
    pretty Malloc = "malloc"
    pretty Free   = "free"

data X86 reg freg a = Label { ann :: a, label :: Label }
                    | IAddRR { ann :: a, rAdd1 :: reg, rAdd2 :: reg }
                    | IAddRI { ann :: a, rAdd1 :: reg, rAddI :: Int64 }
                    | ISubRR { ann :: a, rSub1 :: reg, rSub2 :: reg }
                    | ISubRI { ann :: a, rSub :: reg, rSubI :: Int64 }
                    | IMulRR { ann :: a, rMul1 :: reg, rMul2 :: reg }
                    | MovRR { ann :: a, rDest :: reg, rSrc :: reg }
                    | MovRA { ann :: a, rDest :: reg, aSrc :: Addr reg }
                    | MovAR { ann :: a, aDest :: Addr reg, rSrc :: reg }
                    | MovAI32 { ann :: a, aDest :: Addr reg, i32Src :: Int32 }
                    | MovRI { ann :: a, rDest :: reg, iSrc :: Int64 }
                    | MovqXR { ann :: a, fDest :: freg, rSrc :: reg }
                    | MovqXA { ann :: a, fDest :: freg, aSrc :: Addr reg }
                    | MovqAX { ann :: a, aDest :: Addr reg, fSrc :: freg }
                    | Fld { ann :: a, a87 :: Addr reg }
                    | FldS { ann :: a, stIsn :: ST }
                    | Fldl2e { ann :: a }
                    | Fldln2 { ann :: a }
                    | Fld1 { ann :: a }
                    | Fyl2x { ann :: a }
                    | Fstp { ann :: a, a87 :: Addr reg }
                    | F2xm1 { ann :: a }
                    | Fmulp { ann :: a }
                    | Fprem { ann :: a }
                    | Faddp { ann :: a }
                    | Fscale { ann :: a }
                    | Fxch { ann :: a, stIsn :: ST }
                    | J { ann :: a, label :: Label }
                    | Je { ann :: a, jLabel :: Label }
                    | Jne { ann :: a, jLabel :: Label }
                    | Jg { ann :: a, jLabel :: Label }
                    | Jge { ann :: a, jLabel :: Label }
                    | Jl { ann :: a, jLabel :: Label }
                    | Jle { ann :: a, jLabel :: Label }
                    | CmpRR { ann :: a, rCmp :: reg, rCmp' :: reg }
                    | CmpRI { ann :: a, rCmp :: reg, cmpI32 :: Int32 }
                    | Ret { ann :: a }
                    | Vdivsd { ann :: a, fDest :: freg, fSrc1 :: freg, fSrc2 :: freg }
                    | Movapd { ann :: a, fDest :: freg, fSrc :: freg }
                    | Roundsd { ann :: a, fDest :: freg, fSrc :: freg, mode :: RoundMode }
                    | Cvttsd2si { ann :: a, rDest :: reg, fSrc :: freg }
                    | Mulsd { ann :: a, fDest :: freg, fSrc :: freg }
                    | Addsd { ann :: a, fDest :: freg, fSrc :: freg }
                    | Subsd { ann :: a, fDest :: freg, fSrc :: freg }
                    | Divsd { ann :: a, fDest :: freg, fSrc :: freg }
                    | Vmulsd { ann :: a, fDest :: freg, fSrc1 :: freg, fSrc2 :: freg }
                    | Vaddsd { ann :: a, fDest :: freg, fSrc1 :: freg, fSrc2 :: freg }
                    | Vsubsd { ann :: a, fDest :: freg, fSrc1 :: freg, fSrc2 :: freg }
                    | Cvtsi2sd { ann :: a, fDest :: freg, rSrc :: reg }
                    | Vfmadd231sd { ann :: a, fDest :: freg, fSrc1 :: freg, fSrc2 :: freg }
                    | Push { ann :: a, rSrc :: reg }
                    | Pop { ann :: a, rDest :: reg }
                    | Call { ann :: a, cfunc :: CFunc }
                    | IDiv { ann :: a, rSrc :: reg }
                    | Sal { ann :: a, rSrc :: reg, iExp :: Int8 }
                    | Sar { ann :: a, rSrc :: reg, iExp :: Int8 }
                    | Sqrtsd { ann :: a, fDest :: freg, fSrc :: freg }
                    | Maxsd { ann :: a, fDest :: freg, fSrc :: freg }
                    | Vmaxsd { ann :: a, fDest :: freg, fSrc1 :: freg, fSrc2 :: freg }
                    | Minsd { ann :: a, fDest :: freg, fSrc :: freg }
                    | Vminsd { ann :: a, fDest :: freg, rSrc1 :: freg, rSrc2 :: freg }
                    | Not { ann :: a, rSrc :: reg }
                    | And { ann :: a, rDest :: reg, rSrc :: reg }
                    | Cmovnle { ann :: a, rDest :: reg, rSrc :: reg }
                    deriving (Functor, Generic)

instance (NFData a, NFData reg, NFData freg) => NFData (X86 reg freg a) where

instance Copointed (X86 reg freg) where
    copoint = ann

prettyLabel :: Label -> Doc ann
prettyLabel l = "apple_" <> pretty l

i4 :: Doc ann -> Doc ann
i4 = indent 4

instance (Pretty reg, Pretty freg) => Pretty (X86 reg freg a) where
  pretty (J _ l)                  = i4 ("jmp" <+> prettyLabel l)
  pretty (Label _ l)              = prettyLabel l <> colon
  pretty (CmpRR _ r0 r1)          = i4 ("cmp" <+> pretty r0 <> "," <+> pretty r1)
  pretty (MovRR _ r0 r1)          = i4 ("mov" <+> pretty r0 <> "," <+> pretty r1)
  pretty (MovRI _ r i)            = i4 ("mov" <+> pretty r <> "," <+> pretty i)
  pretty (MovqXR _ r0 r1)         = i4 ("movq" <+> pretty r0 <> "," <+> pretty r1)
  pretty (IAddRR _ r0 r1)         = i4 ("add" <+> pretty r0 <> "," <+> pretty r1)
  pretty (IAddRI _ r i)           = i4 ("add" <+> pretty r <> "," <+> pretty i)
  pretty (ISubRR _ r0 r1)         = i4 ("sub" <+> pretty r0 <> "," <+> pretty r1)
  pretty (ISubRI _ r i)           = i4 ("sub" <+> pretty r <> "," <+> pretty i)
  pretty (IMulRR _ r0 r1)         = i4 ("imul" <+> pretty r0 <> "," <+> pretty r1)
  pretty (Jne _ l)                = i4 ("jne" <+> prettyLabel l)
  pretty (Jle _ l)                = i4 ("jle" <+> prettyLabel l)
  pretty (Je _ l)                 = i4 ("je" <+> prettyLabel l)
  pretty (Jge _ l)                = i4 ("jge" <+> prettyLabel l)
  pretty (Jg _ l)                 = i4 ("jg" <+> prettyLabel l)
  pretty (Jl _ l)                 = i4 ("jl" <+> prettyLabel l)
  pretty Ret{}                    = i4 "ret"
  pretty (Vdivsd _ rD r0 r1)      = i4 ("vdivsd" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
  pretty (Movapd _ r0 r1)         = i4 ("movapd" <+> pretty r0 <> "," <+> pretty r1)
  pretty (Cvttsd2si _ r0 r1)      = i4 ("cvttsd2si" <+> pretty r0 <> "," <+> pretty r1)
  pretty (Vmulsd _ rD r0 r1)      = i4 ("vmulsd" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
  pretty (Vaddsd _ rD r0 r1)      = i4 ("vaddsd" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
  pretty (Vsubsd _ rD r0 r1)      = i4 ("vsubsd" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
  pretty (Cvtsi2sd _ r0 r1)       = i4 ("cvtsi2sd" <+> pretty r0 <> "," <+> pretty r1)
  pretty (Roundsd _ r0 r1 m)      = i4 ("roundsd" <+> pretty r0 <> "," <+> pretty r1 <> "," <+> pretty m)
  pretty (CmpRI _ r i)            = i4 ("cmp" <+> pretty r <> "," <+> pretty i)
  pretty (Divsd _ r0 r1)          = i4 ("divsd" <+> pretty r0 <> "," <+> pretty r1)
  pretty (Mulsd _ r0 r1)          = i4 ("mulsd" <+> pretty r0 <> "," <+> pretty r1)
  pretty (Addsd _ r0 r1)          = i4 ("addsd" <+> pretty r0 <> "," <+> pretty r1)
  pretty (Subsd _ r0 r1)          = i4 ("subsd" <+> pretty r0 <> "," <+> pretty r1)
  pretty (MovRA _ r a)            = i4 ("mov" <+> pretty r <> "," <+> pretty a)
  pretty (MovAR _ a r)            = i4 ("mov" <+> pretty a <> "," <+> pretty r)
  pretty (MovAI32 _ a i)          = i4 ("mov qword" <+> pretty a <> "," <+> pretty i)
  pretty (MovqXA _ x a)           = i4 ("movq" <+> pretty x <> "," <+> pretty a)
  pretty (MovqAX _ a x)           = i4 ("movq" <+> pretty a <> "," <+> pretty x)
  pretty (Fld _ a)                = i4 ("fld qword" <+> pretty a)
  pretty Fyl2x{}                  = i4 "fyl2x"
  pretty (Fstp _ a)               = i4 ("fstp qword" <+> pretty a)
  pretty F2xm1{}                  = i4 "f2xm1"
  pretty Fldl2e{}                 = i4 "fldl2e"
  pretty Fldln2{}                 = i4 "fldln2"
  pretty Fld1{}                   = i4 "fld1"
  pretty Fprem{}                  = i4 "fprem"
  pretty Faddp{}                  = i4 "faddp"
  pretty Fscale{}                 = i4 "fscale"
  pretty (Fxch _ st)              = i4 ("fxch" <+> pretty st)
  pretty (FldS _ st)              = i4 ("fld" <+> pretty st)
  pretty Fmulp{}                  = i4 "fmulp"
  pretty (Vfmadd231sd _ rD r0 r1) = i4 ("vfmadd231sd" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
  pretty (Push _ r)               = i4 ("push" <+> pretty r)
  pretty (Pop _ r)                = i4 ("pop" <+> pretty r)
  pretty (IDiv _ r)               = i4 ("idiv" <+> pretty r)
  pretty (Call _ f)               = i4 ("call" <+> pretty f <+> "wrt ..plt")
  pretty (Sal _ r i)              = i4 ("sal" <+> pretty r <> "," <+> pretty i)
  pretty (Sar _ r i)              = i4 ("sar" <+> pretty r <> "," <+> pretty i)
  pretty (Sqrtsd _ r0 r1)         = i4 ("sqrtsd" <+> pretty r0 <> "," <+> pretty r1)
  pretty (Maxsd _ r0 r1)          = i4 ("maxsd" <+> pretty r0 <> "," <+> pretty r1)
  pretty (Vmaxsd _ r0 r1 r2)      = i4 ("maxsd" <+> pretty r0 <> "," <+> pretty r1 <> "," <+> pretty r2)
  pretty (Minsd _ r0 r1)          = i4 ("minsd" <+> pretty r0 <> "," <+> pretty r1)
  pretty (Vminsd _ r0 r1 r2)      = i4 ("minsd" <+> pretty r0 <> "," <+> pretty r1 <> "," <+> pretty r2)
  pretty (Not _ r)                = i4 ("not" <+> pretty r)
  pretty (And _ r0 r1)            = i4 ("and" <+> pretty r0 <> "," <+> pretty r1)
  pretty (Cmovnle _ r0 r1)        = i4 ("cmovnle" <+> pretty r0 <> "," <+> pretty r1)

instance (Pretty reg, Pretty freg) => Show (X86 reg freg a) where show = show . pretty

prettyLive :: (Pretty reg, Pretty freg, Pretty o) => X86 reg freg o -> Doc ann
prettyLive r = pretty r <+> pretty (ann r)

prettyX86 :: (Pretty reg, Pretty freg) => [X86 reg freg a] -> Doc ann
prettyX86 = prettyLines . fmap pretty

prettyDebugX86 :: (Pretty freg, Pretty reg, Pretty o) => [X86 reg freg o] -> Doc ann
prettyDebugX86 = prettyLines . fmap prettyLive
