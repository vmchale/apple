{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Asm.Aarch64 ( AArch64 (..)
                   , Addr (..)
                   , Cond (..)
                   , Shift (..), BM (..)
                   , AbsReg (..), FAbsReg (..), F2Abs (..)
                   , AReg (..), FAReg (..), F2Reg (..)
                   , SIMD (..), simd2
                   , prettyDebug
                   , mapR, mapFR, mapF2
                   , toInt, fToInt, f2ToInt
                   , pus, pos
                   , puds, pods
                   , pSym
                   ) where

import           Asm.M
import           Control.DeepSeq   (NFData (..))
import           Data.Copointed
import           Data.List         (scanl')
import           Data.Word         (Word16, Word8)
import           GHC.Generics      (Generic)
import           Numeric           (showHex)
import           Prettyprinter     (Doc, Pretty (..), brackets, (<+>))
import           Prettyprinter.Ext
import           System.Info       (os)

-- https://developer.arm.com/documentation/102374/0101/Registers-in-AArch64---other-registers
data AReg = X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9 | X10 | X11 | X12 | X13 | X14 | X15 | X16 | X17 | X18 | X19 | X20 | X21 | X22 | X23 | X24 | X25 | X26 | X27 | X28 | X29 | X30 | SP deriving (Eq, Ord, Enum, Generic)

instance Pretty AReg where
    pretty X0 = "x0"; pretty X1 = "x1"; pretty X2 = "x2"; pretty X3 = "x3"; pretty X4 = "x4"; pretty X5 = "x5"; pretty X6 = "x6"; pretty X7 = "x7"
    pretty X8 = "x8"; pretty X9 = "x9"; pretty X10 = "x10"; pretty X11 = "x11"; pretty X12 = "x12"; pretty X13 = "x13"; pretty X14 = "x14"; pretty X15 = "x15"
    pretty X16 = "x16"; pretty X17 = "x17"; pretty X18 = "x18"; pretty X19 = "x19"; pretty X20 = "x20"; pretty X21 = "x21"; pretty X22 = "x22"; pretty X23 = "x23"
    pretty X24 = "x24"; pretty X25 = "x25"; pretty X26 = "x26"; pretty X27 = "x27"; pretty X28 = "x28"; pretty X29 = "x29"; pretty X30 = "x30"; pretty SP = "sp"

instance Show AReg where show = show.pretty

simd2 :: FAReg -> F2Reg
simd2 = toEnum.fromEnum

data FAReg = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 | D10 | D11 | D12 | D13 | D14 | D15 | D16 | D17 | D18 | D19 | D20 | D21 | D22 | D23 | D24 | D25 | D26 | D27 | D28 | D29 | D30 | D31 deriving (Eq, Ord, Enum, Generic)

instance Pretty FAReg where
    pretty D0 = "d0"; pretty D1 = "d1"; pretty D2 = "d2"; pretty D3 = "d3"; pretty D4 = "d4"; pretty D5 = "d5"; pretty D6 = "d6"; pretty D7 = "d7"
    pretty D8 = "d8"; pretty D9 = "d9"; pretty D10 = "d10"; pretty D11 = "d11"; pretty D12 = "d12"; pretty D13 = "d13"; pretty D14 = "d14"; pretty D15 = "d15"
    pretty D16 = "d16"; pretty D17 = "d17"; pretty D18 = "d18"; pretty D19 = "d19"; pretty D20 = "d20"; pretty D21 = "d21"; pretty D22 = "d22"; pretty D23 = "d23"
    pretty D24 = "d24"; pretty D25 = "d25"; pretty D26 = "d26"; pretty D27 = "d27"; pretty D28 = "d28"; pretty D29 = "d29"; pretty D30 = "d30"; pretty D31 = "d31"

instance Show FAReg where show=show.pretty

data F2Reg = V0 | V1 | V2 | V3 | V4 | V5 | V6 | V7 | V8 | V9 | V10 | V11 | V12 | V13 | V14 | V15 | V16 | V17 | V18 | V19 | V20 | V21 | V22 | V23 | V24 | V25 | V26 | V27 | V28 | V29 | V30 | V31 deriving (Eq, Ord, Enum, Generic)

class SIMD a where
    pv :: a -> Doc ann
    pq :: a -> Doc ann

instance SIMD F2Reg where
    pv V0 = "v0"; pv V1 = "v1"; pv V2 = "v2"; pv V3 = "v3"; pv V4 = "v4"; pv V5 = "v5"; pv V6 = "v6"; pv V7 = "v7"
    pv V8 = "v8"; pv V9 = "v9"; pv V10 = "v10"; pv V11 = "v11"; pv V12 = "v12"; pv V13 = "v13"; pv V14 = "v14"; pv V15 = "v15"
    pv V16 = "v16"; pv V17 = "v17"; pv V18 = "v18"; pv V19 = "v19"; pv V20 = "v20"; pv V21 = "v21"; pv V22 = "v22"; pv V23 = "v23"
    pv V24 = "v24"; pv V25 = "v25"; pv V26 = "v26"; pv V27 = "v27"; pv V28 = "v28"; pv V29 = "v29"; pv V30 = "v30"; pv V31 = "v31"

    pq V0 = "q0"; pq V1 = "q1"; pq V2 = "q2"; pq V3 = "q3"; pq V4 = "q4"; pq V5 = "q5"; pq V6 = "q6"; pq V7 = "q7"
    pq V8 = "q8"; pq V9 = "q9"; pq V10 = "q10"; pq V11 = "q11"; pq V12 = "q12"; pq V13 = "q13"; pq V14 = "q14"; pq V15 = "q15"
    pq V16 = "q16"; pq V17 = "q17"; pq V18 = "q18"; pq V19 = "q19"; pq V20 = "q20"; pq V21 = "q21"; pq V22 = "q22"; pq V23 = "q23"
    pq V24 = "q24"; pq V25 = "q25"; pq V26 = "q26"; pq V27 = "q27"; pq V28 = "q28"; pq V29 = "q29"; pq V30 = "q30"; pq V31 = "q31"

instance NFData AReg where
instance NFData FAReg where
instance NFData F2Reg where

data AbsReg = IReg !Int | CArg0 | CArg1 | CArg2 | CArg3 | CArg4 | CArg5 | CArg6 | CArg7 | LR | FP | ASP
-- r0-r7 used for return values as well

instance Pretty AbsReg where
    pretty (IReg i) = "T" <> pretty i
    pretty LR       = "LR"
    pretty ASP      = "SP"
    pretty CArg0    = "X0"
    pretty CArg1    = "X1"
    pretty CArg2    = "X2"
    pretty CArg3    = "X3"
    pretty CArg4    = "X4"
    pretty CArg5    = "X5"
    pretty CArg6    = "X6"
    pretty CArg7    = "X7"
    pretty FP       = "FP"

data F2Abs = F2Reg !Int

instance SIMD F2Abs where pq (F2Reg i) = "~Q" <> pretty i; pv (F2Reg i) = "~V" <> pretty i

data FAbsReg = FReg !Int | FArg0 | FArg1 | FArg2 | FArg3 | FArg4 | FArg5 | FArg6 | FArg7

instance Pretty FAbsReg where
    pretty (FReg i) = "F" <> pretty i
    pretty FArg0    = "D0"
    pretty FArg1    = "D1"
    pretty FArg2    = "D2"
    pretty FArg3    = "D3"
    pretty FArg4    = "D4"
    pretty FArg5    = "D5"
    pretty FArg6    = "D6"
    pretty FArg7    = "D7"

toInt :: AbsReg -> Int
toInt CArg0    = 0
toInt CArg1    = 1
toInt CArg2    = 2
toInt CArg3    = 3
toInt CArg4    = 4
toInt CArg5    = 5
toInt CArg6    = 6
toInt CArg7    = 7
toInt LR       = 8
toInt ASP      = 9
toInt FP       = 18
toInt (IReg i) = 19+i

fToInt :: FAbsReg -> Int
fToInt FArg0    = 10
fToInt FArg1    = 11
fToInt FArg2    = 12
fToInt FArg3    = 13
fToInt FArg4    = 14
fToInt FArg5    = 15
fToInt FArg6    = 16
fToInt FArg7    = 17
fToInt (FReg i) = 19+i

f2ToInt :: F2Abs -> Int
f2ToInt (F2Reg i) = 19+i

data Shift = Zero | Three

instance NFData Shift where rnf Zero = (); rnf Three = ()

instance Pretty Shift where
    pretty Zero = "#0"; pretty Three = "#3"

-- left: shift left by this much
data BM = BM { ims, left :: !Word8 } deriving Eq

instance NFData BM where rnf (BM i ls) = rnf i `seq` rnf ls

instance Pretty BM where
    pretty (BM m l) = "0b" <> pretty (replicate (fromIntegral m) '1' ++ replicate (fromIntegral l) '0')

data Addr reg = R reg | RP reg Word16 | BI reg reg Shift deriving (Functor, Generic)

instance NFData a => NFData (Addr a) where

instance Pretty reg => Pretty (Addr reg) where
    pretty (R r)      = brackets (pretty r)
    pretty (RP r 0)   = brackets (pretty r)
    pretty (RP r u)   = brackets (pretty r <> "," <+> hexd u)
    pretty (BI b i s) = brackets (pretty b <> "," <+> pretty i <> "," <+> "LSL" <+> pretty s)

data Cond = Eq | Neq | Geq | Lt | Gt | Leq

instance NFData Cond where rnf Eq=(); rnf Neq=(); rnf Geq=(); rnf Lt=(); rnf Gt=(); rnf Leq=()

instance Pretty Cond where
    pretty Eq = "EQ"; pretty Neq = "NE"; pretty Geq = "GE"
    pretty Lt = "LT"; pretty Gt = "GT"; pretty Leq = "LE"

pSym :: Pretty a => a -> Doc ann
pSym = case os of {"linux" -> id; "darwin" -> ("_"<>)}.pretty

-- https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions
data AArch64 reg freg f2 a = Label { ann :: a, label :: Label }
                         | B { ann :: a, label :: Label }
                         | Blr { ann :: a, rSrc :: reg }
                         | C { ann :: a, label :: Label }
                         | Bl { ann :: a, cfunc :: CFunc }
                         | Bc { ann :: a, cond :: Cond, label :: Label }
                         | Ret { ann :: a }                                              | RetL { ann :: a, label :: Label }
                         | FMovXX { ann :: a, dDest, dSrc :: freg }
                         | FMovDR { ann :: a, dDest :: freg, rSrc :: reg }
                         | MovRR { ann :: a, rDest, rSrc :: reg }
                         | MovRC { ann :: a, rDest :: reg, cSrc :: Word16 }
                         | MovZ { ann :: a, rDest :: reg, cSrc :: Word16, lsl :: Int }
                         | MovRCf { ann :: a, rDest :: reg, cfunc :: CFunc }
                         | LdrRL { ann :: a, rDest :: reg, lSrc :: Int }
                         | MovK { ann :: a, rDest :: reg, cSrc :: Word16, lsl :: Int }
                         | Ldr { ann :: a, rDest :: reg, aSrc :: Addr reg }
                         | LdrB { ann :: a, rDest :: reg, aSrc :: Addr reg }
                         | Str { ann :: a, rSrc :: reg, aDest :: Addr reg }
                         | StrB { ann :: a, rSrc :: reg, aDest :: Addr reg }
                         | LdrD { ann :: a, dDest :: freg, aSrc :: Addr reg }
                         | StrD { ann :: a, dSrc :: freg, aDest :: Addr reg }
                         | SubRR { ann :: a, rDest, rSrc1, rSrc2 :: reg }
                         | AddRR { ann :: a, rDest, rSrc1, rSrc2 :: reg }
                         | AddRRS { ann :: a, rDest, rSrc1, rSrc2 :: reg, sC :: Word8 }
                         | ZeroR { ann :: a, rDest :: reg }
                         | Mvn { ann :: a, rDest, rSrc :: reg }
                         | AndRR { ann :: a, rDest, rSrc1, rSrc2 :: reg }
                         | OrRR { ann :: a, rDest, rSrc1, rSrc2 :: reg }
                         | Eor { ann :: a, rDest, rSrc1, rSrc2 :: reg }
                         | MulRR { ann :: a, rDest, rSrc1, rSrc2 :: reg }
                         | Madd { ann :: a, rDest, rSrc1, rSrc2, rSrc3 :: reg }
                         | Msub { ann :: a, rDest, rSrc1, rSrc2, rSrc3 :: reg }
                         | Sdiv { ann :: a, rDest, rSrc1, rSrc2 :: reg }
                         | AddRC { ann :: a, rDest, rSrc :: reg, rC :: Word16 }
                         | SubRC { ann :: a, rDest, rSrc :: reg, rC :: Word16 }
                         | Lsl { ann :: a, rDest, rSrc :: reg, sC :: Word8 }
                         | Asr { ann :: a, rDest, rSrc :: reg, sC :: Word8 }
                         | CmpRC { ann :: a, rSrc :: reg, cSrc :: Word16 }
                         | CmpRR { ann :: a, rSrc1, rSrc2 :: reg }
                         | Neg { ann :: a, rDest, rSrc :: reg }
                         | Fmul { ann :: a, dDest, dSrc1, dSrc2 :: freg }
                         | Fadd { ann :: a, dDest, dSrc1, dSrc2 :: freg }
                         | Fsub { ann :: a, dDest, dSrc1, dSrc2 :: freg }
                         | Fdiv { ann :: a, dDest, dSrc1, dSrc2 :: freg }
                         | FcmpZ { ann :: a, dSrc :: freg }
                         | Fcmp { ann :: a, dSrc1, dSrc2 :: freg }
                         | Fneg { ann :: a, dDest, dSrc :: freg }
                         | Scvtf { ann :: a, dDest :: freg, rSrc :: reg }
                         | Fcvtms { ann :: a, rDest :: reg, dSrc :: freg }
                         | Fcvtas { ann :: a, rDest :: reg, dSrc :: freg }
                         | Stp { ann :: a, rSrc1, rSrc2 :: reg, aDest :: Addr reg }
                         | Ldp { ann :: a, rDest1, rDest2 :: reg, aSrc :: Addr reg }
                         | Stp2 { ann :: a, r2Src1, r2Src2 :: f2, aDest :: Addr reg }
                         | Ldp2 { ann :: a, r2Dest1, r2Dest2 :: f2, aRc :: Addr reg }
                         | StpD { ann :: a, dSrc1, dSrc2 :: freg, aDest :: Addr reg }
                         | LdpD { ann :: a, dDest1, dDest2 :: freg, aSrc :: Addr reg }
                         | Fmadd { ann :: a, dDest, dSrc1, dSrc2, dSrc3 :: freg }
                         | Fmsub { ann :: a, dDest, dSrc1, dSrc2, dSrc3 :: freg }
                         | Fsqrt { ann :: a, dDest, dSrc :: freg }
                         | Frintm { ann :: a, dDest, dSrc :: freg }
                         | MrsR { ann :: a, rDest :: reg }
                         | Fmax { ann :: a, dDest, dSrc1, dSrc2 :: freg }
                         | Fmin { ann :: a, dDest, dSrc1, dSrc2 :: freg }
                         | Fabs { ann :: a, dDest, dSrc :: freg }
                         | Csel { ann :: a, rDest, rSrc1, rSrc2 :: reg, cond :: Cond }
                         | Tbnz { ann :: a, rSrc :: reg, bit :: Word8, label :: Label }
                         | Tbz { ann :: a, rSrc :: reg, bit :: Word8, label :: Label }
                         | Cbnz { ann :: a, rSrc :: reg, label :: Label }
                         | Cbz { ann :: a, rSrc :: reg, label :: Label }
                         | Fcsel { ann :: a, dDest, dSrc1, dSrc2 :: freg, cond :: Cond }
                         | Cset { ann :: a, rDest :: reg, cond :: Cond }
                         | TstI { ann :: a, rSrc1 :: reg, imm :: BM }
                         | EorI { ann :: a, rSrc, rDesg :: reg, imm :: BM }
                         deriving (Functor, Generic)

instance (NFData r, NFData d, NFData x, NFData a) => NFData (AArch64 r d x a) where

instance Copointed (AArch64 reg freg f2) where copoint = ann

mapR :: (areg -> reg) -> AArch64 areg afreg af2 a -> AArch64 reg afreg af2 a
mapR _ (Label x l)           = Label x l
mapR _ (B x l)               = B x l
mapR _ (Bc x c l)            = Bc x c l
mapR _ (Bl x f)              = Bl x f
mapR _ (C x l)               = C x l
mapR _ (FMovXX l r0 r1)      = FMovXX l r0 r1
mapR f (MovRR l r0 r1)       = MovRR l (f r0) (f r1)
mapR f (MovRC l r c)         = MovRC l (f r) c
mapR f (Ldr l r a)           = Ldr l (f r) (f <$> a)
mapR f (LdrB l r a)          = LdrB l (f r) (f <$> a)
mapR f (Str l r a)           = Str l (f r) (f <$> a)
mapR f (StrB l r a)          = StrB l (f r) (f<$>a)
mapR f (LdrD l xr a)         = LdrD l xr (f <$> a)
mapR f (AddRR l r0 r1 r2)    = AddRR l (f r0) (f r1) (f r2)
mapR f (AddRRS l r0 r1 r2 s) = AddRRS l (f r0) (f r1) (f r2) s
mapR f (SubRR l r0 r1 r2)    = SubRR l (f r0) (f r1) (f r2)
mapR f (AddRC l r0 r1 c)     = AddRC l (f r0) (f r1) c
mapR f (SubRC l r0 r1 c)     = SubRC l (f r0) (f r1) c
mapR f (ZeroR l r)           = ZeroR l (f r)
mapR f (Mvn l r0 r1)         = Mvn l (f r0) (f r1)
mapR f (AndRR l r0 r1 r2)    = AndRR l (f r0) (f r1) (f r2)
mapR f (OrRR l r0 r1 r2)     = OrRR l (f r0) (f r1) (f r2)
mapR f (Eor l r0 r1 r2)      = Eor l (f r0) (f r1) (f r2)
mapR f (Lsl l r0 r1 s)       = Lsl l (f r0) (f r1) s
mapR f (Asr l r0 r1 s)       = Asr l (f r0) (f r1) s
mapR f (CmpRR l r0 r1)       = CmpRR l (f r0) (f r1)
mapR f (CmpRC l r c)         = CmpRC l (f r) c
mapR f (Neg l r0 r1)         = Neg l (f r0) (f r1)
mapR _ (Fadd l xr0 xr1 xr2)  = Fadd l xr0 xr1 xr2
mapR _ (Fsub l xr0 xr1 xr2)  = Fsub l xr0 xr1 xr2
mapR _ (Fmul l xr0 xr1 xr2)  = Fmul l xr0 xr1 xr2
mapR _ (Fneg l xr0 xr1)      = Fneg l xr0 xr1
mapR _ (FcmpZ l xr)          = FcmpZ l xr
mapR _ (Ret l)               = Ret l
mapR _ (RetL x l)            = RetL x l
mapR f (MulRR l r0 r1 r2)    = MulRR l (f r0) (f r1) (f r2)
mapR f (Madd l r0 r1 r2 r3)  = Madd l (f r0) (f r1) (f r2) (f r3)
mapR f (Msub l r0 r1 r2 r3)  = Msub l (f r0) (f r1) (f r2) (f r3)
mapR f (Sdiv l r0 r1 r2)     = Sdiv l (f r0) (f r1) (f r2)
mapR f (StrD l d a)          = StrD l d (f <$> a)
mapR _ (Fdiv l d0 d1 d2)     = Fdiv l d0 d1 d2
mapR f (Scvtf l d r)         = Scvtf l d (f r)
mapR f (Fcvtms l r d)        = Fcvtms l (f r) d
mapR f (Fcvtas l r d)        = Fcvtas l (f r) d
mapR f (MovK l r u s)        = MovK l (f r) u s
mapR f (MovZ l r u s)        = MovZ l (f r) u s
mapR f (FMovDR l d r)        = FMovDR l d (f r)
mapR _ (Fcmp l d0 d1)        = Fcmp l d0 d1
mapR f (Ldp l r0 r1 a)       = Ldp l (f r0) (f r1) (f <$> a)
mapR f (Stp l r0 r1 a)       = Stp l (f r0) (f r1) (f <$> a)
mapR f (LdpD l d0 d1 a)      = LdpD l d0 d1 (f <$> a)
mapR f (StpD l d0 d1 a)      = StpD l d0 d1 (f <$> a)
mapR _ (Fmadd l d0 d1 d2 d3) = Fmadd l d0 d1 d2 d3
mapR _ (Fmsub l d0 d1 d2 d3) = Fmsub l d0 d1 d2 d3
mapR _ (Fsqrt l d0 d1)       = Fsqrt l d0 d1
mapR _ (Frintm l d0 d1)      = Frintm l d0 d1
mapR f (MrsR l r)            = MrsR l (f r)
mapR f (MovRCf l r cf)       = MovRCf l (f r) cf
mapR f (LdrRL x r l)         = LdrRL x (f r) l
mapR f (Blr l r)             = Blr l (f r)
mapR _ (Fmax l d0 d1 d2)     = Fmax l d0 d1 d2
mapR _ (Fmin l d0 d1 d2)     = Fmin l d0 d1 d2
mapR _ (Fabs l d0 d1)        = Fabs l d0 d1
mapR f (Csel l r0 r1 r2 p)   = Csel l (f r0) (f r1) (f r2) p
mapR f (Tbnz l r n p)        = Tbnz l (f r) n p
mapR f (Tbz l r n p)         = Tbz l (f r) n p
mapR f (Cbnz x r l)          = Cbnz x (f r) l
mapR f (Cbz x r l)           = Cbz x (f r) l
mapR _ (Fcsel l d0 d1 d2 p)  = Fcsel l d0 d1 d2 p
mapR f (TstI l r i)          = TstI l (f r) i
mapR f (Cset l r c)          = Cset l (f r) c
mapR f (EorI l r0 r1 i)      = EorI l (f r0) (f r1) i
mapR f (Ldp2 l r0 r1 a)      = Ldp2 l r0 r1 (f<$>a)
mapR f (Stp2 l r0 r1 a)      = Stp2 l r0 r1 (f<$>a)

mapF2 :: (af2 -> f2) -> AArch64 areg afreg af2 a -> AArch64 areg afreg f2 a
mapF2 _ (Label x l)           = Label x l
mapF2 _ (B x l)               = B x l
mapF2 _ (Bc x c l)            = Bc x c l
mapF2 _ (Bl x f)              = Bl x f
mapF2 _ (C x l)               = C x l
mapF2 _ (FMovXX l xr0 xr1)    = FMovXX l xr0 xr1
mapF2 _ (MovRR l r0 r1)       = MovRR l r0 r1
mapF2 _ (MovRC l r0 c)        = MovRC l r0 c
mapF2 _ (Ldr l r a)           = Ldr l r a
mapF2 _ (LdrB l r a)          = LdrB l r a
mapF2 _ (Str l r a)           = Str l r a
mapF2 _ (StrB l r a)          = StrB l r a
mapF2 _ (LdrD l xr a)         = LdrD l xr a
mapF2 _ (AddRR l r0 r1 r2)    = AddRR l r0 r1 r2
mapF2 _ (AddRRS l r0 r1 r2 s) = AddRRS l r0 r1 r2 s
mapF2 _ (AddRC l r0 r1 c)     = AddRC l r0 r1 c
mapF2 _ (SubRR l r0 r1 r2)    = SubRR l r0 r1 r2
mapF2 _ (SubRC l r0 r1 c)     = SubRC l r0 r1 c
mapF2 _ (ZeroR l r)           = ZeroR l r
mapF2 _ (Mvn l r0 r1)         = Mvn l r0 r1
mapF2 _ (AndRR l r0 r1 r2)    = AndRR l r0 r1 r2
mapF2 _ (OrRR l r0 r1 r2)     = OrRR l r0 r1 r2
mapF2 _ (Eor l r0 r1 r2)      = Eor l r0 r1 r2
mapF2 _ (EorI l r0 r1 i)      = EorI l r0 r1 i
mapF2 _ (Lsl l r0 r1 s)       = Lsl l r0 r1 s
mapF2 _ (Asr l r0 r1 s)       = Asr l r0 r1 s
mapF2 _ (CmpRC l r c)         = CmpRC l r c
mapF2 _ (CmpRR l r0 r1)       = CmpRR l r0 r1
mapF2 _ (Neg l r0 r1)         = Neg l r0 r1
mapF2 _ (Fmul l xr0 xr1 xr2)  = Fmul l xr0 xr1 xr2
mapF2 _ (Fadd l xr0 xr1 xr2)  = Fadd l xr0 xr1 xr2
mapF2 _ (Fsub l xr0 xr1 xr2)  = Fsub l xr0 xr1 xr2
mapF2 _ (FcmpZ l xr)          = FcmpZ l xr
mapF2 _ (Ret l)               = Ret l
mapF2 _ (RetL x l)            = RetL x l
mapF2 _ (Fdiv l d0 d1 d2)     = Fdiv l d0 d1 d2
mapF2 _ (MulRR l r0 r1 r2)    = MulRR l r0 r1 r2
mapF2 _ (Madd l r0 r1 r2 r3)  = Madd l r0 r1 r2 r3
mapF2 _ (Msub l r0 r1 r2 r3)  = Msub l r0 r1 r2 r3
mapF2 _ (Sdiv l r0 r1 r2)     = Sdiv l r0 r1 r2
mapF2 _ (StrD l d a)          = StrD l d a
mapF2 _ (Scvtf l d r)         = Scvtf l d r
mapF2 _ (Fcvtms l r d)        = Fcvtms l r d
mapF2 _ (Fcvtas l r d)        = Fcvtas l r d
mapF2 _ (MovK l r u s)        = MovK l r u s
mapF2 _ (MovZ l r u s)        = MovZ l r u s
mapF2 _ (FMovDR l d r)        = FMovDR l d r
mapF2 _ (Fcmp l d0 d1)        = Fcmp l d0 d1
mapF2 _ (Stp l r0 r1 a)       = Stp l r0 r1 a
mapF2 _ (Ldp l r0 r1 a)       = Ldp l r0 r1 a
mapF2 _ (StpD l d0 d1 a)      = StpD l d0 d1 a
mapF2 _ (LdpD l d0 d1 a)      = LdpD l d0 d1 a
mapF2 _ (Fmadd l d0 d1 d2 d3) = Fmadd l d0 d1 d2 d3
mapF2 _ (Fmsub l d0 d1 d2 d3) = Fmsub l d0 d1 d2 d3
mapF2 _ (Fsqrt l d0 d1)       = Fsqrt l d0 d1
mapF2 _ (Fneg l d0 d1)        = Fneg l d0 d1
mapF2 _ (Frintm l d0 d1)      = Frintm l d0 d1
mapF2 _ (MrsR l r)            = MrsR l r
mapF2 _ (Blr l r)             = Blr l r
mapF2 _ (MovRCf l r cf)       = MovRCf l r cf
mapF2 _ (LdrRL x r l)         = LdrRL x r l
mapF2 _ (Fmax l d0 d1 d2)     = Fmax l d0 d1 d2
mapF2 _ (Fmin l d0 d1 d2)     = Fmin l d0 d1 d2
mapF2 _ (Fabs l d0 d1)        = Fabs l d0 d1
mapF2 _ (Csel l r0 r1 r2 p)   = Csel l r0 r1 r2 p
mapF2 _ (Tbnz l r n p)        = Tbnz l r n p
mapF2 _ (Tbz l r n p)         = Tbz l r n p
mapF2 _ (Cbnz x r l)          = Cbnz x r l
mapF2 _ (Cbz x r l)           = Cbz x r l
mapF2 _ (Fcsel l d0 d1 d2 p)  = Fcsel l d0 d1 d2 p
mapF2 _ (TstI l r i)          = TstI l r i
mapF2 _ (Cset l r c)          = Cset l r c
mapF2 f (Ldp2 l r0 r1 a)      = Ldp2 l (f r0) (f r1) a
mapF2 f (Stp2 l r0 r1 a)      = Stp2 l (f r0) (f r1) a

mapFR :: (afreg -> freg) -> AArch64 areg afreg af2 a -> AArch64 areg freg af2 a
mapFR _ (Label x l)           = Label x l
mapFR _ (B x l)               = B x l
mapFR _ (Bc x c l)            = Bc x c l
mapFR _ (Bl x f)              = Bl x f
mapFR _ (C x l)               = C x l
mapFR f (FMovXX l xr0 xr1)    = FMovXX l (f xr0) (f xr1)
mapFR _ (MovRR l r0 r1)       = MovRR l r0 r1
mapFR _ (MovRC l r0 c)        = MovRC l r0 c
mapFR _ (Ldr l r a)           = Ldr l r a
mapFR _ (LdrB l r a)          = LdrB l r a
mapFR _ (Str l r a)           = Str l r a
mapFR _ (StrB l r a)          = StrB l r a
mapFR f (LdrD l xr a)         = LdrD l (f xr) a
mapFR _ (AddRR l r0 r1 r2)    = AddRR l r0 r1 r2
mapFR _ (AddRRS l r0 r1 r2 s) = AddRRS l r0 r1 r2 s
mapFR _ (AddRC l r0 r1 c)     = AddRC l r0 r1 c
mapFR _ (SubRR l r0 r1 r2)    = SubRR l r0 r1 r2
mapFR _ (SubRC l r0 r1 c)     = SubRC l r0 r1 c
mapFR _ (ZeroR l r)           = ZeroR l r
mapFR _ (Mvn l r0 r1)         = Mvn l r0 r1
mapFR _ (AndRR l r0 r1 r2)    = AndRR l r0 r1 r2
mapFR _ (OrRR l r0 r1 r2)     = OrRR l r0 r1 r2
mapFR _ (Eor l r0 r1 r2)      = Eor l r0 r1 r2
mapFR _ (EorI l r0 r1 i)      = EorI l r0 r1 i
mapFR _ (Lsl l r0 r1 s)       = Lsl l r0 r1 s
mapFR _ (Asr l r0 r1 s)       = Asr l r0 r1 s
mapFR _ (CmpRC l r c)         = CmpRC l r c
mapFR _ (CmpRR l r0 r1)       = CmpRR l r0 r1
mapFR _ (Neg l r0 r1)         = Neg l r0 r1
mapFR f (Fmul l xr0 xr1 xr2)  = Fmul l (f xr0) (f xr1) (f xr2)
mapFR f (Fadd l xr0 xr1 xr2)  = Fadd l (f xr0) (f xr1) (f xr2)
mapFR f (Fsub l xr0 xr1 xr2)  = Fsub l (f xr0) (f xr1) (f xr2)
mapFR f (FcmpZ l xr)          = FcmpZ l (f xr)
mapFR _ (Ret l)               = Ret l
mapFR _ (RetL x l)            = RetL x l
mapFR f (Fdiv l d0 d1 d2)     = Fdiv l (f d0) (f d1) (f d2)
mapFR _ (MulRR l r0 r1 r2)    = MulRR l r0 r1 r2
mapFR _ (Madd l r0 r1 r2 r3)  = Madd l r0 r1 r2 r3
mapFR _ (Msub l r0 r1 r2 r3)  = Msub l r0 r1 r2 r3
mapFR _ (Sdiv l r0 r1 r2)     = Sdiv l r0 r1 r2
mapFR f (StrD l d a)          = StrD l (f d) a
mapFR f (Scvtf l d r)         = Scvtf l (f d) r
mapFR f (Fcvtms l r d)        = Fcvtms l r (f d)
mapFR f (Fcvtas l r d)        = Fcvtas l r (f d)
mapFR _ (MovK l r u s)        = MovK l r u s
mapFR _ (MovZ l r u s)        = MovZ l r u s
mapFR f (FMovDR l d r)        = FMovDR l (f d) r
mapFR f (Fcmp l d0 d1)        = Fcmp l (f d0) (f d1)
mapFR _ (Stp l r0 r1 a)       = Stp l r0 r1 a
mapFR _ (Ldp l r0 r1 a)       = Ldp l r0 r1 a
mapFR f (StpD l d0 d1 a)      = StpD l (f d0) (f d1) a
mapFR f (LdpD l d0 d1 a)      = LdpD l (f d0) (f d1) a
mapFR f (Fmadd l d0 d1 d2 d3) = Fmadd l (f d0) (f d1) (f d2) (f d3)
mapFR f (Fmsub l d0 d1 d2 d3) = Fmsub l (f d0) (f d1) (f d2) (f d3)
mapFR f (Fsqrt l d0 d1)       = Fsqrt l (f d0) (f d1)
mapFR f (Fneg l d0 d1)        = Fneg l (f d0) (f d1)
mapFR f (Frintm l d0 d1)      = Frintm l (f d0) (f d1)
mapFR _ (MrsR l r)            = MrsR l r
mapFR _ (Blr l r)             = Blr l r
mapFR _ (MovRCf l r cf)       = MovRCf l r cf
mapFR _ (LdrRL x r l)         = LdrRL x r l
mapFR f (Fmax l d0 d1 d2)     = Fmax l (f d0) (f d1) (f d2)
mapFR f (Fmin l d0 d1 d2)     = Fmin l (f d0) (f d1) (f d2)
mapFR f (Fabs l d0 d1)        = Fabs l (f d0) (f d1)
mapFR _ (Csel l r0 r1 r2 p)   = Csel l r0 r1 r2 p
mapFR _ (Tbnz l r n p)        = Tbnz l r n p
mapFR _ (Tbz l r n p)         = Tbz l r n p
mapFR _ (Cbnz x r l)          = Cbnz x r l
mapFR _ (Cbz x r l)           = Cbz x r l
mapFR f (Fcsel l d0 d1 d2 p)  = Fcsel l (f d0) (f d1) (f d2) p
mapFR _ (TstI l r i)          = TstI l r i
mapFR _ (Cset l r c)          = Cset l r c
mapFR _ (Ldp2 l q0 q1 a)      = Ldp2 l q0 q1 a
mapFR _ (Stp2 l q0 q1 a)      = Stp2 l q0 q1 a

s2 :: [a] -> [(a, Maybe a)]
s2 (r0:r1:rs) = (r0, Just r1):s2 rs
s2 [r]        = [(r, Nothing)]
s2 []         = []

offs :: [a] -> [Word16]
offs rs = scanl' (\i _ -> i+16) 0 rs

rsOffs :: [a] -> ([(a, Maybe a)], [Word16], Word16)
rsOffs rs = let ixs=offs rs in (s2 rs, ixs, last ixs)

pus, pos :: [AReg] -> [AArch64 AReg freg f2reg ()]
pus rs = let (pps, ixs, r) = rsOffs rs in SubRC () SP SP r:concat (zipWith go pps ixs)
  where go (r0, Just r1) ix = [Stp () r0 r1 (RP SP ix)]; go (r, Nothing) ix = [Str () r (RP SP ix)]
pos rs = let (pps, ixs, r) = rsOffs rs in concat (zipWith go pps ixs)++[AddRC () SP SP r]
  where go (r0, Just r1) ix = [Ldp () r0 r1 (RP SP ix)]; go (r, Nothing) ix = [Ldr () r (RP SP ix)]

puds, pods :: [freg] -> [AArch64 AReg freg f2reg ()]
puds rs = let (pps, ixs, r) = rsOffs rs in SubRC () SP SP r:concat (zipWith go pps ixs)
  where go (r0, Just r1) ix = [StpD () r0 r1 (RP SP ix)]; go (r, Nothing) ix = [StrD () r (RP SP ix)]
pods rs = let (pps, ixs, r) = rsOffs rs in concat (zipWith go pps ixs)++[AddRC () SP SP r]
  where go (r0, Just r1) ix = [LdpD () r0 r1 (RP SP ix)]; go (r, Nothing) ix = [LdrD () r (RP SP ix)]

hexd :: Integral a => a -> Doc ann
hexd = pretty.($"").(("#0x"++).).showHex

instance (Pretty reg, Pretty freg, SIMD f2reg) => Pretty (AArch64 reg freg f2reg a) where
    pretty (Label _ l)            = prettyLabel l <> ":"
    pretty (B _ l)                = i4 ("b" <+> prettyLabel l)
    pretty (Blr _ r)              = i4 ("blr" <+> pretty r)
    pretty (Bl _ l)               = i4 ("bl" <+> pSym l)
    pretty (C _ l)                = i4 ("call" <+> pretty l)
    pretty (Bc _ c l)             = i4 ("b." <> pretty c <+> prettyLabel l)
    pretty (FMovXX _ xr0 xr1)     = i4 ("fmov" <+> pretty xr0 <> "," <+> pretty xr1)
    pretty (FMovDR _ d r)         = i4 ("fmov" <+> pretty d <> "," <+> pretty r)
    pretty (MovRR _ r0 r1)        = i4 ("mov" <+> pretty r0 <> "," <+> pretty r1)
    pretty (MovRC _ r u)          = i4 ("mov" <+> pretty r <> "," <+> hexd u)
    pretty (Ldr _ r a)            = i4 ("ldr" <+> pretty r <> "," <+> pretty a)
    pretty (LdrB _ r a)           = i4 ("ldrb" <+> pretty r <> "," <+> pretty a)
    pretty (Str _ r a)            = i4 ("str" <+> pretty r <> "," <+> pretty a)
    pretty (StrB _ r a)           = i4 ("strb" <+> pretty r <> "," <+> pretty a)
    pretty (LdrD _ xr a)          = i4 ("ldr" <+> pretty xr <> "," <+> pretty a)
    pretty (StrD _ xr a)          = i4 ("str" <+> pretty xr <> "," <+> pretty a)
    pretty (AddRR _ rD rS rS')    = i4 ("add" <+> pretty rD <> "," <+> pretty rS <> "," <+> pretty rS')
    pretty (AddRRS _ rD rS rS' s) = i4 ("add" <+> pretty rD <> "," <+> pretty rS <> "," <+> pretty rS' <> "," <+> "LSL" <+> "#" <> pretty s)
    pretty (SubRR _ rD rS rS')    = i4 ("sub" <+> pretty rD <> "," <+> pretty rS <> "," <+> pretty rS')
    pretty (AndRR _ rD rS rS')    = i4 ("and" <+> pretty rD <> "," <+> pretty rS <> "," <+> pretty rS')
    pretty (OrRR _ rD rS rS')     = i4 ("orr" <+> pretty rD <> "," <+> pretty rS <> "," <+> pretty rS')
    pretty (Eor _ rD rS rS')      = i4 ("eor" <+> pretty rD <> "," <+> pretty rS <> "," <+> pretty rS')
    pretty (EorI _ rD rS i)       = i4 ("eor" <+> pretty rD <> "," <+> pretty rS <> "," <+> pretty i)
    pretty (ZeroR _ rD)           = i4 ("eor" <+> pretty rD <> "," <+> pretty rD <> "," <+> pretty rD)
    pretty (Mvn _ rD rS)          = i4 ("mvn" <+> pretty rD <> "," <+> pretty rS)
    pretty (MulRR _ rD rS rS')    = i4 ("mul" <+> pretty rD <> "," <+> pretty rS <> "," <+> pretty rS')
    pretty (SubRC _ rD rS u)      = i4 ("sub" <+> pretty rD <> "," <+> pretty rS <> "," <+> hexd u)
    pretty (AddRC _ rD rS u)      = i4 ("add" <+> pretty rD <> "," <+> pretty rS <> "," <+> hexd u)
    pretty (Lsl _ rD rS u)        = i4 ("lsl" <+> pretty rD <> "," <+> pretty rS <> "," <+> hexd u)
    pretty (Asr _ rD rS u)        = i4 ("asr" <+> pretty rD <> "," <+> pretty rS <> "," <+> hexd u)
    pretty (CmpRC _ r u)          = i4 ("cmp" <+> pretty r <> "," <+> hexd u)
    pretty (CmpRR _ r0 r1)        = i4 ("cmp" <+> pretty r0 <> "," <+> pretty r1)
    pretty (Neg _ rD rS)          = i4 ("neg" <+> pretty rD <> "," <+> pretty rS)
    pretty (Fmul _ rD r0 r1)      = i4 ("fmul" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (Fadd _ rD r0 r1)      = i4 ("fadd" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (Fsub _ rD r0 r1)      = i4 ("fsub" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (Fdiv _ rD r0 r1)      = i4 ("fdiv" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (FcmpZ _ xr)           = i4 ("fcmp" <+> pretty xr <> "," <+> "#0.0")
    pretty (Fneg _ d0 d1)         = i4 ("fneg" <+> pretty d0 <> "," <+> pretty d1)
    pretty Ret{}                  = i4 "ret"
    pretty RetL{}                 = i4 "ret"
    pretty (Scvtf _ d r)          = i4 ("scvtf" <+> pretty d <> "," <+> pretty r)
    pretty (Fcvtms _ r d)         = i4 ("fcvtms" <+> pretty r <> "," <+> pretty d)
    pretty (Fcvtas _ r d)         = i4 ("fcvtas" <+> pretty r <> "," <+> pretty d)
    pretty (MovK _ r i s)         = i4 ("movk" <+> pretty r <> "," <+> hexd i <> "," <+> "LSL" <+> "#" <> pretty s)
    pretty (MovZ _ r i s)         = i4 ("movz" <+> pretty r <> "," <+> hexd i <> "," <+> "LSL" <+> "#" <> pretty s)
    pretty (Fcmp _ d0 d1)         = i4 ("fcmp" <+> pretty d0 <> "," <+> pretty d1)
    pretty (Stp _ r0 r1 a)        = i4 ("stp" <+> pretty r0 <> "," <+> pretty r1 <> "," <+> pretty a)
    pretty (Ldp _ r0 r1 a)        = i4 ("ldp" <+> pretty r0 <> "," <+> pretty r1 <> "," <+> pretty a)
    pretty (Ldp2 _ q0 q1 a)       = i4 ("ldp" <+> pq q0 <> "," <+> pq q1 <> "," <+> pretty a)
    pretty (Stp2 _ q0 q1 a)       = i4 ("stp" <+> pq q0 <> "," <+> pq q1 <> "," <+> pretty a)
    pretty (StpD _ d0 d1 a)       = i4 ("stp" <+> pretty d0 <> "," <+> pretty d1 <> "," <+> pretty a)
    pretty (LdpD _ d0 d1 a)       = i4 ("ldp" <+> pretty d0 <> "," <+> pretty d1 <> "," <+> pretty a)
    pretty (Fmadd _ d0 d1 d2 d3)  = i4 ("fmadd" <+> pretty d0 <> "," <+> pretty d1 <> "," <+> pretty d2 <> "," <+> pretty d3)
    pretty (Fmsub _ d0 d1 d2 d3)  = i4 ("fmsub" <+> pretty d0 <> "," <+> pretty d1 <> "," <+> pretty d2 <> "," <+> pretty d3)
    pretty (Madd _ r0 r1 r2 r3)   = i4 ("madd" <+> pretty r0 <> "," <+> pretty r1 <> "," <+> pretty r2 <> "," <+> pretty r3)
    pretty (Msub _ r0 r1 r2 r3)   = i4 ("msub" <+> pretty r0 <> "," <+> pretty r1 <> "," <+> pretty r2 <> "," <+> pretty r3)
    pretty (Sdiv _ rD rS rS')     = i4 ("sdiv" <+> pretty rD <> "," <+> pretty rS <> "," <+> pretty rS')
    pretty (Fsqrt _ d0 d1)        = i4 ("fsqrt" <+> pretty d0 <> "," <+> pretty d1)
    pretty (Frintm _ d0 d1)       = i4 ("frintm" <+> pretty d0 <> "," <+> pretty d1)
    pretty (MrsR _ r)             = i4 ("mrs" <+> pretty r <> "," <+> "rndr")
    pretty (MovRCf _ r cf)        = i4 ("mov" <+> pretty r <> "," <+> pretty cf)
    pretty (LdrRL _ r l)          = i4 ("ldr" <+> pretty r <> "," <+> "=arr_" <> pretty l)
    pretty (Fmax _ d0 d1 d2)      = i4 ("fmax" <+> pretty d0 <> "," <+> pretty d1 <> "," <+> pretty d2)
    pretty (Fmin _ d0 d1 d2)      = i4 ("fmin" <+> pretty d0 <> "," <+> pretty d1 <> "," <+> pretty d2)
    pretty (Fabs _ d0 d1)         = i4 ("fabs" <+> pretty d0 <> "," <+> pretty d1)
    pretty (Csel _ r0 r1 r2 p)    = i4 ("csel" <+> pretty r0 <> "," <+> pretty r1 <> "," <+> pretty r2 <> "," <+> pretty p)
    pretty (Tbnz _ r n l)         = i4 ("tbnz" <+> pretty r <> "," <+> "#" <> pretty n <> "," <+> prettyLabel l)
    pretty (Tbz _ r n l)          = i4 ("tbz" <+> pretty r <> "," <+> "#" <> pretty n <> "," <+> prettyLabel l)
    pretty (Cbnz _ r l)           = i4 ("cbnz" <+> pretty r <> "," <+> prettyLabel l)
    pretty (Cbz _ r l)            = i4 ("cbz" <+> pretty r <> "," <+> prettyLabel l)
    pretty (Fcsel _ d0 d1 d2 p)   = i4 ("fcsel" <+> pretty d0 <> "," <+> pretty d1 <> "," <+> pretty d2 <> "," <+> pretty p)
    pretty (TstI _ r i)           = i4 ("tst" <+> pretty r <> "," <+> pretty i)
    pretty (Cset _ r c)           = i4 ("cset" <+> pretty r <> "," <+> pretty c)

instance (Pretty reg, Pretty freg, SIMD f2reg) => Show (AArch64 reg freg f2reg a) where show=show.pretty

prettyLive :: (Pretty reg, Pretty freg, SIMD f2reg, Pretty o) => AArch64 reg freg f2reg o -> Doc ann
prettyLive r = pretty r <+> pretty (ann r)

prettyDebug :: (Pretty freg, Pretty reg, SIMD f2reg, Pretty o) => [AArch64 reg freg f2reg o] -> Doc ann
prettyDebug = prettyLines . fmap prettyLive
