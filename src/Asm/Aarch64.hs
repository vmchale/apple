{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Asm.Aarch64 ( AArch64 (..)
                   , Addr (..)
                   , Cond (..)
                   , Shift (..), ISl (..), BM (..)
                   , Pfop (..), TT (..), PT (..), CT (..)
                   , AbsReg (..), FAbsReg (..), F2Abs
                   , AReg (..), FAReg (..), V2Reg (..)
                   , SIMD (..)
                   , prettyDebug
                   , mapR, mapFR
                   , fR
                   , toInt, fToInt
                   , pus, pos
                   , puds, pods
                   , puxs, poxs
                   , pSym
                   ) where

import           Asm.M
import           Control.DeepSeq   (NFData (..), deepseq, rwhnf)
import           Data.Copointed
import           Data.Int          (Int16)
import           Data.Word         (Word16, Word8)
import           GHC.Generics      (Generic)
import           Numeric           (showHex)
import           Prettyprinter     (Doc, Pretty (..), brackets, (<+>))
import           Prettyprinter.Ext
import           Q
import           System.Info       (os)

-- https://developer.arm.com/documentation/102374/0101/Registers-in-AArch64---other-registers
data AReg = X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9 | X10 | X11 | X12 | X13 | X14 | X15 | X16 | X17 | X18 | X19 | X20 | X21 | X22 | X23 | X24 | X25 | X26 | X27 | X28 | X29 | X30 | SP deriving (Eq, Ord, Enum, Generic)

instance Pretty AReg where
    pretty X0 = "x0"; pretty X1 = "x1"; pretty X2 = "x2"; pretty X3 = "x3"; pretty X4 = "x4"; pretty X5 = "x5"; pretty X6 = "x6"; pretty X7 = "x7"
    pretty X8 = "x8"; pretty X9 = "x9"; pretty X10 = "x10"; pretty X11 = "x11"; pretty X12 = "x12"; pretty X13 = "x13"; pretty X14 = "x14"; pretty X15 = "x15"
    pretty X16 = "x16"; pretty X17 = "x17"; pretty X18 = "x18"; pretty X19 = "x19"; pretty X20 = "x20"; pretty X21 = "x21"; pretty X22 = "x22"; pretty X23 = "x23"
    pretty X24 = "x24"; pretty X25 = "x25"; pretty X26 = "x26"; pretty X27 = "x27"; pretty X28 = "x28"; pretty X29 = "x29"; pretty X30 = "x30"; pretty SP = "sp"

instance Show AReg where show = show.pretty

data FAReg = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 | D10 | D11 | D12 | D13 | D14 | D15 | D16 | D17 | D18 | D19 | D20 | D21 | D22 | D23 | D24 | D25 | D26 | D27 | D28 | D29 | D30 | D31 deriving (Eq, Ord, Enum, Generic)

instance Pretty FAReg where
    pretty D0 = "d0"; pretty D1 = "d1"; pretty D2 = "d2"; pretty D3 = "d3"; pretty D4 = "d4"; pretty D5 = "d5"; pretty D6 = "d6"; pretty D7 = "d7"
    pretty D8 = "d8"; pretty D9 = "d9"; pretty D10 = "d10"; pretty D11 = "d11"; pretty D12 = "d12"; pretty D13 = "d13"; pretty D14 = "d14"; pretty D15 = "d15"
    pretty D16 = "d16"; pretty D17 = "d17"; pretty D18 = "d18"; pretty D19 = "d19"; pretty D20 = "d20"; pretty D21 = "d21"; pretty D22 = "d22"; pretty D23 = "d23"
    pretty D24 = "d24"; pretty D25 = "d25"; pretty D26 = "d26"; pretty D27 = "d27"; pretty D28 = "d28"; pretty D29 = "d29"; pretty D30 = "d30"; pretty D31 = "d31"

instance Show FAReg where show=show.pretty

newtype V2Reg a = V2Reg { simd2 :: a } deriving (Eq, Ord, Enum, NFData, Functor)

class SIMD a where
    pv :: a -> Doc ann
    pq :: a -> Doc ann

instance SIMD (V2Reg FAReg) where
    pv (V2Reg D0) = "v0"; pv (V2Reg D1) = "v1"; pv (V2Reg D2) = "v2"; pv (V2Reg D3) = "v3"; pv (V2Reg D4) = "v4"; pv (V2Reg D5) = "v5"; pv (V2Reg D6) = "v6"; pv (V2Reg D7) = "v7"
    pv (V2Reg D8) = "v8"; pv (V2Reg D9) = "v9"; pv (V2Reg D10) = "v10"; pv (V2Reg D11) = "v11"; pv (V2Reg D12) = "v12"; pv (V2Reg D13) = "v13"; pv (V2Reg D14) = "v14"; pv (V2Reg D15) = "v15"
    pv (V2Reg D16) = "v16"; pv (V2Reg D17) = "v17"; pv (V2Reg D18) = "v18"; pv (V2Reg D19) = "v19"; pv (V2Reg D20) = "v20"; pv (V2Reg D21) = "v21"; pv (V2Reg D22) = "v22"; pv (V2Reg D23) = "v23"
    pv (V2Reg D24) = "v24"; pv (V2Reg D25) = "v25"; pv (V2Reg D26) = "v26"; pv (V2Reg D27) = "v27"; pv (V2Reg D28) = "v28"; pv (V2Reg D29) = "v29"; pv (V2Reg D30) = "v30"; pv (V2Reg D31) = "v31"

    pq (V2Reg D0) = "q0"; pq (V2Reg D1) = "q1"; pq (V2Reg D2) = "q2"; pq (V2Reg D3) = "q3"; pq (V2Reg D4) = "q4"; pq (V2Reg D5) = "q5"; pq (V2Reg D6) = "q6"; pq (V2Reg D7) = "q7"
    pq (V2Reg D8) = "q8"; pq (V2Reg D9) = "q9"; pq (V2Reg D10) = "q10"; pq (V2Reg D11) = "q11"; pq (V2Reg D12) = "q12"; pq (V2Reg D13) = "q13"; pq (V2Reg D14) = "q14"; pq (V2Reg D15) = "q15"
    pq (V2Reg D16) = "q16"; pq (V2Reg D17) = "q17"; pq (V2Reg D18) = "q18"; pq (V2Reg D19) = "q19"; pq (V2Reg D20) = "q20"; pq (V2Reg D21) = "q21"; pq (V2Reg D22) = "q22"; pq (V2Reg D23) = "q23"
    pq (V2Reg D24) = "q24"; pq (V2Reg D25) = "q25"; pq (V2Reg D26) = "q26"; pq (V2Reg D27) = "q27"; pq (V2Reg D28) = "q28"; pq (V2Reg D29) = "q29"; pq (V2Reg D30) = "q30"; pq (V2Reg D31) = "q31"

instance NFData AReg where
instance NFData FAReg where

class P32 a where
    pw :: a -> Doc ann

instance P32 AReg where
    pw X0 = "w0"; pw X1 = "w1"; pw X2 = "w2"; pw X3 = "w3"; pw X4 = "w4"; pw X5 = "w5"; pw X6 = "w6"; pw X7 = "w7"
    pw X8 = "w8"; pw X9 = "w9"; pw X10 = "w10"; pw X11 = "w11"; pw X12 = "w12"; pw X13 = "w13"; pw X14 = "w14"; pw X15 = "w15"
    pw X16 = "w16"; pw X17 = "w17"; pw X18 = "w18"; pw X19 = "w19"; pw X20 = "w20"; pw X21 = "w21"; pw X22 = "w22"; pw X23 = "w23"
    pw X24 = "w24"; pw X25 = "w25"; pw X26 = "w26"; pw X27 = "w27"; pw X28 = "w28"; pw X29 = "w29"; pw X30 = "w30"; pw SP = "sp"

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

instance P32 AbsReg where
    pw (IReg i) = "W" <> pretty i
    pw CArg0    = "W0"
    pw CArg1    = "W1"
    pw CArg2    = "W2"
    pw CArg3    = "W3"
    pw CArg4    = "W4"
    pw CArg5    = "W5"
    pw CArg6    = "W6"
    pw CArg7    = "W7"

type F2Abs = V2Reg FAbsReg

instance SIMD F2Abs where pq (V2Reg (FReg i)) = "~Q" <> pretty i; pv (V2Reg (FReg i)) = "~V" <> pretty i

data FAbsReg = FReg !Int | FArg0 | FArg1 | FArg2 | FArg3 | FArg4 | FArg5 | FArg6 | FArg7 deriving Eq

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

data TT = PLD | PLI | PST; data CT=L1|L2|L3; data PT=Keep|Strm

instance NFData PT where rnf=rwhnf
instance NFData TT where rnf=rwhnf
instance NFData CT where rnf=rwhnf

instance Pretty TT where pretty PLD="pld"; pretty PLI="pli"; pretty PST="pst"
instance Pretty CT where pretty L1="l1"; pretty L2="l2"; pretty L3="l3"
instance Pretty PT where pretty Keep="keep"; pretty Strm="strm"

data Pfop=Pfop !TT !CT !PT
instance Pretty Pfop where pretty (Pfop t c p) = pretty t <> pretty c <> pretty p
instance NFData Pfop where rnf (Pfop t c p) = t `deepseq` c `deepseq` p `deepseq` ()

data Shift = Zero | Three | Four

instance NFData Shift where rnf=rwhnf

instance Pretty Shift where
    pretty Zero = "#0"; pretty Three = "#3"; pretty Four = "#4"

data ISl = IZero | Twelve

instance NFData ISl where rnf=rwhnf

instance Pretty ISl where pretty IZero = "#0"; pretty Twelve="#0xc"

-- left: shift left by this much
data BM = BM { ims, left :: !Word8 }

instance NFData BM where rnf (BM i ls) = rnf i `seq` rnf ls

instance Pretty BM where
    pretty (BM m l) = "0b" <> pretty (replicate (fromIntegral m) '1' ++ replicate (fromIntegral l) '0')

data Addr reg = R reg | RP reg Word16 | BI reg reg Shift | Po reg Int16 | Pr reg Int16 deriving (Functor, Foldable, Generic)

instance NFData a => NFData (Addr a) where

instance Pretty reg => Pretty (Addr reg) where
    pretty (Po r 0)      = brackets (pretty r)
    pretty (Po r u)      = brackets (pretty r) <> "," <+> hexd u
    pretty (Pr r 0)      = brackets (pretty r)
    pretty (Pr r u)      = brackets (pretty r <> "," <+> hexd u) <> "!"
    pretty (R r)         = brackets (pretty r)
    pretty (RP r 0)      = brackets (pretty r)
    pretty (RP r u)      = brackets (pretty r <> "," <+> hexd u)
    pretty (BI b i Zero) = brackets (pretty b <> "," <+> pretty i)
    pretty (BI b i s)    = brackets (pretty b <> "," <+> pretty i <> "," <+> "LSL" <+> pretty s)

data Cond = Eq | Neq | Geq | Lt | Gt | Leq

instance NFData Cond where rnf Eq=(); rnf Neq=(); rnf Geq=(); rnf Lt=(); rnf Gt=(); rnf Leq=()

instance Pretty Cond where
    pretty Eq = "EQ"; pretty Neq = "NE"; pretty Geq = "GE"
    pretty Lt = "LT"; pretty Gt = "GT"; pretty Leq = "LE"

pSym :: Pretty a => a -> Doc ann
pSym = case os of {"linux" -> id; "darwin" -> ("_"<>)}.pretty

-- https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions
data AArch64 reg freg a = Label { ann :: a, label :: Label }
                         | B { ann :: a, label :: Label }
                         | Blr { ann :: a, rSrc :: reg }
                         | C { ann :: a, label :: Label }
                         | Bl { ann :: a, cfunc :: CFunc }
                         | Bc { ann :: a, cond :: Cond, label :: Label }
                         | Ret { ann :: a } | RetL { ann :: a, label :: Label }
                         | FMovXX { ann :: a, dDest, dSrc :: freg }
                         | FMovDR { ann :: a, dDest :: freg, rSrc :: reg }
                         | Dup { ann :: a, vDest :: V2Reg freg, rSrc :: reg }
                         | Ins { ann :: a, vDest :: V2Reg freg, vIx :: Word8, rSrc :: reg }
                         | DupD { ann :: a, vDest, vSrc :: V2Reg freg, qix :: !Word8 }
                         | MovRR { ann :: a, rDest, rSrc :: reg }
                         | MovQQ { ann :: a, qDest, qSrc :: V2Reg freg }
                         | MovRC { ann :: a, rDest :: reg, cSrc :: Word16 }
                         | MovZ { ann :: a, rDest :: reg, cSrc :: Word16, lsl :: Int }
                         | MovRCf { ann :: a, rDest :: reg, cfunc :: CFunc }
                         | LdrRL { ann :: a, rDest :: reg, lSrc :: Int }
                         | MovK { ann :: a, rDest :: reg, cSrc :: Word16, lsl :: Int }
                         | Ldr { ann :: a, rDest :: reg, aSrc :: Addr reg }
                         | LdrB { ann :: a, rDest :: reg, aSrc :: Addr reg }
                         | Str { ann :: a, rSrc :: reg, aDest :: Addr reg }
                         | StrB { ann :: a, rSrc :: reg, aDest :: Addr reg }
                         | StrS { ann :: a, qDest :: V2Reg freg, aSrc :: Addr reg }
                         | LdrD { ann :: a, dDest :: freg, aSrc :: Addr reg }
                         | StrD { ann :: a, dSrc :: freg, aDest :: Addr reg }
                         | LdrS { ann :: a, qDest :: V2Reg freg, aSrc :: Addr reg }
                         | SubRR { ann :: a, rDest, rSrc1, rSrc2 :: reg }
                         | AddRR { ann :: a, rDest, rSrc1, rSrc2 :: reg }
                         | AddRRS { ann :: a, rDest, rSrc1, rSrc2 :: reg, sC :: Word8 }
                         | ZeroR { ann :: a, rDest :: reg }
                         | AndRR { ann :: a, rDest, rSrc1, rSrc2 :: reg }
                         | OrRR { ann :: a, rDest, rSrc1, rSrc2 :: reg }
                         | Eor { ann :: a, rDest, rSrc1, rSrc2 :: reg }
                         | Eon { ann :: a, rDest, rSrc, rSrc2 :: reg }
                         | ZeroS { ann :: a, qDest :: V2Reg freg }
                         | ZeroD { ann :: a, dDest :: freg }
                         | EorS { ann :: a, qDest, qSrc1, qSrc2 :: V2Reg freg }
                         | EorD { ann :: a, dDest, dSrc1, dSrc2 :: freg }
                         | MulRR { ann :: a, rDest, rSrc1, rSrc2 :: reg }
                         | Madd { ann :: a, rDest, rSrc1, rSrc2, rSrc3 :: reg }
                         | Msub { ann :: a, rDest, rSrc1, rSrc2, rSrc3 :: reg }
                         | Sdiv { ann :: a, rDest, rSrc1, rSrc2 :: reg }
                         | AddRC { ann :: a, rDest, rSrc :: reg, rC :: Word16, s12 :: !ISl }
                         | SubRC { ann :: a, rDest, rSrc :: reg, rC :: Word16, s12 :: !ISl }
                         | SubsRC { ann :: a, rDest, rSrc :: reg, rC :: Word16 }
                         | Lsl { ann :: a, rDest, rSrc :: reg, sC :: Word8 }
                         | Asr { ann :: a, rDest, rSrc :: reg, sC :: Word8 }
                         | CmpRC { ann :: a, rSrc :: reg, cSrc :: Word16 }
                         | CmpRR { ann :: a, rSrc1, rSrc2 :: reg }
                         | Neg { ann :: a, rDest, rSrc :: reg }
                         | Fmul { ann :: a, dDest, dSrc1, dSrc2 :: freg }
                         | Fadd { ann :: a, dDest, dSrc1, dSrc2 :: freg }
                         | Fsub { ann :: a, dDest, dSrc1, dSrc2 :: freg }
                         | Fdiv { ann :: a, dDest, dSrc1, dSrc2 :: freg }
                         | Fadd2 { ann :: a, vDest, vSrc1, vSrc2 :: V2Reg freg }
                         | Fsub2 { ann :: a, vDest, vSrc1, vSrc2 :: V2Reg freg }
                         | Faddp { ann :: a, dDest :: freg, vSrc :: V2Reg freg }
                         | Fmaxp { ann :: a, dDest :: freg, vSrc :: V2Reg freg }
                         | Fminp { ann :: a, dDest :: freg, vSrc :: V2Reg freg }
                         | Fmul2 { ann :: a, vDest, vSrc1, vSrc2 :: V2Reg freg }
                         | Fdiv2 { ann :: a, vDest, vSrc1, vSrc2 :: V2Reg freg }
                         | Fmax2 { ann :: a, vDest, vSrc1, vSrc2 :: V2Reg freg }
                         | Fmin2 { ann :: a, vDest, vSrc1, vSrc2 :: V2Reg freg }
                         | Fsqrt2 { ann :: a, vDest, vSrc :: V2Reg freg }
                         | Fsqrte2 { ann :: a, vDest, vSrc :: V2Reg freg }
                         | Fneg2 { ann :: a, vDest, vSrc :: V2Reg freg }
                         | FcmpZ { ann :: a, dSrc :: freg }
                         | Fcmp { ann :: a, dSrc1, dSrc2 :: freg }
                         | Fneg { ann :: a, dDest, dSrc :: freg }
                         | Scvtf { ann :: a, dDest :: freg, rSrc :: reg }
                         | Fcvtms { ann :: a, rDest :: reg, dSrc :: freg }
                         | Fcvtas { ann :: a, rDest :: reg, dSrc :: freg }
                         | Stp { ann :: a, rSrc1, rSrc2 :: reg, aDest :: Addr reg }
                         | Ldp { ann :: a, rDest1, rDest2 :: reg, aSrc :: Addr reg }
                         | Stp2 { ann :: a, r2Src1, r2Src2 :: V2Reg freg, aDest :: Addr reg }
                         | Ldp2 { ann :: a, r2Dest1, r2Dest2 :: V2Reg freg, aRc :: Addr reg }
                         | StpD { ann :: a, dSrc1, dSrc2 :: freg, aDest :: Addr reg }
                         | LdpD { ann :: a, dDest1, dDest2 :: freg, aSrc :: Addr reg }
                         | Fmadd { ann :: a, dDest, dSrc1, dSrc2, dSrc3 :: freg }
                         | Fmsub { ann :: a, dDest, dSrc1, dSrc2, dSrc3 :: freg }
                         | Fsqrt { ann :: a, dDest, dSrc :: freg }
                         | Fsqrte { ann :: a, dDest, dSrc :: freg }
                         | Fmla { ann :: a, vDest, vSrc1, vSrc2 :: V2Reg freg }
                         | Fmls { ann :: a, vDest, vSrc1, vSrc2 :: V2Reg freg }
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
                         | EorI { ann :: a, rDest, rSrc :: reg, imm :: BM }
                         | Bfc { ann :: a, rDest :: reg, lsb :: Word8, width :: Word8 }
                         | Prfm { ann :: a, pro :: !Pfop, aSrc :: Addr reg }
                         deriving (Functor, Generic)

instance (NFData r, NFData d, NFData a) => NFData (AArch64 r d a) where

instance Copointed (AArch64 reg freg) where copoint = ann

mapR :: (areg -> reg) -> AArch64 areg afreg a -> AArch64 reg afreg a
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
mapR f (StrS l r a)          = StrS l r (f<$>a)
mapR f (LdrD l xr a)         = LdrD l xr (f <$> a)
mapR f (AddRR l r0 r1 r2)    = AddRR l (f r0) (f r1) (f r2)
mapR f (AddRRS l r0 r1 r2 s) = AddRRS l (f r0) (f r1) (f r2) s
mapR f (SubRR l r0 r1 r2)    = SubRR l (f r0) (f r1) (f r2)
mapR f (AddRC l r0 r1 c s)   = AddRC l (f r0) (f r1) c s
mapR f (SubRC l r0 r1 c s)   = SubRC l (f r0) (f r1) c s
mapR f (SubsRC l r0 r1 c)    = SubsRC l (f r0) (f r1) c
mapR f (ZeroR l r)           = ZeroR l (f r)
mapR f (AndRR l r0 r1 r2)    = AndRR l (f r0) (f r1) (f r2)
mapR f (OrRR l r0 r1 r2)     = OrRR l (f r0) (f r1) (f r2)
mapR f (Eor l r0 r1 r2)      = Eor l (f r0) (f r1) (f r2)
mapR f (Eon l r0 r1 r2)      = Eon l (f r0) (f r1) (f r2)
mapR f (Lsl l r0 r1 s)       = Lsl l (f r0) (f r1) s
mapR f (Asr l r0 r1 s)       = Asr l (f r0) (f r1) s
mapR f (CmpRR l r0 r1)       = CmpRR l (f r0) (f r1)
mapR f (CmpRC l r c)         = CmpRC l (f r) c
mapR f (Neg l r0 r1)         = Neg l (f r0) (f r1)
mapR _ (EorS l q0 q1 q2)     = EorS l q0 q1 q2
mapR _ (ZeroS l q)           = ZeroS l q
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
mapR _ (Fsqrte l d0 d1)      = Fsqrte l d0 d1
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
mapR f (Bfc x r l w)         = Bfc x (f r) l w
mapR f (LdrS l q a)          = LdrS l q (f<$>a)
mapR _ (Fadd2 l x0 x1 x2)    = Fadd2 l x0 x1 x2
mapR _ (Fsub2 l x0 x1 x2)    = Fsub2 l x0 x1 x2
mapR _ (Fmul2 l x0 x1 x2)    = Fmul2 l x0 x1 x2
mapR _ (Fdiv2 l x0 x1 x2)    = Fdiv2 l x0 x1 x2
mapR _ (Fmax2 l x0 x1 x2)    = Fmax2 l x0 x1 x2
mapR _ (Fmin2 l x0 x1 x2)    = Fmin2 l x0 x1 x2
mapR _ (Fsqrt2 l v0 v1)      = Fsqrt2 l v0 v1
mapR _ (Fsqrte2 l v0 v1)     = Fsqrte2 l v0 v1
mapR _ (Fneg2 l v0 v1)       = Fneg2 l v0 v1
mapR _ (Faddp l d v)         = Faddp l d v
mapR _ (Fmaxp l d v)         = Fmaxp l d v
mapR _ (Fminp l d v)         = Fminp l d v
mapR _ (MovQQ l v0 v1)       = MovQQ l v0 v1
mapR _ (Fmla l v0 v1 v2)     = Fmla l v0 v1 v2
mapR _ (Fmls l v0 v1 v2)     = Fmls l v0 v1 v2
mapR f (Dup l v r)           = Dup l v (f r)
mapR f (Ins l v i r)         = Ins l v i (f r)
mapR _ (DupD l v0 v1 i)      = DupD l v0 v1 i
mapR _ (ZeroD l q)           = ZeroD l q
mapR _ (EorD l v0 v1 v2)     = EorD l v0 v1 v2
mapR f (Prfm l po r)         = Prfm l po (f<$>r)

fR :: Monoid m => (areg -> m) -> AArch64 areg afreg a -> m
fR _ Label{}               = mempty
fR _ B{}                   = mempty
fR _ Bc{}                  = mempty
fR _ C{}                   = mempty
fR _ FMovXX{}              = mempty
fR f (MovRR _ r0 r1)       = f r0<>f r1
fR f (MovRC _ r _)         = f r
fR f (Ldr _ r a)           = f r<>f@<>a
fR f (Blr _ r)             = f r
fR f (LdrB _ r a)          = f r<>f@<>a
fR f (Str _ r a)           = f r<>f@<>a
fR f (StrB _ r a)          = f r<>f@<>a
fR f (LdrD _ _ a)          = f@<>a
fR f (AddRR _ r0 r1 r2)    = f r0<>f r1<>f r2
fR f (AddRRS _ r0 r1 r2 _) = f r0<>f r1<>f r2
fR f (AddRC _ r0 r1 _ _)   = f r0<>f r1
fR f (SubRR _ r0 r1 r2)    = f r0<>f r1<>f r2
fR f (SubRC _ r0 r1 _ _)   = f r0<>f r1
fR f (SubsRC _ r0 r1 _)    = f r0<>f r1
fR f (ZeroR _ r)           = f r
fR f (AndRR _ r0 r1 r2)    = f r0<>f r1<>f r2
fR f (OrRR _ r0 r1 r2)     = f r0<>f r1<>f r2
fR f (Eor _ r0 r1 r2)      = f r0<>f r1<>f r2
fR f (Eon _ r0 r1 r2)      = f r0<>f r1<>f r2
fR f (EorI _ r0 r1 _)      = f r0<>f r1
fR f (Lsl _ r0 r1 _)       = f r0<>f r1
fR f (Asr _ r0 r1 _)       = f r0<>f r1
fR f (CmpRR _ r0 r1)       = f r0<>f r1
fR f (CmpRC _ r _)         = f r
fR f (Neg _ r0 r1)         = f r0<>f r1
fR _ Fmul{}                = mempty
fR _ Fadd{}                = mempty
fR _ Fsub{}                = mempty
fR _ Fdiv{}                = mempty
fR _ Ret{}                 = mempty
fR _ RetL{}                = mempty
fR _ FcmpZ{}               = mempty
fR f (Cbz _ r _)           = f r
fR f (Cbnz _ r _)          = f r
fR f (Tbz _ r _ _)         = f r
fR f (Tbnz _ r _ _)        = f r
fR _ Bl{}                  = mempty
fR f (FMovDR _ _ r)        = f r
fR f (Dup _ _ r)           = f r
fR f (Ins _ _ _ r)         = f r
fR f (MovZ _ r _ _)        = f r
fR _ MovQQ{}               = mempty
fR _ DupD{}                = mempty
fR f (MovK _ r _ _)        = f r
fR f (MovRCf _ r _)        = f r
fR f (LdrRL _ r _)         = f r
fR f (StrS _ _ a)          = f@<>a
fR f (StrD _ _ a)          = f@<>a
fR f (LdrS _ _ a)          = f@<>a
fR _ ZeroS{}               = mempty
fR _ ZeroD{}               = mempty
fR _ EorS{}                = mempty
fR _ EorD{}                = mempty
fR f (MulRR _ r0 r1 r2)    = f r0<>f r1<>f r2
fR f (Madd _ r0 r1 r2 r3)  = f r0<>f r1<>f r2<>f r3
fR f (Msub _ r0 r1 r2 r3)  = f r0<>f r1<>f r2<>f r3
fR f (Sdiv _ r0 r1 r2)     = f r0<>f r1<>f r2
fR _ Fadd2{}               = mempty
fR _ Fsub2{}               = mempty
fR _ Fmul2{}               = mempty
fR _ Fdiv2{}               = mempty
fR _ Fmax2{}               = mempty
fR _ Fmin2{}               = mempty
fR _ Faddp{}               = mempty
fR _ Fmaxp{}               = mempty
fR _ Fminp{}               = mempty
fR _ Fneg2{}               = mempty
fR _ Fsqrt2{}              = mempty
fR _ Fsqrte2{}             = mempty
fR f (Scvtf _ _ r)         = f r
fR f (Fcvtms _ r _)        = f r
fR f (Fcvtas _ r _)        = f r
fR f (Stp _ r0 r1 a)       = f r0<>f r1<>f@<>a
fR f (Ldp _ r0 r1 a)       = f r0<>f r1<>f@<>a
fR f (Stp2 _ _ _ a)        = f@<>a
fR f (Ldp2 _ _ _ a)        = f@<>a
fR _ Fmadd{}               = mempty
fR _ Fmsub{}               = mempty
fR _ Fmla{}                = mempty
fR _ Fmls{}                = mempty
fR _ Frintm{}              = mempty
fR f (MrsR _ r)            = f r
fR _ Fmax{}                = mempty
fR _ Fmin{}                = mempty
fR _ Fabs{}                = mempty
fR f (Csel _ r0 r1 r2 _)   = f r0<>f r1<>f r2
fR f (Bfc _ r _ _)         = f r
fR f (Prfm _ _ a)          = f@<>a
fR _ Fcmp{}                = mempty
fR _ Fneg{}                = mempty
fR _ Fsqrt{}               = mempty
fR _ Fsqrte{}              = mempty
fR _ Fcsel{}               = mempty
fR f (Cset _ r _)          = f r
fR f (TstI _ r _)          = f r
fR f (StpD _ _ _ a)        = f@<>a
fR f (LdpD _ _ _ a)        = f@<>a

mapFR :: (afreg -> freg) -> AArch64 areg afreg a -> AArch64 areg freg a
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
mapFR _ (AddRC l r0 r1 c s)   = AddRC l r0 r1 c s
mapFR _ (SubRR l r0 r1 r2)    = SubRR l r0 r1 r2
mapFR _ (SubRC l r0 r1 c s)   = SubRC l r0 r1 c s
mapFR _ (SubsRC l r0 r1 c)    = SubsRC l r0 r1 c
mapFR _ (ZeroR l r)           = ZeroR l r
mapFR _ (AndRR l r0 r1 r2)    = AndRR l r0 r1 r2
mapFR _ (OrRR l r0 r1 r2)     = OrRR l r0 r1 r2
mapFR _ (Eor l r0 r1 r2)      = Eor l r0 r1 r2
mapFR _ (Eon l r0 r1 r2)      = Eon l r0 r1 r2
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
mapFR f (Fsqrte l d0 d1)      = Fsqrte l (f d0) (f d1)
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
mapFR f (Ldp2 l q0 q1 a)      = Ldp2 l (f<$>q0) (f<$>q1) a
mapFR f (Stp2 l q0 q1 a)      = Stp2 l (f<$>q0) (f<$>q1) a
mapFR _ (Bfc x r l w)         = Bfc x r l w
mapFR f (LdrS l q a)          = LdrS l (f<$>q) a
mapFR f (StrS l q a)          = StrS l (f<$>q) a
mapFR f (Fadd2 l x0 x1 x2)    = Fadd2 l (f<$>x0) (f<$>x1) (f<$>x2)
mapFR f (Fsub2 l x0 x1 x2)    = Fsub2 l (f<$>x0) (f<$>x1) (f<$>x2)
mapFR f (Fmul2 l x0 x1 x2)    = Fmul2 l (f<$>x0) (f<$>x1) (f<$>x2)
mapFR f (Fdiv2 l x0 x1 x2)    = Fdiv2 l (f<$>x0) (f<$>x1) (f<$>x2)
mapFR f (Fmax2 l x0 x1 x2)    = Fmax2 l (f<$>x0) (f<$>x1) (f<$>x2)
mapFR f (Fmin2 l x0 x1 x2)    = Fmin2 l (f<$>x0) (f<$>x1) (f<$>x2)
mapFR f (Fsqrt2 l v0 v1)      = Fsqrt2 l (f<$>v0) (f<$>v1)
mapFR f (Fsqrte2 l v0 v1)     = Fsqrte2 l (f<$>v0) (f<$>v1)
mapFR f (Fneg2 l v0 v1)       = Fneg2 l (f<$>v0) (f<$>v1)
mapFR f (EorS l v0 v1 v2)     = EorS l (f<$>v0) (f<$>v1) (f<$>v2)
mapFR f (ZeroS l v)           = ZeroS l (f<$>v)
mapFR f (Faddp l d v)         = Faddp l (f d) (f<$>v)
mapFR f (Fmaxp l d v)         = Fmaxp l (f d) (f<$>v)
mapFR f (Fminp l d v)         = Fminp l (f d) (f<$>v)
mapFR f (MovQQ l v0 v1)       = MovQQ l (f<$>v0) (f<$>v1)
mapFR f (Fmla l v0 v1 v2)     = Fmla l (f<$>v0) (f<$>v1) (f<$>v2)
mapFR f (Fmls l v0 v1 v2)     = Fmls l (f<$>v0) (f<$>v1) (f<$>v2)
mapFR f (Dup l v r)           = Dup l (f<$>v) r
mapFR f (Ins l v i r)         = Ins l (f<$>v) i r
mapFR f (DupD l v0 v1 i)      = DupD l (f<$>v0) (f<$>v1) i
mapFR f (ZeroD l d)           = ZeroD l (f d)
mapFR f (EorD l d0 d1 d2)     = EorD l (f d0) (f d1) (f d2)
mapFR _ (Prfm l po a)         = Prfm l po a

s2 :: [a] -> [(a, Maybe a)]
s2 (r0:r1:rs) = (r0, Just r1):s2 rs
s2 [r]        = [(r, Nothing)]
s2 []         = []

pus, pos :: [AReg] -> [AArch64 AReg freg ()]
pus = map go.s2 where go (r0, Just r1) = Stp () r0 r1 (Pr SP (-16)); go (r, Nothing) = Str () r (Pr SP (-16))
pos = map go.reverse.s2 where go (r0, Just r1) = Ldp () r0 r1 (Po SP 16); go (r, Nothing) = Ldr () r (Po SP 16)

puds, pods :: [freg] -> [AArch64 AReg freg ()]
puds = map go.s2 where go (r0, Just r1) = StpD () r0 r1 (Pr SP (-16)); go (r, Nothing) = StrD () r (Pr SP (-16))
pods = map go.reverse.s2 where go (r0, Just r1) = LdpD () r0 r1 (Po SP 16); go (r, Nothing) = LdrD () r (Po SP 16)

puxs, poxs :: [freg] -> [AArch64 AReg freg ()]
puxs = map go.s2 where go (r0, Just r1) = Stp2 () (V2Reg r0) (V2Reg r1) (Pr SP (-32)); go (r, Nothing) = StrS () (V2Reg r) (Pr SP (-16))
poxs = map go.reverse.s2 where go (r0, Just r1) = Ldp2 () (V2Reg r0) (V2Reg r1) (Po SP 32); go (r, Nothing) = LdrS () (V2Reg r) (Po SP 16)

hexd :: Integral a => a -> Doc ann
hexd n | n < 0 = pretty ("#-0x"++showHex (-n) "")
       | otherwise = pretty ("#0x"++showHex n "")

pvd v = pv v <> ".2d"
pvv v = pv v <> ".16b"; pvs v = pv v <> ".8b"

ar2 r0 r1 = pretty r0 <> "," <+> pretty r1; aw r a = pw r <> "," <+> pretty a
ar3 r0 r1 r2 = pretty r0 <> "," <+> pretty r1 <> "," <+> pretty r2
ar4 r0 r1 r2 r3 = pretty r0 <> "," <+> pretty r1 <> "," <+> pretty r2 <> "," <+> pretty r3
ri r u = pretty r <> "," <+> hexd u
r2i r0 r1 u = pretty r0 <> "," <+> pretty r1 <> "," <+> hexd u
av2 q0 q1 = pvd q0 <> "," <+> pvd q1
v3 q0 q1 q2 = pvd q0 <> "," <+> pvd q1 <> "," <+> pvd q2
qa q0 q1 a = pq q0 <> "," <+> pq q1 <> "," <+> pretty a

instance (Pretty reg, Pretty freg, SIMD (V2Reg freg), P32 reg) => Pretty (AArch64 reg freg a) where
    pretty (Label _ l)            = prettyLabel l <> ":"
    pretty isn = i4 (p4 isn)
      where
        p4 Label{}                 = error "shouldn't happen."
        p4 (B _ l)                 = "b" <+> prettyLabel l
        p4 (Blr _ r)               = "blr" <+> pretty r
        p4 (Bl _ l)                = "bl" <+> pSym l
        p4 (C _ l)                 = "call" <+> pretty l
        p4 (Bc _ c l)              = "b." <> pretty c <+> prettyLabel l
        p4 (MovQQ _ v0 v1)         = "mov" <+> pvv v0 <> "," <+> pvv v1
        p4 (FMovXX _ xr0 xr1)      = "fmov" <+> ar2 xr0 xr1
        p4 (FMovDR _ d r)          = "fmov" <+> ar2 d r
        p4 (MovRR _ r0 r1)         = "mov" <+> ar2 r0 r1
        p4 (MovRC _ r u)           = "mov" <+> ri r u
        p4 (Ldr _ r a)             = "ldr" <+> ar2 r a
        p4 (LdrB _ r a)            = "ldrb" <+> aw r a
        p4 (Str _ r a)             = "str" <+> ar2 r a
        p4 (StrB _ r a)            = "strb" <+> aw r a
        p4 (LdrD _ xr a)           = "ldr" <+> ar2 xr a
        p4 (StrD _ xr a)           = "str" <+> ar2 xr a
        p4 (AddRRS _ rD rS rS' s)  = "add" <+> ar3 rD rS rS' <> "," <+> "LSL" <+> "#" <> pretty s
        p4 (AddRR _ rD rS rS')     = "add" <+> ar3 rD rS rS'
        p4 (SubRR _ rD rS rS')     = "sub" <+> ar3 rD rS rS'
        p4 (AndRR _ rD rS rS')     = "and" <+> ar3 rD rS rS'
        p4 (OrRR _ rD rS rS')      = "orr" <+> ar3 rD rS rS'
        p4 (Eor _ rD rS rS')       = "eor" <+> ar3 rD rS rS'
        p4 (Eon _ rD rS rS')       = "eon" <+> ar3 rD rS rS'
        p4 (EorI _ rD rS i)        = "eor" <+> ar2 rD rS <> "," <+> pretty i
        p4 (ZeroR _ rD)            = "eor" <+> ar3 rD rD rD
        p4 (MulRR _ rD rS rS')     = "mul" <+> ar3 rD rS rS'
        p4 (SubRC _ rD rS u IZero) = "sub" <+> r2i rD rS u
        p4 (SubRC _ rD rS u s)     = "sub" <+> r2i rD rS u <> "," <+> pretty s
        p4 (SubsRC _ rD rS u)      = "subs" <+> r2i rD rS u
        p4 (AddRC _ rD rS u IZero) = "add" <+> r2i rD rS u
        p4 (AddRC _ rD rS u s)     = "add" <+> r2i rD rS u <> "," <+> pretty s
        p4 (Lsl _ rD rS u)         = "lsl" <+> r2i rD rS u
        p4 (Asr _ rD rS u)         = "asr" <+> r2i rD rS u
        p4 (CmpRC _ r u)           = "cmp" <+> pretty r <> "," <+> hexd u
        p4 (CmpRR _ r0 r1)         = "cmp" <+> ar2 r0 r1
        p4 (Neg _ rD rS)           = "neg" <+> ar2 rD rS
        p4 (Fmul _ rD r0 r1)       = "fmul" <+> ar3 rD r0 r1
        p4 (Fadd _ rD r0 r1)       = "fadd" <+> ar3 rD r0 r1
        p4 (Fsub _ rD r0 r1)       = "fsub" <+> ar3 rD r0 r1
        p4 (Fdiv _ rD r0 r1)       = "fdiv" <+> ar3 rD r0 r1
        p4 (Fmul2 _ xD x0 x1)      = "fmul" <+> v3 xD x0 x1
        p4 (Fdiv2 _ xD x0 x1)      = "fdiv" <+> v3 xD x0 x1
        p4 (Fmax2 _ xD x0 x1)      = "fmax" <+> v3 xD x0 x1
        p4 (Fmin2 _ xD x0 x1)      = "fmin" <+> v3 xD x0 x1
        p4 (Fsqrt2 _ xD xS)        = "fsqrt" <+> av2 xD xS
        p4 (Fsqrte2 _ xD xS)       = "fsqrte" <+> av2 xD xS
        p4 (Fneg2 _ xD xS)         = "fneg" <+> av2 xD xS
        p4 (Fadd2 _ xD x0 x1)      = "fadd" <+> v3 xD x0 x1
        p4 (Fsub2 _ xD x0 x1)      = "fsub" <+> v3 xD x0 x1
        p4 (Faddp _ dD v0)         = "faddp" <+> pretty dD <> "," <+> pvd v0
        p4 (Fmaxp _ dD v0)         = "fmaxp" <+> pretty dD <> "," <+> pvd v0
        p4 (Fminp _ dD v0)         = "fminp" <+> pretty dD <> "," <+> pvd v0
        p4 (EorS _ vD v0 v1)       = "eor" <+> pvv vD <> "," <+> pvv v0 <> "," <+> pvv v1
        p4 (ZeroS _ v)             = "eor" <+> pvv v <> "," <+> pvv v <> "," <+> pvv v
        p4 (ZeroD _ d)             = let q=V2Reg d in "eor" <+> pvs q <> "," <> pvs q <> "," <+> pvs q
        p4 (EorD _ d0 d1 d2)       = "eor" <+> pvs (V2Reg d0) <> "," <+> pvs (V2Reg d1) <> "," <+> pvs (V2Reg d2)
        p4 (FcmpZ _ xr)            = "fcmp" <+> pretty xr <> "," <+> "#0.0"
        p4 (Fneg _ d0 d1)          = "fneg" <+> ar2 d0 d1
        p4 Ret{}                   = "ret"
        p4 RetL{}                  = "ret"
        p4 (Scvtf _ d r)           = "scvtf" <+> ar2 d r
        p4 (Fcvtms _ r d)          = "fcvtms" <+> ar2 r d
        p4 (Fcvtas _ r d)          = "fcvtas" <+> ar2 r d
        p4 (MovK _ r i s)          = "movk" <+> ri r i <> "," <+> "LSL" <+> "#" <> pretty s
        p4 (MovZ _ r i s)          = "movz" <+> ri r i <> "," <+> "LSL" <+> "#" <> pretty s
        p4 (Fcmp _ d0 d1)          = "fcmp" <+> ar2 d0 d1
        p4 (Stp _ r0 r1 a)         = "stp" <+> ar3 r0 r1 a
        p4 (Ldp _ r0 r1 a)         = "ldp" <+> ar3 r0 r1 a
        p4 (Ldp2 _ q0 q1 a)        = "ldp" <+> qa q0 q1 a
        p4 (Stp2 _ q0 q1 a)        = "stp" <+> qa q0 q1 a
        p4 (LdrS _ q a)            = "ldr" <+> pq q <> "," <+> pretty a
        p4 (StrS _ q a)            = "str" <+> pq q <> "," <+> pretty a
        p4 (StpD _ d0 d1 a)        = "stp" <+> ar3 d0 d1 a
        p4 (LdpD _ d0 d1 a)        = "ldp" <+> ar3 d0 d1 a
        p4 (Fmadd _ d0 d1 d2 d3)   = "fmadd" <+> ar4 d0 d1 d2 d3
        p4 (Fmsub _ d0 d1 d2 d3)   = "fmsub" <+> ar4 d0 d1 d2 d3
        p4 (Fmla _ v0 v1 v2)       = "fmla" <+> v3 v0 v1 v2
        p4 (Fmls _ v0 v1 v2)       = "fmls" <+> v3 v0 v1 v2
        p4 (Madd _ r0 r1 r2 r3)    = "madd" <+> ar4 r0 r1 r2 r3
        p4 (Msub _ r0 r1 r2 r3)    = "msub" <+> ar4 r0 r1 r2 r3
        p4 (Sdiv _ rD rS rS')      = "sdiv" <+> ar3 rD rS rS'
        p4 (Fsqrt _ d0 d1)         = "fsqrt" <+> ar2 d0 d1
        p4 (Fsqrte _ d0 d1)        = "fsqrte" <+> ar2 d0 d1
        p4 (Frintm _ d0 d1)        = "frintm" <+> ar2 d0 d1
        p4 (MrsR _ r)              = "mrs" <+> pretty r <> "," <+> "rndr"
        p4 (MovRCf _ r cf)         = "mov" <+> ar2 r cf
        p4 (LdrRL _ r l)           = "ldr" <+> pretty r <> "," <+> "=arr_" <> pretty l
        p4 (Fmax _ d0 d1 d2)       = "fmax" <+> ar3 d0 d1 d2
        p4 (Fmin _ d0 d1 d2)       = "fmin" <+> ar3 d0 d1 d2
        p4 (Fabs _ d0 d1)          = "fabs" <+> ar2 d0 d1
        p4 (Csel _ r0 r1 r2 p)     = "csel" <+> ar3 r0 r1 r2 <> "," <+> pretty p
        p4 (Tbnz _ r n l)          = "tbnz" <+> pretty r <> "," <+> "#" <> pretty n <> "," <+> prettyLabel l
        p4 (Tbz _ r n l)           = "tbz" <+> pretty r <> "," <+> "#" <> pretty n <> "," <+> prettyLabel l
        p4 (Cbnz _ r l)            = "cbnz" <+> pretty r <> "," <+> prettyLabel l
        p4 (Cbz _ r l)             = "cbz" <+> pretty r <> "," <+> prettyLabel l
        p4 (Fcsel _ d0 d1 d2 p)    = "fcsel" <+> ar3 d0 d1 d2 <> "," <+> pretty p
        p4 (TstI _ r i)            = "tst" <+> pretty r <> "," <+> pretty i
        p4 (Cset _ r c)            = "cset" <+> ar2 r c
        p4 (Bfc _ r l w)           = "bfc" <+> pretty r <> "," <+> pretty l <> "," <+> pretty w
        p4 (Dup _ v r)             = "dup" <+> pvd v <> "," <+> pretty r
        p4 (Ins _ v i r)           = "ins" <+> pvd v <> brackets (pretty i) <> "," <+> pretty r
        p4 (DupD _ v0 v1 i)        = "dup" <+> pvd v0 <> "," <+> pvd v1 <> brackets (pretty i)
        p4 (Prfm _ po r)           = "prfm" <+> ar2 po r

instance (Pretty reg, Pretty freg, SIMD (V2Reg freg), P32 reg) => Show (AArch64 reg freg a) where show=show.pretty

prettyLive :: (Pretty reg, Pretty freg, SIMD (V2Reg freg), P32 reg, Pretty o) => AArch64 reg freg o -> Doc ann
prettyLive r = pretty r <+> pretty (ann r)

prettyDebug :: (Pretty freg, Pretty reg, SIMD (V2Reg freg), P32 reg, Pretty o) => [AArch64 reg freg o] -> Doc ann
prettyDebug = prettyLines . fmap prettyLive
