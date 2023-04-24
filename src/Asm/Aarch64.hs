{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Asm.Aarch64 ( AArch64 (..)
                   , Addr (..)
                   , Cond (..)
                   , Shift (..)
                   , AbsReg (..)
                   , FAbsReg (..)
                   , AReg (..)
                   , FAReg (..)
                   , mapR
                   , mapFR
                   , toInt
                   , fToInt
                   , pu, po
                   ) where

import           Asm.M
import           Control.DeepSeq (NFData (..))
import           Data.Copointed
import           Data.Semigroup  ((<>))
import           Data.Word       (Word16, Word8)
import           GHC.Float       (castDoubleToWord64)
import           GHC.Generics    (Generic)
import           Numeric         (showHex)
import           Prettyprinter   (Doc, Pretty (..), brackets, (<+>))

data AReg = X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9 | X10 | X11 | X12 | X13 | X14 | X15 | X16 | X17 | X18 | X19 | X20 | X21 | X22 | X23 | X24 | X25 | X26 | X27 | X28 | X29 | X30 | X31 | SP deriving (Eq, Ord, Enum, Generic)

instance NFData AReg where

instance Pretty AReg where
    pretty X0 = "X0"; pretty X1 = "X1"; pretty X2 = "X2"; pretty X3 = "X3"; pretty X4 = "X4"; pretty X5 = "X5"; pretty X6 = "X6"; pretty X7 = "X7"
    pretty X8 = "X8"; pretty X9 = "X9"; pretty X10 = "X10"; pretty X11 = "X11"; pretty X12 = "X12"; pretty X13 = "X13"; pretty X14 = "X14"; pretty X15 = "X15"
    pretty X16 = "X16"; pretty X17 = "X17"; pretty X18 = "X18"; pretty X19 = "X19"; pretty X20 = "X20"; pretty X21 = "X21"; pretty X22 = "X22"; pretty X23 = "X23"
    pretty X24 = "X24"; pretty X25 = "X25"; pretty X26 = "X26"; pretty X27 = "X27"; pretty X28 = "X28"; pretty X29 = "X29"; pretty X30 = "X30"; pretty X31 = "X31"
    pretty SP = "SP"

instance Show AReg where show = show.pretty

data FAReg = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 | D10 | D11 | D12 | D13 | D14 | D15 | D16 | D17 | D18 | D19 | D20 | D21 | D22 | D23 | D24 | D25 | D26 | D27 | D28 | D29 | D30 | D31 deriving (Eq, Ord, Enum, Generic)

instance Pretty FAReg where
    pretty D0 = "D0"; pretty D1 = "D1"; pretty D2 = "D2"; pretty D3 = "D3"; pretty D4 = "D4"; pretty D5 = "D5"; pretty D6 = "D6"; pretty D7 = "D7"
    pretty D8 = "D8"; pretty D9 = "D9"; pretty D10 = "D10"; pretty D11 = "D11"; pretty D12 = "D12"; pretty D13 = "D13"; pretty D14 = "D14"; pretty D15 = "D15"
    pretty D16 = "D16"; pretty D17 = "D17"; pretty D18 = "D18"; pretty D19 = "D19"; pretty D20 = "D20"; pretty D21 = "D21"; pretty D22 = "D22"; pretty D23 = "D23"
    pretty D24 = "D24"; pretty D25 = "D25"; pretty D26 = "D26"; pretty D27 = "D27"; pretty D28 = "D28"; pretty D29 = "D29"; pretty D30 = "D30"; pretty D31 = "D31"

instance Show FAReg where show = show.pretty

data AbsReg = IReg !Int | CArg0 | CArg1 | CArg2 | CArg3 | CArg4 | CArg5 | CArg6 | CArg7 | LR | ASP
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
toInt (IReg i) = 18+i

fToInt :: FAbsReg -> Int
fToInt FArg0    = 10
fToInt FArg1    = 11
fToInt FArg2    = 12
fToInt FArg3    = 13
fToInt FArg4    = 14
fToInt FArg5    = 15
fToInt FArg6    = 16
fToInt FArg7    = 17
fToInt (FReg i) = 18+i

data Shift = Zero | Three

instance Pretty Shift where
    pretty Zero = "#0"; pretty Three = "#3"

data Addr reg = R reg | RP reg Word16 | BI reg reg Shift deriving Functor

instance Pretty reg => Pretty (Addr reg) where
    pretty (R r)      = brackets (pretty r)
    pretty (RP r u)   = brackets (pretty r <> "," <+> hexd u)
    pretty (BI b i s) = brackets (pretty b <> "," <+> pretty i <> "," <+> "LSL" <+> pretty s)

data Cond = Eq | Neq | ULeq | UGeq | ULt
          | Geq | Lt | Gt | Leq

instance Pretty Cond where
    pretty Eq = "EQ"; pretty Neq = "NE"; pretty ULeq = "LS"; pretty Geq = "GE"
    pretty Lt = "LT"; pretty Gt = "GT"; pretty Leq = "LE"; pretty ULt = "LO"

-- https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions
data AArch64 reg freg a = Label { ann :: a, label :: Label }
                        | B { ann :: a, label :: Label }
                        | Bc { ann :: a, cond :: Cond, label :: Label }
                        | Bl { ann :: a, cfunc :: CFunc }
                        | Ret { ann :: a }
                        | FMovXX { ann :: a, dDest :: freg, dSrc :: freg }
                        | FMovDR { ann :: a, dDest :: freg, rSrc :: reg }
                        | FMovXC { ann :: a, dDest :: freg, dC :: Double }
                        | MovRR { ann :: a, rDest :: reg, rSrc :: reg }
                        | MovRC { ann :: a, rDest :: reg, cSrc :: Word16 }
                        | MovK { ann :: a, rDest :: reg, cSrc :: Word16, lsl :: Int }
                        | Ldr { ann :: a, rDest :: reg, aSrc :: Addr reg }
                        | Str { ann :: a, rSrc :: reg, aDest :: Addr reg }
                        | LdrD { ann :: a, dDest :: freg, aSrc :: Addr reg }
                        | StrD { ann :: a, dSrc :: freg, aDest :: Addr reg }
                        | SubRR { ann :: a, rDest :: reg, rSrc1 :: reg, rSrc2 :: reg }
                        | AddRR { ann :: a, rDest :: reg, rSrc1 :: reg, rSrc2 :: reg }
                        | MulRR { ann :: a, rDest :: reg, rSrc1 :: reg, rSrc2 :: reg }
                        | AddRC { ann :: a, rDest :: reg, rSrc :: reg, rC :: Word16 }
                        | SubRC { ann :: a, rDest :: reg, rSrc :: reg, rC :: Word16 }
                        | Lsl { ann :: a, rDest :: reg, rSrc :: reg, sC :: Word8 }
                        | CmpRC { ann :: a, rSrc :: reg, cSrc :: Word16 }
                        | CmpRR { ann :: a, rSrc1 :: reg, rSrc2 :: reg }
                        | Neg { ann :: a, rDest :: reg, rSrc :: reg }
                        | CpyfP { ann :: a, rDest :: reg, rSrc :: reg, rNb :: reg }
                        | CpyfM { ann :: a, rDest :: reg, rSrc :: reg, rNb :: reg }
                        | CpyfE { ann :: a, rDest :: reg, rSrc :: reg, rNb :: reg }
                        | Fmul { ann :: a, dDest :: freg, dSrc1 :: freg, dSrc2 :: freg }
                        | Fadd { ann :: a, dDest :: freg, dSrc1 :: freg, dSrc2 :: freg }
                        | Fsub { ann :: a, dDest :: freg, dSrc1 :: freg, dSrc2 :: freg }
                        | Fdiv { ann :: a, dDest :: freg, dSrc1 :: freg, dSrc2 :: freg }
                        | FcmpZ { ann :: a, dSrc :: freg }
                        | Scvtf { ann :: a, dDest :: freg, rSrc :: reg }
                        | Fcvtms { ann :: a, rDest :: reg, dSrc :: freg }
                        deriving (Functor)

instance Copointed (AArch64 reg freg) where copoint = ann

mapR :: (areg -> reg) -> AArch64 areg afreg a -> AArch64 reg afreg a
mapR _ (Label x l)          = Label x l
mapR _ (B x l)              = B x l
mapR _ (Bc x c l)           = Bc x c l
mapR _ (Bl x f)             = Bl x f
mapR _ (FMovXX l r0 r1)     = FMovXX l r0 r1
mapR _ (FMovXC l r0 c)      = FMovXC l r0 c
mapR f (MovRR l r0 r1)      = MovRR l (f r0) (f r1)
mapR f (MovRC l r c)        = MovRC l (f r) c
mapR f (Ldr l r a)          = Ldr l (f r) (f <$> a)
mapR f (Str l r a)          = Str l (f r) (f <$> a)
mapR f (LdrD l xr a)        = LdrD l xr (f <$> a)
mapR f (AddRR l r0 r1 r2)   = AddRR l (f r0) (f r1) (f r2)
mapR f (SubRR l r0 r1 r2)   = SubRR l (f r0) (f r1) (f r2)
mapR f (AddRC l r0 r1 c)    = AddRC l (f r0) (f r1) c
mapR f (SubRC l r0 r1 c)    = SubRC l (f r0) (f r1) c
mapR f (Lsl l r0 r1 s)      = Lsl l (f r0) (f r1) s
mapR f (CmpRR l r0 r1)      = CmpRR l (f r0) (f r1)
mapR f (CmpRC l r c)        = CmpRC l (f r) c
mapR f (Neg l r0 r1)        = Neg l (f r0) (f r1)
mapR f (CpyfP l r0 r1 r2)   = CpyfP l (f r0) (f r1) (f r2)
mapR f (CpyfM l r0 r1 r2)   = CpyfM l (f r0) (f r1) (f r2)
mapR f (CpyfE l r0 r1 r2)   = CpyfE l (f r0) (f r1) (f r2)
mapR _ (Fadd l xr0 xr1 xr2) = Fadd l xr0 xr1 xr2
mapR _ (Fsub l xr0 xr1 xr2) = Fsub l xr0 xr1 xr2
mapR _ (Fmul l xr0 xr1 xr2) = Fmul l xr0 xr1 xr2
mapR _ (FcmpZ l xr)         = FcmpZ l xr
mapR _ (Ret l)              = Ret l
mapR f (MulRR l r0 r1 r2)   = MulRR l (f r0) (f r1) (f r2)
mapR f (StrD l d a)         = StrD l d (f <$> a)
mapR _ (Fdiv l d0 d1 d2)    = Fdiv l d0 d1 d2
mapR f (Scvtf l d r)        = Scvtf l d (f r)
mapR f (Fcvtms l r d)       = Fcvtms l (f r) d
mapR f (MovK l r u s)       = MovK l (f r) u s
mapR f (FMovDR l d r)       = FMovDR l d (f r)

mapFR :: (afreg -> freg) -> AArch64 areg afreg a -> AArch64 areg freg a
mapFR _ (Label x l)          = Label x l
mapFR _ (B x l)              = B x l
mapFR _ (Bc x c l)           = Bc x c l
mapFR _ (Bl x f)             = Bl x f
mapFR f (FMovXX l xr0 xr1)   = FMovXX l (f xr0) (f xr1)
mapFR f (FMovXC l xr c)      = FMovXC l (f xr) c
mapFR _ (MovRR l r0 r1)      = MovRR l r0 r1
mapFR _ (MovRC l r0 c)       = MovRC l r0 c
mapFR _ (Ldr l r a)          = Ldr l r a
mapFR _ (Str l r a)          = Str l r a
mapFR f (LdrD l xr a)        = LdrD l (f xr) a
mapFR _ (AddRR l r0 r1 r2)   = AddRR l r0 r1 r2
mapFR _ (AddRC l r0 r1 c)    = AddRC l r0 r1 c
mapFR _ (SubRR l r0 r1 r2)   = SubRR l r0 r1 r2
mapFR _ (SubRC l r0 r1 c)    = SubRC l r0 r1 c
mapFR _ (Lsl l r0 r1 s)      = Lsl l r0 r1 s
mapFR _ (CmpRC l r c)        = CmpRC l r c
mapFR _ (CmpRR l r0 r1)      = CmpRR l r0 r1
mapFR _ (Neg l r0 r1)        = Neg l r0 r1
mapFR _ (CpyfP l r0 r1 r2)   = CpyfP l r0 r1 r2
mapFR _ (CpyfM l r0 r1 r2)   = CpyfM l r0 r1 r2
mapFR _ (CpyfE l r0 r1 r2)   = CpyfE l r0 r1 r2
mapFR f (Fmul l xr0 xr1 xr2) = Fmul l (f xr0) (f xr1) (f xr2)
mapFR f (Fadd l xr0 xr1 xr2) = Fadd l (f xr0) (f xr1) (f xr2)
mapFR f (Fsub l xr0 xr1 xr2) = Fsub l (f xr0) (f xr1) (f xr2)
mapFR f (FcmpZ l xr)         = FcmpZ l (f xr)
mapFR _ (Ret l)              = Ret l
mapFR f (Fdiv l d0 d1 d2)    = Fdiv l (f d0) (f d1) (f d2)
mapFR _ (MulRR l r0 r1 r2)   = MulRR l r0 r1 r2
mapFR f (StrD l d a)         = StrD l (f d) a
mapFR f (Scvtf l d r)        = Scvtf l (f d) r
mapFR f (Fcvtms l r d)       = Fcvtms l r (f d)
mapFR _ (MovK l r u s)       = MovK l r u s
mapFR f (FMovDR l d r)       = FMovDR l (f d) r

pu, po :: AReg -> [AArch64 AReg freg ()]
pu r = [SubRC () SP SP 8, Str () r (R SP)]
po r = [Ldr () r (R SP), AddRC () SP SP 8]

hexd :: Integral a => a -> Doc ann
hexd = pretty.($"").(("#0x"++).).showHex


instance (Pretty reg, Pretty freg) => Pretty (AArch64 reg freg a) where
    pretty (Label _ l)         = prettyLabel l <> ":"
    pretty (B _ l)             = i4 ("b" <+> prettyLabel l)
    pretty (Bc _ c l)          = i4 ("b." <> pretty c <+> prettyLabel l)
    pretty (Bl _ l)            = i4 ("bl" <+> pretty l)
    pretty (FMovXX _ xr0 xr1)  = i4 ("fmov" <+> pretty xr0 <> "," <+> pretty xr1)
    pretty (FMovDR _ d r)      = i4 ("fmov" <+> pretty d <> "," <+> pretty r)
    pretty (MovRR _ r0 r1)     = i4 ("mov" <+> pretty r0 <> "," <+> pretty r1)
    pretty (MovRC _ r u)       = i4 ("mov" <+> pretty r <> "," <+> hexd u)
    pretty (Ldr _ r a)         = i4 ("ldr" <+> pretty r <> "," <+> pretty a)
    pretty (Str _ r a)         = i4 ("str" <+> pretty r <> "," <+> pretty a)
    pretty (LdrD _ xr a)       = i4 ("ldr" <+> pretty xr <> "," <+> pretty a)
    pretty (StrD _ xr a)       = i4 ("str" <+> pretty xr <> "," <+> pretty a)
    pretty (AddRR _ rD rS rS') = i4 ("add" <+> pretty rD <> "," <+> pretty rS <> "," <+> pretty rS')
    pretty (SubRR _ rD rS rS') = i4 ("sub" <+> pretty rD <> "," <+> pretty rS <> "," <+> pretty rS')
    pretty (MulRR _ rD rS rS') = i4 ("mul" <+> pretty rD <> "," <+> pretty rS <> "," <+> pretty rS')
    pretty (SubRC _ rD rS u)   = i4 ("sub" <+> pretty rD <> "," <+> pretty rS <> "," <+> hexd u)
    pretty (AddRC _ rD rS u)   = i4 ("add" <+> pretty rD <> "," <+> pretty rS <> "," <+> hexd u)
    pretty (Lsl _ rD rS u)     = i4 ("lsl" <+> pretty rD <> "," <+> pretty rS <> "," <+> hexd u)
    pretty (CmpRC _ r u)       = i4 ("cmp" <+> pretty r <> "," <+> hexd u)
    pretty (CmpRR _ r0 r1)     = i4 ("cmp" <+> pretty r0 <> "," <+> pretty r1)
    pretty (Neg _ rD rS)       = i4 ("neg" <+> pretty rD <> "," <+> pretty rS)
    pretty (CpyfP _ rD rS rN)  = i4 ("cpyfp" <+> brackets (pretty rD) <> "!," <+> brackets (pretty (rS)) <> "!," <+> pretty rN <> "!")
    pretty (CpyfM _ rD rS rN)  = i4 ("cpyfm" <+> brackets (pretty rD) <> "!," <+> brackets (pretty (rS)) <> "!," <+> pretty rN <> "!")
    pretty (CpyfE _ rD rS rN)  = i4 ("cpyfe" <+> brackets (pretty rD) <> "!," <+> brackets (pretty (rS)) <> "!," <+> pretty rN <> "!")
    pretty (Fmul _ rD r0 r1)   = i4 ("fmul" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (Fadd _ rD r0 r1)   = i4 ("fadd" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (Fsub _ rD r0 r1)   = i4 ("fsub" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (Fdiv _ rD r0 r1)   = i4 ("fdiv" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (FcmpZ _ xr)        = i4 ("fcmp" <+> pretty xr <> "," <+> "#0.0")
    pretty Ret{}               = i4 "ret"
    pretty (Scvtf _ d r)       = i4 ("scvtf" <+> pretty d <> "," <+> pretty r)
    pretty (Fcvtms _ r d)      = i4 ("fcvtms" <+> pretty r <> "," <+> pretty d)
    pretty (MovK _ r i s)      = i4 ("movk" <+> pretty r <> "," <+> hexd i <> "," <+> "LSL" <+> "#" <> pretty s )
