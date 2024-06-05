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
                   , prettyDebug
                   , mapR
                   , mapFR
                   , toInt
                   , fToInt
                   , pus, pos
                   , puds, pods
                   , pSym
                   ) where

import           Asm.M
import           Control.DeepSeq   (NFData (..))
import           Data.Copointed
import           Data.Word         (Word16, Word8)
import           GHC.Generics      (Generic)
import           Numeric           (showHex)
import           Prettyprinter     (Doc, Pretty (..), brackets, (<+>))
import           Prettyprinter.Ext
import           System.Info       (os)

-- https://developer.arm.com/documentation/102374/0101/Registers-in-AArch64---other-registers
data AReg = X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7 | X8 | X9 | X10 | X11 | X12 | X13 | X14 | X15 | X16 | X17 | X18 | X19 | X20 | X21 | X22 | X23 | X24 | X25 | X26 | X27 | X28 | X29 | X30 | SP deriving (Eq, Ord, Enum, Generic)

instance NFData AReg where

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

instance Show FAReg where show = show.pretty

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

data Shift = Zero | Three

instance Pretty Shift where
    pretty Zero = "#0"; pretty Three = "#3"

data Addr reg = R reg | RP reg Word16 | BI reg reg Shift deriving Functor

instance Pretty reg => Pretty (Addr reg) where
    pretty (R r)      = brackets (pretty r)
    pretty (RP r u)   = brackets (pretty r <> "," <+> hexd u)
    pretty (BI b i s) = brackets (pretty b <> "," <+> pretty i <> "," <+> "LSL" <+> pretty s)

data Cond = Eq | Neq | Geq | Lt | Gt | Leq

instance Pretty Cond where
    pretty Eq = "EQ"; pretty Neq = "NE"; pretty Geq = "GE"
    pretty Lt = "LT"; pretty Gt = "GT"; pretty Leq = "LE"

pSym :: Pretty a => a -> Doc ann
pSym = case os of {"linux" -> id; "darwin" -> ("_"<>)}.pretty

-- https://developer.arm.com/documentation/ddi0596/2020-12/Base-Instructions
data AArch64 reg freg a = Label { ann :: a, label :: Label }
                        | B { ann :: a, label :: Label }
                        | Blr { ann :: a, rSrc :: reg }
                        | Bl { ann :: a, cfunc :: CFunc }
                        | Bc { ann :: a, cond :: Cond, label :: Label }
                        | Ret { ann :: a }
                        | FMovXX { ann :: a, dDest, dSrc :: freg }
                        | FMovDR { ann :: a, dDest :: freg, rSrc :: reg }
                        | FMovXC { ann :: a, dDest :: freg, dC :: Double }
                        | MovRR { ann :: a, rDest, rSrc :: reg }
                        | MovRC { ann :: a, rDest :: reg, cSrc :: Word16 }
                        | MovRCf { ann :: a, rDest :: reg, cfunc :: CFunc }
                        | MovRL { ann :: a, rDest :: reg, lSrc :: Int }
                        | MovK { ann :: a, rDest :: reg, cSrc :: Word16, lsl :: Int }
                        | Ldr { ann :: a, rDest :: reg, aSrc :: Addr reg }
                        | Str { ann :: a, rSrc :: reg, aDest :: Addr reg }
                        | LdrD { ann :: a, dDest :: freg, aSrc :: Addr reg }
                        | StrD { ann :: a, dSrc :: freg, aDest :: Addr reg }
                        | SubRR { ann :: a, rDest, rSrc1, rSrc2 :: reg }
                        | AddRR { ann :: a, rDest, rSrc1, rSrc2 :: reg }
                        | MulRR { ann :: a, rDest, rSrc1, rSrc2 :: reg }
                        | Madd { ann :: a, rDest, rSrc1, rSrc2, rSrc3 :: reg }
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
                        | Scvtf { ann :: a, dDest :: freg, rSrc :: reg }
                        | Fcvtms { ann :: a, rDest :: reg, dSrc :: freg }
                        | Stp { ann :: a, rSrc1, rSrc2 :: reg, aDest :: Addr reg }
                        | Ldp { ann :: a, rDest1, rDest2 :: reg, aSrc :: Addr reg }
                        | StpD { ann :: a, dSrc1, dSrc2 :: freg, aDest :: Addr reg }
                        | LdpD { ann :: a, dDest1, dDest2 :: freg, aSrc :: Addr reg }
                        | Fmadd { ann :: a, dDest, dSrc1, dSrc2, dSrc3 :: freg }
                        | Fmsub { ann :: a, dDest, dSrc1, dSrc2, dSrc3 :: freg }
                        | Fsqrt { ann :: a, dDest, dSrc :: freg }
                        | MrsR { ann :: a, rDest :: reg }
                        | Fmax { ann :: a, dDest, dSrc1, dSrc2 :: freg }
                        | Fabs { ann :: a, dDest, dSrc :: freg }
                        | Csel { ann :: a, rDest, rSrc1, rSrc2 :: reg, cond :: Cond }
                        | Tbnz { ann :: a, rSrc :: reg, bit :: Word8, label :: Label }
                        | Tbz { ann :: a, rSrc :: reg, bit :: Word8, label :: Label }
                        | Cbnz { ann :: a, rSrc :: reg, label :: Label }
                        | Fcsel { ann :: a, dDest, dSrc1, dSrc2 :: freg, cond :: Cond }
                        | Cset { ann :: a, rDest :: reg, cond :: Cond }
                        | TstI { ann :: a, rSrc1 :: reg, cSrc :: Word16 }
                        deriving (Functor)

instance Copointed (AArch64 reg freg) where copoint = ann

mapR :: (areg -> reg) -> AArch64 areg afreg a -> AArch64 reg afreg a
mapR _ (Label x l)           = Label x l
mapR _ (B x l)               = B x l
mapR _ (Bc x c l)            = Bc x c l
mapR _ (Bl x f)              = Bl x f
mapR _ (FMovXX l r0 r1)      = FMovXX l r0 r1
mapR _ (FMovXC l r0 c)       = FMovXC l r0 c
mapR f (MovRR l r0 r1)       = MovRR l (f r0) (f r1)
mapR f (MovRC l r c)         = MovRC l (f r) c
mapR f (Ldr l r a)           = Ldr l (f r) (f <$> a)
mapR f (Str l r a)           = Str l (f r) (f <$> a)
mapR f (LdrD l xr a)         = LdrD l xr (f <$> a)
mapR f (AddRR l r0 r1 r2)    = AddRR l (f r0) (f r1) (f r2)
mapR f (SubRR l r0 r1 r2)    = SubRR l (f r0) (f r1) (f r2)
mapR f (AddRC l r0 r1 c)     = AddRC l (f r0) (f r1) c
mapR f (SubRC l r0 r1 c)     = SubRC l (f r0) (f r1) c
mapR f (Lsl l r0 r1 s)       = Lsl l (f r0) (f r1) s
mapR f (Asr l r0 r1 s)       = Asr l (f r0) (f r1) s
mapR f (CmpRR l r0 r1)       = CmpRR l (f r0) (f r1)
mapR f (CmpRC l r c)         = CmpRC l (f r) c
mapR f (Neg l r0 r1)         = Neg l (f r0) (f r1)
mapR _ (Fadd l xr0 xr1 xr2)  = Fadd l xr0 xr1 xr2
mapR _ (Fsub l xr0 xr1 xr2)  = Fsub l xr0 xr1 xr2
mapR _ (Fmul l xr0 xr1 xr2)  = Fmul l xr0 xr1 xr2
mapR _ (FcmpZ l xr)          = FcmpZ l xr
mapR _ (Ret l)               = Ret l
mapR f (MulRR l r0 r1 r2)    = MulRR l (f r0) (f r1) (f r2)
mapR f (Madd l r0 r1 r2 r3)  = Madd l (f r0) (f r1) (f r2) (f r3)
mapR f (StrD l d a)          = StrD l d (f <$> a)
mapR _ (Fdiv l d0 d1 d2)     = Fdiv l d0 d1 d2
mapR f (Scvtf l d r)         = Scvtf l d (f r)
mapR f (Fcvtms l r d)        = Fcvtms l (f r) d
mapR f (MovK l r u s)        = MovK l (f r) u s
mapR f (FMovDR l d r)        = FMovDR l d (f r)
mapR _ (Fcmp l d0 d1)        = Fcmp l d0 d1
mapR f (Ldp l r0 r1 a)       = Ldp l (f r0) (f r1) (f <$> a)
mapR f (Stp l r0 r1 a)       = Stp l (f r0) (f r1) (f <$> a)
mapR f (LdpD l d0 d1 a)      = LdpD l d0 d1 (f <$> a)
mapR f (StpD l d0 d1 a)      = StpD l d0 d1 (f <$> a)
mapR _ (Fmadd l d0 d1 d2 d3) = Fmadd l d0 d1 d2 d3
mapR _ (Fmsub l d0 d1 d2 d3) = Fmsub l d0 d1 d2 d3
mapR _ (Fsqrt l d0 d1)       = Fsqrt l d0 d1
mapR f (MrsR l r)            = MrsR l (f r)
mapR f (MovRCf l r cf)       = MovRCf l (f r) cf
mapR f (MovRL x r l)         = MovRL x (f r) l
mapR f (Blr l r)             = Blr l (f r)
mapR _ (Fmax l d0 d1 d2)     = Fmax l d0 d1 d2
mapR _ (Fabs l d0 d1)        = Fabs l d0 d1
mapR f (Csel l r0 r1 r2 p)   = Csel l (f r0) (f r1) (f r2) p
mapR f (Tbnz l r n p)        = Tbnz l (f r) n p
mapR f (Tbz l r n p)         = Tbz l (f r) n p
mapR f (Cbnz x r l)          = Cbnz x (f r) l
mapR _ (Fcsel l d0 d1 d2 p)  = Fcsel l d0 d1 d2 p
mapR f (TstI l r i)          = TstI l (f r) i
mapR f (Cset l r c)          = Cset l (f r) c

mapFR :: (afreg -> freg) -> AArch64 areg afreg a -> AArch64 areg freg a
mapFR _ (Label x l)           = Label x l
mapFR _ (B x l)               = B x l
mapFR _ (Bc x c l)            = Bc x c l
mapFR _ (Bl x f)              = Bl x f
mapFR f (FMovXX l xr0 xr1)    = FMovXX l (f xr0) (f xr1)
mapFR f (FMovXC l xr c)       = FMovXC l (f xr) c
mapFR _ (MovRR l r0 r1)       = MovRR l r0 r1
mapFR _ (MovRC l r0 c)        = MovRC l r0 c
mapFR _ (Ldr l r a)           = Ldr l r a
mapFR _ (Str l r a)           = Str l r a
mapFR f (LdrD l xr a)         = LdrD l (f xr) a
mapFR _ (AddRR l r0 r1 r2)    = AddRR l r0 r1 r2
mapFR _ (AddRC l r0 r1 c)     = AddRC l r0 r1 c
mapFR _ (SubRR l r0 r1 r2)    = SubRR l r0 r1 r2
mapFR _ (SubRC l r0 r1 c)     = SubRC l r0 r1 c
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
mapFR f (Fdiv l d0 d1 d2)     = Fdiv l (f d0) (f d1) (f d2)
mapFR _ (MulRR l r0 r1 r2)    = MulRR l r0 r1 r2
mapFR _ (Madd l r0 r1 r2 r3)  = Madd l r0 r1 r2 r3
mapFR f (StrD l d a)          = StrD l (f d) a
mapFR f (Scvtf l d r)         = Scvtf l (f d) r
mapFR f (Fcvtms l r d)        = Fcvtms l r (f d)
mapFR _ (MovK l r u s)        = MovK l r u s
mapFR f (FMovDR l d r)        = FMovDR l (f d) r
mapFR f (Fcmp l d0 d1)        = Fcmp l (f d0) (f d1)
mapFR _ (Stp l r0 r1 a)       = Stp l r0 r1 a
mapFR _ (Ldp l r0 r1 a)       = Ldp l r0 r1 a
mapFR f (StpD l d0 d1 a)      = StpD l (f d0) (f d1) a
mapFR f (LdpD l d0 d1 a)      = LdpD l (f d0) (f d1) a
mapFR f (Fmadd l d0 d1 d2 d3) = Fmadd l (f d0) (f d1) (f d2) (f d3)
mapFR f (Fmsub l d0 d1 d2 d3) = Fmsub l (f d0) (f d1) (f d2) (f d3)
mapFR f (Fsqrt l d0 d1)       = Fsqrt l (f d0) (f d1)
mapFR _ (MrsR l r)            = MrsR l r
mapFR _ (Blr l r)             = Blr l r
mapFR _ (MovRCf l r cf)       = MovRCf l r cf
mapFR _ (MovRL x r l)         = MovRL x r l
mapFR f (Fmax l d0 d1 d2)     = Fmax l (f d0) (f d1) (f d2)
mapFR f (Fabs l d0 d1)        = Fabs l (f d0) (f d1)
mapFR _ (Csel l r0 r1 r2 p)   = Csel l r0 r1 r2 p
mapFR _ (Tbnz l r n p)        = Tbnz l r n p
mapFR _ (Tbz l r n p)         = Tbz l r n p
mapFR _ (Cbnz x r l)          = Cbnz x r l
mapFR f (Fcsel l d0 d1 d2 p)  = Fcsel l (f d0) (f d1) (f d2) p
mapFR _ (TstI l r i)          = TstI l r i
mapFR _ (Cset l r c)          = Cset l r c

s2 :: [a] -> [(a, Maybe a)]
s2 (r0:r1:rs) = (r0, Just r1):s2 rs
s2 [r]        = [(r, Nothing)]
s2 []         = []

pus, pos :: [AReg] -> [AArch64 AReg freg ()]
pus = concatMap go.s2 where go (r0, Just r1) = [SubRC () SP SP 16, Stp () r0 r1 (R SP)]; go (r, Nothing) = [SubRC () SP SP 16, Str () r (R SP)]
pos = concatMap go.reverse.s2 where go (r0, Just r1) = [Ldp () r0 r1 (R SP), AddRC () SP SP 16]; go (r, Nothing) = [Ldr () r (R SP), AddRC () SP SP 16]

puds, pods :: [freg] -> [AArch64 AReg freg ()]
puds = concatMap go.s2 where go (r0, Just r1) = [SubRC () SP SP 16, StpD () r0 r1 (R SP)]; go (r, Nothing) = [SubRC () SP SP 16, StrD () r (R SP)]
pods = concatMap go.reverse.s2 where go (r0, Just r1) = [LdpD () r0 r1 (R SP), AddRC () SP SP 16]; go (r, Nothing) = [LdrD () r (R SP), AddRC () SP SP 16]

hexd :: (Integral a, Show a) => a -> Doc ann
hexd = pretty.($"").(("#0x"++).).showHex

instance (Pretty reg, Pretty freg) => Pretty (AArch64 reg freg a) where
    pretty (Label _ l)           = prettyLabel l <> ":"
    pretty (B _ l)               = i4 ("b" <+> prettyLabel l)
    pretty (Blr _ r)             = i4 ("blr" <+> pretty r)
    pretty (Bl _ l)              = i4 ("bl" <+> pSym l)
    pretty (Bc _ c l)            = i4 ("b." <> pretty c <+> prettyLabel l)
    pretty (FMovXX _ xr0 xr1)    = i4 ("fmov" <+> pretty xr0 <> "," <+> pretty xr1)
    pretty (FMovDR _ d r)        = i4 ("fmov" <+> pretty d <> "," <+> pretty r)
    pretty (MovRR _ r0 r1)       = i4 ("mov" <+> pretty r0 <> "," <+> pretty r1)
    pretty (MovRC _ r u)         = i4 ("mov" <+> pretty r <> "," <+> hexd u)
    pretty (Ldr _ r a)           = i4 ("ldr" <+> pretty r <> "," <+> pretty a)
    pretty (Str _ r a)           = i4 ("str" <+> pretty r <> "," <+> pretty a)
    pretty (LdrD _ xr a)         = i4 ("ldr" <+> pretty xr <> "," <+> pretty a)
    pretty (StrD _ xr a)         = i4 ("str" <+> pretty xr <> "," <+> pretty a)
    pretty (AddRR _ rD rS rS')   = i4 ("add" <+> pretty rD <> "," <+> pretty rS <> "," <+> pretty rS')
    pretty (SubRR _ rD rS rS')   = i4 ("sub" <+> pretty rD <> "," <+> pretty rS <> "," <+> pretty rS')
    pretty (MulRR _ rD rS rS')   = i4 ("mul" <+> pretty rD <> "," <+> pretty rS <> "," <+> pretty rS')
    pretty (SubRC _ rD rS u)     = i4 ("sub" <+> pretty rD <> "," <+> pretty rS <> "," <+> hexd u)
    pretty (AddRC _ rD rS u)     = i4 ("add" <+> pretty rD <> "," <+> pretty rS <> "," <+> hexd u)
    pretty (Lsl _ rD rS u)       = i4 ("lsl" <+> pretty rD <> "," <+> pretty rS <> "," <+> hexd u)
    pretty (Asr _ rD rS u)       = i4 ("asr" <+> pretty rD <> "," <+> pretty rS <> "," <+> hexd u)
    pretty (CmpRC _ r u)         = i4 ("cmp" <+> pretty r <> "," <+> hexd u)
    pretty (CmpRR _ r0 r1)       = i4 ("cmp" <+> pretty r0 <> "," <+> pretty r1)
    pretty (Neg _ rD rS)         = i4 ("neg" <+> pretty rD <> "," <+> pretty rS)
    pretty (Fmul _ rD r0 r1)     = i4 ("fmul" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (Fadd _ rD r0 r1)     = i4 ("fadd" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (Fsub _ rD r0 r1)     = i4 ("fsub" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (Fdiv _ rD r0 r1)     = i4 ("fdiv" <+> pretty rD <> "," <+> pretty r0 <> "," <+> pretty r1)
    pretty (FcmpZ _ xr)          = i4 ("fcmp" <+> pretty xr <> "," <+> "#0.0")
    pretty Ret{}                 = i4 "ret"
    pretty (Scvtf _ d r)         = i4 ("scvtf" <+> pretty d <> "," <+> pretty r)
    pretty (Fcvtms _ r d)        = i4 ("fcvtms" <+> pretty r <> "," <+> pretty d)
    pretty (MovK _ r i s)        = i4 ("movk" <+> pretty r <> "," <+> hexd i <> "," <+> "LSL" <+> "#" <> pretty s )
    pretty (Fcmp _ d0 d1)        = i4 ("fcmp" <+> pretty d0 <> "," <+> pretty d1)
    pretty (Stp _ r0 r1 a)       = i4 ("stp" <+> pretty r0 <> "," <+> pretty r1 <> "," <+> pretty a)
    pretty (Ldp _ r0 r1 a)       = i4 ("ldp" <+> pretty r0 <> "," <+> pretty r1 <> "," <+> pretty a)
    pretty (StpD _ d0 d1 a)      = i4 ("stp" <+> pretty d0 <> "," <+> pretty d1 <> "," <+> pretty a)
    pretty (LdpD _ d0 d1 a)      = i4 ("ldp" <+> pretty d0 <> "," <+> pretty d1 <> "," <+> pretty a)
    pretty (Fmadd _ d0 d1 d2 d3) = i4 ("fmadd" <+> pretty d0 <> "," <+> pretty d1 <> "," <+> pretty d2 <> "," <+> pretty d3)
    pretty (Fmsub _ d0 d1 d2 d3) = i4 ("fmsub" <+> pretty d0 <> "," <+> pretty d1 <> "," <+> pretty d2 <> "," <+> pretty d3)
    pretty (Madd _ r0 r1 r2 r3)  = i4 ("madd" <+> pretty r0 <> "," <+> pretty r1 <> "," <+> pretty r2 <> "," <+> pretty r3)
    pretty (Fsqrt _ d0 d1)       = i4 ("fsqrt" <+> pretty d0 <> "," <+> pretty d1)
    pretty (MrsR _ r)            = i4 ("mrs" <+> pretty r <> "," <+> "rndr")
    pretty (MovRCf _ r cf)       = i4 ("mov" <+> pretty r <> "," <+> pretty cf)
    pretty (MovRL _ r l)         = i4 ("mov" <+> pretty r <> "," <+> pretty l)
    pretty (Fmax _ d0 d1 d2)     = i4 ("fmax" <+> pretty d0 <> "," <+> pretty d1 <> "," <+> pretty d2)
    pretty (Fabs _ d0 d1)        = i4 ("fabs" <+> pretty d0 <> "," <+> pretty d1)
    pretty (Csel _ r0 r1 r2 p)   = i4 ("csel" <+> pretty r0 <> "," <+> pretty r1 <> "," <+> pretty r2 <> "," <+> pretty p)
    pretty (Tbnz _ r n l)        = i4 ("tbnz" <+> pretty r <> "," <+> "#" <> pretty n <> "," <+> prettyLabel l)
    pretty (Tbz _ r n l)         = i4 ("tbz" <+> pretty r <> "," <+> "#" <> pretty n <> "," <+> prettyLabel l)
    pretty (Cbnz _ r l)          = i4 ("cbnz" <+> pretty r <> "," <+> prettyLabel l)
    pretty (Fcsel _ d0 d1 d2 p)  = i4 ("fcsel" <+> pretty d0 <> "," <+> pretty d1 <> "," <+> pretty d2 <> "," <+> pretty p)
    pretty (TstI _ r i)          = i4 ("tst" <+> pretty r <> "," <+> pretty i)
    pretty (Cset _ r c)          = i4 ("cset" <+> pretty r <> "," <+> pretty c)

instance (Pretty reg, Pretty freg) => Show (AArch64 reg freg a) where show=show.pretty

prettyLive :: (Pretty reg, Pretty freg, Pretty o) => AArch64 reg freg o -> Doc ann
prettyLive r = pretty r <+> pretty (ann r)

prettyDebug :: (Pretty freg, Pretty reg, Pretty o) => [AArch64 reg freg o] -> Doc ann
prettyDebug = prettyLines . fmap prettyLive
