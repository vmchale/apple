{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Asm.Aarch64 ( AArch64 (..)
                   , AbsReg (..)
                   , FAbsReg (..)
                   , AReg (..)
                   , FAReg (..)
                   , dc
                   ) where

import           Asm.M
import           Control.DeepSeq (NFData (..))
import           Data.Semigroup  ((<>))
import           GHC.Float       (castDoubleToWord64)
import           GHC.Generics    (Generic)
import           Numeric         (showHex)
import           Prettyprinter   (Doc, Pretty (..), (<+>))

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

data AbsReg = IReg !Int | CArg0 | CArg1 | CArg2 | CArg3 | CArg4 | CArg5 | CArg6| CArg7 | LR | ASP
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

data FAbsReg = FReg !Int

instance Pretty FAbsReg where
    pretty (FReg i) = "F" <> pretty i

data AArch64 reg freg a = Label { ann :: a, label :: Label }
                        | B { ann :: a, label :: Label }
                        | FMovXX { ann :: a, dDest :: freg, dSrc :: freg }
                        | FMovXC { ann :: a, dDest :: freg, dC :: Double }
                        | MovRR { ann :: a, rDest :: reg, rSrc :: reg }

dc :: Double -> Doc ann
dc = pretty . ($ "") . (("#0x"++).) . showHex . castDoubleToWord64

instance (Pretty reg, Pretty freg) => Pretty (AArch64 reg freg a) where
    pretty (Label _ l)        = prettyLabel l
    pretty (B _ l)            = i4 ("b" <+> prettyLabel l)
    pretty (FMovXX _ xr0 xr1) = i4 ("fmov" <+> pretty xr0 <> "," <+> pretty xr1)
    pretty (FMovXC _ xr c)    = i4 ("fmov" <+> pretty xr <> "," <+> dc c)
    pretty (MovRR _ r0 r1)    = i4 ("mov" <+> pretty r0 <> "," <+> pretty r1)
