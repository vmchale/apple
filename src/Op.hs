{-# LANGUAGE OverloadedStrings #-}

module Op ( FUn (..)
          , FBin (..)
          , IUn (..)
          , IBin (..)
          , BBin (..)
          , IRel (..)
          , FRel (..)
          ) where

import           Prettyprinter (Pretty (..))

data FUn = FSqrt | FLog | FSin | FCos | FAbs

data IUn = IEven | IOdd

data FBin = FPlus | FMinus | FTimes | FDiv | FMax | FMin | FExp

data BBin = AndB | OrB | XorB

data IBin = IPlus | IMinus | ITimes | IAsr | IMax | IMin | IDiv | IAsl | IRem | BI !BBin

data IRel = IEq | INeq | IGt | ILt | ILeq | IGeq
data FRel = FEq | FNeq | FGt | FLt | FLeq | FGeq

instance Pretty IRel where
    pretty IEq  = "="; pretty INeq = "!="; pretty IGt  = ">"
    pretty ILt  = "<"; pretty ILeq = "≤"; pretty IGeq = "≥"

instance Pretty FRel where
    pretty FEq  = "="; pretty FNeq = "!="; pretty FGt  = ">"
    pretty FLt  = "<"; pretty FLeq = "≤"; pretty FGeq = "≥"

instance Pretty BBin where
   pretty AndB = "∧"; pretty XorB = "⊻"; pretty OrB = "∨"

instance Pretty IBin where
    pretty IPlus  = "+"
    pretty IMinus = "-"
    pretty ITimes = "*"
    pretty IDiv   = "div"
    pretty IAsl   = "asl"; pretty IAsr   = "asr"
    pretty IMax   = "max"; pretty IMin   = "min"
    pretty IRem   = "rem"
    pretty (BI p) = pretty p

instance Pretty FBin where
    pretty FPlus  = "+";
    pretty FMinus = "-"
    pretty FTimes = "*"
    pretty FDiv   = "%"
    pretty FExp   = "^"
    pretty FMax   = "max"
    pretty FMin   = "min"

instance Pretty FUn where
    pretty FSqrt = "sqrt"
    pretty FLog  = "log"
    pretty FSin  = "sin"; pretty FCos  = "cos"
    pretty FAbs  = "abs"

instance Pretty IUn where
    pretty IEven = "even"; pretty IOdd = "odd"
