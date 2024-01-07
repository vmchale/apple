{-# LANGUAGE OverloadedStrings #-}

-- first IR with for loops and array accesses, inspired by C
module C ( Temp (..)
         , FTemp (..)
         , ArrAcc (..)
         , CE (..)
         , CFE (..)
         , CS (..)
         , FBin (..)
         , IRel (..)
         , Label, AsmData
         , LSt (..)
         , prettyCS
         ) where

import           Data.Int          (Int64)
import qualified Data.IntMap       as IM
import           Data.Word         (Word64)
import           Prettyprinter     (Doc, Pretty (..), braces, brackets, comma, dot, hardline, indent, parens, (<+>))
import           Prettyprinter.Ext

type Label = Word; type AsmData = IM.IntMap [Word64]

data Temp = ITemp !Int | ATemp !Int
          | C0 | C1 | C2 | C3 | C4 | C5 | CRet

data FTemp = FTemp !Int
           | F0 | F1 | F2 | F3 | F4 | F5 | FRet0 | FRet1

instance Pretty Temp where
    pretty (ITemp i) = "T" <> pretty i
    pretty (ATemp i) = "AT" <> pretty i
    pretty C0        = "CArg0"
    pretty C1        = "CArg1"
    pretty C2        = "CArg2"
    pretty C3        = "CArg3"
    pretty C4        = "CArg4"
    pretty C5        = "CArg5"
    pretty CRet      = "CRet"

instance Pretty FTemp where
    pretty (FTemp i) = "X" <> pretty i
    pretty F0        = "FArg0"
    pretty F1        = "FArg1"
    pretty F2        = "FArg2"
    pretty F3        = "FArg3"
    pretty F4        = "FArg4"
    pretty F5        = "FArg5"
    pretty FRet0     = "FRet0"
    pretty FRet1     = "FRet1"

data ArrAcc = AElem Temp CE (Maybe Int) -- pointer, elem., label for tracking liveness
            | ARnk Temp (Maybe Int)
            | ADim Temp CE (Maybe Int)

instance Pretty ArrAcc where
    pretty (AElem t e _) = pretty t <> brackets (pretty e)
    pretty (ADim t e _)  = pretty t <> dot <> "dim" <> brackets (pretty e)
    pretty (ARnk t _)    = "rnk" <> parens (pretty t)

data IRel = Gt | Lt | Lte | Gte | Eq | Neq

instance Pretty IRel where
    pretty Gt=">"; pretty Lt="<"; pretty Lte="≤"; pretty Gte="≥"; pretty Eq="="; pretty Neq="≠"

data IBin = IPlus | ITimes | IMinus | IAsl

instance Pretty IBin where
    pretty IPlus="+"; pretty IAsl="asl"; pretty ITimes="*"; pretty IMinus="-"

data FBin = FPlus | FTimes | FMinus

instance Pretty FBin where
    pretty FPlus="+"; pretty FTimes="*"; pretty FMinus="-"

-- array access to be desugared (size, element...)
data CE = EAt ArrAcc | Bin IBin CE CE | Tmp Temp | ConstI Int64

instance Pretty CE where
    pretty (Tmp t)        = pretty t
    pretty (ConstI i)     = pretty i
    pretty (Bin op e0 e1) = parens (pretty op <+> pretty e0 <+> pretty e1)
    pretty (EAt a)        = pretty a

instance Num CE where
    (+) = Bin IPlus; (*) = Bin ITimes; (-) = Bin IMinus; fromInteger=ConstI . fromInteger

data CFE = FAt ArrAcc | FBin FBin CFE CFE | FTmp FTemp | ConstF Double

instance Num CFE where
    (+) = FBin FPlus; (*) = FBin FTimes; (-) = FBin FMinus; fromInteger=ConstF . fromInteger

instance Pretty CFE where
    pretty (FAt a)         = pretty a
    pretty (FBin op x0 x1) = parens (pretty op <+> pretty x0 <+> pretty x1)
    pretty (FTmp t)        = pretty t
    pretty (ConstF x)      = pretty x

data CS = For Temp IRel CE [CS]
        | MT Temp CE
        | MX FTemp CFE
        | Wr ArrAcc CE
        | WrF ArrAcc CFE
        | Ma Int Temp CE -- label, temp, size
        | Free Temp
        | RA !Int -- return array no-op (takes label)

instance Pretty CS where
    pretty (MT t e)         = pretty t <+> "=" <+> pretty e
    pretty (MX t e)         = pretty t <+> "=" <+> pretty e
    pretty (Wr a e)         = pretty a <+> "=" <+> pretty e
    pretty (WrF a e)        = pretty a <+> "=" <+> pretty e
    pretty (Ma _ t e)       = pretty t <+> "=" <+> "malloc" <> parens (pretty e)
    pretty (For t rel e ss) = "for" <> parens (pretty t <> comma <+> pretty t <+> pretty rel <+> pretty e) <#> braces (indent 4 (pCS ss<>hardline))

prettyCS :: (AsmData, [CS]) -> Doc ann
prettyCS (ds,ss) = pCS ss

pCS=prettyLines.fmap pretty

data LSt = LSt { clabels :: [Label], ctemps :: [Int] }
