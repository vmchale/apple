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
import           Prettyprinter     (Doc, Pretty (..), brackets, dot, parens, (<+>))
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

data ArrAcc = AElem Temp CE (Maybe Int) -- pointer, elem., label for tracking liveness
            | ARnk Temp (Maybe Int)
            | ADim Temp CE (Maybe Int)

instance Pretty ArrAcc where
    pretty (AElem t e _) = pretty t <> brackets (pretty e)
    pretty (ADim t e _)  = pretty t <> dot <> "dim" <> brackets (pretty e)
    pretty (ARnk t _)    = "rnk" <> parens (pretty t)

data IRel = Gt | Lt | Lte | Gte | Eq | Neq

data IBin = IPlus | IAsl

instance Pretty IBin where
    pretty IPlus = "+"; pretty IAsl = "asl"

data FBin = FPlus | FTimes

-- array access to be desugared (size, element...)
data CE = EAt ArrAcc | Bin IBin CE CE | Tmp Temp | ConstI Int64

instance Pretty CE where
    pretty (Tmp t)        = pretty t
    pretty (ConstI i)     = pretty i
    pretty (Bin op e0 e1) = parens (pretty op <+> pretty e0 <+> pretty e1)
    pretty (EAt arrAcc)   = pretty arrAcc

data CFE = FAt ArrAcc | FBin FBin CFE CFE | FTmp FTemp | ConstF Double

data CS = For Temp IRel CE [CS]
        | MT Temp CE
        | MX FTemp CFE
        | Wr ArrAcc CE
        | WrF ArrAcc CFE
        | Ma Int Temp CE -- label, temp, size
        | Free Temp
        | RA !Int -- return array no-op (takes label)

instance Pretty CS where
    pretty (MT t e) = pretty t <+> "=" <+> pretty e

prettyCS :: (AsmData, [CS]) -> Doc ann
prettyCS (ds,ss) = prettyLines (pretty<$>ss)

data LSt = LSt { clabels :: [Label], ctemps :: [Int] }
