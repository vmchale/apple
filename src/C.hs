{-# LANGUAGE OverloadedStrings #-}

-- first IR with for loops and array accesses, inspired by C
module C ( Temp (..)
         , FTemp (..)
         , CE (..)
         , CFE (..)
         , CS (..)
         , FBin (..)
         , IRel (..)
         ) where

import Data.Int (Int64)
import Prettyprinter (Pretty (..))

data Temp = ITemp !Int | ATemp !Int
          | C0 | C1 | C2 | C3 | C4 | C5 | CRet

data FTemp = FTemp !Int
           | F0 | F1 | F2 | F3 | F4 | F5 | FRet0 | FRet1

instance Pretty Temp where
    pretty (ITemp i) = "T" <> pretty i
    pretty (ATemp i) = "AT" <> pretty i
    pretty C0 = "CArg0"
    pretty C1 = "CArg1"
    pretty C2 = "CArg2"

data ArrAcc = AElem Temp CE (Maybe Int) -- offet, elem., label for tracking liveness
            | ARnk Temp (Maybe Int)
            | ADim Temp CE (Maybe Int)

data IRel = Gt | Lt | Lte | Gte | Eq | Neq

data IBin = IPlus | IAsl

data FBin = FPlus | FTimes

-- array access to be desugared (size, element...)
data CE = EAt ArrAcc | Bin IBin CE CE | Tmp Temp | ConstI Int64
data CFE = FAt ArrAcc | FBin FBin CFE CFE | FTmp FTemp | ConstF Double

data CS = For Temp IRel CE CS
        | MT Temp CE
        | MX FTemp CFE
        | Wr ArrAcc CE
        | WrF ArrAcc CFE
