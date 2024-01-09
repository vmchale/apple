{-# LANGUAGE OverloadedStrings #-}

-- first IR with for loops and array accesses, inspired by C
module C ( Temp (..)
         , FTemp (..)
         , ArrAcc (..)
         , CE (..)
         , CFE (..)
         , PE (..)
         , CS (..)
         , Label, AsmData
         , LSt (..)
         , prettyCS
         ) where

import           Data.Int          (Int64)
import qualified Data.IntMap       as IM
import           Data.Word         (Word64)
import           Op
import           Prettyprinter     (Doc, Pretty (..), brackets, comma, dot, indent, lbrace, parens, rbrace, (<+>))
import           Prettyprinter.Ext

type Label = Word; type AsmData = IM.IntMap [Word64]

data Temp = ITemp !Int | ATemp !Int
          | C0 | C1 | C2 | C3 | C4 | C5 | CRet deriving Eq

data FTemp = FTemp !Int
           | F0 | F1 | F2 | F3 | F4 | F5 | FRet0 | FRet1 deriving Eq

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

data ArrAcc = AElem Temp CE CE (Maybe Int) Int64 -- pointer, rank, elem., label for tracking liveness, elem. size (bytes)
            | ARnk Temp (Maybe Int)
            | ADim Temp CE (Maybe Int) -- pointer, #, label

instance Pretty ArrAcc where
    pretty (AElem t _ e _ _) = pretty t <> brackets (pretty e)
    pretty (ADim t e _)      = pretty t <> dot <> "dim" <> brackets (pretty e)
    pretty (ARnk t _)        = "rnk" <> parens (pretty t)

mPrec IPlus=Just 6;mPrec ITimes=Just 7;mPrec IMinus=Just 6;mPrec IDiv=Nothing;mPrec IAsl=Nothing; mPrec IMax=Nothing; mPrec IMin=Nothing; mPrec IAsr=Nothing
fprec FPlus=6;fprec FMinus=6;fprec FTimes=7; fprec FDiv=7

data CE = EAt ArrAcc | Bin IBin CE CE | Tmp Temp | ConstI Int64

instance Pretty CE where pretty=ps 0

instance PS CE where
    ps _ (Tmp t)        = pretty t
    ps _ (ConstI i)     = pretty i
    ps d (Bin op e0 e1) | Just d' <- mPrec op = parensp (d>d') (ps (d+1) e0 <> pretty op <> ps (d+1) e1)
                        | otherwise = parens (pretty op <+> pretty e0 <+> pretty e1)
    ps _ (EAt a)        = pretty a

instance Show CE where show=show.pretty

instance Num CE where
    (+) = Bin IPlus; (*) = Bin ITimes; (-) = Bin IMinus; fromInteger=ConstI . fromInteger

data CFE = FAt ArrAcc | FBin FBin CFE CFE | FUn FUn CFE | FTmp FTemp | ConstF Double | IE CE

instance Num CFE where
    (+) = FBin FPlus; (*) = FBin FTimes; (-) = FBin FMinus; fromInteger=ConstF . fromInteger

instance Fractional CFE where
    (/) = FBin FDiv; fromRational=ConstF . fromRational

instance Pretty CFE where pretty=ps 0

data PE = IRel IRel CE CE | FRel FRel CFE CFE | IUn IUn CE

instance Pretty PE where
    pretty (IRel rel e0 e1) = pretty e0 <+> pretty rel <+> pretty e1
    pretty (FRel rel e0 e1) = pretty e0 <+> pretty rel <+> pretty e1
    pretty (IUn p e)        = parens (pretty p <+> pretty e)

instance PS CFE where
    ps _ (FAt a)         = pretty a
    ps _ (FUn f e)       = parens (pretty f <+> pretty e)
    ps d (FBin op x0 x1) = parensp (d>fprec op) (ps (d+1) x0 <+> pretty op <+> ps (d+1) x1)
    ps _ (FTmp t)        = pretty t
    ps _ (ConstF x)      = pretty x
    ps d (IE e)          = parensp (d>10) ("itof" <+> ps (d+1) e)

instance Show CFE where show=show.pretty

data CS = For Temp CE IRel CE [CS]
        | While Temp IRel CE [CS]
        | MT Temp CE
        | MX FTemp CFE
        | Wr ArrAcc CE
        | WrF ArrAcc CFE
        | Ma Int Temp CE CE !Int64 -- label, temp, rank, #elements, element size in bytes
        | RA !Int -- return array no-op (takes label)
        | CpyE ArrAcc ArrAcc CE !Int64 -- copy elements
        | Ifn't PE [CS]

instance Pretty CS where
    pretty (MT t (Bin IPlus (Tmp t') e)) | t==t' = pretty t <+> "+=" <+> pretty e
    pretty (MT t e)             = pretty t <+> "=" <+> pretty e -- TODO: +=
    pretty (MX t (FBin FPlus (FTmp t') e)) | t==t' = pretty t <+> "+=" <+> pretty e
    pretty (MX t e)             = pretty t <+> "=" <+> pretty e
    pretty (Wr a e)             = pretty a <+> "=" <+> pretty e
    pretty (WrF a e)            = pretty a <+> "=" <+> pretty e
    pretty (Ma _ t rnk e sz)    = pretty t <+> "=" <+> "malloc" <> parens ("rnk=" <> pretty rnk <> comma <+> pretty e <> "*" <> pretty sz)
    pretty (For t el rel eu ss) = "for" <> parens (pretty t <> comma <+> pretty t <> "≔" <> pretty el <> comma <+> pretty t <> pretty rel <> pretty eu) <+> lbrace <#> indent 4 (pCS ss) <#> rbrace
    pretty (While t rel eb ss)  = "while" <> parens (pretty t <> pretty rel <> pretty eb) <+> lbrace <#> indent 4 (pCS ss) <#> rbrace
    pretty (Ifn't p s)          = "ifn't" <+> parens (pretty p) <+> lbrace <#> indent 4 (pCS s) <#> rbrace
    pretty RA{}                 = mempty
    pretty (CpyE a a' e n)      = "cpy" <+> pretty a <> comma <+> pretty a' <+> parens (pretty e<>"*"<>pretty n)

instance Show CS where show=show.pretty

prettyCS :: (AsmData, [CS]) -> Doc ann
prettyCS (ds,ss) = pCS ss

pCS=prettyLines.fmap pretty

data LSt = LSt { clabels :: [Label], ctemps :: [Int] }
