{-# LANGUAGE OverloadedStrings #-}

-- first IR with for loops and array accesses, inspired by C
module C ( Temp (..), FTemp (..), BTemp (..)
         , ArrAcc (..)
         , CE (..), CFE (..)
         , PE (..)
         , CS (..)
         , Label, AsmData
         , LSt (..)
         , prettyCS
         ) where

import           CF.AL
import           Data.Int          (Int64)
import qualified Data.IntMap       as IM
import           Data.Word         (Word64)
import           Op
import           Prettyprinter     (Doc, Pretty (..), brackets, comma, dot, hardline, indent, lbrace, parens, rbrace, tupled, (<+>))
import           Prettyprinter.Ext

type Label=Word; type AsmData = IM.IntMap [Word64]

data Temp = ITemp !Int | ATemp !Int
          | C0 | C1 | C2 | C3 | C4 | C5 | CRet deriving Eq

data BTemp = BTemp !Int | CBRet deriving Eq

data FTemp = FTemp !Int
           | F0 | F1 | F2 | F3 | F4 | F5 | FRet0 | FRet1 deriving Eq

instance Pretty BTemp where pretty (BTemp i) = "P" <> pretty i; pretty CBRet = "PRet"

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

data ArrAcc = AElem Temp CE CE (Maybe AL) Int64 -- pointer, rank, elem., label for tracking liveness, elem. size (bytes)
            -- TODO: more robust way to handle rank (often statically known)
            | ARnk Temp (Maybe AL)
            | ADim Temp CE (Maybe AL) -- pointer, #, label
            | At Temp [CE] [CE] (Maybe AL) Int64 -- pointer to data, strides, indices, label, elem. size (bytes)
            | Raw Temp CE (Maybe AL) Int64 -- pointer to data, offset, label, element size
            | TupM Temp (Maybe AL)

instance Pretty ArrAcc where
    pretty (AElem t _ e _ _) = pretty t <> brackets (pretty e)
    pretty (ADim t e _)      = pretty t <> dot <> "dim" <> brackets (pretty e)
    pretty (ARnk t _)        = "rnk" <> parens (pretty t)
    pretty (At t s ix _ _)   = pretty t <> foldMap (brackets.pretty) ix <+> foldMap (parens.pretty) s
    pretty (Raw t o _ _)     = pretty t <> "@" <> pretty o
    pretty (TupM t _)        = "tup@" <> pretty t

instance Show ArrAcc where show=show.pretty

bPrec AndB=3; bPrec OrB=2; bPrec XorB=6

mPrec IPlus=Just 6;mPrec ITimes=Just 7;mPrec IMinus=Just 6;mPrec IDiv=Nothing;mPrec IRem=Nothing;mPrec IAsl=Nothing; mPrec IMax=Nothing; mPrec IMin=Nothing; mPrec IAsr=Nothing; mPrec (BI p) = Just$bPrec p
fprec FPlus=Just 6;fprec FMinus=Just 6;fprec FTimes=Just 7; fprec FDiv=Just 7; fprec FExp=Just 8; fprec FMax=Nothing; fprec FMin=Nothing

data CE = EAt ArrAcc | Bin IBin CE CE | Tmp Temp | ConstI !Int64 | CFloor CFE
        | LA !Int -- assembler data
        | DP Temp CE -- pointer, rank

instance Pretty CE where pretty=ps 0

instance PS CE where
    ps _ (Tmp t)        = pretty t
    ps _ (ConstI i)     = pretty i
    ps d (Bin op e0 e1) | Just d' <- mPrec op = parensp (d>d') (ps (d'+1) e0 <> pretty op <> ps (d'+1) e1)
                        | otherwise = parens (pretty op <+> ps 11 e0 <+> ps 11 e1)
    ps _ (EAt a)        = pretty a
    ps _ (LA n)         = "A_" <> pretty n
    ps _ (DP t _)       = "DATA" <> parens (pretty t)
    ps _ (CFloor x)     = "⌊" <> pretty x

instance Show CE where show=show.pretty

instance Num CE where
    (+) = Bin IPlus; (*) = Bin ITimes; (-) = Bin IMinus; fromInteger=ConstI . fromInteger

data CFE = FAt ArrAcc | FBin FBin CFE CFE | FUn FUn CFE | FTmp FTemp | ConstF !Double | IE CE

instance Num CFE where
    (+) = FBin FPlus; (*) = FBin FTimes; (-) = FBin FMinus; fromInteger=ConstF . fromInteger

instance Fractional CFE where
    (/) = FBin FDiv; fromRational=ConstF . fromRational

instance Pretty CFE where pretty=ps 0

data PE = IRel IRel CE CE
        | FRel FRel CFE CFE
        | Boo BBin PE PE
        | BConst Bool
        | IUn IUn CE
        | Is BTemp
        | PAt ArrAcc
        | BU BUn PE

instance Pretty PE where
    pretty (IRel rel e0 e1) = pretty e0 <+> pretty rel <+> pretty e1
    pretty (FRel rel e0 e1) = pretty e0 <+> pretty rel <+> pretty e1
    pretty (IUn p e)        = pretty p <+> pretty e
    pretty (Is t)           = "is?" <+> pretty t
    pretty (PAt a)          = "b@" <> pretty a
    pretty (BConst True)    = "true"
    pretty (BConst False)   = "false"
    pretty (Boo op e0 e1)   = pretty e0 <+> pretty op <+> pretty e1
    pretty (BU op e)        = pretty op <> pretty e

instance PS CFE where
    ps _ (FAt a)         = pretty a
    ps _ (FUn f e)       = parens (pretty f <+> pretty e)
    ps d (FBin op x0 x1) | Just d' <- fprec op = parensp (d>d') (ps (d'+1) x0 <+> pretty op <+> ps (d'+1) x1)
                         | otherwise = parens (pretty op <+> ps 11 x0 <+> ps 11 x1)
    ps _ (FTmp t)        = pretty t
    ps _ (ConstF x)      = pretty x
    ps d (IE e)          = parensp (d>10) ("itof" <+> ps 11 e)

instance Show CFE where show=show.pretty

infix 9 :=

data CS = For Temp CE IRel CE [CS] | For1 Temp CE IRel CE [CS]
        | While Temp IRel CE [CS]
        | Temp := CE | MX FTemp CFE | MB BTemp PE
        | Wr ArrAcc CE | WrF ArrAcc CFE | WrP ArrAcc PE
        | Ma AL Temp CE CE !Int64 -- label, temp, rank, #elements, element size in bytes
        | MaΠ AL Temp CE
        | RA !AL -- return array no-op (takes label)
        | CpyE ArrAcc ArrAcc CE !Int64 -- copy elements
        | CpyD ArrAcc ArrAcc CE -- copy dims
        | Ifn't PE [CS]
        | If PE [CS] [CS]
        | Sa Temp CE | Pop CE
        | Cmov PE Temp CE | Fcmov PE FTemp CFE
        -- TODO: Fcneg?
        | Cset PE BTemp
        | SZ Temp Temp CE (Maybe AL) -- dest., pointer to array, rank, label
        | PlProd Temp [CE]
        | Rnd Temp | FRnd FTemp
        | Def Label [CS] | G Label Label
        -- TODO: PlDims cause we have diml

instance Pretty CS where
    pretty (t := (Bin IPlus (Tmp t') e)) | t==t' = pretty t <+> "+=" <+> pretty e
    pretty (t := e)             = pretty t <+> "=" <+> pretty e
    pretty (MX t (FBin FPlus (FTmp t') e)) | t==t' = pretty t <+> "+=" <+> pretty e
    pretty (MX t e)             = pretty t <+> "=" <+> pretty e
    pretty (MB t e)             = pretty t <+> "=" <+> pretty e
    pretty (Wr a e)             = pretty a <+> "=" <+> pretty e
    pretty (WrF a e)            = pretty a <+> "=" <+> pretty e
    pretty (WrP a e)            = pretty a <+> "=" <+> pretty e
    pretty (Ma _ t rnk e sz)    = pretty t <+> "=" <+> "malloc" <> parens ("rnk=" <> pretty rnk <> comma <+> pretty e <> "*" <> pretty sz)
    pretty (MaΠ _ t sz)         = pretty t <+> "=" <+> "malloc" <> parens (pretty sz)
    pretty (For t el rel eu ss) = "for" <> parens (pretty t <> comma <+> pretty t <> "≔" <> pretty el <> comma <+> pretty t <> pretty rel <> pretty eu) <+> lbrace <#> indent 4 (pCS ss) <#> rbrace
    pretty (For1 t el rel eu ss) = "for-1" <> parens (pretty t <> comma <+> pretty t <> "≔" <> pretty el <> comma <+> pretty t <> pretty rel <> pretty eu) <+> lbrace <#> indent 4 (pCS ss) <#> rbrace
    pretty (While t rel eb ss)  = "while" <> parens (pretty t <> pretty rel <> pretty eb) <+> lbrace <#> indent 4 (pCS ss) <#> rbrace
    pretty (Ifn't p s)          = "ifn't" <+> parens (pretty p) <+> lbrace <#> indent 4 (pCS s) <#> rbrace
    pretty (If p s0 s1)         = "if" <+> parens (pretty p) <+> lbrace <#> indent 4 (pCS s0) <#> rbrace <+> "else" <+> lbrace <#> indent 4 (pCS s1) <#> rbrace
    pretty RA{}                 = mempty
    pretty (CpyE a a' e n)      = "cpy" <+> pretty a <> comma <+> pretty a' <+> parens (pretty e<>"*"<>pretty n)
    pretty (CpyD a a' e)        = "cpydims" <+> pretty a <+> pretty a' <+> pretty e
    pretty (Sa t e)             = pretty t <+> "=" <+> "salloc" <> parens (pretty e)
    pretty (Pop e)              = "pop" <+> pretty e
    pretty (Cmov p t e)         = "if" <+> parens (pretty p) <+> lbrace <#> indent 4 (pretty t <+> "=" <+> pretty e) <#> rbrace
    pretty (Fcmov p t e)        = "if" <+> parens (pretty p) <+> lbrace <#> indent 4 (pretty t <+> "=" <+> pretty e) <#> rbrace
    pretty (Cset p t)           = pretty t <+> "=" <+> pretty p
    pretty (SZ td t _ _)        = pretty td <+> "=" <+> "SIZE" <> parens (pretty t)
    pretty (PlProd t ts)        = pretty t <+> "=" <+> "PRODUCT" <> tupled (pretty<$>ts)
    pretty (Rnd t)              = pretty t <+> "=" <+> "(rnd)"
    pretty (FRnd x)             = pretty x <+> "=" <+> "(frnd)"
    pretty (Def l cs)           = hardline <> pS l <> ":" <#> indent 4 (pCS cs)
    pretty (G l _)              = "GOTO" <+> pS l

pS :: Label -> Doc ann
pS l = "fun_" <> pretty l

instance Show CS where show=show.pretty

prettyCS :: (AsmData, [CS]) -> Doc ann
prettyCS (ds,ss) = pCS ss

pCS=prettyLines.fmap pretty

data LSt = LSt { clabel :: !Label, ctemps :: !Int }
