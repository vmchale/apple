{-# LANGUAGE FlexibleInstances #-}

-- first IR with for loops and array accesses, inspired by C
module C ( Temp (..), FTemp (..), F2Temp (..), BTemp (..)
         , ArrAcc (..)
         , CE (..), CFE (..), F1E, F2E
         , PE (..)
         , CS (..)
         , (=:)
         , Label, AsmData
         , LSt (..)
         , prettyCS
         , pL
         ) where

import           CF.AL
import           Data.Copointed
import           Data.Int          (Int64)
import qualified Data.IntMap       as IM
import           Data.Void         (Void)
import           Data.Word         (Word64)
import           Op
import           Prettyprinter     (Doc, Pretty (..), brackets, comma, dot, hardline, indent, lbrace, parens, rbrace, tupled, (<+>))
import           Prettyprinter.Ext
import           Q
import           Sh

type Label=Word; type AsmData = IM.IntMap [Word64]

data Temp = ITemp !Int | ATemp !Int
          | C0 | C1 | C2 | C3 | C4 | C5 | CRet deriving Eq

data BTemp = BTemp !Int | CBRet deriving Eq

data FTemp = FTemp !Int
           | F0 | F1 | F2 | F3 | F4 | F5 | FRet0 | FRet1 deriving Eq

newtype F2Temp = F2Temp Int deriving Eq

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

instance Pretty F2Temp where
    pretty (F2Temp i) = "Y" <> pretty i

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

instance Show Temp where show=show.pretty
instance Show FTemp where show=show.pretty
instance Show BTemp where show=show.pretty

data ArrAcc = AElem Temp CE (Maybe AL) CE Int64 -- pointer, rank, label for tracking liveness, elem., elem. size (bytes)
            -- TODO: more robust way to handle rank (often statically known)
            | ARnk Temp (Maybe AL)
            | ADim Temp CE (Maybe AL) -- pointer, #, label
            -- TODO: shape information
            | At Temp [CE] [CE] (Maybe AL) Int64 -- pointer to data, strides, indices, label, elem. size (bytes)
            | Raw Temp CE (Maybe AL) Int64 -- pointer to data, offset, label, element size
            | TupM Temp (Maybe AL)

instance Pretty ArrAcc where
    pretty (AElem t _ _ e _) = pretty t <> brackets (pretty e)
    pretty (ADim t e _)      = pretty t <> dot <> "dim" <> brackets (pretty e)
    pretty (ARnk t _)        = "rnk" <> parens (pretty t)
    pretty (At t s ix _ _)   = pretty t <> (brackets.pretty) @<> ix <+> (parens.pretty) @<> s
    pretty (Raw t o _ _)     = pretty t <> "@" <> pretty o
    pretty (TupM t _)        = "tup@" <> pretty t

instance Show ArrAcc where show=show.pretty

bPrec AndB=3; bPrec OrB=2; bPrec XorB=6; bPrec BEq=4

mPrec IPlus=Just 6;mPrec ITimes=Just 7;mPrec IMinus=Just 6;mPrec IDiv=Nothing;mPrec IRem=Nothing;mPrec IAsl=Nothing; mPrec IMax=Nothing; mPrec IMin=Nothing; mPrec IAsr=Nothing; mPrec (BI p) = Just$bPrec p
fprec FPlus=Just 6;fprec FMinus=Just 6;fprec FTimes=Just 7; fprec FDiv=Just 7; fprec FExp=Just 8; fprec FMax=Nothing; fprec FMin=Nothing

data CE = EAt ArrAcc | Bin IBin CE CE | Tmp Temp | KI !Int64 | CFloor (CFE FTemp Double CE)
        | LA !Int -- assembler data
        | DP Temp CE -- pointer, rank

instance Pretty CE where pretty=ps 0

instance PS CE where
    ps _ (Tmp t)        = pretty t
    ps _ (KI i)         = pretty i
    ps d (Bin op e0 e1) | Just d' <- mPrec op = parensp (d>d') (ps (d'+1) e0 <> pretty op <> ps (d'+1) e1)
                        | otherwise = parens (pretty op <+> ps 11 e0 <+> ps 11 e1)
    ps _ (EAt a)        = pretty a
    ps _ (LA n)         = "A_" <> pretty n
    ps _ (DP t _)       = "DATA" <> parens (pretty t)
    ps _ (CFloor x)     = "⌊" <> pretty x

instance Show CE where show=show.pretty

instance Num CE where
    (+) = Bin IPlus; (*) = Bin ITimes; (-) = Bin IMinus; fromInteger=KI . fromInteger

type F1E=CFE FTemp Double CE; type F2E=CFE F2Temp (Double, Double) Void

data CFE t x e = FAt ArrAcc | FBin FBin (CFE t x e) (CFE t x e) | FUn FUn (CFE t x e) | FTmp t | ConstF !x | IE e

instance Num (CFE t Double e) where
    (+) = FBin FPlus; (*) = FBin FTimes; (-) = FBin FMinus; fromInteger=ConstF . fromInteger

instance Fractional (CFE t Double e) where
    (/) = FBin FDiv; fromRational=ConstF . fromRational

instance (Pretty x, PS e, Pretty t, Pretty e) => Pretty (CFE t x e) where pretty=ps 0

data PE = IRel IRel CE CE
        | FRel FRel (CFE FTemp Double CE) (CFE FTemp Double CE)
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

instance (Pretty x, Pretty e, Pretty t, PS e) => PS (CFE t x e) where
    ps _ (FAt a)         = pretty a
    ps _ (FUn f e)       = parens (pretty f <+> pretty e)
    ps d (FBin op x0 x1) | Just d' <- fprec op = parensp (d>d') (ps (d'+1) x0 <+> pretty op <+> ps (d'+1) x1)
                         | otherwise = parens (pretty op <+> ps 11 x0 <+> ps 11 x1)
    ps _ (FTmp t)        = pretty t
    ps _ (ConstF x)      = pretty x
    ps d (IE e)          = parensp (d>10) ("itof" <+> ps 11 e)

instance (Pretty x, PS e, Pretty t, Pretty e) => Show (CFE t x e) where show=show.pretty

infix 9 =:

(=:) = MT ()

data CS a = For { lann :: a, ixVar :: Temp, eLow :: CE, loopCond :: IRel, eUpper :: CE, body :: [CS a] }
          | Rof { lann :: a, ixVar :: Temp, eCnt :: CE, body :: [CS a] }
          | Rof1 { lann :: a, ixVar :: Temp, eCnt :: CE, body :: [CS a] }
          | R2of { lann :: a, ixVar :: Temp, eCnt :: CE, body, body1 :: [CS a] }
          | R2ofE { lann :: a, ixVar :: Temp, eCnt :: CE, body :: [CS a] }
          | R2ofO { lann :: a, ixVar :: Temp, eCnt :: CE, body, body1 :: [CS a] }
          | For1 { lann :: a, tck :: CE, ixVar :: Temp, eLow :: CE, loopCond :: IRel, eUpper :: CE, body :: [CS a] }
          | F2or { lann :: a, ixVar :: Temp, eLow :: CE, loopCond :: IRel, eUpper :: CE, body, body1 :: [CS a] }
          | F2orE { lann :: a, ixVar :: Temp, eLow :: CE, loopCond :: IRel, eUpper :: CE, body :: [CS a] }
          | F2orO { lann :: a, ixVar :: Temp, eLow :: CE, loopCond :: IRel, eUpper :: CE, body, body1 :: [CS a]}
          | While { lann :: a, iVar :: Temp, loopCond :: IRel, eDone :: CE, body :: [CS a] }
          | WT { lann :: a, bE :: PE, body :: [CS a] }
          | MT { lann :: a, tDest :: Temp, tSrc :: CE }
          | MX { lann :: a, ftDest :: FTemp, ftSrc :: CFE FTemp Double CE }
          | MX2 { lann :: a, f2tDest :: F2Temp, f2tSrc :: CFE F2Temp (Double, Double) Void }
          | Comb { lann :: a, op2 :: FBin, ftDest :: FTemp, f2Src :: F2Temp }
          | DS { lann :: a, f2Dest :: F2Temp, fSrc :: FTemp } | Ins { lann :: a, f2dest :: F2Temp, fSrc :: FTemp }
          | MB { lann :: a, bDest :: BTemp, pSrc :: PE }
          | Wr { lann :: a, addr :: ArrAcc, wrE :: CE }
          | WrF { lann :: a, addr :: ArrAcc, wrF :: CFE FTemp Double CE }
          | Wr2F { lann :: a, addr :: ArrAcc, wrF2 :: CFE F2Temp (Double, Double) Void }
          | WrP { lann :: a, addr :: ArrAcc , wrB :: PE }
          | Ma { lann :: a, ash :: Sh (), label :: AL, temp :: Temp, rank :: CE, nElem :: CE, elemSz :: !Int64 }
          | Aa { lann :: a, label :: AL, dTemp, srcTemp :: Temp }
          | Free Temp
          | MaΠ { lann :: a, label :: AL, temp :: Temp, aBytes :: Int64 }
          | RA { lann :: a, label :: !AL } -- return array no-op (takes label)
          | CpyE { lann :: a, aDest, aSrc :: ArrAcc, nElem :: CE, elemSz :: !Int64 } -- copy elems
          | CpyD { lann :: a, aDest, aSrc :: ArrAcc, nDims :: CE } -- copy dims
          | Mv { lann :: a, aDest, aSrc :: ArrAcc, elemSz :: !Int64 }
          | Ifn't { lann :: a, scond :: PE, branch :: [CS a] }
          | If { lann :: a, scond :: PE, iBranch, eBranch :: [CS a] }
          | Sa8 { lann :: a, temp :: Temp, alloc8 :: CE } | Pop8 { lann :: a, aeBytes :: CE }
          | Sa { lann :: a, temp :: Temp, allocBytes :: CE } | Pop { lann :: a, aeBytes :: CE }
          | Cmov { lann :: a, scond :: PE, tdest :: Temp, src :: CE }
          | Fcmov { lann :: a, scond :: PE, fdest :: FTemp, fsrc :: CFE FTemp Double CE }
          -- TODO: Fcneg?
          | Cset { lann :: a, scond :: PE, bdest :: BTemp }
          | SZ { lann :: a, szDest, arr :: Temp, rank :: CE, mLabel :: Maybe AL }
          | PlProd { lann :: a, nDest :: Temp, pdims :: [CE] }
          | Rnd { lann :: a, rndDest :: Temp }
          | FRnd { lann :: a, frndDest :: FTemp }
          | Def { lann :: a, fLabel :: Label, body :: [CS a] }
          | G { lann :: a, gt, retLabel :: Label }
          deriving Functor
          -- TODO: PlDims cause we have diml
          -- STRIDES(...)

instance Copointed CS where copoint=lann

instance Pretty (CS a) where
    pretty = pL (const"")

pL f (MT l t (Bin IPlus (Tmp t') e)) | t==t' = pretty t <+> "+=" <+> pretty e <> f l
pL f (MT l t e)             = pretty t <+> "=" <+> pretty e <> f l
pL f (MX l t (FBin FPlus (FTmp t') e)) | t==t' = pretty t <+> "+=" <+> pretty e <> f l
pL f (MX2 l t (FBin FPlus (FTmp t') e)) | t==t' = pretty t <+> "+=" <+> pretty e <> f l
pL f (MX l t e)             = pretty t <+> "=" <+> pretty e <> f l
pL f (MX2 l t e)            = pretty t <+> "=" <+> pretty e <> f l
pL f (MB l t e)             = pretty t <+> "=" <+> pretty e <> f l
pL f (Wr l a e)             = pretty a <+> "=" <+> pretty e <> f l
pL f (WrF l a e)            = pretty a <+> "=" <+> pretty e <> f l
pL f (Wr2F l a e)           = pretty a <+> "=" <+> pretty e <> f l
pL f (WrP l a e)            = pretty a <+> "=" <+> pretty e <> f l
pL _ (Free t)               = "free" <+> pretty t
pL f (Ma l _ _ t rnk e sz)  = pretty t <+> "=" <+> "alloc" <> parens ("rnk=" <> pretty rnk <> comma <+> pretty e <> "*" <> pretty sz) <> f l
pL f (Aa l _ d s)           = pretty d <+> "=" <+> "a@" <> pretty s <> f l
pL f (MaΠ l _ t sz)         = pretty t <+> "=" <+> "malloc" <> parens (pretty sz) <> f l
pL f (For l t el rel eu ss) = "for" <> parens (pretty t <> comma <+> pretty t <> "≔" <> pretty el <> comma <+> pretty t <> pretty rel <> pretty eu) <+> lbrace <#> indent 4 (pCS f ss) <#> rbrace <> f l
pL f (Rof l t ec ss)         = "rof" <> parens (pretty t <> "≔" <> pretty ec <> comma <+> "nz" <+> pretty t <> comma <+> pretty t <> "--") <+> lbrace <#> indent 4 (pCS f ss) <#> rbrace <> f l
pL f (Rof1 l t ec ss)        = "rof-1" <> parens (pretty t <> "≔" <> pretty ec <> comma <+> "nz" <+> pretty t <> comma <+> pretty t <> "--") <+> lbrace <#> indent 4 (pCS f ss) <#> rbrace <> f l
pL f (R2of l t ec ss ss1)    = "rof" <> parens (pretty t <> "≔" <> pretty ec <> comma <+> "nz" <+> pretty t <> comma <+> pretty t <> "-=2") <+> lbrace <#> indent 4 (pCS f ss) <#> rbrace <#> lbrace <#> indent 4 (pCS f ss1) <#> rbrace <> f l
pL f (R2ofO l t ec ss ss1)   = "rof-o" <> parens (pretty t <> "≔" <> pretty ec <> comma <+> "nz" <+> pretty t <> comma <+> pretty t <> "-=2") <+> lbrace <#> indent 4 (pCS f ss) <#> rbrace <#> lbrace <#> indent 4 (pCS f ss1) <#> rbrace <> f l
pL f (R2ofE l t ec ss)       = "rof-e" <> parens (pretty t <> "≔" <> pretty ec <> comma <+> "nz" <+> pretty t <> comma <+> pretty t <> "-=2") <+> lbrace <#> indent 4 (pCS f ss) <#> rbrace <> f l
pL f (For1 l tk t el rel eu ss) = "for-1" <> parens (pretty t <> comma <+> pretty t <> "≔" <> pretty el <> comma <+> pretty t <> pretty rel <> pretty eu <> comma <+> pretty t <> "+=" <> pretty tk) <+> lbrace <#> indent 4 (pCS f ss) <#> rbrace <> f l
pL f (F2or l t el rel eu ss ss1) = "for" <> parens (pretty t <> comma <+> pretty t <> "=" <> pretty el <> comma <+> pretty t <> pretty rel <> pretty eu <> comma <+> pretty t <> "+=2") <#> lbrace <#> indent 4 (pCS f ss) <#> rbrace <#> lbrace <#> indent 4 (pCS f ss1) <#> rbrace <> f l
pL f (F2orE l t el rel eu ss) = "fore" <> parens (pretty t <> comma <+> pretty t <> "=" <> pretty el <> comma <+> pretty t <> pretty rel <> pretty eu <> comma <+> pretty t <> "+=2") <#> lbrace <#> indent 4 (pCS f ss) <#> rbrace <> f l
pL f (F2orO l t el rel eu ss ss1) = "for-o" <> parens (pretty t <> comma <+> pretty t <> "=" <> pretty el <> comma <+> pretty t <> pretty rel <> pretty eu <> comma <+> pretty t <> "+=2") <#> lbrace <#> indent 4 (pCS f ss) <#> rbrace <#> lbrace <#> indent 4 (pCS f ss1) <#> rbrace <> f l
pL f (While l t rel eb ss) = "while" <> parens (pretty t <> pretty rel <> pretty eb) <+> lbrace <#> indent 4 (pCS f ss) <#> rbrace <> f l
pL f (WT l p ss)           = "while" <> parens (pretty p) <+> lbrace <#> indent 4 (pCS f ss) <#> rbrace <> f l
pL f (Ifn't l p s)         = "ifn't" <+> parens (pretty p) <+> lbrace <#> indent 4 (pCS f s) <#> rbrace <> f l
pL f (If l p s0 s1)        = "if" <+> parens (pretty p) <+> lbrace <#> indent 4 (pCS f s0) <#> rbrace <+> "else" <+> lbrace <#> indent 4 (pCS f s1) <#> rbrace <> f l
pL _ RA{}                  = mempty
pL f (CpyE l a a' e n)     = "cpy" <+> pretty a <> comma <+> pretty a' <+> parens (pretty e<>"*"<>pretty n) <> f l
pL f (Mv l a a' n)         = "mv" <+> pretty a <> comma <+> pretty a' <+> parens (pretty n) <> f l
pL f (CpyD l a a' e)       = "cpydims" <+> pretty a <+> pretty a' <+> pretty e <> f l
pL f (Sa8 l t e)           = pretty t <+> "=" <+> "salloc" <> parens (pretty e) <> f l
pL f (Pop8 l e)            = "pop" <+> pretty e <> f l
pL f (Sa l t e)            = pretty t <+> "=" <+> "salloc" <> parens (pretty e) <> f l
pL f (Pop l e)             = "pop" <+> pretty e <> f l
pL f (Cmov l p t e)        = "if" <+> parens (pretty p) <+> lbrace <#> indent 4 (pretty t <+> "=" <+> pretty e) <#> rbrace <> f l
pL f (Fcmov l p t e)       = "if" <+> parens (pretty p) <+> lbrace <#> indent 4 (pretty t <+> "=" <+> pretty e) <#> rbrace <> f l
pL f (Cset l p t)          = pretty t <+> "=" <+> pretty p <> f l
pL f (SZ l td t _ _)       = pretty td <+> "=" <+> "SIZE" <> parens (pretty t) <> f l
pL f (PlProd l t ts)       = pretty t <+> "=" <+> "PRODUCT" <> tupled (pretty<$>ts) <> f l
pL f (Rnd l t)             = pretty t <+> "=" <+> "(rnd)" <> f l
pL f (FRnd l x)            = pretty x <+> "=" <+> "(frnd)" <> f l
pL f (Def la l cs)         = hardline <> pS l <> ":" <#> indent 4 (pCS f cs) <> f la
pL f (G la l _)            = "GOTO" <+> pS l <> f la
pL f (Comb l s t r)        = parens ("combine" <> pretty s <+> pretty t <> "," <+> pretty r) <> f l
pL f (DS l r t)            = parens ("dup" <+> pretty r <+> brackets (pretty t)) <> f l
pL f (Ins l r t)           = parens ("ins" <+> pretty r <+> pretty t) <> f l

pS :: Label -> Doc ann
pS l = "fun_" <> pretty l

instance Show (CS a) where show=show.pretty

prettyCS :: (AsmData, [CS a]) -> Doc ann
prettyCS (ds,ss) = pCS (const"") ss

pCS :: (a -> Doc ann) -> [CS a] -> Doc ann
pCS f=prettyLines.fmap (pL f)

data LSt = LSt { clabel :: !Label, ctemps :: !Int }
