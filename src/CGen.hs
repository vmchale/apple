module CGen (CType(..), CAt(..), TTE(..), tCTy, pCty) where

import           A
import           Control.Exception (Exception)
import           Data.Bifunctor    (first)
import qualified Data.Text         as T
import           Prettyprinter     (Doc, Pretty (..), braces, parens, softline', tupled, (<+>))
import           Prettyprinter.Ext
import           Q

data CAt = CR | CI | CB

instance Pretty CAt where pretty CR="F"; pretty CI="J"; pretty CB="B"

data CType = SC !CAt | AC !CAt | ΠC [CType] | ΠA [CType]

instance Pretty CType where
    pretty (AC CR)="Af"; pretty (AC CI)="Ai"; pretty (AC CB)="Ab"
    pretty (SC at)=pretty at; pretty (ΠC [SC CR,SC CR]) = "F2"

data CF = CF !T.Text [CType] CType

instance Pretty CF where
    pretty (CF n ins out) =
        let args = zip ins ['a'..] in
        "extern" <+> pretty out <+> pretty n <+> tupled (px<$>ins) <> ";"
            <#> px out <+> pretty n <> "_wrapper" <+> tupled (fmap (\(t,var) -> pretty t <+> pretty var) args)
            <> softline' <> braces
                (d @<> args
                <> pretty out <+> "res" <> "=" <> ax out (pretty n<>tupled (l.snd<$>args))<>";"
                <> f @<> args
                <> "R res;")
        where px (SC CR)="F"; px (SC CI)="J"; px (SC CB)="B"; px (ΠC [SC CR,SC CR])="F2"; px _="U"
              ax (AC at)=(("poke_a"<>wa at)<>).parens;ax _=id;wa CR="f"; wa CI="i"; wa CB="b"
              d (t,var) = px t <+> l var <> "=" <> ax t (pretty var) <> ";"
              f (AC{},var) = "free" <> parens (l var) <> ";"
              f _          = mempty
              l var = "_" <> pretty var

-- type translation error
data TTE = HO | Poly | FArg | ArrFn | ArrΠ deriving Show

instance Pretty TTE where
    pretty HO = "Higher order"; pretty Poly = "Too polymorphic"; pretty FArg = "Function as argument"; pretty ArrFn = "Arrays of functions are not supported."; pretty ArrΠ = "Arrays of tuples of arrays not supported."

pCty :: T.Text -> T a -> Either TTE (Doc ann)
pCty nm t = ("#include<apple_abi.h>" <#>) . pretty <$> nmtCTy nm t

nmtCTy :: T.Text -> T a -> Either TTE CF
nmtCTy nm t = do{(ins,out) <- irTy (rLi t); CF nm<$>traverse cTy ins<*>cTy out}

tCTy :: T a -> Either TTE ([CType], CType)
tCTy t = do{(ins,out) <- irTy (rLi t); (,)<$>traverse cTy ins<*>cTy out}

isArr = \case Arr{} -> True; _ -> False

cTy :: T a -> Either TTE CType
cTy F                 = pure (SC CR)
cTy I                 = pure (SC CI)
cTy B                 = pure (SC CB)
cTy (Arr _ F)         = pure (AC CR)
cTy (Arr _ I)         = pure (AC CI)
cTy (Arr _ B)         = pure (AC CB)
cTy (P ts)            = ΠC <$> traverse cTy ts
cTy (Arr _ (P ts))    | any isArr ts = Left ArrΠ
                      | otherwise = ΠA <$> traverse cTy ts
cTy (Arrow Arrow{} _) = Left FArg
cTy (Arr _ Arrow{})   = Left ArrFn

instance Exception TTE where

irTy :: T a -> Either TTE ([T a], T a)
irTy F                 = pure ([], F)
irTy I                 = pure ([], I)
irTy B                 = pure ([], B)
irTy t@Arr{}           = pure ([], t)
irTy t@P{}             = pure ([], t)
irTy (Arrow Arrow{} _) = Left HO
irTy (Arrow t0 t1)     = first (t0:) <$> irTy t1
irTy TV{}              = Left Poly
irTy Ρ{}               = Left Poly
irTy IZ{}              = Left Poly
