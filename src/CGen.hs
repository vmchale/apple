{-# LANGUAGE OverloadedStrings #-}

module CGen (CType(..), TTE(..), tCTy, pCty) where

import           A
import           Control.Exception (Exception)
import           Data.Bifunctor    (first)
import qualified Data.Text         as T
import           Prettyprinter     (Doc, Pretty (..), braces, parens, tupled, (<+>))
import           Prettyprinter.Ext

data CType = CR | CI | Af | Ai

instance Pretty CType where
    pretty CR = "F"; pretty CI = "I"; pretty Af = "Af"; pretty Ai = "Ai"

data CF = CF !T.Text [CType] CType

instance Pretty CF where
    pretty (CF n ins out) =
        let args = zip ins ['a'..] in
        "extern" <+> pretty out <+> pretty n <+> tupled (px<$>ins) <> ";"
            <#> px out <+> pretty n <> "_wrapper" <+> tupled (fmap (\(t,var) -> pretty t <+> pretty var) args)
            <> braces
                (foldMap d args
                <> pretty out <+> "res" <> "=" <> ax out (pretty n<>tupled (l.snd<$>args))<>";"
                <> foldMap f args
                <> "R res;")
        where px CR="F"; px CI="I"; px _="U"
              ax Af=("poke_af"<>).parens;ax Ai=("poke_ai"<>).parens;ax _=id
              d (t,var) = px t <+> l var <> "=" <> ax t (pretty var) <> ";"
              f (Af,var) = "free" <> parens (l var) <> ";"
              f (Ai,var) = "free" <> parens (l var) <> ";"
              f _        = mempty
              l var = "_" <> pretty var

-- type translation error
data TTE = HO | Poly | FArg | ArrFn deriving Show

instance Pretty TTE where
    pretty HO = "Higher order"; pretty Poly = "Too polymorphic"; pretty FArg = "Function as argument"; pretty ArrFn = "Arrays of functions are not supported."

pCty :: T.Text -> T a -> Either TTE (Doc ann)
pCty nm t = ("#include<apple_abi.h>" <#>) . pretty <$> nmtCTy nm t

nmtCTy :: T.Text -> T a -> Either TTE CF
nmtCTy nm t = do{(ins,out) <- irTy (rLi t); CF nm<$>traverse cTy ins<*>cTy out}

tCTy :: T a -> Either TTE ([CType], CType)
tCTy t = do{(ins,out) <- irTy (rLi t); (,)<$>traverse cTy ins<*>cTy out}

cTy :: T a -> Either TTE CType
cTy F               = pure CR
cTy I               = pure CI
cTy (Arr _ F)       = pure Af
cTy (Arr _ I)       = pure Ai
cTy (Arr _ Arr{})   = Left FArg
cTy (Arr _ Arrow{}) = Left ArrFn

instance Exception TTE where

irTy :: T a -> Either TTE ([T a], T a)
irTy F                 = pure ([], F)
irTy I                 = pure ([], I)
irTy B                 = pure ([], B)
irTy t@Arr{}           = pure ([], t)
irTy (Arrow Arrow{} _) = Left HO
irTy (Arrow t0 t1)     = first (t0:) <$> irTy t1
irTy TVar{}            = Left Poly
irTy Ρ{}               = Left Poly
