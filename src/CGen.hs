{-# LANGUAGE OverloadedStrings #-}

module CGen ( pCty
            ) where

import           A
import           Control.Exception (Exception)
import           Data.Bifunctor    (first)
import qualified Data.Text         as T
import           Prettyprinter     (Doc, Pretty (..), tupled, (<+>))
import           Prettyprinter.Ext

data CType = CR | CI | Af | Ai

instance Pretty CType where
    pretty CR = "F"
    pretty CI = "I"
    pretty Af = "Af"
    pretty Ai = "Ai"

data CF = CF !T.Text [CType] CType

instance Pretty CF where
    pretty (CF n ins out) = pretty out <+> pretty n <+> tupled (pretty<$>ins)

-- type translation error
data TTE = HigherOrder | Poly | Nested | ArrFn deriving Show

pCty :: T.Text -> T a -> Either TTE (Doc ann)
pCty nm t = ("#include<apple_abi.h>" <#>) . pretty <$> tCTy nm t

tCTy :: T.Text -> T a -> Either TTE CF
tCTy nm t = do{(ins,out) <- irTy t; CF nm<$>traverse cTy ins<*>cTy out}

cTy :: T a -> Either TTE CType
cTy F               = pure CR
cTy I               = pure CI
cTy (Arr _ F)       = pure Af
cTy (Arr _ I)       = pure Ai
cTy (Arr _ Arr{})   = Left Nested
cTy (Arr _ Arrow{}) = Left ArrFn

instance Exception TTE where

irTy :: T a -> Either TTE ([T a], T a)
irTy F                 = pure ([], F)
irTy I                 = pure ([], I)
irTy B                 = pure ([], B)
irTy t@Arr{}           = pure ([], t)
irTy (Arrow Arrow{} _) = Left HigherOrder
irTy (Arrow t0 t1)     = first (t0:) <$> irTy t1
irTy TVar{}            = Left Poly
irTy Ρ{}               = Left Poly
