{-# LANGUAGE OverloadedStrings #-}

module Prettyprinter.Ext ( (<#>)
                         , prettyLines
                         , tupledBy
                         , ptxt
                         , aText
                         , prettyDumpBinds
                         , pAD
                         ) where

import           Data.Bits                 (Bits (..))
import qualified Data.IntMap               as IM
import qualified Data.Text                 as T
import           Data.Word                 (Word64)
import           Numeric                   (showHex)
import           Prettyprinter             (Doc, LayoutOptions (..), PageWidth (AvailablePerLine), Pretty (..), SimpleDocStream, concatWith, encloseSep, flatAlt, group, hardline,
                                            layoutSmart, vsep, (<+>))
import           Prettyprinter.Render.Text (renderStrict)

infixr 6 <#>
(<#>) :: Doc a -> Doc a -> Doc a
(<#>) x y = x <> hardline <> y

prettyLines :: [Doc ann] -> Doc ann
prettyLines = concatWith (<#>)

tupledBy :: Doc ann -> [Doc ann] -> Doc ann
tupledBy sep = group . encloseSep (flatAlt "( " "(") (flatAlt " )" ")") sep

appleLO :: LayoutOptions
appleLO = LayoutOptions (AvailablePerLine 180 1.0)

smartA :: Doc a -> SimpleDocStream a
smartA = layoutSmart appleLO

aText :: Doc a -> T.Text
aText = renderStrict.smartA

ptxt :: Pretty a => a -> T.Text
ptxt = aText.pretty

prettyBind :: (Pretty c, Pretty b) => (c, b) -> Doc a
prettyBind (i, j) = pretty i <+> "→" <+> pretty j

prettyDumpBinds :: Pretty b => IM.IntMap b -> Doc a
prettyDumpBinds b = vsep (prettyBind <$> IM.toList b)

ahex :: (Integral a, Show a) => a -> Doc ann
ahex = pretty.($"").showHex

pAD ds = prettyLines ((\(n,dd) -> "arr_" <> pretty n <> ":" <+> mconcat (fmap p64 dd)) <$> IM.toList ds)

p64 :: Word64 -> Doc ann
p64 w = ahex w0<>ahex w1<>ahex w2<>ahex w3
    where w0=w .&. 0xffff; w1=(w .&. 0xffff0000) `rotateR` 16; w2=(w .&. 0xFFFF00000000) `rotateR` 32; w3=(w .&. 0xFFFF000000000000) `rotateR` 48
