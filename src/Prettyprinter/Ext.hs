{-# LANGUAGE OverloadedStrings #-}

module Prettyprinter.Ext ( (<#>)
                         , prettyLines
                         , tupledBy
                         , ptxt
                         , aText
                         ) where

import           Data.Semigroup            ((<>))
import qualified Data.Text                 as T
import           Prettyprinter             (Doc, LayoutOptions (..), PageWidth (AvailablePerLine), Pretty (..), SimpleDocStream, concatWith, encloseSep, flatAlt, group, hardline,
                                            layoutSmart)
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
