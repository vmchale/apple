{-# LANGUAGE OverloadedStrings #-}

module Prettyprinter.Ext ( (<#>)
                         , prettyLines
                         , tupledBy
                         ) where

import           Data.Semigroup ((<>))
import           Prettyprinter  (Doc, concatWith, encloseSep, flatAlt, group, hardline)

infixr 6 <#>
(<#>) :: Doc a -> Doc a -> Doc a
(<#>) x y = x <> hardline <> y

prettyLines :: [Doc ann] -> Doc ann
prettyLines = concatWith (<#>)

tupledBy :: Doc ann -> [Doc ann] -> Doc ann
tupledBy sep = group . encloseSep (flatAlt "( " "(") (flatAlt " )" ")") sep
