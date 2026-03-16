module Prettyprinter.Ext ( (<#>), (<?>), (<!>)
                         , PS (..)
                         , parensp
                         , appPrec
                         , prettyLines
                         , tupledBy
                         , smartA
                         , ptxt
                         , aText
                         , prettyDumpBinds
                         , hex2
                         , pAD
                         ) where

import qualified Data.IntMap                as IM
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           Data.Text.Lazy.Builder     (toLazyTextWith)
import           Data.Text.Lazy.Builder.Int (hexadecimal)
import           Data.Void                  (Void, absurd)
import           Prettyprinter              (Doc, LayoutOptions (..), PageWidth (AvailablePerLine), Pretty (..), SimpleDocStream, align, concatWith, encloseSep, fillCat, flatAlt,
                                             group, hardline, indent, layoutSmart, parens, punctuate, softline', space, vsep, (<+>))
import           Prettyprinter.Render.Text  (renderStrict)

infixr 6 <#>
infixr 5 <?>
infixr 5 <!>

(<#>), (<?>), (<!>) :: Doc a -> Doc a -> Doc a
(<#>) x y = x <> hardline <> y
(<?>) x y = x <> softline' <> y
(<!>) x y = flatAlt (x <> hardline <> indent 4 y) (x <> space <> y)

class PS a where ps :: Int -> a -> Doc ann

appPrec=10::Int

instance PS Void where
    ps _ = absurd

parensp True=parens; parensp False=id

prettyLines :: [Doc ann] -> Doc ann
prettyLines = concatWith (<#>)

tupledBy :: Doc ann -> [Doc ann] -> Doc ann
tupledBy sep = group . encloseSep (flatAlt "( " "(") (flatAlt " )" ")") sep

appleLO :: LayoutOptions
appleLO = LayoutOptions (AvailablePerLine 80 1.0)

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

hex2 :: Integral a => a -> TL.Text
hex2 i | i < 16 = toLazyTextWith 2 ("0" <> hexadecimal i)
         | otherwise = toLazyTextWith 2 (hexadecimal i)

-- FIXME: this is probably wrong for arm/endianness
pAD ds = prettyLines ((\(n,dd) -> "arr_" <> pretty n <> ":" <+> ".byte" <+> align (fillCat (punctuate "," (pretty.hex2<$>dd)))) <$> IM.toList ds)
  -- where p8 (w0:ws) = "0x"<>hex2 w0<>","<>p8 ws; p8 [] = ""
