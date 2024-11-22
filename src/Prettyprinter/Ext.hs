module Prettyprinter.Ext ( (<#>), (<?>), (<!>)
                         , PS (..)
                         , parensp
                         , appPrec
                         , prettyLines
                         , tupledBy
                         , ptxt
                         , aText
                         , prettyDumpBinds
                         , tlhex2
                         , pAD
                         ) where

import           B
import qualified Data.IntMap                as IM
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import           Data.Text.Lazy.Builder     (toLazyTextWith)
import           Data.Text.Lazy.Builder.Int (hexadecimal)
import           Data.Void                  (Void, absurd)
import           Data.Word                  (Word64)
import           Prettyprinter              (Doc, LayoutOptions (..), PageWidth (AvailablePerLine), Pretty (..), SimpleDocStream, concatWith, encloseSep, flatAlt, group, hardline,
                                             indent, layoutSmart, parens, softline', space, vsep, (<+>))
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

hex2 :: Integral a => a -> Doc ann
hex2 = pretty.tlhex2

tlhex2 :: Integral a => a -> TL.Text
tlhex2 i | i < 16 = toLazyTextWith 2 ("0" <> hexadecimal i)
         | otherwise = toLazyTextWith 2 (hexadecimal i)

-- FIXME: this is certainly wrong for arm/endianness
pAD ds = prettyLines ((\(n,dd) -> "arr_" <> pretty n <> ":" <+> ".8byte" <+> concatWith (\x y -> x <> "," <> y) (fmap p64 dd)) <$> IM.toList ds)

p64 :: Word64 -> Doc ann
p64 w = "0x"<>hex2 w3<>hex2 w2<>hex2 w1<>hex2 w0 where [w0,w1,w2,w3]=b4 w
