module Asm.Pr ( Pr (..)
              , brackets
              , embed
              , i4
              , prettyLabel
              , pAD
              , pAsm
              , prLines
              , (<+>)
              , (<#>)
              ) where

import           Asm.M
import qualified Data.IntMap                as IM
import           Data.List                  (foldl')
import qualified Data.Text.Lazy.Builder     as B
import           Data.Text.Lazy.Builder.Int (decimal, hexadecimal)
import           Prettyprinter              (Doc, pretty)

infixr 6 <+>
infixr 6 <#>

(<+>), (<#>) :: B.Builder -> B.Builder -> B.Builder
x <+> y = x <> B.singleton ' ' <> y
x <#> y = x <> B.singleton '\n' <> y

i4 :: B.Builder -> B.Builder
i4 = ("   " <>)

class Pr a where
    pr :: a -> B.Builder

brackets :: B.Builder -> B.Builder
brackets x = "[" <> x <> "]"

prettyLabel :: Label -> B.Builder
prettyLabel l = "apple_" <> decimal l

instance Pr CFunc where pr=B.fromText . pCFunc

prLines :: [B.Builder] -> B.Builder
prLines = foldl' (<#>) mempty

-- FIXME: this is probably wrong for arm/endianness
pAD ds = prLines ((\(n,dd) -> "arr_" <> decimal n <> ":" <+> ".byte" <+> p8 dd) <$> IM.toList ds)
  where p8 (w0:ws) = "0x"<>tlhex2 w0<>","<>p8 ws; p8 [] = ""

tlhex2 :: Integral a => a -> B.Builder
tlhex2 i | i < 16 = "0" <> hexadecimal i
         | otherwise = hexadecimal i

pAsm :: Pr isn => [isn] -> B.Builder
pAsm = prLines.fmap pr

embed :: B.Builder -> Doc ann
embed = pretty.B.toLazyText
