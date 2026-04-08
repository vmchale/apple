{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module A.C ( C (..)
           , Cs
           , insC
           , memberC
           , notMemberC
           , bitC
           , nullC
           , isSubsetOfC
           , pcs
           , (\\)
           , mapMaybeC
           ) where

import           Control.DeepSeq (NFData (rnf))
import           Data.Bits       (Bits, complement, xor, (.&.), (.|.))
import           Data.Maybe      (mapMaybe)
import           Prettyprinter   (Pretty (..), braces, concatWith)

infixl 9 \\

newtype Cs = Cs Word deriving (Eq, Num, Bits, NFData)

pcs cs = concatWith (\x y -> x<>","<>y) [ pretty c | c <- [IsOrd .. IsZ], c `memberC` cs ]

instance Pretty Cs where pretty = braces.pcs

data C = IsOrd | IsEq | HasBits | IsZ deriving (Eq, Ord, Enum)

instance NFData C where rnf x=seq x ()

instance Pretty C where pretty IsOrd = "IsOrd"; pretty IsEq = "IsEq"; pretty HasBits = "HasBits"; pretty IsZ = "IsNum"

instance Show C where show=show.pretty
instance Show Cs where show=show.pretty

instance Semigroup Cs where (<>) = (.|.)

bitC :: C -> Cs
bitC IsOrd = Cs 0x1; bitC IsEq = Cs 0x2; bitC HasBits = Cs 0x4; bitC IsZ = Cs 0x8

insC :: C -> Cs -> Cs
insC c = (.|. bitC c)

nullC :: Cs -> Bool
nullC (Cs b) = b == 0

memberC :: C -> Cs -> Bool
memberC c cs = cs .&. bitC c /= 0

notMemberC :: C -> Cs -> Bool
notMemberC c cs = cs .&. bitC c == 0

isSubsetOfC :: Cs -> Cs -> Bool
isSubsetOfC c0 c1 = complement c0 .|. c1 == (-1)

(\\) :: Cs -> Cs -> Maybe Cs
c0 \\ c1 | c' <- (c0 `xor` c1) .&. c0, c'/=0 = Just c'
         | otherwise = Nothing

mapMaybeC :: (C -> Maybe a) -> Cs -> [a]
mapMaybeC f cs = mapMaybe f (filter (`memberC` cs) [IsOrd .. IsZ])
