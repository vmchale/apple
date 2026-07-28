module Asm.G.Set ( M, pack, unpack
                 , MS
                 , minsert
                 , del
                 , isEmpty
                 , member
                 , notMember
                 , one
                 , intersect
                 , minView
                 , mlist
                 , toList
                 ) where

import           Data.Bifunctor (bimap)
import           Data.Bits      (shiftL, shiftR, testBit, (.&.), (.|.))
import           Data.Int       (Int32)
import qualified Data.IntSet    as IS

newtype M = MV Int -- !Int !Int
newtype MS = MS { und :: IS.IntSet }

-- assumes 64-bit
pack :: (Int, Int) -> M
pack (x, y)= MV (x `shiftL` 32 .|. s (0xffffffff .&. y))
    where s = if testBit y 63 then (1 `shiftL` 31 .|.) else id

unpackI :: Int -> (Int, Int)
unpackI x = (x `shiftR` 32,  s x)
    where s = negate.fromIntegral.negate.(fromIntegral::Int->Int32)

unpack :: M -> (Int, Int)
unpack (MV x) = unpackI x

instance Semigroup MS where (MS x) <> (MS y) = MS (x<>y)
instance Monoid MS where mempty = MS mempty

minsert :: M -> MS -> MS
minsert (MV x) (MS xs) = MS (IS.insert x xs)

del :: M -> MS -> MS
del (MV x) (MS xs) = MS (IS.delete x xs)

one :: M -> MS
one (MV x) = MS (IS.singleton x)

member :: M -> MS -> Bool
member (MV x) (MS xs) = x `IS.member` xs

notMember :: M -> MS -> Bool
notMember (MV x) (MS xs) = x `IS.notMember` xs

isEmpty :: MS -> Bool
isEmpty (MS x) = IS.null x

intersect :: MS -> MS -> MS
intersect (MS x) (MS y) = MS (x `IS.intersection` y)

mlist :: MS -> [M]
mlist = map MV . IS.toList . und

toList :: MS -> [(Int, Int)]
toList = map unpackI . IS.toList . und

minView :: MS -> Maybe (M, MS)
minView (MS k) = bimap MV MS <$> IS.minView k
