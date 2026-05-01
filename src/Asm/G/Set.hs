module Asm.G.Set ( M (..)
                 , MS
                 , minsert
                 , del
                 , isEmpty
                 , member
                 , notMember
                 , one
                 , intersect
                 , minView
                 , toList
                 ) where

import           Data.Bifunctor (bimap)
import           Data.Bits      (shiftL, shiftR, testBit, (.&.), (.|.))
import qualified Data.IntSet    as IS

data M = MV !Int !Int
newtype MS = MS { und :: IS.IntSet }

-- assumes 64-bit
pack :: M -> Int
pack (MV x y)= x `shiftL` 32 .|. s (0xffffffff .&. y)
    where s = if testBit y 63 then (1 `shiftL` 31 .|.) else id

unpack :: Int -> M
unpack x = MV (x `shiftR` 32) (s (0xffffffff .&. x))
    where s = if testBit x 31 then negate else id

instance Semigroup MS where (MS x) <> (MS y) = MS (x<>y)
instance Monoid MS where mempty = MS mempty

minsert :: M -> MS -> MS
minsert x (MS xs) = MS (IS.insert (pack x) xs)

del :: M -> MS -> MS
del x (MS xs) = MS (IS.delete (pack x) xs)

one :: M -> MS
one x = MS (IS.singleton (pack x))

member :: M -> MS -> Bool
member x (MS xs) = pack x `IS.member` xs

notMember :: M -> MS -> Bool
notMember x (MS xs) = pack x `IS.notMember` xs

isEmpty :: MS -> Bool
isEmpty (MS x) = IS.null x

intersect :: MS -> MS -> MS
intersect (MS x) (MS y) = MS (x `IS.intersection` y)

toList :: MS -> [M]
toList = map unpack . IS.toList . und

minView :: MS -> Maybe (M, MS)
minView (MS k) = bimap unpack MS <$> IS.minView k
