module QC ( gD ) where

import           A
import           Data.Bifunctor        (bimap)
import           Data.Functor          (($>))
import           Data.Int              (Int64)
import           Foreign.Marshal.Alloc (mallocBytes)
import           Foreign.Ptr           (Ptr, castPtr)
import           Foreign.Storable      (poke, sizeOf)
import           Hs.A
import           Test.QuickCheck.Gen   (Gen, chooseInt64, frequency, genDouble, generate, vectorOf)

rnk :: Gen Int64
rnk = frequency [(8, pure 1), (3, pure 2), (2, pure 3)]

dim :: Gen Int64
dim = chooseInt64 (0, 20)

data AD = Fixed Int | Any | Bounded Int | RAny

unroll :: Sh a -> [AD]
unroll (Ix _ i `Cons` sh)                      = Fixed i:unroll sh
unroll (IVar{} `Cons` sh)                      = Any:unroll sh
unroll ((StaPlus _ (Ix _ i) IVar{}) `Cons` sh) = Bounded i:unroll sh
unroll ((StaPlus _ IVar{} (Ix _ i)) `Cons` sh) = Bounded i:unroll sh
unroll SVar{}                                  = [RAny]
unroll Nil                                     = []

gg :: Sh a -> Gen (Int64, [Int64])
gg = gd.unroll where
    gd (Fixed i:sh)   = bimap (+1) (fromIntegral i:)<$>gd sh
    gd (Any:sh)       = do {d <- dim; bimap (+1) (d:)<$>gd sh}
    gd (RAny:sh)      = do {r <- rnk; ds <- vectorOf (fromIntegral r) dim; bimap (+r) (ds++)<$>gd sh}
    gd (Bounded i:sh) = let i64=fromIntegral i in do {d <- chooseInt64 (i64, i64+10); bimap (+1) (d:)<$>gd sh}
    gd []             = pure (0, [])

ga :: T a -> IO (Ptr ())
ga (Arr sh F) = do
    a <- generate (gD sh)
    p <- mallocBytes (sizeOf a)
    poke p a $> castPtr p

gD :: Sh a -> Gen (Apple Double)
gD sh = do
    (r, ds) <- gg sh
    let n=fromIntegral$product ds
    es <- vectorOf n genDouble
    pure (AA r ds es)
