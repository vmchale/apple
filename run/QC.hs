module QC ( gD ) where

import           A
import           Data.Int            (Int64)
import           Foreign.Ptr         (Ptr)
import           Hs.A
import           Test.QuickCheck.Gen (Gen, chooseInt64, frequency, genDouble, vectorOf)

rnk :: Gen Int64
rnk = frequency [(8, pure 1), (3, pure 2), (2, pure 3)]

dim :: Gen Int64
dim = chooseInt64 (0, 20)

gD :: Gen (Apple Double)
gD = do
    r <- rnk
    ds <- vectorOf (fromIntegral r) dim
    let n=fromIntegral$product ds
    es <- vectorOf n genDouble
    pure (AA r ds es)
