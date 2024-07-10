module Bits ( cLog ) where

import           Data.Bits (FiniteBits, countTrailingZeros, popCount)
import           Data.Int  (Int64)

cLog :: FiniteBits a => a -> Maybe Int64
cLog n | popCount n == 1 = Just (fromIntegral$countTrailingZeros n) | otherwise = Nothing
