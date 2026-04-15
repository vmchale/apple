module B ( b4, le ) where

import           Data.Bits        (Bits, rotateR, shiftL, (.&.))
import           Data.Word        (Word16, Word8)
import           Foreign.Storable (Storable (sizeOf))

b4 :: (Integral a, Bits a) => a -> [Word16]
b4 x = take 4 $ fromIntegral <$> zipWith (\m e -> (x.&.m) `rotateR` e) masks ee
    where ee = [0,16..]; masks = iterate (`shiftL` 16) 0xffff

-- little endian
le :: (Bits a, Storable a, Integral a) => a -> [Word8]
le x = fromIntegral <$> zipWith (\m e -> (x .&. m) `rotateR` e) masks ee
    where ee = [0,8..(8*(sizeOf x-1))]
          masks = iterate (`shiftL` 8) 0xff
