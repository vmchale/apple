{-# LANGUAGE ScopedTypeVariables #-}
module B ( b4, le ) where

import           Data.Bits        (Bits, FiniteBits, finiteBitSize, rotateR, (.&.))
import           Data.Word        (Word16, Word8)
import           Foreign.Storable (Storable (sizeOf))

b4 :: (Integral a, Bits a) => a -> [Word16]
b4 x = take 4 $ fromIntegral <$> zipWith (\m e -> (x.&.m) `rotateR` e) masks ee
    where ee = [0,16..]; masks = iterate (*0x10000) 0xffff

-- little endian
le :: forall a. (FiniteBits a,Storable a, Integral a) => a -> [Word8]
le x = take (finiteBitSize (undefined::a) `quot` 8) (fromIntegral <$> zipWith (\m e -> (x .&. m) `rotateR` e) masks ee)
    where ee = [0,8..(8*(sizeOf x-1))]
          masks = iterate (*0x100) 0xff
