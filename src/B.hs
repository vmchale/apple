module B ( b4 ) where

import           Data.Bits (rotateR, (.&.))
import           Data.Word (Word16, Word64)

b4 :: Word64 -> [Word16]
b4 x = take 4 $ fromIntegral <$> zipWith (\m e -> (x.&.m) `rotateR` e) masks ee
    where ee = [0,16..]; masks = iterate (*0x10000) 0xffff

