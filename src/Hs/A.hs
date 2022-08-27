{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hs.A ( Apple (..)
            , AI
            , AF
            ) where

import           Control.Monad     (forM, zipWithM_)
import           Data.Int          (Int64)
import           Foreign.Ptr       (castPtr, plusPtr)
import           Foreign.Storable  (Storable (..))
import           Prettyprinter     (Pretty (..), (<+>))
import           Prettyprinter.Ext

type AI = Apple Int64
type AF = Apple Double

-- TODO: Int8, Int32?
data Apple a = AA !Int64 [Int64] [a]

instance Pretty a => Pretty (Apple a) where
    pretty (AA _ dims xs) = "Arr" <+> tupledBy "×" (pretty <$> dims) <+> pretty xs

instance Storable a => Storable (Apple a) where
    sizeOf (AA rnk dims _) = 8+8*fromIntegral rnk+(sizeOf (undefined::a)*fromIntegral (product dims))
    poke p (AA rnk dims xs) = do
        poke (castPtr p) rnk
        zipWithM_ (\i o -> poke (p `plusPtr` (8+8*o)) i) dims [0..]
        let datOffs = 8+8*fromIntegral rnk
        zipWithM_ (\x o -> poke (p `plusPtr` (datOffs+8*o)) x) xs [0..]
    peek p = do
        rnk <- peek (castPtr p)
        dims <- forM [1..fromIntegral rnk] $ \o -> peek $ p `plusPtr` (8*o)
        let datOffs = 8+8*fromIntegral rnk
        xs <- forM [1..fromIntegral (product dims)] $ \o -> peek $ p `plusPtr` (datOffs+8*(o-1))
        pure $ AA rnk dims xs
