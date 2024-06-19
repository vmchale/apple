{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hs.A ( Apple (..)
            , AI
            , AF
            , Pp (..), P4 (..)
            , dbgAB
            , hsTup, hs4
            ) where

import           Control.Monad         (forM, zipWithM_)
import           Data.Int              (Int64)
import           Data.List.Split       (chunksOf)
import qualified Data.Text             as T
import           Data.Word             (Word8)
import           Foreign.Marshal.Array (peekArray)
import           Foreign.Ptr           (Ptr, castPtr, plusPtr)
import           Foreign.Storable      (Storable (..))
import           Numeric               (showHex)
import           Prettyprinter         (Doc, Pretty (..), align, brackets, concatWith, hardline, space, (<+>))
import           Prettyprinter.Ext

type AI = Apple Int64
type AF = Apple Double

-- TODO: Int8, Int32?
data Apple a = AA !Int64 [Int64] [a]

data Pp a b = Pp a b
hsTup (Pp a b) = (a,b)
data P4 a b c d = P4 a b c d; hs4 (P4 a b c d) = (a,b,c,d)

instance (Storable a, Storable b) => Storable (Pp a b) where
    sizeOf _ = sizeOf(undefined::a)+sizeOf(undefined::b)
    peek p = Pp <$> peek (castPtr p) <*> peek (p `plusPtr` sizeOf(undefined::a))

instance (Storable a, Storable b, Storable c, Storable d) => Storable (P4 a b c d) where
    sizeOf _ = sizeOf(undefined::a)+sizeOf(undefined::b)+sizeOf(undefined::c)+sizeOf(undefined::d)
    peek p = P4 <$> peek (castPtr p) <*> peek (p `plusPtr` sizeOf(undefined::a)) <*> peek (p `plusPtr` (sizeOf(undefined::a)+sizeOf(undefined::b))) <*> peek (p `plusPtr` (sizeOf(undefined::a)+sizeOf(undefined::b)+sizeOf(undefined::c)))

pE :: Pretty a => [Int64] -> [a] -> Doc ann
pE [_, n] xs = align (brackets (space <> concatWith (\x y -> x <> hardline <> ", " <> y) (pretty<$>chunksOf (fromIntegral n) xs) <> space))
pE _ xs      = pretty xs

instance Pretty a => Pretty (Apple a) where
    pretty (AA _ dims xs) = "Arr" <+> tupledBy "×" (pretty <$> dims) <+> pE dims xs

instance (Pretty a, Pretty b) => Pretty (Pp a b) where
    pretty (Pp x y) = tupledBy "*" [pretty x, pretty y]

dbgAB :: forall a. Storable a => Ptr (Apple a) -> IO T.Text
dbgAB p = do
    rnk <- peek (castPtr p :: Ptr Int64)
    dims <- forM [1..fromIntegral rnk] $ \o -> peek $ p `plusPtr` (8*o)
    let sz = 8+8*rnk+fromIntegral (sizeOf (undefined::a))*product dims
    hextext <$> peekArray (fromIntegral sz) (castPtr p :: Ptr Word8)

hextext = T.unwords . fmap (T.pack.($"").showHex)

instance Storable a => Storable (Apple a) where
    sizeOf (AA rnk dims _) = 8+8*fromIntegral rnk+(sizeOf (undefined::a)*fromIntegral (product dims))
    poke p (AA rnk dims xs) = do
        poke (castPtr p) rnk
        zipWithM_ (\i o -> poke (p `plusPtr` (8+8*o)) i) dims [0..]
        let datOffs = 8+8*fromIntegral rnk
        zipWithM_ (\x o -> poke (p `plusPtr` (datOffs+sizeOf (undefined::a)*o)) x) xs [0..]
    peek p = do
        rnk <- peek (castPtr p)
        dims <- forM [1..fromIntegral rnk] $ \o -> peek $ p `plusPtr` (8*o)
        let datOffs = 8+8*fromIntegral rnk
        xs <- forM [1..fromIntegral (product dims)] $ \o -> peek $ p `plusPtr` (datOffs+sizeOf (undefined::a)*(o-1))
        pure $ AA rnk dims xs
