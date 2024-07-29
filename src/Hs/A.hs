{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hs.A ( Apple (..), U
            , AB (..), AI, AF
            , P2 (..), P3 (..), P4 (..)
            , hs2, hs3, hs4
            ) where

import           Control.Monad     (forM, zipWithM_)
import           Data.Int          (Int64)
import           Data.List.Split   (chunksOf)
import           Data.Word         (Word8)
import           Foreign.Ptr       (Ptr, castPtr, plusPtr)
import           Foreign.Storable  (Storable (..))
import           Prettyprinter     (Doc, Pretty (..), align, brackets, concatWith, hardline, space, (<+>))
import           Prettyprinter.Ext

type AI = Apple Int64; type AF = Apple Double
type U a = Ptr (Apple a)

data AB = F | T deriving Eq

instance Pretty AB where pretty T="#t"; pretty F="#f"

instance Show AB where show=show.pretty

data Apple a = AA !Int64 [Int64] [a] deriving (Functor)

data P2 a b = P2 a b; hs2 (P2 a b) = (a,b)
data P3 a b c = P3 a b c; hs3 (P3 a b c) = (a,b,c)
data P4 a b c d = P4 a b c d; hs4 (P4 a b c d) = (a,b,c,d)

instance Storable AB where
    sizeOf _ = 1
    peek p = (\b -> case b of 1 -> T; 0 -> F) <$> peek (castPtr p :: Ptr Word8)
    poke p F = poke (castPtr p :: Ptr Word8) 0
    poke p T = poke (castPtr p :: Ptr Word8) 1

instance (Storable a, Storable b) => Storable (P2 a b) where
    sizeOf _ = sizeOf(undefined::a)+sizeOf(undefined::b)
    peek p = P2 <$> peek (castPtr p) <*> peek (p `plusPtr` sizeOf(undefined::a))

instance (Storable a, Storable b, Storable c) => Storable (P3 a b c) where
    sizeOf _ = sizeOf (undefined::a)+sizeOf (undefined::b)+sizeOf (undefined::c)
    peek p = P3 <$> peek (castPtr p) <*> peek (p `plusPtr` sizeOf (undefined::a)) <*> peek (p `plusPtr` (sizeOf (undefined::a)+sizeOf (undefined::b)))

instance (Storable a, Storable b, Storable c, Storable d) => Storable (P4 a b c d) where
    sizeOf _ = sizeOf(undefined::a)+sizeOf(undefined::b)+sizeOf(undefined::c)+sizeOf(undefined::d)
    peek p = P4 <$> peek (castPtr p) <*> peek (p `plusPtr` sizeOf(undefined::a)) <*> peek (p `plusPtr` (sizeOf(undefined::a)+sizeOf(undefined::b))) <*> peek (p `plusPtr` (sizeOf(undefined::a)+sizeOf(undefined::b)+sizeOf(undefined::c)))

pE :: Pretty a => [Int64] -> [a] -> Doc ann
pE [_, n] xs = align (brackets (space <> concatWith (\x y -> x <> hardline <> ", " <> y) (pretty<$>chunksOf (fromIntegral n) xs) <> space))
pE _ xs      = pretty xs

instance Pretty a => Pretty (Apple a) where
    pretty (AA _ dims xs) = "Arr" <+> tupledBy "×" (pretty <$> dims) <+> pE dims xs

instance (Pretty a, Pretty b) => Pretty (P2 a b) where
    pretty (P2 x y) = tupledBy "*" [pretty x, pretty y]

instance (Pretty a, Pretty b, Pretty c) => Pretty (P3 a b c) where
    pretty (P3 x y z) = tupledBy "*" [pretty x, pretty y, pretty z]

instance (Pretty a, Pretty b, Pretty c, Pretty d) => Pretty (P4 a b c d) where
    pretty (P4 x y z w) = tupledBy "*" [pretty x, pretty y, pretty z, pretty w]

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
