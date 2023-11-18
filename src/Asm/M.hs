{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Asm.M ( CFunc (..)
             , WM
             , Label
             , nextI
             , nextL
             , foldMapA
             , prettyLabel
             , i4
             , pAsm, prettyAsm
             , aArr, mFree
             ) where

import           Control.DeepSeq            (NFData)
import           Control.Monad.State.Strict (State, state)
import           Data.Foldable              (fold, traverse_)
import qualified Data.IntMap                as IM
import           Data.List                  (scanl')
import           Data.Word                  (Word64)
import           Foreign.Marshal.Alloc      (free)
import           Foreign.Marshal.Array      (mallocArray, pokeArray)
import           Foreign.Ptr                (Ptr, plusPtr)
import           GHC.Generics               (Generic)
import qualified IR
import           Prettyprinter              (Doc, Pretty (pretty), indent)
import           Prettyprinter.Ext

type WM = State IR.WSt

type Label = Word

foldMapA :: (Applicative f, Traversable t, Monoid m) => (a -> f m) -> t a -> f m
foldMapA = (fmap fold .) . traverse

prettyLabel :: Label -> Doc ann
prettyLabel l = "apple_" <> pretty l

i4 = indent 4

prettyAsm :: (Pretty isn) => (IR.AsmData, [isn]) -> Doc ann
prettyAsm (ds,is) = pAD ds <#> pAsm is

pAsm :: Pretty isn => [isn] -> Doc ann
pAsm = prettyLines.fmap pretty

nextI :: WM Int
nextI = state (\(IR.WSt l (i:t)) -> (i, IR.WSt l t))

nextL :: WM Label
nextL = state (\(IR.WSt (i:l) t) -> (i, IR.WSt l t))

data CFunc = Malloc | Free deriving (Generic)

instance NFData CFunc where

instance Pretty CFunc where pretty Malloc = "malloc"; pretty Free = "free"

mFree :: Maybe (Ptr a) -> IO ()
mFree = traverse_ free

aArr :: IM.IntMap [Word64] -> IO (IM.IntMap (Ptr Word64))
aArr as = do
    let bls = fmap length as; bl = sum bls
    p <- mallocArray bl
    let bs = concat (IM.elems as)
    pokeArray p bs
    pure $ case IM.toList bls of
        []             -> IM.empty
        ((k0,l0):bls') -> IM.fromList . fmap (\(x,_,z) -> (x,z)) $ scanl' (\(_, lϵ, pϵ) (k, l) -> (k, l, pϵ `plusPtr` (lϵ*8))) (k0, l0, p) bls'
