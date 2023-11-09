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
             , aArr, freeze
             ) where

import           Control.DeepSeq            (NFData)
import           Control.Monad.State.Strict (State, state)
import           Data.Foldable              (fold, traverse_)
import           Data.Functor               (($>))
import qualified Data.IntMap                as IM
import           Data.Word                  (Word64)
import           Foreign.Marshal.Alloc      (free)
import           Foreign.Marshal.Array      (mallocArray, pokeArray)
import           Foreign.Ptr                (Ptr)
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

freeze :: [Ptr Word64] -> IO ()
freeze = traverse_ free

aArr :: IM.IntMap [Word64] -> IO (IM.IntMap (Ptr Word64))
aArr = traverse pLeek

pLeek :: [Word64] -> IO (Ptr Word64)
pLeek xs = do
    p <- mallocArray (length xs)
    pokeArray p xs $> p
