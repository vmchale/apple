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
             , prettyAsm
             ) where

import           Control.DeepSeq            (NFData)
import           Control.Monad.State.Strict (State, gets, modify)
import           Data.Foldable              (fold)
import           Data.Functor               (($>))
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

prettyAsm :: (Pretty isn) => [isn] -> Doc ann
prettyAsm = prettyLines . fmap pretty

nextI :: WM Int
nextI = do { i <- gets (head.IR.wtemps); modify (\(IR.WSt l (_:t)) -> IR.WSt l t) $> i }

nextL :: WM Label
nextL = do { i <- gets (head.IR.wlabels); modify (\(IR.WSt (_:l) t) -> IR.WSt l t) $> i }

data CFunc = Malloc | Free deriving (Generic)

instance NFData CFunc where

instance Pretty CFunc where
    pretty Malloc = "_malloc"
    pretty Free   = "_free"
