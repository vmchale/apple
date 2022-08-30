{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module CF ( ControlAnn (..)
          , Liveness (..)
          , NLiveness (..)
          , Interval (..)
          ) where

import           Control.DeepSeq (NFData)
import qualified Data.IntSet     as IS
import           Data.Semigroup  ((<>))
import           GHC.Generics    (Generic)
import           Prettyprinter   (Pretty (pretty), braces, punctuate, (<+>))

data Liveness = Liveness { ins :: !IS.IntSet, out :: !IS.IntSet, fins :: !IS.IntSet, fout :: !IS.IntSet } -- strictness annotations make it perform better
    deriving (Eq, Generic, NFData)

data NLiveness = NLiveness { nx :: Int, liveness :: !Liveness }
    deriving (Generic, NFData)

data Interval = Interval { new :: !IS.IntSet, done :: !IS.IntSet, fnew :: !IS.IntSet, fdone :: !IS.IntSet }
    deriving (Generic, NFData)

instance Pretty Liveness where
    pretty (Liveness is os fis fos) = pretty (Interval is os fis fos)

instance Pretty Interval where
    pretty (Interval is os fis fos) = braces (pp (is<>fis) <+> ";" <+> pp (os<>fos))
        where pp = mconcat . punctuate "," . fmap pretty . IS.toList

-- | Control-flow annotations
data ControlAnn = ControlAnn { node     :: !Int
                             , conn     :: [Int]
                             , usesNode :: IS.IntSet
                             , usesFNode :: IS.IntSet
                             , defsNode :: IS.IntSet
                             , defsFNode :: IS.IntSet
                             } deriving (Generic, NFData)
