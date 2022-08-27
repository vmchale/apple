{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module CF ( ControlAnn (..)
          , Liveness (..)
          , NLiveness (..)
          , GLiveness (..)
          , Interval (..)
          ) where

import           Control.DeepSeq (NFData)
import qualified Data.IntSet     as IS
import           Data.Semigroup  ((<>))
import           GHC.Generics    (Generic)
import           Prettyprinter   (Pretty (pretty), braces, punctuate, (<+>))

data GLiveness = GLiveness { isMove :: !Bool, gLiveness :: !Liveness }

data Liveness = Liveness { ins :: !IS.IntSet, out :: !IS.IntSet } -- strictness annotations make it perform better
    deriving (Eq, Generic, NFData)

data NLiveness = NLiveness { nx :: Int, liveness :: !Liveness }
    deriving (Generic, NFData)

data Interval = Interval { new :: !IS.IntSet, done :: !IS.IntSet }
    deriving (Generic, NFData)

instance Pretty Liveness where
    pretty (Liveness is os) = pretty (Interval is os)

instance Pretty Interval where
    pretty (Interval is os) = braces (pp is <+> ";" <+> pp os)
        where pp = mconcat . punctuate "," . fmap pretty . IS.toList

-- | Control-flow annotations
data ControlAnn = ControlAnn { node     :: !Int
                             , conn     :: [Int]
                             , usesNode :: IS.IntSet
                             , defsNode :: IS.IntSet
                             } deriving (Generic, NFData)
