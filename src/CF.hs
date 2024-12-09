module CF ( ControlAnn (..)
          , UD (..)
          , Liveness (..)
          , NLiveness (..)
          , Live (..)
          ) where

import qualified Data.IntSet       as IS
import           Prettyprinter     (Pretty (pretty), braces, punctuate, (<+>))
import           Prettyprinter.Ext

data Liveness = Liveness { ins, out, fins, fout :: !IS.IntSet } deriving Eq

data NLiveness = NLiveness { nx :: Int, liveness :: !Liveness }

data Live = Live { new, done, fnew, fdone :: !IS.IntSet }

instance Pretty Liveness where
    pretty (Liveness is os fis fos) = pretty (Live is os fis fos)

instance Show Liveness where show=show.pretty

instance Pretty Live where
    pretty (Live is os fis fos) = braces (pp (is<>fis) <+> ";" <+> pp (os<>fos))
        where pp = mconcat . punctuate "," . fmap pretty . IS.toList

-- | Control-flow annotations
data ControlAnn = ControlAnn { node :: !Int
                             , conn :: [Int]
                             , ud   :: !UD
                             }

data UD = UD { usesNode, usesFNode, defsNode, defsFNode :: !IS.IntSet }

instance Pretty UD where
    pretty (UD u uf d df) = "←" <+> pretty (IS.toList$u<>uf) <#> "=" <+> pretty (IS.toList$d<>df)

instance Show UD where show=show.pretty
