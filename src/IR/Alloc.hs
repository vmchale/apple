module IR.Alloc ( prettyIRI, live, frees ) where

import           CF
import qualified Data.IntMap       as IM
import qualified Data.IntSet       as IS
import           Data.Maybe        (mapMaybe)
import           IR
import qualified IR.CF             as IR
import           LI
import           LR
import           Prettyprinter     (Doc, pretty, (<+>))
import           Prettyprinter.Ext

frees :: IM.IntMap Temp -> [Stmt] -> [Stmt]
frees a = pf.iF a.live

live :: [Stmt] -> [(Stmt, Interval)]
live = intervals . reconstruct . IR.mkControlFlow

prettyIRI :: [(Stmt, Interval)] -> Doc ann
prettyIRI = prettyLines . fmap (\(s,i) -> pretty s <+> pretty i)

iF :: IM.IntMap Temp -> [(Stmt, Interval)] -> [Stmt]
iF a = concatMap g where
    g (RA{}, _) = []
    g (s, i)    = s:[ Free t | t <- ts ] where ts = mapMaybe (`IM.lookup` a) (IS.toList (done i))

pf :: [Stmt] -> [Stmt]
pf (s0@J{}:s1@Free{}:s2@L{}:ss) = s0:s2:s1:pf ss
pf (s:ss)                       = s:pf ss
pf []                           = []
