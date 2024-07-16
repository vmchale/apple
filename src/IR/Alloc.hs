module IR.Alloc ( prettyIRI, live, frees ) where

import           CF
import qualified Data.IntMap       as IM
import qualified Data.IntSet       as IS
import           Data.Maybe        (mapMaybe)
import           Data.Tuple.Extra  (first3)
import           IR
import qualified IR.CFA            as A
import           LI
import           LR
import           Prettyprinter     (Doc, pretty, (<+>))
import           Prettyprinter.Ext

frees :: IM.IntMap Temp -> [Stmt] -> [Stmt]
frees a = pf.iF a.live

live :: [Stmt] -> [(Stmt, Live)]
live = intervals . reconstruct . fst . A.mkControlFlow

prettyIRI :: [(Stmt, Live)] -> Doc ann
prettyIRI = prettyLines . fmap (\(s,i) -> pretty s <+> pretty i)

iF :: IM.IntMap Temp -> [(Stmt, Live)] -> [Stmt]
iF a = concatMap g where
    g (RA{}, _) = []
    g (s, i)    = s:[ Free t | t <- ts ] where ts = mapMaybe (`IM.lookup` a) (IS.toList (done i))

pf :: [Stmt] -> [Stmt]
pf (s@J{}:ss) | Just (fs, l, ss') <- cfs ss = s:l:fs ++ pf ss'
pf (s:ss)                       = s:pf ss
pf []                           = []

cfs :: [Stmt] -> Maybe ([Stmt], Stmt, [Stmt])
cfs (s@Free{}:ss) = first3 (s:) <$> cfs ss
cfs (s@L{}:ss)    = Just ([], s, ss)
cfs _             = Nothing
