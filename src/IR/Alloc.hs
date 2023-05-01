module IR.Alloc ( prettyIRI, live, frees ) where

import           CF
import qualified Data.IntMap       as IM
import qualified Data.IntSet       as IS
import           Data.Maybe        (mapMaybe)
import           Data.Tuple.Extra  (first3)
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
    -- FIXME: this needs to actually push it out of the loop lol
    -- it's definitely fucking up with closures or something
    --
    -- TODO: examine this in light of post-loop conditionals... might be fine?

pf :: [Stmt] -> [Stmt]
pf (s@J{}:ss) | Just (fs, l, ss') <- cfs ss = s:l:fs ++ pf ss'
pf (s:ss)                       = s:pf ss
pf []                           = []

cfs :: [Stmt] -> Maybe ([Stmt], Stmt, [Stmt])
cfs (s@Free{}:ss) = first3 (s:) <$> cfs ss
cfs (s@L{}:ss)    = Just ([], s, ss)
cfs _             = Nothing
