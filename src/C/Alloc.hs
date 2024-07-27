module C.Alloc ( live, frees ) where

import           C
import           C.CF
import           CF
import           Data.Functor      (void)
import qualified Data.IntMap       as IM
import qualified Data.IntSet       as IS
import           Data.Maybe        (mapMaybe)
import           Data.Tuple.Extra  (first3)
import           LI
import           LR
import           Prettyprinter     (Doc, pretty, (<+>))
import           Prettyprinter.Ext

frees :: IM.IntMap Temp -> [CS ()] -> [CS Live]
frees a = iF a.live

live :: [CS ()] -> [CS Live]
live = intervals . reconstruct . mkControlFlow

iF :: IM.IntMap Temp -> [CS Live] -> [CS Live]
iF a = concatMap g where
    g RA{} = []
    g s    = s:[ Free t | t <- ts ] where ts = mapMaybe (`IM.lookup` a) (IS.toList (done (lann s)))

    {-
pf :: [CS ()] -> [CS ()]
pf (s@J{}:ss) | Just (fs, l, ss') <- cfs ss = s:l:fs ++ pf ss'
pf (s:ss)                       = s:pf ss
pf []                           = []

cfs :: [CS ()] -> Maybe ([CS ()], CS (), [CS ()])
cfs (s@Free{}:ss) = first3 (s:) <$> cfs ss
cfs (s@L{}:ss)    = Just ([], s, ss)
cfs _             = Nothing
-}
