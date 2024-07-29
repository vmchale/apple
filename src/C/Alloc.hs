module C.Alloc ( live, frees ) where

import           C
import           C.CF
import           CF
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import           Data.Maybe  (mapMaybe)
-- import           LI
import           LR

frees :: IM.IntMap Temp -> [CS ()] -> [CS Liveness]
frees a = iF a.live

live :: [CS ()] -> [CS Liveness]
live = fmap (fmap liveness) . (\(is,isns,lm) -> reconstruct is lm isns) . cfC

iF :: IM.IntMap Temp -> [CS Liveness] -> [CS Liveness]
iF a = concatMap g where
    g RA{}                  = []
    g s@(For l _ _ _ _ cs)  = s { body = concatMap g cs }:fs l
    g s@(While l _ _ _ cs)  = s { body = concatMap g cs }:fs l
    g s@(For1 l _ _ _ _ cs) = s { body = concatMap g cs }:fs l
    g s@(If l _ b0 b1)      = s { iBranch = concatMap g b0, eBranch = concatMap g b1 }:fs l
    g s@(Ifn't l _ b)       = s { branch = concatMap g b }:fs l
    g s@(Def _ _ cs)        = s { body = concatMap g cs }:[]
    g s                     = s:fs (lann s)

    fs l = [ Free t | t <- ts l ]
    ts l = mapMaybe (`IM.lookup` a) (IS.toList (ins l `IS.difference` out l))
