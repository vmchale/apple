module C.Alloc ( live, frees ) where

import           C
import           C.CF
import           CF
import qualified Data.IntMap as IM
import qualified Data.IntSet as IS
import           Data.Maybe  (mapMaybe)
import           LR

frees :: IM.IntMap Temp -> [CS ()] -> [CS Liveness]
frees a = iF a.live

live :: [CS ()] -> [CS Liveness]
live = fmap (fmap liveness) . (\(is,isns,lm) -> reconstruct is lm isns) . cfC

iF :: IM.IntMap Temp -> [CS Liveness] -> [CS Liveness]
iF a = gg where
    gg (RA{}:cs)                         = gg cs
    gg [s@(For l _ _ _ _ cs)]            = s { body = gg cs }:fs0 l
    gg [s@(While l _ _ _ cs)]            = s { body = gg cs }:fs0 l
    gg [s@(For1 l _ _ _ _ cs)]           = s { body = gg cs }:fs0 l
    gg [s@(If l _ b0 b1)]                = s { iBranch = gg b0, eBranch = gg b1 }:fs0 l
    gg [s@(Ifn't l _ b)]                 = s { branch = gg b }:fs0 l
    gg [s@(Def _ _ cs)]                  = [s { body = gg cs }]
    gg (s@(For l _ _ _ _ cs):ss@(s0:_))  = s { body = gg cs }:fs l s0++gg ss
    gg (s@(While l _ _ _ cs):ss@(s0:_))  = s { body = gg cs }:fs l s0++gg ss
    gg (s@(For1 l _ _ _ _ cs):ss@(s0:_)) = s { body = gg cs }:fs l s0++gg ss
    gg (s@(If l _ b0 b1):ss@(s0:_))      = s { iBranch = gg b0, eBranch = gg b1 }:fs l s0++gg ss
    gg (s@(Ifn't l _ b):ss@(s0:_))       = s { branch = gg b }:fs l s0++gg ss
    gg (s@(Def l _ cs):ss@(s0:_))        = s { body = gg cs }:fs l s0++gg ss
    gg [s]                               = s:fs0 (lann s)
    gg (s:ss@(s0:_))                     = s:fs (lann s) s0++gg ss
    gg []                                = []

    fs0 l = [ Free t | t <- ts0 l ]
    ts0 l = mapMaybe (`IM.lookup` a) (IS.toList (ins l `IS.difference` out l))
    fs l0 s1 = [ Free t | t <- ts l0 s1 ]
    ts l0 s1 = mapMaybe (`IM.lookup` a) (IS.toList (ins l0 `IS.difference` ins (lann s1)))
