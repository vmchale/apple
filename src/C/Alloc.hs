module C.Alloc ( live, frees ) where

import           C
import           C.CF
import           CF
import           CF.AL
import           Control.Monad.Trans.State.Strict (State, modify,state)
import           Data.Bifunctor                   (second)
import qualified Data.IntMap                      as IM
import qualified Data.IntSet                      as IS
import           Data.List                        (find)
import           Data.Maybe                       (mapMaybe)
import           LR
import           Sh

frees :: IM.IntMap Temp -> [CS ()] -> [CS Liveness]
frees a = iF a.live

live :: [CS ()] -> [CS Liveness]
live = fmap (fmap liveness) . (\(is,isns,lm) -> reconstruct is lm isns) . cfC

sus = error "Array only freed at the beginning of one branch of the conditional."

type Subst = IM.IntMap AL

data Slots = Ss { livest :: !IS.IntSet, mLive :: [(AL, Sh ())] }

m'liven :: AL -> Sh () -> State Slots (Maybe (AL, AL))
m'liven l sh = state (\st@(Ss _ ls) -> case ffit st sh of Nothing -> (Nothing, st { mLive = (l,sh):ls }))

ffit :: Slots -> Sh () -> Maybe AL
ffit (Ss mask aaϵ) sh = fst <$> find (\(AL lϵ, sh') -> lϵ `IS.notMember` mask && fits sh sh') aaϵ

ilt :: I a -> I a -> Bool
ilt (Ix _ i) (Ix _ j)                     = i <= j
ilt (IVar _ i) (IVar _ i')                = i == i'
ilt (StaPlus _ i0 i1) (StaPlus _ i0' i1') | ilt i0 i0' && ilt i1 i1' = True
ilt (StaMul _ i0 i1) (StaMul _ i0' i1')   | ilt i0 i0' && ilt i1 i1' = True
ilt _ _                                   = False

fits :: Sh a -> Sh a -> Bool
fits (ix `Cons` Nil) (ix' `Cons` Nil) = ilt ix ix'
fits (Rev sh0) (Rev sh1)              = fits sh0 sh1
fits (SVar sv0) (SVar sv1)            = sv0 == sv1
fits (Cat sh0 sh1) (Cat sh0' sh1')    | fits sh0 sh0' && fits sh1 sh1' = True
fits _ _                              = False

-- every time we encounter an allocation, make note (size via malloc). Then when its live interval is over, remove from livest?
--
-- first cut: don't do re-fills
aa :: [CS Liveness] -> State Slots (Subst, [CS Liveness])
aa (c@(Ma _ sh l _ _ _ _):cs) = do {m'liven l sh; second (c:) <$> aa cs}
-- livest should be updated here?

iF :: IM.IntMap Temp -> [CS Liveness] -> [CS Liveness]
iF a = gg where
    gg (RA{}:cs)                             = gg cs
    gg [s@(For l _ _ _ _ cs)]                = s { body = gg cs }:fss l
    gg [s@(Rof l _ _ cs)]                    = s { body = gg cs }:fss l
    gg [s@(Rof1 l _ _ cs)]                   = s { body = gg cs }:fss l
    gg [s@(While l _ _ _ cs)]                = s { body = gg cs }:fss l
    gg [s@(For1 l _ _ _ _ _ cs)]             = s { body = gg cs }:fss l
    gg [s@(Ifn't l _ b@(b0:_))]              = gEs l b0 [s { branch = gg b }]
    gg [s@(Ifn't l _ [])]                    = s:fss l
    gg [s@(If l _ [] [])]                    = s:fss l
    gg [s@(If l _ b0@(b00:_) [])]            = gEs l b00 [s { iBranch = gg b0 }]
    gg [s@(If l _ [] b1@(b10:_))]            = gEs l b10 [s { eBranch = gg b1 }]
    gg [s@(Def _ _ cs)]                      = [s { body = gg cs }]
    gg (s@(For l _ _ _ _ cs):ss@(s0:_))      = s { body = gg cs }:fs l s0++gg ss
    gg (s@(Rof l _ _ cs):ss@(s0:_))          = s { body = gg cs }:fs l s0++gg ss
    gg (s@(While l _ _ _ cs):ss@(s0:_))      = s { body = gg cs }:fs l s0++gg ss
    gg (s@(For1 l _ _ _ _ _ cs):ss@(s0:_))   = s { body = gg cs }:fs l s0++gg ss
    gg (s@(Rof1 l _ _ cs):ss@(s0:_))         = s { body = gg cs }:fs l s0++gg ss
    gg (s@(If l _ b0@(b00:_) b1@(b10:_)):ss) = s { iBranch = fs l b00++gg b0, eBranch = fs l b10++gg b1 }:gg ss
    gg (s@(If l _ b0@(b00:_) _):ss@(s0:_))   = gEs l b00 $ s { iBranch = gg b0 }:fs l s0++gg ss
    gg (s@(If l _ _ b1@(b10:_)):ss@(s0:_))   = gEs l b10 $ s { eBranch = gg b1 }:fs l s0++gg ss
    gg (s@(If l _ _ _):ss@(s0:_))            = s:fs l s0++gg ss
    gg (s@(Ifn't l _ b@(b0:_)):ss@(s0:_))    = gEs l b0 $ s { branch = gg b }:fs l s0++gg ss
    gg (s@(Ifn't l _ _):ss@(s0:_))           = s:fs l s0 ++ gg ss
    gg (s@(Def l _ cs):ss@(s0:_))            = s { body = gg cs }:fs l s0++gg ss
    gg [s]                                   = s:fss (lann s)
    gg (s:ss@(s0:_))                         = s:fs (lann s) s0++gg ss
    gg []                                    = []

    es l s = null$ts l s
    gEs l s x = if es l s then x else sus

    fss l = [ Free t | t <- tss l ]
    tss l = mapMaybe (`IM.lookup` a) (IS.toList (ins l `IS.difference` out l))
    fs l0 s1 = [ Free t | t <- ts l0 s1 ]
    ts l0 s1 = mapMaybe (`IM.lookup` a) (IS.toList (ins l0 `IS.difference` ins (lann s1)))
