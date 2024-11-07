module C.Alloc ( live, frees ) where

import           C
import           C.CF
import           CF
import           CF.AL
import           Control.Composition              ((.*))
import           Control.Monad.Trans.State.Strict (State, gets, runState, state)
import           Data.Bifunctor                   (second)
import qualified Data.IntMap                      as IM
import qualified Data.IntSet                      as IS
import           Data.List                        (find)
import           Data.Maybe                       (mapMaybe)
import           Data.Tuple                       (swap)
import           LR
import           Sh

frees :: IM.IntMap Temp -> [CS ()] -> [CS Liveness]
frees a = (\(n,cs) -> iF n a cs).second live.raa a.live

live :: [CS a] -> [CS Liveness]
live = fmap (fmap liveness) . (\(is,isns,lm) -> reconstruct is lm isns) . cfC

sus = error "Array only freed at the beginning of one branch of the conditional."

type Alias = IM.IntMap Int

data Slots = Ss { keep :: !Alias, nfree :: !IS.IntSet, mLive :: [(AL, Sh ())] }

(@@) :: Alias -> IS.IntSet -> IS.IntSet
(@@) s = IS.map (\l -> IM.findWithDefault l l s)

m'liven :: IS.IntSet -> AL -> Sh () -> State Slots (Maybe AL)
m'liven il l@(AL i) sh = state g where
    g s@(Ss k m ls) =
        case ffit il s sh of
            Nothing         -> (Nothing, Ss k m ((l,sh):ls))
            Just l'@(AL i') -> (Just l', Ss (IM.insert i i' k) (IS.insert i m) ls)

ffit :: IS.IntSet -> Slots -> Sh () -> Maybe AL
ffit il (Ss ms _ aaϵ) sh = fst <$> find (\(lϵ, sh') -> lϵ `notMember` (ms@@il) && fits sh sh') aaϵ

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

st :: IM.IntMap Temp -> AL -> Temp
st k (AL i)= IM.findWithDefault (error "Internal error: bad substitution?") i k

raa :: IM.IntMap Temp -> [CS Liveness] -> (IS.IntSet, [CS Liveness])
raa = swap . second nfree . flip runState (Ss IM.empty IS.empty []) .* aa

sCF :: Alias -> Liveness -> Liveness
sCF al (Liveness i o fi fo) = Liveness (al@@i) (al@@o) fi fo

-- every time we encounter an allocation, make note (size via malloc).
--
-- first cut: don't do re-fills
aa :: IM.IntMap Temp -> [CS Liveness] -> State Slots [CS Liveness]
aa ts (c@(Ma a sh l t _ _ _):cs) = do
    s <- m'liven (ins a) l sh
    next <- case s of
            Nothing         -> pure c
            Just l' -> do
                al <- gets keep
                pure $ Aa (sCF al a) l t (st ts l') 0
    (next:) <$> aa ts cs
aa ts (c:cs) = do {s <- gets keep; (fmap (sCF s) c:) <$> aa ts cs}
-- new liveness information needs to prevent frees oops
aa _ [] = pure []

iF :: IS.IntSet -> IM.IntMap Temp -> [CS Liveness] -> [CS Liveness]
iF m a = gg where
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
    tss l = mapMaybe (`IM.lookup` a) (IS.toList ((ins l IS.\\ out l) IS.\\ m))
    fs l0 s1 = [ Free t | t <- ts l0 s1 ]
    ts l0 s1 = mapMaybe (`IM.lookup` a) (IS.toList ((ins l0 IS.\\ ins (lann s1)) IS.\\ m))
