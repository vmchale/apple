{-# LANGUAGE RankNTypes #-}

module Ty.Clone ( cloneT ) where


import           A
import           Control.Monad.Trans.State.Strict (State, gets, modify, runState, state)
import           Data.Functor                     (($>))
import qualified Data.IntMap                      as IM
import           Nm
import           Sh
import           U

data TR = TR { maxT :: Int, boundTV, boundSh, boundIx :: IM.IntMap Int }

type CM = State TR

next :: CM Int
next = state (\(TR m t s i) -> let m'=m+1 in (m', TR m' t s i))

data TRLens = TRLens { setM :: Int -> Int -> CM (), field :: TR -> IM.IntMap Int }

ttl, tsl, til :: TRLens
ttl = TRLens (\i j -> modify (\(TR u t s ix) -> TR u (IM.insert i j t) s ix)) boundTV
tsl = TRLens (\i j -> modify (\(TR u t s ix) -> TR u t (IM.insert i j s) ix)) boundSh
til = TRLens (\i j -> modify (\(TR u t s ix) -> TR u t s (IM.insert i j ix))) boundIx

-- for clone
freshen :: (Int -> Int -> CM ()) -- ^ TVars, shape var, etc.
        -> Nm a -> CM (Nm a)
freshen set (Nm n (U i) l) = do j <- next; set i j $> Nm n (U j) l

tryReplaceInT :: TRLens -> Nm a -> CM (Nm a)
tryReplaceInT lens n@(Nm t (U i) l) = do
    st <- gets (field lens)
    case IM.lookup i st of
        Just j  -> pure (Nm t (U j) l)
        Nothing -> freshen (setM lens) n

cIx :: I a -> CM (I a)
cIx i@Ix{}           = pure i
cIx (StaPlus l i i') = StaPlus l <$> cIx i <*> cIx i'
cIx (StaMul l i i')  = StaMul l <$> cIx i <*> cIx i'
cIx (IV l n)         = IV l <$> tryReplaceInT til n
cIx (IEV l n)        = IEV l <$> tryReplaceInT til n

cSh :: Sh a -> CM (Sh a)
cSh Nil           = pure Nil
cSh (Cons i sh)   = Cons <$> cIx i <*> cSh sh
cSh (SVar n)      = SVar <$> tryReplaceInT tsl n
cSh (Rev sh)      = Rev <$> cSh sh
cSh (Cat sh0 sh1) = Cat <$> cSh sh0 <*> cSh sh1
cSh (Π sh)        = Π <$> cSh sh

iSt u = TR u IM.empty IM.empty IM.empty

cloneT :: Int -> T a -> (Int, T a)
cloneT u = (\(t, TR uϵ _ _ _) -> (uϵ,t)).flip runState (iSt u).cT
  where
    cT :: T a -> CM (T a)
    cT F            = pure F
    cT I            = pure I
    cT B            = pure B
    cT (Li ix)      = Li <$> cIx ix
    cT (Arrow t t') = Arrow <$> cT t <*> cT t'
    cT (Arr sh t)   = Arr <$> cSh sh <*> cT t
    cT (TV n c)     = TV <$> tryReplaceInT ttl n<*>pure c
    cT (P ts)       = P <$> traverse cT ts
    cT (IZ ix n)    = IZ <$> cIx ix <*> tryReplaceInT ttl n
