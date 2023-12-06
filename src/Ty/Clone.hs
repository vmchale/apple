{-# LANGUAGE RankNTypes #-}

module Ty.Clone ( cloneTClosed ) where


import           A
import           Control.Monad.State.Strict (State, gets, runState)
import           Data.Functor               (($>))
import qualified Data.IntMap                as IM
import           Lens.Micro                 (Lens')
import           Lens.Micro.Mtl             (modifying, use)
import           Nm
import           U

data TRenames = TRenames { maxT    :: Int
                         , boundTV :: IM.IntMap Int
                         , boundSh :: IM.IntMap Int
                         , boundIx :: IM.IntMap Int
                         }

type CM = State TRenames

maxTLens :: Lens' TRenames Int
maxTLens f s = fmap (\x -> s { maxT = x }) (f (maxT s))

boundTVLens :: Lens' TRenames (IM.IntMap Int)
boundTVLens f s = fmap (\x -> s { boundTV = x }) (f (boundTV s))

boundShLens :: Lens' TRenames (IM.IntMap Int)
boundShLens f s = fmap (\x -> s { boundSh = x }) (f (boundSh s))

boundIxLens :: Lens' TRenames (IM.IntMap Int)
boundIxLens f s = fmap (\x -> s { boundIx = x }) (f (boundIx s))

-- for clone
freshen :: Lens' TRenames (IM.IntMap Int) -- ^ TVars, shape var, etc.
        -> Nm a -> CM (Nm a)
freshen lens (Nm n (U i) l) = do
    j <- gets maxT
    modifying lens (IM.insert i (j+1))
    modifying maxTLens (+1) $> Nm n (U$j+1) l

tryReplaceInT :: Lens' TRenames (IM.IntMap Int) -> Nm a -> CM (Nm a)
tryReplaceInT lens n@(Nm t (U i) l) = do
    st <- use lens
    case IM.lookup i st of
        Just j  -> pure (Nm t (U j) l)
        Nothing -> freshen lens n

cloneTClosed :: Int -> T a
             -> (Int, T a, IM.IntMap Int) -- ^ Substition on type variables, returned so constraints can be propagated/copied
cloneTClosed u = (\(t, TRenames uϵ tvs _ _) -> (uϵ,t,tvs)) . flip runState (TRenames u IM.empty IM.empty IM.empty) . cloneT
  where
    cloneIx :: I a -> CM (I a)
    cloneIx i@Ix{}           = pure i
    cloneIx (StaPlus l i i') = StaPlus l <$> cloneIx i <*> cloneIx i'
    cloneIx (StaMul l i i')  = StaMul l <$> cloneIx i <*> cloneIx i'
    cloneIx (IVar l n)       = IVar l <$> tryReplaceInT boundIxLens n
    cloneIx e@IEVar{}        = pure e

    cloneSh :: Sh a -> CM (Sh a)
    cloneSh Nil           = pure Nil
    cloneSh (Cons i sh)   = Cons <$> cloneIx i <*> cloneSh sh
    cloneSh (SVar n)      = SVar <$> tryReplaceInT boundShLens n
    cloneSh (Rev sh)      = Rev <$> cloneSh sh
    cloneSh (Cat sh0 sh1) = Cat <$> cloneSh sh0 <*> cloneSh sh1

    cloneT :: T a -> CM (T a)
    cloneT F            = pure F
    cloneT I            = pure I
    cloneT B            = pure B
    cloneT (Arrow t t') = Arrow <$> cloneT t <*> cloneT t'
    cloneT (Arr sh t)   = Arr <$> cloneSh sh <*> cloneT t
    cloneT (TVar n)     = TVar <$> tryReplaceInT boundTVLens n
    cloneT (P ts)       = P <$> traverse cloneT ts
