{-# LANGUAGE RankNTypes #-}

module Ty.Clone ( cloneT ) where


import           A
import           Control.Monad.State.Strict (State, gets, runState)
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
    modifying maxTLens (+1)
    j <- gets maxT
    modifying lens (IM.insert i j)
    pure $ Nm n (U j) l

tryReplaceInT :: Lens' TRenames (IM.IntMap Int) -> Nm a -> CM (Nm a)
tryReplaceInT lens n@(Nm t (U i) l) = do
    st <- use lens
    case IM.lookup i st of
        Just j  -> pure (Nm t (U j) l)
        Nothing -> freshen lens n

cloneT :: Int -> T a
              -> (Int, T a, IM.IntMap Int) -- ^ Substition on type variables, returned so constraints can be propagated/copied
cloneT u = (\(t, TRenames uϵ tvs _ _) -> (uϵ,t,tvs)).flip runState (TRenames u IM.empty IM.empty IM.empty).cT
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

    cT :: T a -> CM (T a)
    cT F            = pure F
    cT I            = pure I
    cT B            = pure B
    cT (Arrow t t') = Arrow <$> cT t <*> cT t'
    cT (Arr sh t)   = Arr <$> cloneSh sh <*> cT t
    cT (TVar n)     = TVar <$> tryReplaceInT boundTVLens n
    cT (P ts)       = P <$> traverse cT ts
