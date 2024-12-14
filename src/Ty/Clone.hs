{-# LANGUAGE RankNTypes #-}

module Ty.Clone ( cloneT ) where


import           A
import           Control.Monad.Trans.State.Strict (State, gets, runState)
import qualified Data.IntMap                      as IM
import           Lens.Micro                       (Lens')
import           Lens.Micro.Mtl                   (modifying, use)
import           Nm
import           Sh
import           U

data TR = TR { maxT :: Int, boundTV, boundSh, boundIx :: IM.IntMap Int }

type CM = State TR

maxTLens :: Lens' TR Int
maxTLens f (TR m t s i) = (\x -> TR x t s i) <$> f m

boundTVLens, boundShLens, boundIxLens :: Lens' TR (IM.IntMap Int)
boundTVLens f (TR m t s i) = (\x -> TR m x s i) <$> f t
boundShLens f (TR m t s i) = (\x -> TR m t x i) <$> f s
boundIxLens f (TR m t s i) = TR m t s <$> f i

-- for clone
freshen :: Lens' TR (IM.IntMap Int) -- ^ TVars, shape var, etc.
        -> Nm a -> CM (Nm a)
freshen lens (Nm n (U i) l) = do
    modifying maxTLens (+1)
    j <- gets maxT
    modifying lens (IM.insert i j)
    pure $ Nm n (U j) l

tryReplaceInT :: Lens' TR (IM.IntMap Int) -> Nm a -> CM (Nm a)
tryReplaceInT lens n@(Nm t (U i) l) = do
    st <- use lens
    case IM.lookup i st of
        Just j  -> pure (Nm t (U j) l)
        Nothing -> freshen lens n

cloneT :: Int -> T a
              -> (Int, T a, IM.IntMap Int) -- ^ Substition on type variables, returned so constraints can be propagated/copied
cloneT u = (\(t, TR uϵ tvs _ _) -> (uϵ,t,tvs)).flip runState (TR u IM.empty IM.empty IM.empty).cT
  where
    cloneIx :: I a -> CM (I a)
    cloneIx i@Ix{}           = pure i
    cloneIx (StaPlus l i i') = StaPlus l <$> cloneIx i <*> cloneIx i'
    cloneIx (StaMul l i i')  = StaMul l <$> cloneIx i <*> cloneIx i'
    cloneIx (IVar l n)       = IVar l <$> tryReplaceInT boundIxLens n
    cloneIx (IEVar l n)      = IEVar l <$> tryReplaceInT boundIxLens n

    cloneSh :: Sh a -> CM (Sh a)
    cloneSh Nil           = pure Nil
    cloneSh (Cons i sh)   = Cons <$> cloneIx i <*> cloneSh sh
    cloneSh (SVar n)      = SVar <$> tryReplaceInT boundShLens n
    cloneSh (Rev sh)      = Rev <$> cloneSh sh
    cloneSh (Cat sh0 sh1) = Cat <$> cloneSh sh0 <*> cloneSh sh1
    cloneSh (Π sh)        = Π <$> cloneSh sh

    cT :: T a -> CM (T a)
    cT F            = pure F
    cT I            = pure I
    cT B            = pure B
    cT (Li ix)      = Li <$> cloneIx ix
    cT (Arrow t t') = Arrow <$> cT t <*> cT t'
    cT (Arr sh t)   = Arr <$> cloneSh sh <*> cT t
    cT (TVar n)     = TVar <$> tryReplaceInT boundTVLens n
    cT (P ts)       = P <$> traverse cT ts
