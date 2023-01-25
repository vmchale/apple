{-# LANGUAGE RankNTypes #-}

module R ( Renames (..)
         , HasRenames (..)
         , maxLens
         , rG
         , rE
         ) where

import           A
import           Control.Monad.State.Strict (MonadState, runState)
import           Data.Bifunctor             (second)
import           Data.Functor               (($>))
import qualified Data.IntMap                as IM
import           Lens.Micro                 (Lens')
import           Lens.Micro.Mtl             (use, (%=), (.=))
import           Name
import           Ty.Clone
import           U

data Renames = Renames { max_ :: Int, bound :: IM.IntMap Int }

class HasRenames a where
    rename :: Lens' a Renames

instance HasRenames Renames where
    rename = id

maxLens :: Lens' Renames Int
maxLens f s = fmap (\x -> s { max_ = x }) (f (max_ s))

boundLens :: Lens' Renames (IM.IntMap Int)
boundLens f s = fmap (\x -> s { bound = x }) (f (bound s))

mapBound :: (IM.IntMap Int -> IM.IntMap Int) -> Renames -> Renames
mapBound f (Renames m b) = Renames m (f b)

setMax :: Int -> Renames -> Renames
setMax i r = r { max_ = i }

-- Make sure you don't have cycles in the renames map!
replaceUnique :: (MonadState s m, HasRenames s) => U -> m U
replaceUnique u@(U i) = do
    rSt <- use (rename.boundLens)
    case IM.lookup i rSt of
        Nothing -> pure u
        Just j  -> replaceUnique (U j)

replaceVar :: (MonadState s m, HasRenames s) => Name a -> m (Name a)
replaceVar (Name n u l) = do
    u' <- replaceUnique u
    pure $ Name n u' l

withRenames :: (HasRenames s, MonadState s m) => (Renames -> Renames) -> m a -> m a
withRenames modSt act = do
    preSt <- use rename
    rename %= modSt
    res <- act
    postMax <- use (rename.maxLens)
    rename .= setMax postMax preSt
    pure res

withName :: (HasRenames s, MonadState s m)
         => Name a
         -> m (Name a, Renames -> Renames)
withName (Name t (U i) l) = do
    m <- use (rename.maxLens)
    let newUniq = m+1
    rename.maxLens .= newUniq
    pure (Name t (U newUniq) l, mapBound (IM.insert i (m+1)))

-- globally unique
rG :: Int -> E a -> (E a, Int)
rG i = second max_ . flip runState (Renames i IM.empty) . rE

{-# INLINABLE liftR #-}
liftR :: (HasRenames s, MonadState s m) => T a -> m (T a)
liftR t = do
    i <- use (rename.maxLens)
    let (u,t',_) = cloneTClosed i t
    (rename.maxLens .= u) $> t'

{-# INLINABLE rE #-}
rE :: (HasRenames s, MonadState s m) => E a -> m (E a)
rE (Lam l n e) = do
    (n', modR) <- withName n
    Lam l n' <$> withRenames modR (rE e)
rE (Let l (n, eϵ) e) = do
    eϵ' <- rE eϵ
    (n', modR) <- withName n
    Let l (n', eϵ') <$> withRenames modR (rE e)
rE (Def l (n, eϵ) e) = do
    eϵ' <- rE eϵ
    (n', modR) <- withName n
    Def l (n', eϵ') <$> withRenames modR (rE e)
rE (LLet l (n, eϵ) e) = do
    eϵ' <- rE eϵ
    (n', modR) <- withName n
    LLet l (n', eϵ') <$> withRenames modR (rE e)
rE e@Builtin{} = pure e
rE e@FLit{} = pure e
rE e@ILit{} = pure e
rE e@BLit{} = pure e
rE (ALit l es) = ALit l <$> traverse rE es
rE (Tup l es) = Tup l <$> traverse rE es
rE (EApp l e e') = EApp l <$> rE e <*> rE e'
rE (Cond l e e' e'') = Cond l <$> rE e <*> rE e' <*> rE e''
rE (Var l n) = Var l <$> replaceVar n
rE (Ann l e t) = Ann l <$> rE e <*> liftR t
