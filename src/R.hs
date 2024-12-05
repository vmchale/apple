{-# LANGUAGE RankNTypes #-}

module R ( Rs (..), HasRs (..)
         , maxLens
         , rG, rE
         ) where

import           A
import           Control.Monad.Trans.State.Strict (StateT, runState)
import           Data.Bifunctor                   (second)
import           Data.Functor                     (($>))
import qualified Data.IntMap                      as IM
import           Lens.Micro                       (Lens')
import           Lens.Micro.Mtl                   (modifying, use, (.=))
import           Nm
import           Ty.Clone
import           U

data Rs = Rs { max_ :: Int, bound :: IM.IntMap Int }

class HasRs a where
    rename :: Lens' a Rs

instance HasRs Rs where rename=id

maxLens :: Lens' Rs Int
maxLens f s = fmap (\x -> s { max_ = x }) (f (max_ s))

boundLens :: Lens' Rs (IM.IntMap Int)
boundLens f s = fmap (\x -> s { bound = x }) (f (bound s))

-- Make sure you don't have cycles in the renames map!
replaceUnique :: (Monad m, HasRs s) => U -> StateT s m U
replaceUnique u@(U i) = do
    rSt <- use (rename.boundLens)
    case IM.lookup i rSt of
        Nothing -> pure u
        Just j  -> replaceUnique (U j)

replaceVar :: (Monad m, HasRs s) => Nm a -> StateT s m (Nm a)
replaceVar (Nm n u l) = do
    u' <- replaceUnique u
    pure $ Nm n u' l

doLocal :: (HasRs s, Monad m) => StateT s m a -> StateT s m a
doLocal act = do
    preB <- use (rename.boundLens)
    act <* ((rename.boundLens) .= preB)

freshen :: (HasRs s, Monad m) => Nm a -> StateT s m (Nm a)
freshen (Nm t (U i) l) = do
    m <- use (rename.maxLens)
    let nU=m+1
    rename.maxLens .= nU
    modifying (rename.boundLens) (IM.insert i nU) $> Nm t (U nU) l

-- globally unique
rG :: Int -> E a -> (E a, Int)
rG i = second max_ . flip runState (Rs i IM.empty) . rE

{-# INLINABLE liftR #-}
liftR :: (HasRs s, Monad m) => T a -> StateT s m (T a)
liftR t = do
    i <- use (rename.maxLens)
    let (u,t',_) = cloneT i t
    (rename.maxLens .= u) $> t'

{-# INLINABLE rE #-}
rE :: (HasRs s, Monad m) => E a -> StateT s m (E a)
rE (Lam l n e) = doLocal $ do
    n' <- freshen n
    Lam l n' <$> rE e
rE (Let l (n, eϵ) e) = do
    eϵ' <- rE eϵ
    n' <- freshen n
    Let l (n', eϵ') <$> rE e
rE (Def l (n, eϵ) e) = do
    eϵ' <- rE eϵ
    n' <- freshen n
    Def l (n', eϵ') <$> rE e
rE (LLet l (n, eϵ) e) = do
    eϵ' <- rE eϵ
    n' <- freshen n
    LLet l (n', eϵ') <$> rE e
rE e@Builtin{} = pure e; rE e@BLit{} = pure e
rE e@FLit{} = pure e; rE e@ILit{} = pure e
rE (ALit l es) = ALit l <$> traverse rE es
rE (Tup l es) = Tup l <$> traverse rE es
rE (EApp l e e') = EApp l <$> rE e <*> rE e'
rE (Cond l e e' e'') = Cond l <$> rE e <*> rE e' <*> rE e''
rE (Var l n) = Var l <$> replaceVar n
rE (Ann l e t) = Ann l <$> rE e <*> liftR t
rE (Id l idm) = Id l <$> rId idm

rId (AShLit is es)          = AShLit is <$> traverse rE es
rId (U2 seeds gs c f n)     = U2 <$> traverse rE seeds <*> traverse rE gs <*> rE c <*> rE f <*> rE n
rId (FoldGen seed g f n)    = FoldGen <$> rE seed <*> rE g <*> rE f <*> rE n
rId (FoldOfZip zop op es)   = FoldOfZip <$> rE zop <*> rE op <*> traverse rE es
rId (FoldSOfZip seed op es) = FoldSOfZip <$> rE seed <*> rE op <*> traverse rE es
rId (Aɴ e ns)               = Aɴ <$> rE e <*> traverse rE ns
