{-# LANGUAGE RankNTypes #-}

module R ( Rs (..), HasRs (..)
         , maxLens
         , rG, rE
         ) where

import           A
import           Control.Monad.Trans.State.Strict (StateT, runState)
import           Data.Bifunctor                   (first, second)
import           Data.Functor                     (($>))
import qualified Data.IntMap                      as IM
import qualified Data.Text                        as T
import           Lens.Micro                       (Lens')
import           Lens.Micro.Mtl                   (modifying, use, (%=), (.=))
import           Nm
import           Ty.Clone
import           U

data Rs = Rs { max_ :: Int, bound :: IM.IntMap Int }

class HasRs a where
    rename :: Lens' a Rs

instance HasRs Rs where rename=id

maxLens :: Lens' Rs Int
maxLens f (Rs m b) = (\x -> Rs x b) <$> f m

boundLens :: Lens' Rs (IM.IntMap Int)
boundLens f (Rs m b) = Rs m <$> f b

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

dummyName :: (Monad m, HasRs s) => a -> T.Text -> StateT s m (Nm a)
dummyName l n = do
    rename.maxLens %= (+1)
    st <- use (rename.maxLens)
    pure (Nm n (U st) l)

doLocal :: (HasRs s, Monad m) => StateT s m a -> StateT s m a
doLocal act = do
    preB <- use (rename.boundLens)
    act <* (rename.boundLens .= preB)

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
    let (u,t') = cloneT i t
    (rename.maxLens .= u) $> t'

{-# INLINABLE rE #-}
rE :: (HasRs s, Monad m) => E a -> StateT s m (E a)
rE = fmap fst.r undefined undefined

{-# INLINABLE r #-}
r :: (HasRs s, Monad m) => (a -> Nm a) -> (a -> Nm a) ->  E a -> StateT s m (E a, Bool)
r x y (Lam l n e) = doLocal $ do
    n' <- freshen n
    first (Lam l n') <$> r x y e
r x y (LamΠ l n e) = doLocal $ do
    n' <- traverse freshen n
    first (LamΠ l n') <$> r x y e
r x y (Let l (n, eb) e) = do
    (eb',bb) <- r x y eb
    n' <- freshen n
    (eϵ',b) <- r x y e
    pure (Let l (n', eb') eϵ', b||bb)
r x y (Def l (n, eb) eϵ) = do
    (eb',bb) <- r x y eb
    n' <- freshen n
    (eϵ',b) <- r x y eϵ
    pure (Def l (n', eb') eϵ', b||bb)
r x y (LLet l (n, eb) eϵ) = do
    (eb',bb) <- r x y eb
    n' <- freshen n
    (eϵ',b) <- r x y eϵ
    pure (LLet l (n', eb') eϵ', b||bb)
r _ _ (Dfn l e) = do
    x@(Nm nX uX _) <- dummyName l "x"
    y@(Nm nY uY _) <- dummyName l "y"
    (e', hasY) <- r (Nm nX uX) (Nm nY uY) e
    pure $ if hasY
        then (Lam l x (Lam l y e'), False)
        else (Lam l x e', False)
r _ _ eϵ@Builtin{} = pure (eϵ, False)
r _ _ eϵ@BLit{} = pure (eϵ, False)
r _ _ eϵ@ILit{} = pure (eϵ, False)
r _ _ eϵ@FLit{} = pure (eϵ, False)
r x y (ALit l es)   = do {(es',b) <- unzip <$> traverse (r x y) es; pure (ALit l es', or b)}
r x y (Tup l es)    = do {(es',b) <- unzip <$> traverse (r x y) es; pure (Tup l es', or b)}
r x y (EApp l e0 e1) = do
    (e0',b0) <- r x y e0
    (e1',b1) <- r x y e1
    pure (EApp l e0' e1', b0||b1)
r x y (Cond l p e0 e1) = do
    (p',b0) <- r x y p
    (e0',b1) <- r x y e0
    (e1',b2) <- r x y e1
    pure (Cond l p' e0' e1', b0||b1||b2)
r x y (Ann l e t) = do
    (e',b) <- r x y e
    t' <- liftR t
    pure (Ann l e' t', b)
r _ _ (Var l n)     = (\n' -> (Var l n', False)) <$> replaceVar n
r _ _ (Id l idm) = (\idm' -> (Id l idm', error"internal error: idioms not expected to be present when desugaring dfns")) <$> rId idm
r x y (Parens _ e) = r x y e
r x _ (ResVar l X)  = pure (Var l (x l), False)
r _ y (ResVar l Y)  = pure (Var l (y l), True)

rId (AShLit is es)          = AShLit is <$> traverse rE es
rId (U2 seeds gs c f n)     = U2 <$> traverse rE seeds <*> traverse rE gs <*> rE c <*> rE f <*> rE n
rId (FoldGen seed g f n)    = FoldGen <$> rE seed <*> rE g <*> rE f <*> rE n
rId (FoldOfZip zop op es)   = FoldOfZip <$> rE zop <*> rE op <*> traverse rE es
rId (FoldSOfZip seed op es) = FoldSOfZip <$> rE seed <*> rE op <*> traverse rE es
rId (Aɴ e ns)               = Aɴ <$> rE e <*> traverse rE ns
rId (Iter f x n)            = Iter <$> rE f <*> rE x <*> rE n
