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
rE (Lam l n e) = doLocal $ do
    n' <- freshen n
    Lam l n' <$> rE e
rE (LamΠ l n e) = doLocal $ do
    ns' <- traverse freshen n
    LamΠ l ns' <$> rE e
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
rE (Dfn l e) = do
    x@(Nm nX uX _) <- dummyName l "x"
    y@(Nm nY uY _) <- dummyName l "y"
    (e', hasY) <- r (Nm nX uX) (Nm nY uY) e
    pure $ if hasY
        then Lam l x (Lam l y e')
        else Lam l x e'
  where
    r x _ (ResVar lϵ X)  = pure (Var lϵ (x lϵ), False)
    r _ y (ResVar lϵ Y)  = pure (Var lϵ (y lϵ), True)
    r _ _ eϵ@Builtin{}   = pure (eϵ, False)
    r _ _ eϵ@BLit{}      = pure (eϵ, False)
    r _ _ eϵ@ILit{}      = pure (eϵ, False)
    r _ _ eϵ@FLit{}      = pure (eϵ, False)
    r x y (ALit lϵ es)   = do {(es',b) <- unzip <$> traverse (r x y) es; pure (ALit lϵ es', or b)}
    r x y (Tup lϵ es)    = do {(es',b) <- unzip <$> traverse (r x y) es; pure (Tup lϵ es', or b)}
    r _ _ (Var lϵ n)     = (\n' -> (Var lϵ n', False)) <$> replaceVar n
    r x y (EApp lϵ e0 e1) = do
        (e0',b0) <- r x y e0
        (e1',b1) <- r x y e1
        pure (EApp lϵ e0' e1', b0||b1)
    r x y (Cond lϵ p e0 e1) = do
        (p',b0) <- r x y p
        (e0',b1) <- r x y e0
        (e1',b2) <- r x y e1
        pure (Cond lϵ p' e0' e1', b0||b1||b2)
    r x y (Ann lϵ eϵ t) = do
        (e',b) <- r x y eϵ
        t' <- liftR t
        pure (Ann lϵ e' t', b)
    r x y (Lam lϵ n eϵ) = doLocal $ do
        n' <- freshen n
        first (Lam lϵ n') <$> r x y eϵ
    r x y (LamΠ lϵ n eϵ) = doLocal $ do
        n' <- traverse freshen n
        first (LamΠ lϵ n') <$> r x y eϵ
    r x y (Let lϵ (n, eb) eϵ) = do
        (eb',bb) <- r x y eb
        n' <- freshen n
        (eϵ',b) <- r x y eϵ
        pure (Let lϵ (n', eb') eϵ', b||bb)
    r x y (Def lϵ (n, eb) eϵ) = do
        (eb',bb) <- r x y eb
        n' <- freshen n
        (eϵ',b) <- r x y eϵ
        pure (Def lϵ (n', eb') eϵ', b||bb)
    r x y (LLet lϵ (n, eb) eϵ) = do
        (eb',bb) <- r x y eb
        n' <- freshen n
        (eϵ',b) <- r x y eϵ
        pure (LLet lϵ (n', eb') eϵ', b||bb)
    r _ _ (Dfn lϵ eϵ) = do
        x@(Nm nX uX _) <- dummyName lϵ "x"
        y@(Nm nY uY _) <- dummyName lϵ "y"
        (e', hasY) <- r (Nm nX uX) (Nm nY uY) eϵ
        pure $ if hasY
            then (Lam lϵ x (Lam lϵ y e'), False)
            else (Lam lϵ x e', False)
    r x y (Parens _ eϵ) = r x y eϵ
    r _ _ Id{} = error"internal error: idioms should not be present when desugaring dfns"
rE e@Builtin{} = pure e; rE e@BLit{} = pure e
rE e@FLit{} = pure e; rE e@ILit{} = pure e
rE (ALit l es) = ALit l <$> traverse rE es
rE (Tup l es) = Tup l <$> traverse rE es
rE (EApp l e e') = EApp l <$> rE e <*> rE e'
rE (Cond l e e' e'') = Cond l <$> rE e <*> rE e' <*> rE e''
rE (Var l n) = Var l <$> replaceVar n
rE (Ann l e t) = Ann l <$> rE e <*> liftR t
rE (Id l idm) = Id l <$> rId idm
rE (Parens _ e) = rE e
rE ResVar{} = error"internal error? Bare implicit variable."

rId (AShLit is es)          = AShLit is <$> traverse rE es
rId (U2 seeds gs c f n)     = U2 <$> traverse rE seeds <*> traverse rE gs <*> rE c <*> rE f <*> rE n
rId (FoldGen seed g f n)    = FoldGen <$> rE seed <*> rE g <*> rE f <*> rE n
rId (FoldOfZip zop op es)   = FoldOfZip <$> rE zop <*> rE op <*> traverse rE es
rId (FoldSOfZip seed op es) = FoldSOfZip <$> rE seed <*> rE op <*> traverse rE es
rId (Aɴ e ns)               = Aɴ <$> rE e <*> traverse rE ns
rId (Iter f x n)            = Iter <$> rE f <*> rE x <*> rE n
