{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Ty ( TyE
          , tyClosed
          , match
          -- * Substitutions
          , aT, rwArr
          ) where

import           A
import           Control.DeepSeq            (NFData)
import           Control.Exception          (Exception, throw)
import           Control.Monad              (zipWithM)
import           Control.Monad.Except       (liftEither, throwError)
import           Control.Monad.State.Strict (StateT (runStateT), gets, modify, state)
import           Data.Bifunctor             (first, second)
import           Data.Containers.ListUtils  (nubOrd)
import           Data.Foldable              (traverse_)
import           Data.Functor               (void, ($>))
import qualified Data.IntMap                as IM
import qualified Data.IntSet                as IS
import           Data.Maybe                 (catMaybes)
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import           Nm
import           Nm.IntMap
import           Prettyprinter              (Doc, Pretty (..), hardline, indent, squotes, (<+>))
import           Prettyprinter.Ext
import           Ty.Clone
import           U

data TySt a = TySt { maxU :: !Int, staEnv, polyEnv :: IM.IntMap (T ()), varConstr :: IM.IntMap (C, a) }

data Subst a = Subst { tySubst :: IM.IntMap (T a)
                     , iSubst  :: IM.IntMap (I a) -- ^ Index variables
                     , sSubst  :: IM.IntMap (Sh a) -- ^ Shape variables
                     } deriving (Functor)

data TyE a = IllScoped a (Nm a)
           | UF a (E a) (T a) (T a)
           | UI a (I a) (I a)
           | USh a (Sh a) (Sh a)
           | OT a (T a) (T a)
           | OSh a (Sh a) (Sh a)
           | OI a (I a) (I a)
           | ExistentialArg (T ())
           | MatchFailed (T ()) (T ())
           | MatchShFailed (Sh a) (Sh a)
           | MatchIFailed (I a) (I a)
           | Doesn'tSatisfy a (T a) C
           deriving (Generic)

instance Semigroup (Subst a) where
    (<>) (Subst t i s) (Subst t0 i0 s0) = Subst (t<>t0) (i<>i0) (s<>s0)

instance Monoid (Subst a) where
    mempty = Subst IM.empty IM.empty IM.empty
    mappend = (<>)

instance NFData a => NFData (TyE a) where

instance Pretty a => Pretty (TyE a) where
    pretty (IllScoped l n)         = pretty l <> ":" <+> squotes (pretty n) <+> "is not in scope."
    pretty (UF l e ty ty')         = pretty l <> ":" <+> "could not unify" <+> squotes (pretty ty) <+> "with" <+> squotes (pretty ty') <+> "in expression" <+> squotes (pretty e)
    pretty (USh l sh sh')          = pretty l <> ":" <+> "could not unify shape" <+> squotes (pretty sh) <+> "with" <+> squotes (pretty sh')
    pretty (UI l ix ix')           = pretty l <> ":" <+> "could not unify index" <+> squotes (pretty ix) <+> "with" <+> squotes (pretty ix')
    pretty (OT l ty ty')           = pretty l <> ":" <+> "occurs check failed when unifying" <+> squotes (pretty ty) <+> "and" <+> squotes (pretty ty')
    pretty (OI l i j)              = pretty l <> ":" <+> "occurs check failed when unifying indices" <+> squotes (pretty i) <+> "and" <+> squotes (pretty j)
    pretty (OSh l s0 s1)           = pretty l <> ":" <+> "occurs check failed when unifying shapes" <+> squotes (pretty s0) <+> "and" <+> squotes (pretty s1)
    pretty (ExistentialArg ty)     = "Existential occurs as an argument in" <+> squotes (pretty ty)
    pretty (MatchFailed t t')      = "Failed to match" <+> squotes (pretty t) <+> "against type" <+> squotes (pretty t')
    pretty (MatchShFailed sh sh')  = "Failed to match" <+> squotes (pretty sh) <+> "against shape" <+> squotes (pretty sh')
    pretty (MatchIFailed i i')     = "Failed to match" <+> squotes (pretty i) <+> "against index" <+> squotes (pretty i')
    pretty (Doesn'tSatisfy l ty c) = pretty l <+> squotes (pretty ty) <+> "is not a member of class" <+> pretty c

instance (Pretty a) => Show (TyE a) where
    show = show . pretty

instance (Pretty a, Typeable a) => Exception (TyE a) where

instance Pretty (Subst a) where
    pretty (Subst ty i sh) =
        "type:" <#*> prettyDumpBinds ty
            <#> "index:" <#*> prettyDumpBinds i
            <#> "shape:" <#*> prettyDumpBinds sh

instance Show (Subst a) where show = show . pretty

(<#*>) :: Doc a -> Doc a -> Doc a
(<#*>) x y = x <> hardline <> indent 2 y

type TyM a = StateT (TySt a) (Either (TyE a))
type UM a = StateT Int (Either (TyE a))

nI :: a -> UM b (I a)
nI l = state (\i -> let j=i+1 in (IEVar l (Nm "m" (U j) l), j))

liftU :: UM a x -> TyM a x
liftU a = do
    i <- gets maxU
    (b, j) <- liftEither$runStateT a i
    modify (setMaxU j) $> b

mI :: Focus -> I a -> I a -> Either (TyE a) (Subst a)
mI _ i0@(Ix _ i) i1@(Ix _ j) | i == j = Right mempty
                             | otherwise = Left $ MatchIFailed i0 i1
mI _ (IVar _ (Nm _ (U i) _)) ix = Right $ Subst IM.empty (IM.singleton i ix) IM.empty
mI _ ix (IVar _ (Nm _ (U i) _)) = Right $ Subst IM.empty (IM.singleton i ix) IM.empty
mI _ (IEVar _ n) (IEVar _ n') | n == n' = Right mempty
mI RF IEVar{} IEVar{} = Right mempty
-- TODO: Ix should match against ∃
mI _ i0@IEVar{} i1@IEVar{} = Left $ MatchIFailed i0 i1
mI f (StaPlus _ i (Ix _ iϵ)) (Ix l j) | j >= iϵ = mI f i (Ix l (j-iϵ))
mI f (Ix l iϵ) (StaPlus _ i (Ix _ j)) | iϵ >= j = mI f i (Ix l (iϵ-j))
mI f (StaPlus _ i j) (StaPlus _ i' j') = (<>) <$> mI f i i' <*> mI f j j' -- FIXME: too stringent
mI f (StaMul _ i j) (StaMul _ i' j') = (<>) <$> mI f i i' <*> mI f j j' -- FIXME: too stringent

mSh :: Focus -> Sh a -> Sh a -> Either (TyE a) (Subst a)
mSh _ (SVar (Nm _ (U i) _)) sh      = Right $ Subst IM.empty IM.empty (IM.singleton i sh)
mSh _ Nil Nil                       = Right mempty
mSh f (Cons i sh) (Cons i' sh')     = (<>) <$> mI f i i' <*> mSh f sh sh'
mSh f (Cat sh0 sh1) (Cat sh0' sh1') = (<>) <$> mSh f sh0 sh0' <*> mSh f sh1 sh1'
mSh f (Rev sh) (Rev sh')            = mSh f sh sh'
mSh _ sh sh'                        = Left $ MatchShFailed sh sh'

match :: (Typeable a, Pretty a) => T a -> T a -> Subst a
match t t' = either throw id (maM LF t t')

maM :: Focus -> T a -> T a -> Either (TyE a) (Subst a)
maM f (Li n) (Li m)                 = mI f m n
maM _ I I                           = Right mempty
maM _ F F                           = Right mempty
maM _ B B                           = Right mempty
maM _ (TVar n) (TVar n') | n == n'  = Right mempty
maM _ (TVar (Nm _ (U i) _)) t     = Right $ Subst (IM.singleton i t) IM.empty IM.empty
maM _ (Arrow t0 t1) (Arrow t0' t1') = (<>) <$> maM LF t0 t0' <*> maM RF t1 t1' -- FIXME: use <\> over <>
maM f (Arr sh t) (Arr sh' t')       = (<>) <$> mSh f sh sh' <*> maM f t t'
maM f (Arr sh t) t'                 = (<>) <$> mSh f sh Nil <*> maM f t t'
maM f (P ts) (P ts')                = mconcat <$> zipWithM (maM f) ts ts'
maM _ (Ρ n _) (Ρ n' _) | n == n'    = Right mempty
maM f (Ρ n rs) t@(Ρ _ rs') | IM.keysSet rs' `IS.isSubsetOf` IM.keysSet rs = mapTySubst (insert n t) . mconcat <$> traverse (uncurry (maM f)) (IM.elems (IM.intersectionWith (,) rs rs'))
maM f (Ρ n rs) t@(P ts) | length ts >= fst (IM.findMax rs) = mapTySubst (IM.insert (unU$unique n) t) . mconcat <$> traverse (uncurry (maM f)) [ (ts!!(i-1),tϵ) | (i,tϵ) <- IM.toList rs ]
maM _ t t'                          = Left $ MatchFailed (void t) (void t')

shSubst :: Subst a -> Sh a -> Sh a
shSubst _ Nil           = Nil
shSubst s (Cons i sh)   = Cons (iSubst s !> i) (shSubst s sh)
shSubst s (Cat sh0 sh1) = Cat (shSubst s sh0) (shSubst s sh1)
shSubst s (Rev sh)      = Rev (shSubst s sh)
shSubst s (Π sh)        = Π (shSubst s sh)
shSubst s@(Subst ts is ss) sh'@(SVar (Nm _ (U u) _)) =
    case IM.lookup u ss of
        Just sh''@SVar{} -> shSubst (Subst ts is (IM.delete u ss)) sh''
        Just sh          -> shSubst s sh
        Nothing          -> sh'

infixr 4 !>
(!>) :: IM.IntMap (I a) -> I a -> I a
(!>) ixes ix'@(IVar _ (Nm _ (U u) _)) =
    case IM.lookup u ixes of
        Just ix@IVar{} -> IM.delete u ixes !> ix
        Just ix        -> ixes !>ix
        Nothing        -> ix'
(!>) ixes (StaPlus l ix ix') = StaPlus l (ixes !> ix) (ixes !> ix')
(!>) ixes (StaMul l ix ix') = StaMul l (ixes !> ix) (ixes !> ix')
(!>) _ ix@Ix{} = ix
(!>) _ ix@IEVar{} = ix

aT :: Subst a -> T a -> T a
aT s (Arr sh ty) = Arr (shSubst s sh) (aT s ty)
aT s (Arrow t₁ t₂) = Arrow (aT s t₁) (aT s t₂)
aT s@(Subst ts is ss) ty'@(TVar n) =
    let u = unU $ unique n in
    case IM.lookup u ts of
        Just ty@TVar{} -> aT (Subst (IM.delete u ts) is ss) ty
        Just ty@Ρ{}    -> aT (Subst (IM.delete u ts) is ss) ty
        Just ty        -> aT s ty
        Nothing        -> ty'
aT s (P ts) = P (aT s <$> ts)
aT s@(Subst ts is ss) (Ρ n rs) =
    let u = unU (unique n) in
    case IM.lookup u ts of
        Just ty@Ρ{}    -> aT (Subst (IM.delete u ts) is ss) ty
        Just ty@TVar{} -> aT (Subst (IM.delete u ts) is ss) ty
        Just ty        -> aT s ty
        Nothing        -> Ρ n (aT s<$>rs)
aT _ ty = ty

runTyM :: Int -> TyM a b -> Either (TyE a) (b, Int)
runTyM i = fmap (second maxU) . flip runStateT (TySt i IM.empty IM.empty IM.empty)

mapMaxU :: (Int -> Int) -> TySt a -> TySt a
mapMaxU f (TySt u l v vcs) = TySt (f u) l v vcs

setMaxU :: Int -> TySt a -> TySt a
setMaxU i (TySt _ l v vcs) = TySt i l v vcs

addStaEnv :: Nm a -> T () -> TySt a -> TySt a
addStaEnv n t (TySt u l v vcs) = TySt u (insert n t l) v vcs

addPolyEnv :: Nm a -> T () -> TySt a -> TySt a
addPolyEnv n t (TySt u l v vcs) = TySt u l (insert n t v) vcs

addVarConstrI :: Int -> a -> C -> TySt a -> TySt a
addVarConstrI i ann c (TySt u l v vcs) = TySt u l v (IM.insert i (c, ann) vcs)

addVarConstr :: TyNm a -> a -> C -> TySt a -> TySt a
addVarConstr tn = addVarConstrI (unU$unique tn)

pushVarConstraint :: TyNm a -> a -> C -> TyM a ()
pushVarConstraint tn l c = modify (addVarConstr tn l c)

freshN :: T.Text -> b -> TyM a (Nm b)
freshN n l = do
    modify (mapMaxU (+1))
    st <- gets maxU
    pure $ Nm n (U st) l

ft :: T.Text -> b -> TyM a (T b)
ft n l = TVar <$> freshN n l

fsh :: T.Text -> TyM a (Sh ())
fsh n = SVar <$> freshN n ()

fc :: T.Text -> a -> C -> TyM a (T ())
fc n l c = do
    nϵ <- freshN n l
    pushVarConstraint nϵ l c $> TVar (void nϵ)

ftv :: T.Text -> TyM a (T ())
ftv n = ft n ()

fti :: T.Text -> TyM a (I ())
fti n = IVar () <$> freshN n ()

ftie :: TyM a (I ())
ftie = IEVar () <$> freshN "n" ()

mapTySubst f (Subst t i sh) = Subst (f t) i sh
mapShSubst f (Subst t i sh) = Subst t i (f sh)

data Focus = LF | RF

instance Pretty Focus where pretty LF="⦠"; pretty RF="∢"

mguIPrep :: Focus -> IM.IntMap (I a) -> I a -> I a -> UM a (I a, IM.IntMap (I a))
mguIPrep f is i0 i1 =
    let i0' = is !> i0
        i1' = is !> i1
    in mguI f is (rwI i0') (rwI i1')

mguI :: Focus -> IM.IntMap (I a) -> I a -> I a -> UM a (I a, IM.IntMap (I a))
mguI _ inp i0@(Ix _ i) (Ix _ j) | i == j = pure (i0, inp)
mguI RF inp (Ix l _) Ix{} = do {m <- nI l; pure (m, inp)}
mguI _ _ i0@(Ix l _) i1@Ix{} = throwError $ UI l i0 i1
mguI _ inp i0@(IEVar _ i) (IEVar _ j) | i == j = pure (i0, inp)
mguI RF inp (IEVar l _) (IEVar _ _) = do {m <- nI l; pure (m, inp)}
mguI _ _ i0@(IEVar l _) i1@IEVar{} = throwError $ UI l i0 i1
mguI _ inp i0@(IVar _ i) (IVar _ j) | i == j = pure (i0, inp)
mguI _ inp iix@(IVar l (Nm _ (U i) _)) ix | i `IS.member` occI ix = throwError $ OI l iix ix
                                          | otherwise = pure (ix, IM.insert i ix inp)
mguI _ inp ix iix@(IVar l (Nm _ (U i) _)) | i `IS.member` occI ix = throwError $ OI l ix iix
                                          | otherwise = pure (ix, IM.insert i ix inp)
mguI f inp (StaPlus _ i0 (Ix _ k0)) (StaPlus _ i1 (Ix _ k1)) | k0 == k1 = mguIPrep f inp i0 i1
mguI f inp (StaMul _ i0 (Ix _ k0)) (StaMul _ i1 (Ix _ k1)) | k0 == k1 = mguIPrep f inp i0 i1
mguI f inp i0@(StaPlus l i (Ix _ k)) i1@(Ix lk j) | j >= k = mguIPrep f inp i (Ix lk (j-k))
                                                  | otherwise = throwError $ UI l i0 i1
mguI f inp i0@Ix{} i1@(StaPlus _ _ Ix{}) = mguIPrep f inp i1 i0
mguI f inp (StaPlus l i0 i1) (StaPlus _ j0 j1) = do
    -- FIXME: too stringent
    (k, s) <- mguIPrep f inp i0 j0
    (m, s') <- mguIPrep f s i1 j1
    pure (StaPlus l k m, s')
mguI f inp (StaMul l i0 i1) (StaMul _ j0 j1) = do
    -- FIXME: too stringent
    (k, s) <- mguIPrep f inp i0 j0
    (m, s') <- mguIPrep f s i1 j1
    pure (StaMul l k m, s')
mguI _ _ i0@(IEVar l _) i1@Ix{} = throwError $ UI l i0 i1
mguI _ _ i0@(Ix l _) i1@IEVar{} = throwError $ UI l i0 i1
mguI _ _ i0@(IEVar l _) i1@StaPlus{} = throwError $ UI l i0 i1 -- TODO: focus case
mguI _ _ i0@(StaPlus l _ _) i1@IEVar{} = throwError $ UI l i0 i1
mguI _ _ i0 i1 = error (show (i0,i1))

mgShPrep :: Focus -> a -> Subst a -> Sh a -> Sh a -> UM a (Sh a, Subst a)
mgShPrep f l s sh0 sh1 =
    let sh0' = shSubst s sh0
        sh1' = shSubst s sh1
    in mgSh f l s (rwSh sh0') (rwSh sh1')

mgSh :: Focus -> a -> Subst a -> Sh a -> Sh a -> UM a (Sh a, Subst a)
mgSh _ _ inp Nil Nil = pure (Nil, inp)
mgSh f l inp (Cons i sh) (Cons i' sh') = do
    (i'', sI) <- mguIPrep f (iSubst inp) i i'
    (sh'', s2) <- mgShPrep f l (inp { iSubst = sI }) sh sh'
    pure (Cons i'' sh'', s2)
mgSh _ _ inp s@(SVar sh) (SVar sh') | sh == sh' = pure (s, inp)
mgSh _ l inp s@(SVar (Nm _ (U i) _)) sh | i `IS.member` occSh sh = throwError $ OSh l s sh
                                        | otherwise = pure (sh, mapShSubst (IM.insert i sh) inp)
mgSh _ l inp sh s@(SVar (Nm _ (U i) _)) | i `IS.member` occSh sh = throwError $ OSh l sh s
                                        | otherwise = pure (sh, mapShSubst (IM.insert i sh) inp)
mgSh _ l _ sh@Nil sh'@Cons{} = throwError $ USh l sh sh'
mgSh _ l _ sh@Cons{} sh'@Nil{} = throwError $ USh l sh' sh
mgSh f l inp (Rev sh) (Rev sh') = mgShPrep f l inp sh sh'
mgSh f l inp (Cat sh0 sh0') (Cat sh1 sh1') = do
    (sh', s) <- mgShPrep f l inp sh0 sh1
    (sh'', s') <- mgShPrep f l s sh0' sh1'
    pure (Cat sh' sh'', s')
mgSh f l inp (Rev sh) sh' | (is, Nil) <- unroll sh' = do
    mgShPrep f l inp sh (roll Nil$reverse is)
mgSh f l inp sh (Rev sh') | (is, Nil) <- unroll sh' = do
    mgShPrep f l inp (roll Nil$reverse is) sh

mguPrep :: Focus -> (a, E a) -> Subst a -> T a -> T a -> UM a (T a, Subst a)
mguPrep f l s t0 t1 =
    let t0' = aT s t0
        t1' = aT s t1
    in mgu f l s ({-# SCC "rwArr" #-} rwArr t0') ({-# SCC "rwArr" #-} rwArr t1')

mp :: (a, E a) -> Subst a -> T a -> T a -> UM a (Subst a)
mp l s t0 t1 = snd <$> mguPrep LF l s t0 t1

occSh :: Sh a -> IS.IntSet
occSh (SVar (Nm _ (U i) _)) = IS.singleton i
occSh (Cat sh0 sh1)         = occSh sh0 <> occSh sh1
occSh (_ `Cons` sh)         = occSh sh
occSh Nil{}                 = IS.empty

occI :: I a -> IS.IntSet
occI Ix{}                    = IS.empty
occI (IVar _ (Nm _ (U i) _)) = IS.singleton i
occI (StaPlus _ i j)         = occI i <> occI j
occI (StaMul _ i j)          = occI i <> occI j
occI IEVar{}                 = IS.empty

occ :: T a -> IS.IntSet
occ (TVar (Nm _ (U i) _)) = IS.singleton i
occ (Arrow t t')          = occ t <> occ t'
occ (Arr _ a)             = occ a -- shouldn't need shape?
occ I                     = IS.empty
occ F                     = IS.empty
occ B                     = IS.empty
occ Li{}                  = IS.empty
occ (P ts)                = foldMap occ ts
occ (Ρ (Nm _ (U i) _) rs) = IS.insert i $ foldMap occ rs

mgu :: Focus -> (a, E a) -> Subst a -> T a -> T a -> UM a (T a, Subst a)
mgu f l s (Arrow t0 t1) (Arrow t0' t1') = do
    (t0'', s0) <- mguPrep LF l s t0 t0'
    (t1'', s1) <- mguPrep f l s0 t1 t1'
    pure (Arrow t0'' t1'', s1)
mgu _ _ s I I = pure (I, s)
mgu _ _ s F F = pure (F, s)
mgu _ _ s B B = pure (B, s)
mgu _ _ s t@Li{} I = pure (t, s)
mgu _ _ s I t@Li{} = pure (t, s)
mgu f _ s (Li i0) (Li i1) = do {(i', iS) <- mguIPrep f (iSubst s) i0 i1; pure (Li i', Subst mempty iS mempty <> s)}
mgu _ _ s t@(TVar n) (TVar n') | n == n' = pure (t, s)
mgu _ (l, _) s t'@(TVar (Nm _ (U i) _)) t | i `IS.member` occ t = throwError $ OT l t' t
                                          | otherwise = pure (t, mapTySubst (IM.insert i t) s)
mgu _ (l, _) s t t'@(TVar (Nm _ (U i) _)) | i `IS.member` occ t = throwError $ OT l t' t
                                          | otherwise = pure (t, mapTySubst (IM.insert i t) s)
mgu _ (l, e) _ t0@Arrow{} t1 = throwError $ UF l e t0 t1
mgu _ (l, e) _ t0 t1@Arrow{} = throwError $ UF l e t0 t1
mgu f l s (Arr sh t) (Arr sh' t') = do
    (t'', s0) <- mguPrep f l s t t'
    (sh'', s1) <- mgShPrep f (fst l) s0 sh sh'
    pure (Arr sh'' t'', s1)
mgu _ (l, e) _ F I = throwError $ UF l e F I
mgu _ (l, e) _ I F = throwError $ UF l e I F
mgu _ (l, e) _ F t@Li{} = throwError $ UF l e F t
mgu _ (l, e) _ t@Li{} F = throwError $ UF l e t F
mgu f l s (Arr (SVar n) t) F = second (mapShSubst (insert n Nil)) <$> mguPrep f l s t F
mgu f l s (Arr (SVar n) t) I = second (mapShSubst (insert n Nil)) <$> mguPrep f l s t I
mgu f l s F (Arr (SVar n) t) = second (mapShSubst (insert n Nil)) <$> mguPrep f l s F t
mgu f l s I (Arr (SVar n) t) = second (mapShSubst (insert n Nil)) <$> mguPrep f l s I t
mgu f l s (Arr (SVar n) t) B = second (mapShSubst (insert n Nil)) <$> mguPrep f l s t B
mgu f l s B (Arr (SVar n) t) = second (mapShSubst (insert n Nil)) <$> mguPrep f l s B t
mgu f l s (Arr (SVar n) t) t'@P{} = second (mapShSubst (insert n Nil)) <$> mguPrep f l s t t'
mgu f l s t'@P{} (Arr (SVar n) t) = second (mapShSubst (insert n Nil)) <$> mguPrep f l s t' t
mgu f l s (P ts) (P ts') | length ts == length ts' = first P <$> zSt (mguPrep f l) s ts ts'
-- TODO: rho occurs check
mgu f l@(lϵ, e) s t@(Ρ n rs) t'@(P ts) | length ts >= fst (IM.findMax rs) && fst (IM.findMin rs) > 0 = first P <$> tS (\sϵ (i, tϵ) -> second (mapTySubst (insert n t')) <$> mguPrep f l sϵ (ts!!(i-1)) tϵ) s (IM.toList rs)
                                       | otherwise = throwError $ UF lϵ e t t'
mgu f l s t@P{} t'@Ρ{} = mgu f l s t' t
mgu _ l s (Ρ n rs) (Ρ n' rs') = do
    (_, rss) <- tS (\sϵ (t0,t1) -> mguPrep LF l sϵ t0 t1) s $ IM.elems $ IM.intersectionWith (,) rs rs'
    let t=Ρ n' (rs<>rs') in pure (t, mapTySubst (insert n t) rss)
mgu _ (l, e) _ F t@Arr{} = throwError $ UF l e F t
mgu _ (l, e) _ t@Arr{} F = throwError $ UF l e t F
mgu _ (l, e) _ B t@Arr{} = throwError $ UF l e B t
mgu _ (l, e) _ t@Arr{} B = throwError $ UF l e t B
mgu _ (l, e) _ I t@Arr{} = throwError $ UF l e I t
mgu _ (l, e) _ t@Arr{} I = throwError $ UF l e t I
mgu _ (l, e) _ t@Li{} t'@Arr{} = throwError $ UF l e t t'
mgu _ (l, e) _ t@Arr{} t'@Li{} = throwError $ UF l e t t'
mgu _ (l, e) _ F t@P{} = throwError $ UF l e F t
mgu _ (l, e) _ t@P{} F = throwError $ UF l e t F
mgu _ (l, e) _ I t@P{} = throwError $ UF l e I t
mgu _ (l, e) _ t@P{} I = throwError $ UF l e t I
mgu _ (l, e) _ B t@P{} = throwError $ UF l e B t
mgu _ (l, e) _ t@P{} B = throwError $ UF l e t B
mgu _ (l, e) _ t@P{} t'@Arr{} = throwError $ UF l e t t'
mgu _ (l, e) _ t@Arr{} t'@P{} = throwError $ UF l e t t'
mgu _ (l, e) _ I B= throwError $ UF l e I B
mgu _ (l, e) _ B I = throwError $ UF l e B I
mgu _ (l, e) _ t@Li{} B = throwError $ UF l e t B
mgu _ (l, e) _ B t@Li{} = throwError $ UF l e B t

zSt _ s [] _           = pure ([], s)
zSt _ s _ []           = pure ([], s)
zSt op s (x:xs) (y:ys) = do{(t, next) <- op s x y; first (t:) <$> zSt op next xs ys}

zS _ s [] _           = pure s
zS _ s _ []           = pure s
zS op s (x:xs) (y:ys) = do {next <- op s x y; zS op next xs ys}

tS :: Monad m => (Subst a -> b -> m (x, Subst a)) -> Subst a -> [b] -> m ([x], Subst a)
tS _ s []     = pure ([], s)
tS f s (t:ts) = do{(tϵ, next) <- f s t; first (tϵ:) <$> tS f next ts}

vx = (`Cons` Nil)
vV i = Arr (vx i)

tyNumBinOp :: a -> TyM a (T (), Subst a)
tyNumBinOp l = do
    n <- fc "a" l IsNum
    pure (n ~> n ~> n, mempty)

mm :: a -> TyM a (T (), Subst a)
mm l = do
    n <- fc "o" l IsOrd
    pure (n ~> n ~> n, mempty)

tyBoo :: a -> TyM a (T (), Subst a)
tyBoo l = do
    n <- fc "b" l HasBits
    pure (n ~> n ~> n, mempty)

tyOrdBinRel :: a -> TyM a (T (), Subst a)
tyOrdBinRel l = do
    n <- fc "o" l IsOrd
    pure (n ~> n ~> B, mempty)

tyEqBinRel :: a -> TyM a (T (), Subst a)
tyEqBinRel l = do
    n <- fc "e" l IsEq
    pure (n ~> n ~> B, mempty)

sel :: [Int] -> Sh a -> Sh a
sel axes sh = roll Nil (fmap snd (filter ((`elem` axes) . fst) (zip [1..] unrolled))) where
    (unrolled, _) = unroll sh

tydrop :: Int -> Sh a -> Sh a
tydrop 0 sh            = sh
tydrop _ (_ `Cons` sh) = sh

del :: [Int] -> Sh a -> Sh a
del axes sh = roll t (fmap snd (filter ((`notElem` axes) . fst) (zip [1..] unrolled))) where
    (unrolled, t) = unroll sh

trim :: Sh a -> Sh a
trim = roll Nil . fst . unroll

iunroll (Cons i Nil) = Just i
iunroll (Cons i shϵ) = StaMul (ia i) i <$> iunroll shϵ
iunroll _            = Nothing

unroll (Cons i shϵ) = first (i :) $ unroll shϵ
unroll s            = ([], s)

roll :: Sh a -> [I a] -> Sh a
roll = foldr Cons

tyB :: a -> Builtin -> TyM a (T (), Subst a)
tyB _ Floor = pure (F ~> I, mempty); tyB _ ItoF = pure (I ~> F, mempty)
tyB _ Even = pure (I ~> B, mempty); tyB _ Odd = pure (I ~> B, mempty)
tyB _ Sr = pure (I ~> I ~> I, mempty); tyB _ Sl = pure (I ~> I ~> I, mempty)
tyB l R = do
    n <- fc "a" l IsNum; sh <- fsh "sh"
    pure (n ~> n ~> Arr sh n, mempty)
tyB _ Iter = do{a <- ftv "a"; let s = Arrow a a in pure (s ~> I ~> s, mempty)}
tyB _ ConsE = do
    a <- ftv "a"; i <- fti "i"
    pure (a ~> vV i a ~> vV (StaPlus () i (Ix()1)) a, mempty)
tyB l Snoc = tyB l ConsE
tyB _ A1 = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (Arr (i `Cons` sh) a ~> I ~> Arr sh a, mempty)
tyB _ IOf = do
    a <- ftv "a"; i <- fti "i"
    pure ((a ~> B) ~> vV i a ~> I, mempty)
tyB _ Di = do
    a <- ftv "a"; i <- fti "i"
    pure (Arr (i `Cons` i `Cons` Nil) a ~> vV i a, mempty)
tyB _ LastM = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (Arr (i `Cons` sh) a ~> Arr sh a, mempty)
tyB _ Last = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (Arr (StaPlus () i (Ix()1) `Cons` sh) a ~> Arr sh a, mempty)
tyB _ Head = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (Arr (StaPlus () i (Ix()1) `Cons` sh) a ~> Arr sh a, mempty)
tyB _ Init = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (Arr (StaPlus () i (Ix()1) `Cons` sh) a ~> Arr (i `Cons` sh) a, mempty)
tyB _ InitM = do
    a <- ftv "a"; i <- fti "i"; n <- ftie; sh <- fsh "sh"
    pure (Arr (i `Cons` sh) a ~> Arr (n `Cons` sh) a, mempty)
tyB _ Tail = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (Arr (StaPlus () i (Ix()1) `Cons` sh) a ~> Arr (i `Cons` sh) a, mempty)
tyB _ TailM = do
    a <- ftv "a"; i <- fti "i"; n <- ftie; sh <- fsh "sh"
    pure (Arr (i `Cons` sh) a ~> Arr (n `Cons` sh) a, mempty)
tyB _ Rot = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (I ~> Arr (i `Cons` sh) a ~> Arr (i `Cons` sh) a, mempty)
tyB _ Cyc = do
    sh <- fsh "sh"; a <- ftv "a"; i <- fti "i"; n <- ftie
    pure (Arr (i `Cons` sh) a ~> I ~> Arr (n `Cons` sh) a, mempty)
tyB _ HeadM = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (Arr (i `Cons` sh) a ~> Arr sh a, mempty)
tyB _ Re = do
    a <- ftv "a"; n <- ftie
    pure (I ~> a ~> Arr (n `Cons` Nil) a, mempty)
tyB _ FRange = do {n <- fti "n"; pure (F ~> F ~> Li n ~> Arr (n `Cons` Nil) F, mempty)}
tyB _ Fib = do
    n <- ftie; a <- ftv "a"
    let arrTy = Arr (n `Cons` Nil) a
    pure (a ~> a ~> (a ~> a ~> a) ~> I ~> arrTy, mempty)
tyB _ IRange = do {n <- ftie; pure (I ~> I ~> I ~> Arr (n `Cons` Nil) I, mempty)}
tyB l Plus = tyNumBinOp l; tyB l Minus = tyNumBinOp l
tyB l Times = tyNumBinOp l
tyB l Gte = tyOrdBinRel l; tyB l Gt = tyOrdBinRel l; tyB l Lt = tyOrdBinRel l
tyB l Lte = tyOrdBinRel l; tyB l Eq = tyEqBinRel l; tyB l Neq = tyEqBinRel l
tyB l And = tyBoo l; tyB l Or = tyBoo l; tyB l Xor = tyBoo l
tyB l N = do
    n <- fc "b" l HasBits
    pure (n ~> n, mempty)
tyB _ Exp = pure (F ~> F ~> F, mempty)
tyB l Min = mm l; tyB l Max = mm l
tyB l IntExp = do
    n <- fc "a" l IsNum
    pure (n ~> I ~> n, mempty)
tyB l Neg = do
    n <- fc "a" l IsNum
    pure (n ~> n, mempty)
tyB l Abs = do
    n <- fc "a" l IsNum
    pure (n ~> n, mempty)
tyB _ Sqrt = pure (F ~> F, mempty)
tyB _ Log = pure (F ~> F, mempty)
tyB _ Div = pure (F ~> F ~> F, mempty)
tyB _ Mod = pure (I ~> I ~> I, mempty)
tyB _ IDiv = pure (I ~> I ~> I, mempty)
tyB _ Outer = do
    sh0 <- fsh "sh0"; sh1 <- fsh "sh1"
    a <- ftv "a"; b <- ftv "b"; c <- ftv "c"
    pure ((a ~> b ~> c) ~> Arr sh0 a ~> Arr sh1 b ~> Arr (Cat sh0 sh1) c, mempty)
tyB _ T = do
    sh <- fsh "sh"; a <- ftv "a"
    pure (Arr sh a ~> Arr (Rev sh) a, mempty)
tyB _ Flat = do
    sh <- fsh "sh"; a <- ftv "a"
    pure (Arr sh a ~> Arr (Π sh) a, mempty)
tyB _ AddDim = do
    sh <- fsh "sh"; a <- ftv "a"
    pure (Arr sh a ~> Arr (Ix()1 `Cons` sh) a, mempty)
tyB _ CatE = do
    i <- fti "i"; j <- fti "j"
    n <- ftv "a"
    pure (vV i n ~> vV j n ~> vV (StaPlus () i j) n, mempty)
tyB _ Scan = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    let i1 = StaPlus () i (Ix()1)
        arrTy = Arr (Cons i1 sh) a
    pure ((a ~> a ~> a) ~> arrTy ~> arrTy, mempty)
tyB _ ScanS = do
    a <- ftv "a"; b <- ftv "b"
    i <- fti "i"; sh <- fsh "sh"
    let opTy = b ~> a ~> b
        arrTy = Arr (Cons i sh); rarrTy = Arr (Cons (StaPlus () i (Ix()1)) sh)
        -- FIXME: 1+1?
    pure (opTy ~> b ~> arrTy a ~> rarrTy b, mempty)
tyB l (DI n) = tyB l (Conv [n])
tyB _ (Conv ns) = do
    sh <- fsh "sh"
    is <- zipWithM (\_ t -> fti (T.singleton t)) ns ['i'..]
    a <- ftv "a"; b <- ftv "b"
    let nx = Ix () <$> ns
        opTy = Arr (foldr Cons sh nx) a ~> b
        t = Arrow (Arr (foldr Cons sh (zipWith (StaPlus ()) is nx)) a) (Arr (foldr Cons Nil is) b)
    pure (opTy ~> t, mempty)
tyB _ Succ = do
    i <- fti "i"; sh <- fsh "sh"
    a <- ftv "a"; b <- ftv "b"
    let opTy = a ~> (a ~> b)
    pure (opTy ~> (Arr (StaPlus () i (Ix () 1) `Cons` sh) a ~> Arr (i `Cons` sh) b), mempty)
tyB _ (TAt i) = do
    ρ <- freshN "ρ" ()
    a <- ftv "a"
    pure (Ρ ρ (IM.singleton i a) ~> a, mempty)
tyB _ Map = do
    i <- fti "i"
    a <- ftv "a"; b <- ftv "b"
    let fTy = a ~> b
        gTy = vV i a ~> vV i b
    -- depends on Arr nil a = a, Arr (i+j) a = Arr i (Arr j sh)
    pure (fTy ~> gTy, mempty)
tyB _ Zip = do
    i <- fti "i"
    a <- ftv "a"; b <- ftv "b"; c <- ftv "c"
    let fTy = a ~> b ~> c
        gTy = vV i a ~> vV i b ~> vV i c
    pure (fTy ~> gTy, mempty)
tyB l (Rank as) = do
    let ixN n = zipWithM (\_ c -> fti (T.singleton c)) [1..n] ['i'..]
    shs <- traverse (\(i,ax) -> do {is <- ixN (maybe i maximum ax); sh <- fsh "sh"; pure $ foldr Cons sh is}) as
    vs <- zipWithM (\_ c -> ftv (T.singleton c)) as ['a'..]
    codSh <- fsh "sh"
    cod <- ftv "c"
    let mArrs = zipWith Arr shs vs
        codTy = Arr codSh cod
        fTy = foldr (~>) cod $ zipWith3 (\ax sh t -> case ax of {(_,Nothing) -> Arr (trim sh) t;(_,Just axs) -> Arr (sel axs sh) t}) as shs vs
        rTy = foldr (~>) codTy mArrs
        shsU = zipWith (\ax sh -> case ax of {(n,Nothing) -> tydrop n sh;(_,Just axs) -> del axs sh}) as shs
        shUHere sh sh' = fmap snd (liftU $ mgShPrep LF l mempty (sh$>l) (sh'$>l))
    s <- zipWithM shUHere shsU (tail shsU++[codSh])
    pure (fTy ~> rTy, mconcat s)
tyB _ Fold = do
    i <- fti "i"; sh <- fsh "sh"; a <- ftv "a"
    let sh1 = StaPlus () i (Ix()1) `Cons` sh
    pure ((a ~> a ~> a) ~> Arr sh1 a ~> Arr sh a, mempty)
tyB _ FoldS = do
    i <- fti "i"; sh <- fsh "sh"; a <- ftv "a"
    pure ((a ~> a ~> a) ~> a ~> Arr (i `Cons` sh) a ~> Arr sh a, mempty)
tyB _ Foldl = do
    ix <- fti "i"; sh <- fsh "sh"; a <- ftv "a"
    let sh1 = ix `Cons` sh
    pure ((a ~> a ~> a) ~> a ~> Arr sh1 a ~> Arr sh a, mempty)
tyB _ FoldA = do
    sh <- fsh "sh"
    a <- ftv "a"
    pure ((a ~> a ~> a) ~> a ~> Arr sh a ~> a, mempty)
tyB _ Dim = do
    iV <- fti "i"; shV <- fsh "sh"; a <- ftv "a"
    pure (Arr (iV `Cons` shV) a ~> Li iV, mempty)
tyB _ RevE = do
    iV <- fti "i"; shV <- fsh "sh"; a <- ftv "a"
    let aTy = Arr (iV `Cons` shV) a
    pure (aTy ~> aTy, mempty)
tyB _ Size = do
    shV <- fsh "sh"
    a <- ftv "a"
    pure (Arr shV a ~> I, mempty)
tyB _ Gen = do
    a <- ftv "a"; n <- ftie
    let arrTy = Arr (n `Cons` Nil) a
    pure (a ~> (a ~> a) ~> I ~> arrTy, mempty)
tyB l Mul = do
    a <- fc "a" l IsNum
    i <- fti "i"; j <- fti "j"; k <- fti "k"
    pure (Arr (i `Cons` j `Cons` Nil) a ~> Arr (j `Cons` k `Cons` Nil) a ~> Arr (i `Cons` k `Cons` Nil) a, mempty)
tyB l VMul = do
    a <- fc "a" l IsNum
    i <- fti "i"; j <- fti "j"
    pure (Arr (i `Cons` j `Cons` Nil) a ~> vV j a ~> vV i a, mempty)
tyB l Eye = do
    a <- fc "a" l IsNum; i <- fti "i"
    pure (Arr (i `Cons` i `Cons` Nil) a, mempty)
tyB _ Sin = pure (F ~> F, mempty)
tyB _ Cos = pure (F ~> F, mempty)
tyB _ Tan = pure (F ~> F, mempty)
tyB _ Ices = do
    a <- ftv "a"; i <- fti "i"; n <- ftie
    pure ((a ~> B) ~> vV i a ~> vV n I, mempty)
tyB _ Filt = do
    a <- ftv "a"; i <- fti "i"; n <- ftie
    pure ((a ~> B) ~> vV i a ~> vV n a, mempty)
tyB _ C = do
    a <- ftv "a"; b <- ftv "b"; c <- ftv "c"
    pure ((b ~> c) ~> (a ~> b) ~> a ~> c, mempty)

liftCloneTy :: T b -> TyM a (T b, IM.IntMap Int)
liftCloneTy t = do
    i<- gets maxU
    let (u,t',vs) = cloneT i t
    modify (setMaxU u) $> (t',vs)

cloneWithConstraints :: T b -> TyM a (T b)
cloneWithConstraints t = do
    (t', vs) <- liftCloneTy t
    traverse_ (\(k,v) -> do
        cst <- gets varConstr
        case IM.lookup k cst of
            Just (c,l) -> modify (addVarConstrI v l c)
            Nothing    -> pure ())
        (IM.toList vs)
    pure t'

rwI :: I a -> I a
rwI (StaPlus l i0 i1) =
    case (rwI i0, rwI i1) of
        (Ix lϵ i, Ix _ j) -> Ix lϵ (i+j)
        (i0', i1')        -> StaPlus l i0' i1'
rwI (StaMul l i0 i1) =
    case (rwI i0, rwI i1) of
        (Ix lϵ i, Ix _ j) -> Ix lϵ (i*j)
        (i0', i1')        -> StaMul l i0' i1'
rwI i = i

rwSh :: Sh a -> Sh a
rwSh s@SVar{}     = s
rwSh s@Nil        = s
rwSh (i `Cons` s) = rwI i `Cons` rwSh s
rwSh (Cat s0 s1) | (is, Nil) <- unroll (rwSh s0), (js, Nil) <- unroll (rwSh s1) = roll Nil (is++js)
                 | otherwise = Cat (rwSh s0) (rwSh s1)
rwSh (Rev s) | (is, Nil) <- unroll (rwSh s) = roll Nil (reverse is)
             | otherwise = Rev (rwSh s)
rwSh (Π s) | Just i <- iunroll (rwSh s) = rwI i `Cons` Nil
           | otherwise = Π (rwSh s)

rwArr :: T a -> T a
rwArr (Arrow t t') = Arrow (rwArr t) (rwArr t')
rwArr I            = I
rwArr B            = B
rwArr F            = F
rwArr t@Li{}       = t
rwArr t@TVar{}     = t
rwArr (P ts)       = P (rwArr<$>ts)
rwArr (Arr Nil t)  = rwArr t
rwArr (Arr ixes arr) | (is, Nil) <- unroll (rwSh ixes), Arr sh t <- rwArr arr = Arr (roll sh is) t
rwArr (Arr sh t)   = Arr (rwSh sh) (rwArr t)
rwArr (Ρ n fs)     = Ρ n (rwArr<$>fs)

hasEI :: I a -> Bool
hasEI IEVar{}            = True
hasEI (StaPlus _ ix ix') = hasEI ix || hasEI ix'
hasEI (StaMul _ ix ix')  = hasEI ix || hasEI ix'
hasEI _                  = False

hasESh :: Sh a -> Bool
hasESh (Cons i sh) = hasEI i || hasESh sh
hasESh _           = False

hasE :: T a -> Bool
hasE (Arrow t t'@Arrow{}) = hasE t || hasE t'
hasE (Arr sh t)           = hasESh sh || hasE t
hasE (P ts)               = any hasE ts
hasE _                    = False

chkE :: T () -> Either (TyE a) ()
chkE t@Arrow{} = if hasE t then Left (ExistentialArg t) else Right ()
chkE _         = Right ()

checkTy :: T a -> (C, a) -> Either (TyE a) (Maybe (Nm a, C))
checkTy (TVar n) (c, _)       = pure $ Just(n, c)
checkTy Li{} (IsNum, _)       = pure Nothing
checkTy I (IsNum, _)          = pure Nothing
checkTy F (IsNum, _)          = pure Nothing
checkTy I (IsOrd, _)          = pure Nothing
checkTy I (HasBits, _)        = pure Nothing
checkTy Li{} (HasBits, _)     = pure Nothing
checkTy B (HasBits, _)        = pure Nothing
checkTy F (IsOrd, _)          = pure Nothing
checkTy I (IsEq, _)           = pure Nothing
checkTy F (IsEq, _)           = pure Nothing
checkTy t (c@IsNum, l)        = Left$ Doesn'tSatisfy l t c
checkTy t (c@HasBits, l)      = Left$ Doesn'tSatisfy l t c
checkTy (Arr _ t) c@(IsEq, _) = checkTy t c

substI :: Subst a -> Int -> Maybe (T a)
substI s@(Subst ts is sh) i =
    case IM.lookup i ts of
        Just ty@TVar{} -> Just $ aT (Subst (IM.delete i ts) is sh) ty
        Just ty        -> Just $ aT s ty
        Nothing        -> Nothing

checkClass :: Subst a -> Int -> (C, a) -> Either (TyE a) (Maybe (Nm a, C))
checkClass s i c =
    case substI s i of
        Just ty -> checkTy (rwArr ty) c
        Nothing -> pure Nothing

tyClosed :: Int -> E a -> Either (TyE a) (E (T ()), [(Nm a, C)], Int)
tyClosed u e = do
    ((eS, scs), i) <- runTyM u (do { (e', s) <- tyE mempty e; cvs <- gets varConstr; scs <- liftEither $ catMaybes <$> traverse (uncurry$checkClass s) (IM.toList cvs); pure (rwArr.aT (void s)<$>e', scs) })
    let vs = occ (eAnn eS); scs' = filter (\(Nm _ (U iϵ) _, _) -> iϵ `IS.member` vs) scs
    chkE (eAnn eS) $> (eS, nubOrd scs', i)

tyE :: Subst a -> E a -> TyM a (E (T ()), Subst a)
tyE s (EApp _ (Builtin _ Re) (ILit _ n)) = do
    a <- ftv "a"
    let arrTy = a ~> vV (Ix () (fromInteger n)) a
    pure (EApp arrTy (Builtin (I ~> arrTy) Re) (ILit I n), s)
tyE s (EApp _ (EApp _ (EApp _ (Builtin _ FRange) e0) e1) (ILit _ n)) = do
    (e0',s0) <- tyE s e0; (e1',s1) <- tyE s0 e1
    let tyE0 = eAnn e0'; tyE1 = eAnn e1'
        arrTy = vV (Ix () (fromInteger n)) F
        l0 = eAnn e0; l1 = eAnn e1
    s0' <- liftU $ mp (l0,e0) s1 F (eAnn e0' $> l0); s1' <- liftU $ mp (l1,e1) s0' F (eAnn e1' $> l1)
    pure (EApp arrTy (EApp (I ~> arrTy) (EApp (tyE1 ~> I ~> arrTy) (Builtin (tyE0 ~> tyE1 ~> I ~> arrTy) FRange) e0') e1') (ILit I n), s1')
tyE s (EApp l eϵ@(EApp _ (EApp _ (Builtin _ FRange) e0) e1) n) = do
    (nA, sϵ) <- tyE s n
    case aT (void sϵ) $ eAnn nA of
        iT@(Li ix) -> do
            (e0',s0) <- tyE sϵ e0; (e1',s1) <- tyE s0 e1
            let tyE0 = eAnn e0'; tyE1 = eAnn e1'
                arrTy = vV ix F
                l0 = eAnn e0; l1 = eAnn e1
            s0' <- liftU $ mp (l0,e0) s1 F (eAnn e0' $> l0); s1' <- liftU $ mp (l1,e1) s0' F (eAnn e1' $> l1)
            pure (EApp arrTy (EApp (iT ~> arrTy) (EApp (tyE1 ~> iT ~> arrTy) (Builtin (tyE0 ~> tyE1 ~> iT ~> arrTy) FRange) e0') e1') nA, s1')
        _ -> do
            a <- ft "a" l; b <- ft "b" l
            (eϵ', s0) <- tyE sϵ eϵ
            let eϵTy = a ~> b
            s1 <- liftU $ mp (l,eϵ) s0 (eAnn eϵ'$>l) eϵTy
            s2 <- liftU $ mp (l,n) s1 (eAnn nA$>l) a
            pure (EApp (void b) eϵ' nA, s2)
tyE s (EApp _ (EApp _ (EApp _ (Builtin _ Gen) x) f) (ILit _ n)) = do
    (x',s0) <- tyE s x; (f',s1) <- tyE s0 f
    let tyX = eAnn x'; tyF = eAnn f'
        arrTy = vV (Ix () (fromInteger n)) tyX
        lX = eAnn x; lF = eAnn f
    s1' <- liftU $ mp (lF, f) s1 ((tyX $> lX) ~> (tyX $> lX)) (tyF $> lF)
    pure (EApp arrTy (EApp (I ~> arrTy) (EApp (tyF ~> I ~> arrTy) (Builtin (tyX ~> tyF ~> I ~> arrTy) Gen) x') f') (ILit I n), s1')
tyE s (EApp l e@(EApp _ (EApp _ (Builtin _ Gen) x) f) n) = do
    (nA, sϵ) <- tyE s n
    case aT (void sϵ) $ eAnn nA of
        iT@(Li ix) -> do
            (x',s0) <- tyE sϵ x; (f',s1) <- tyE s0 f
            let tyX = eAnn x'; tyF = eAnn f'
                arrTy = vV ix tyX
                lX = eAnn x; lF = eAnn f
            s1' <- liftU $ mp (lF, f) s1 ((tyX $> lX) ~> (tyX $> lX)) (tyF $> lF)
            pure (EApp arrTy (EApp (iT ~> arrTy) (EApp (tyF ~> iT ~> arrTy) (Builtin (tyX ~> tyF ~> iT ~> arrTy) Gen) x') f') nA, s1')
        _ -> do
            a <- ft "a" l; b <- ft "b" l
            (e', s0) <- tyE sϵ e
            let eT = Arrow a b
            s1 <- liftU $ mp (l,e) s0 (eAnn e'$>l) eT
            s2 <- liftU $ mp (l,n) s1 (eAnn nA$>l) a
            pure (EApp (void b) e' nA, s2)
tyE s eC@(EApp lC (EApp _ (Builtin _ Cyc) e) (ILit _ m)) = do
    (e0, s0) <- tyE s e
    ix <- fti "ix"
    a <- ftv "a"
    let t=Arr (ix `Cons` Nil) a
        arrTy = Arr (StaMul () ix (Ix () (fromIntegral m)) `Cons` Nil) a
        lE=eAnn e
    s1 <- liftU $ mp (lC,eC) s0 (eAnn e0$>lE) (t$>lE)
    pure (EApp arrTy (EApp (I ~> arrTy) (Builtin (t ~> I ~> arrTy) Cyc) e0) (ILit I m), s1)
tyE s (EApp _ (EApp _ (EApp _ (Builtin _ IRange) (ILit _ b)) (ILit _ e)) (ILit _ si)) = do
    let arrTy = vV (Ix () (fromInteger ((e-b+si) `quot` si))) I
    pure (EApp arrTy (EApp (I ~> arrTy) (EApp (I ~> I ~> arrTy) (Builtin (I ~> I ~> I ~> arrTy) IRange) (ILit I b)) (ILit I e)) (ILit I si), s)
tyE s (FLit _ x) = pure (FLit F x, s)
tyE s (BLit _ x) = pure (BLit B x, s)
tyE s (ILit l m) = do
    n <- fc "a" l IsNum
    pure (ILit n m, s)
tyE s (Builtin l b) = do {(t,sϵ) <- tyB l b ; pure (Builtin t b, sϵ<>s)}
tyE s (Lam _ nϵ e) = do
    n <- ftv "a"
    modify (addStaEnv nϵ n)
    (e', s') <- tyE s e
    let lamTy = n ~> eAnn e'
    pure (Lam lamTy (nϵ { loc = n }) e', s')
tyE s (Let _ (n, e') e) = do
    (e'Res, s') <- tyE s e'
    let e'Ty = eAnn e'Res
    modify (addStaEnv n (aT (void s') e'Ty))
    (eRes, s'') <- tyE s' e
    pure (Let (eAnn eRes) (n { loc = e'Ty }, e'Res) eRes, s'')
tyE s (Def _ (n, e') e) = do
    (e'Res, s') <- tyE s e'
    let e'Ty = eAnn e'Res
    modify (addPolyEnv n (aT (void s') e'Ty))
    (eRes, s'') <- tyE s' e
    pure (Def (eAnn eRes) (n { loc = e'Ty }, e'Res) eRes, s'')
tyE s (LLet _ (n, e') e) = do
    (e'Res, s') <- tyE s e'
    let e'Ty = eAnn e'Res
    modify (addStaEnv n (aT (void s') e'Ty))
    (eRes, s'') <- tyE s' e
    pure (LLet (eAnn eRes) (n { loc = e'Ty }, e'Res) eRes, s'')
tyE s e@(ALit l es) = do
    a <- ftv "a"
    (es', s') <- sSt s es
    let eTys = a : fmap eAnn es'
        uHere sϵ t t' = mp (l,e) sϵ (t$>l) (t'$>l)
    ss' <- liftU $ zS uHere s' eTys (tail eTys)
    pure (ALit (vV (Ix () $ length es) a) es', ss')
tyE s (EApp l e0 e1) = do
    a <- ft "a" l; b <- ft "b" l
    (e0', s0) <- tyE s e0
    (e1', s1) <- tyE s0 e1
    let e0Ty = a ~> b
    s2 <- liftU $ mp (l,e0) s1 (eAnn e0'$>l) e0Ty
    s3 <- liftU $ mp (l,e1) s2 (eAnn e1'$>l) a
    pure (EApp (void b) e0' e1', s3)
tyE s (Cond l p e0 e1) = do
    (p',sP) <- tyE s p
    (e0',s0) <- tyE sP e0
    (e1',s1) <- tyE s0 e1
    sP' <- liftU $ mp (eAnn p,p) s1 B (eAnn p'$>eAnn p); (tB, s0') <- liftU $ mguPrep RF (l,e0) sP' (eAnn e0'$>l) (eAnn e1'$>eAnn e1)
    pure (Cond (void tB) p' e0' e1', s0')
tyE s (Var l n@(Nm _ (U u) _)) = do
    lSt<- gets staEnv
    case IM.lookup u lSt of
        Just t  -> pure (Var t (n $> t), s)
        Nothing -> do
            vSt<- gets polyEnv
            case IM.lookup u vSt of
                Just t  -> do {t'<- cloneWithConstraints t; pure (Var t' (n$>t'), s)}
                Nothing -> throwError $ IllScoped l n
tyE s (Tup _ es) = do
    (es', s') <- sSt s es
    let eTys = eAnn<$>es'
    pure (Tup (P eTys) es', s')
tyE s (Ann l e t) = do
    (e', s') <- tyE s e
    s'' <- liftEither $ maM LF (aT s'$fmap ($>l) eAnn e') (aT s' (t$>l))
    pure (e', s'<>s'')

sSt :: Subst a -> [E a] -> TyM a ([E (T ())], Subst a)
sSt s []     = pure([], s)
sSt s (e:es) = do{(e',s') <- tyE s e; first (e':) <$> sSt s' es} -- TODO: recurse other way idk
