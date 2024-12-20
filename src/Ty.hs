{-# LANGUAGE DeriveGeneric #-}

module Ty ( TyE
          , tyClosed
          , match
          -- * Substitutions
          , aT, rwArr
          ) where

import           A
import           Control.DeepSeq                  (NFData (rnf))
import           Control.Exception                (Exception, throw)
import           Control.Monad                    (when, zipWithM)
import           Control.Monad.Except             (liftEither, throwError)
import           Control.Monad.Trans.State.Strict (StateT (runStateT), gets, modify, state)
import           Data.Bifunctor                   (first, second)
import           Data.Containers.ListUtils        (nubOrd)
import           Data.Foldable                    (traverse_)
import           Data.Function                    (on)
import           Data.Functor                     (void, ($>))
import qualified Data.IntMap                      as IM
import qualified Data.IntSet                      as IS
import           Data.Maybe                       (catMaybes, fromMaybe)
import qualified Data.Text                        as T
import           Data.Typeable                    (Typeable)
import           GHC.Generics                     (Generic)
import           Nm
import           Nm.IntMap
import qualified Nm.IntSet                        as Nm
import           Prettyprinter                    (Doc, Pretty (..), hardline, indent, squotes, (<+>))
import           Prettyprinter.Ext
import           Q
import           Sh
import           Ty.Clone
import           U

infixl 7 \-
infixl 6 @@

data TySt a = TySt { maxU :: !Int, staEnv, polyEnv :: IM.IntMap (T ()), varConstr :: IM.IntMap (C, a) }

data Subst a = Subst { tySubst :: IM.IntMap (T a)
                     , iSubst  :: IM.IntMap (I a) -- ^ Index variables
                     , sSubst  :: IM.IntMap (Sh a) -- ^ Shape variables
                     } deriving (Functor)

data TyE a = IllScoped a !(Nm a)
           | UF a (E a) !(T a) !(T a)
           | UI a !(I a) !(I a)
           | USh a !(Sh a) !(Sh a)
           | UShD a !(Sh a) !(Sh a)
           | OT a !(T a) !(T a)
           | OSh a !(Sh a) !(Sh a)
           | OI a !(I a) !(I a)
           | ExistentialArg (T ())
           | MatchFailed !(T ()) !(T ())
           | MatchShFailed !(Sh a) !(Sh a)
           | MatchIFailed !Focus !(I a) !(I a)
           | Doesn'tSatisfy a (T a) !C
           | NegIx a Int
           deriving (Generic)

instance Semigroup (Subst a) where
    (<>) (Subst t i s) (Subst t0 i0 s0) = Subst (t<>t0) (i<>i0) (s<>s0)

instance Monoid (Subst a) where
    mempty = Subst IM.empty IM.empty IM.empty

instance NFData a => NFData (TyE a) where

located l p = pretty l <> ":" <+> p

instance Pretty a => Pretty (TyE a) where
    pretty (IllScoped l n)         = located l$ squotes (pretty n) <+> "is not in scope."
    pretty (UF l e ty ty')         = located l$ "could not unify" <+> squotes (pretty ty) <+> "with" <+> squotes (pretty ty') <+> "in expression" <+> squotes (pretty e)
    pretty (USh l sh sh')          = located l$ "could not unify shape" <+> squotes (pretty sh) <+> "with" <+> squotes (pretty sh')
    pretty (UShD l sh sh')         = located l$ "unification gave up on" <+> squotes (pretty sh) <+> squotes (pretty sh')
    pretty (UI l ix ix')           = located l$ "could not unify index" <+> squotes (pretty ix) <+> "with" <+> squotes (pretty ix')
    pretty (OT l ty ty')           = located l$ "occurs check failed when unifying" <+> squotes (pretty ty) <+> "and" <+> squotes (pretty ty')
    pretty (OI l i j)              = located l$ "occurs check failed when unifying indices" <+> squotes (pretty i) <+> "and" <+> squotes (pretty j)
    pretty (OSh l s0 s1)           = located l$ "occurs check failed when unifying shapes" <+> squotes (pretty s0) <+> "and" <+> squotes (pretty s1)
    pretty (ExistentialArg ty)     = "Existential occurs as an argument in" <+> squotes (pretty ty)
    pretty (MatchFailed t t')      = "Failed to match" <+> squotes (pretty t) <+> "against type" <+> squotes (pretty t')
    pretty (MatchShFailed sh sh')  = "Failed to match" <+> squotes (pretty sh) <+> "against shape" <+> squotes (pretty sh')
    pretty (MatchIFailed f i i')   = pretty f <+> "Failed to match" <+> squotes (pretty i) <+> "against index" <+> squotes (pretty i')
    pretty (Doesn'tSatisfy l ty c) = located l$ squotes (pretty ty) <+> "is not a member of class" <+> pretty c
    pretty (NegIx l i)             = located l$ "negative index" <+> pretty i

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

nI :: a -> UM b (Nm a)
nI l = state (\i -> let j=i+1 in (Nm "m" (U j) l, j))

nIe :: a -> UM b (I a)
nIe l = IEVar l <$> nI l

liftU :: UM a x -> TyM a x
liftU a = do
    i <- gets maxU
    (b, j) <- liftEither$runStateT a i
    setMaxU j $> b

wI iS (Subst t i sh) = Subst t (iS<>i) sh

mI :: Focus -> I a -> I a -> Either (TyE a) (Subst a)
mI f i0@(Ix _ i) i1@(Ix _ j) | i == j = Right mempty
                             | otherwise = Left $ MatchIFailed f i0 i1
mI _ (IVar _ (Nm _ (U i) _)) ix = Right $ Subst IM.empty (IM.singleton i ix) IM.empty
mI _ ix (IVar _ (Nm _ (U i) _)) = Right $ Subst IM.empty (IM.singleton i ix) IM.empty
mI _ (IEVar _ n) (IEVar _ n') | n == n' = Right mempty
mI RF IEVar{} IEVar{} = Right mempty
-- TODO: Ix should match against ∃ (on the right only), also propagate!
mI LF i0@IEVar{} i1@IEVar{} = Left $ MatchIFailed LF i0 i1
mI LF i0@IEVar{} i1@Ix{} = Left $ MatchIFailed LF i0 i1
mI LF i0@Ix{} i1@IEVar{} = Left $ MatchIFailed LF i0 i1
mI f (StaPlus _ i (Ix _ iϵ)) (Ix l j) | j >= iϵ = mI f i (Ix l (j-iϵ))
mI f (Ix l iϵ) (StaPlus _ i (Ix _ j)) | iϵ >= j = mI f i (Ix l (iϵ-j))
mI f (StaPlus _ (Ix _ iϵ) i) (Ix l j) | j >= iϵ = mI f i (Ix l (j-iϵ))
mI f (Ix l iϵ) (StaPlus _ (Ix _ j) i) | iϵ >= j = mI f i (Ix l (iϵ-j))
mI f (StaPlus _ i j) (StaPlus _ i' j') = (<>) <$> mI f i i' <*> mI f j j' -- FIXME: stringent, should enter confessional error context
mI f (StaMul _ i j) (StaMul _ i' j') = (<>) <$> mI f i i' <*> mI f j j' -- FIXME: stringent

mSh :: Focus -> Sh a -> Sh a -> Either (TyE a) (Subst a)
mSh _ (SVar (Nm _ (U i) _)) sh      = Right $ Subst IM.empty IM.empty (IM.singleton i sh)
mSh _ Nil Nil                       = Right mempty
mSh f (Cons i sh) (Cons i' sh')     = (<>) <$> mI f i i' <*> mSh f sh sh'
mSh f (Cat sh0 sh1) (Cat sh0' sh1') = (<>) <$> mSh f sh0 sh0' <*> mSh f sh1 sh1'
mSh f (Rev sh) (Rev sh')            = mSh f sh sh'
mSh f (Π sh) Nil                    = mSh f sh Nil
mSh _ Nil (Π Nil)                   = Right mempty
mSh f (Rev sh) Nil                  = mSh f sh Nil
mSh _ Nil (Rev Nil)                 = Right mempty
mSh f (Cat sh0 sh1) Nil             = (<>) <$> mSh f sh0 Nil <*> mSh f sh1 Nil
mSh _ Nil (Cat Nil Nil)             = Right mempty
mSh _ sh sh'                        = Left $ MatchShFailed sh sh'

match :: (Typeable a, Pretty a) => T a -> T a -> Subst a
match t t' = either throw id (maM LF t t')

maM :: Focus -> T a -> T a -> Either (TyE a) (Subst a)
maM f (Li n) (Li m)                 = mI f m n
maM _ (IZ _ (Nm _ (U u) _)) I       = Right $ Subst (IM.singleton u I) IM.empty IM.empty
maM _ (IZ _ (Nm _ (U u) _)) F       = Right $ Subst (IM.singleton u F) IM.empty IM.empty
maM _ I I                           = Right mempty
maM _ F F                           = Right mempty
maM _ B B                           = Right mempty
maM _ (TVar n) (TVar n') | n == n'  = Right mempty
maM _ (TVar (Nm _ (U i) _)) t       = Right $ Subst (IM.singleton i t) IM.empty IM.empty
maM _ (Arrow t0 t1) (Arrow t0' t1') = (<>) <$> maM LF t0 t0' <*> maM RF t1 t1' -- TODO: use <\> over <>
maM f (Arr sh t) (Arr sh' t')       = (<>) <$> mSh f sh sh' <*> maM f t t'
maM f (Arr sh t) t'                 = (<>) <$> mSh f sh Nil <*> maM f t t'
maM f (P ts) (P ts')                = mconcat <$> zipWithM (maM f) ts ts'
maM _ (Ρ n _) (Ρ n' _) | n == n'    = Right mempty
maM f (Ρ n rs) t@(Ρ _ rs') | IM.keysSet rs' `IS.isSubsetOf` IM.keysSet rs = iTS n t . mconcat <$> traverse (uncurry (maM f)) (IM.elems (IM.intersectionWith (,) rs rs'))
maM f (Ρ n rs) t@(P ts) | length ts >= fst (IM.findMax rs) = iTS n t . mconcat <$> traverse (uncurry (maM f)) [ (ts!!(i-1),tϵ) | (i,tϵ) <- IM.toList rs ]
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

(\-) :: Subst a -> Int -> Subst a
(\-) (Subst ts is ss) u = Subst (IM.delete u ts) is ss

s@@t= aT (void s) t

aT :: Subst a -> T a -> T a
aT s (Arr sh ty) = Arr (shSubst s sh) (aT s ty)
aT s (Arrow t₁ t₂) = Arrow (aT s t₁) (aT s t₂)
aT s@(Subst ts _ _) ty'@(TVar n) =
    let u = unU $ unique n in
    case IM.lookup u ts of
        Just ty@TVar{} -> aT (s\-u) ty
        Just ty@IZ{}   -> aT (s\-u) ty
        Just ty@Ρ{}    -> aT (s\-u) ty
        Just ty        -> aT s ty
        Nothing        -> ty'
aT s (P ts) = P (aT s <$> ts)
aT s@(Subst ts _ _) (Ρ n rs) =
    let u = unU (unique n) in
    case IM.lookup u ts of
        Just ty@Ρ{}    -> aT (s\-u) ty
        Just ty@TVar{} -> aT (s\-u) ty
        Just ty@IZ{}   -> aT (s\-u) ty
        Just ty        -> aT s ty
        Nothing        -> Ρ n (aT s<$>rs)
aT s@(Subst ts _ _) ty'@(IZ _ n) =
    let u = unU $ unique n in
    case IM.lookup u ts of
        Just ty@TVar{} -> aT (s\-u) ty
        Just ty@IZ{}   -> aT (s\-u) ty
        Just ty@Ρ{}    -> aT (s\-u) ty
        Just ty        -> aT s ty
        Nothing        -> ty'
aT _ ty = ty

runTyM :: Int -> TyM a b -> Either (TyE a) (b, Int)
runTyM i = fmap (second maxU) . flip runStateT (TySt i IM.empty IM.empty IM.empty)

mapMaxU :: (Int -> Int) -> TySt a -> TySt a
mapMaxU f (TySt u l v vcs) = TySt (f u) l v vcs

setMaxU :: Int -> TyM a ()
setMaxU i = modify (\(TySt _ l v vcs) -> TySt i l v vcs)

addStaEnv :: Nm a -> T () -> TyM a ()
addStaEnv n t = modify (\(TySt u l v vcs) -> TySt u (insert n t l) v vcs)

addPolyEnv :: Nm a -> T () -> TySt a -> TySt a
addPolyEnv n t (TySt u l v vcs) = TySt u l (insert n t v) vcs

addVarConstrI :: Int -> a -> C -> TySt a -> TySt a
addVarConstrI i ann c (TySt u l v vcs) = TySt u l v (IM.insert i (c, ann) vcs)

addVarConstr :: Nm a -> a -> C -> TySt a -> TySt a
addVarConstr tn = addVarConstrI (unU$unique tn)

pushVarConstraint :: Nm a -> a -> C -> TyM a ()
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

fn :: a -> Integer -> TyM a (T ())
fn l n = do
    nϵ <- freshN "n" l
    pushVarConstraint nϵ l IsNum $> IZ (Ix()$fromInteger n) (void nϵ)

ftv :: T.Text -> TyM a (T ())
ftv n = ft n ()

fti :: T.Text -> TyM a (I ())
fti n = IVar () <$> freshN n ()

ftie :: TyM a (I ())
ftie = IEVar () <$> freshN "n" ()

mapTySubst f (Subst t i sh) = Subst (f t) i sh
mapShSubst f (Subst t i sh) = Subst t i (f sh)

iTS n t = mapTySubst (insert n t)
uTS u n = mapTySubst (IM.insert u n)
iSh u sh = mapShSubst (IM.insert u sh)

data Focus = LF | RF

instance NFData Focus where rnf LF=(); rnf RF=()

instance Pretty Focus where pretty LF="⦠"; pretty RF="∢"

mguIPrep :: Focus -> IM.IntMap (I a) -> I a -> I a -> UM a (I a, IM.IntMap (I a))
mguIPrep f is = mguI f is `on` rwI.(is!>)

mguI :: Focus -> IM.IntMap (I a) -> I a -> I a -> UM a (I a, IM.IntMap (I a))
mguI _ inp i0@(Ix _ i) (Ix _ j) | i == j = pure (i0, inp)
mguI RF inp (Ix l _) Ix{} = do {m <- nIe l; pure (m, inp)}
mguI _ _ i0@(Ix l _) i1@Ix{} = throwError $ UI l i0 i1
mguI _ inp i0@(IEVar _ i) (IEVar _ j) | i == j = pure (i0, inp)
mguI RF inp (IEVar l _) (IEVar _ _) = do {m <- nIe l; pure (m, inp)}
mguI RF inp i@IEVar{} j@Ix{} = pure (i, inp)
mguI RF inp i@Ix{} j@IEVar{} = mguI RF inp j i
mguI _ _ i0@(IEVar l _) i1@IEVar{} = throwError $ UI l i0 i1
mguI _ inp i0@(IVar _ i) (IVar _ j) | i == j = pure (i0, inp)
mguI _ inp iix@(IVar l (Nm _ (U i) _)) ix | i `IS.member` occI ix = throwError $ OI l iix ix
                                          | otherwise = pure (ix, IM.insert i ix inp)
mguI _ inp ix iix@(IVar l (Nm _ (U i) _)) | i `IS.member` occI ix = throwError $ OI l ix iix
                                          | otherwise = pure (ix, IM.insert i ix inp)
mguI f inp (StaPlus _ i0 (Ix _ k0)) (StaPlus _ i1 (Ix _ k1)) | k0 == k1 = mguI f inp i0 i1
mguI f inp (StaMul _ i0 (Ix _ k0)) (StaMul _ i1 (Ix _ k1)) | k0 == k1 = mguI f inp i0 i1
mguI f inp i0@(StaPlus l i (Ix _ k)) i1@(Ix lk j) | j >= k = mguI f inp i (Ix lk (j-k))
                                                  | otherwise = throwError $ UI l i0 i1
mguI f inp i0@Ix{} i1@(StaPlus _ _ Ix{}) = mguI f inp i1 i0
mguI f inp (StaPlus l i0 i1) (StaPlus _ j0 j1) = do
    -- FIXME: too stringent
    (k, s) <- mguI f inp i0 j0
    (m, s') <- mguIPrep f s i1 j1
    pure (StaPlus l k m, s')
mguI f inp (StaMul l i0 i1) (StaMul _ j0 j1) = do
    -- FIXME: too stringent
    (k, s) <- mguI f inp i0 j0
    (m, s') <- mguIPrep f s i1 j1
    pure (StaMul l k m, s')
mguI LF _ i0@(IEVar l _) i1@Ix{} = throwError $ UI l i0 i1
mguI LF _ i0@(Ix l _) i1@IEVar{} = throwError $ UI l i0 i1
mguI RF inp (IEVar l (Nm _ (U i) _)) j@StaPlus{} | i `IS.notMember` occI j = do {m <- nIe l; pure (m, inp)}
mguI RF inp i@StaPlus{} j@IEVar{} = mguI RF inp j i
mguI _ _ i0@(IEVar l _) i1@StaPlus{} = throwError $ UI l i0 i1
mguI _ _ i0@(StaPlus l _ _) i1@IEVar{} = throwError $ UI l i0 i1
mguI f inp (StaMul l n mi@(Ix l₀ m)) (StaPlus _ i (Ix l₁ j)) = do
    k <- IVar l <$> nI l
    (_,s0) <- mguI f inp n (k+:Ix l₀ (c`div`m))
    (_,s1) <- mguIPrep f s0 i (StaMul l₀ mi k+:Ix l₁ (c-j))
    pure (StaMul l mi k+:Ix l₀ c, s1)
  where
    c=lcm m j
-- n*m, i+j, (m,j known) then must be divisible by m and >=j
-- unify to m*k+lcm(m,j)
-- Then n=k+(lcm(m,j)/m), i=m*k+(lcm(m,j)-j)
mguI f inp n@(StaMul _ _ Ix{}) (StaPlus l1 i@Ix{} j) = mguI f inp n (StaPlus l1 j i)
mguI f inp (StaMul l0 n@Ix{} m) i@(StaPlus _ _ Ix{}) = mguI f inp (StaMul l0 m n) i
mguI f inp (StaMul l0 n@Ix{} m) (StaPlus l1 i@Ix{} j) = mguI f inp (StaMul l0 m n) (StaPlus l1 j i)
mguI f inp (StaMul l (Ix _ m) j) (Ix _ n) | (k,0) <- n `quotRem` m = mguI f inp j (Ix l k)
mguI f inp (StaMul l i (Ix _ m)) (Ix _ n) | (k,0) <- n `quotRem` m = mguI f inp i (Ix l k)
mguI f inp i0@Ix{} i1@StaMul{} = mguI f inp i1 i0
mguI _ _ i0 i1 = error (show (i0,i1))

splitFromLeft :: Int -> [a] -> ([a], [a])
splitFromLeft n xs | nl <- length xs = splitAt (nl-n) xs

mgShPrep :: Focus -> a -> Subst a -> Sh a -> Sh a -> UM a (Sh a, Subst a)
mgShPrep f l s = mgSh f l s `on` shSubst s.rwSh

mgSh :: Focus -> a -> Subst a -> Sh a -> Sh a -> UM a (Sh a, Subst a)
mgSh _ _ inp Nil Nil = pure (Nil, inp)
mgSh f l inp (Cons i sh) (Cons i' sh') = do
    (i'', sI) <- mguI f (iSubst inp) i i'
    (sh'', s2) <- mgShPrep f l (inp { iSubst = sI }) sh sh'
    pure (Cons i'' sh'', s2)
mgSh _ _ inp s@(SVar sh) (SVar sh') | sh == sh' = pure (s, inp)
mgSh _ l inp s@(SVar (Nm _ (U i) _)) sh | i `IS.member` occSh sh = throwError $ OSh l s sh
                                        | otherwise = pure (sh, iSh i sh inp)
mgSh _ l inp sh s@(SVar (Nm _ (U i) _)) | i `IS.member` occSh sh = throwError $ OSh l sh s
                                        | otherwise = pure (sh, iSh i sh inp)
mgSh _ l _ sh@Nil sh'@Cons{} = throwError $ USh l sh sh'
mgSh _ l _ sh@Cons{} sh'@Nil{} = throwError $ USh l sh' sh
mgSh f l inp (Rev sh) (Rev sh') = mgSh f l inp sh sh'
mgSh f l inp (Cat sh0 sh0') (Cat sh1 sh1') = do
    (sh', s) <- mgSh f l inp sh0 sh1
    (sh'', s') <- mgShPrep f l s sh0' sh1'
    pure (Cat sh' sh'', s')
mgSh f l inp (Rev sh) sh' | (is, Nil) <- unroll sh' =
    mgSh f l inp sh (roll Nil$reverse is)
mgSh f l inp sh (Rev sh') | (is, Nil) <- unroll sh' =
    mgSh f l inp (roll Nil$reverse is) sh
mgSh f l inp (Rev sh) Nil = mgSh f l inp sh Nil
mgSh f l inp Nil (Rev sh) = mgSh f l inp Nil sh
mgSh f l inp (Π sh) Nil = mgSh f l inp sh Nil
mgSh f l inp Nil (Π sh) = mgSh f l inp Nil sh
mgSh f l inp (Cat sh0 sh1) Nil = do
    (_, s) <- mgSh f l inp sh0 Nil
    (_, s') <- mgShPrep f l s sh1 Nil
    pure (Nil, s')
mgSh f l inp Nil (Cat sh0 sh1) = do
    (_, s) <- mgSh f l inp Nil sh0
    (_, s') <- mgShPrep f l s Nil sh1
    pure (Nil, s')
mgSh f l inp sh0@Rev{} sh1@Π{} = do
    i <- nIe l
    let sh=vx i
    (_, s') <- mgSh f l inp sh sh0
    (_, s'') <- mgShPrep f l s' sh sh1
    pure (sh, s'')
mgSh f l inp sh0@Π{} sh1@Rev{} = do
    i <- nIe l
    let sh=vx i
    (_, s') <- mgSh f l inp sh sh0
    (_, s'') <- mgShPrep f l s' sh sh1
    pure (sh, s'')
mgSh f l s sh0@Cons{} sh1@(Cat shh shϵ) | (is, Nil) <- unroll sh0, (isϵ, Nil) <- unroll shϵ, n <- length is, nϵ <- length isϵ =
    if n<nϵ
      then throwError $ USh l sh0 sh1
      else let (ish, isϵ') = splitFromLeft (n-nϵ) is
           in do
              (_, s0) <- mgSh f l s shh (roll Nil ish)
              (_, s1) <- mgShPrep f l s0 (roll Nil isϵ') shϵ
              pure (sh0, s1)
mgSh f l s sh0@Cat{} sh1@Cons{} = mgSh f l s sh1 sh0
mgSh _ l _ sh0@Cons{} sh1 = throwError $ UShD l sh0 sh1
mgSh _ l _ sh0 sh1@Cons{} = throwError $ UShD l sh0 sh1
mgSh _ l _ sh0@Π{} sh1@Cat{} = throwError $ UShD l sh0 sh1
mgSh _ l _ sh0@Cat{} sh1@Π{} = throwError $ UShD l sh0 sh1
mgSh _ l _ sh0@Rev{} sh1@Cat{} = throwError $ UShD l sh0 sh1
mgSh _ l _ sh0@Cat{} sh1@Rev{} = throwError $ UShD l sh0 sh1
-- TODO: enter confessional context (error messages)
mgSh f l inp (Π t0) (Π t1) = undefined

mguPrep :: Focus -> (a, E a) -> Subst a -> T a -> T a -> UM a (T a, Subst a)
mguPrep f l s t0 t1 =
    let t0' = aT s t0
        t1' = aT s t1
    in mgu f l s ({-# SCC "rwArr" #-} rwArr t0') ({-# SCC "rwArr" #-} rwArr t1')

mp :: (a, E a) -> Focus -> Subst a -> T a -> T a -> UM a (Subst a)
mp l f s t0 t1 = snd <$> mguPrep f l s t0 t1

occSh :: Sh a -> IS.IntSet
occSh (SVar sv)     = Nm.singleton sv
occSh (Cat sh0 sh1) = occSh sh0 <> occSh sh1
occSh (_ `Cons` sh) = occSh sh
occSh Nil{}         = IS.empty
occSh (Rev sh)      = occSh sh
occSh (Π sh)        = occSh sh

occI :: I a -> IS.IntSet
occI Ix{}            = IS.empty
occI (IVar _ n)      = Nm.singleton n
occI (StaPlus _ i j) = occI i <> occI j
occI (StaMul _ i j)  = occI i <> occI j
occI IEVar{}         = IS.empty

occ :: T a -> IS.IntSet
occ (TVar n)     = Nm.singleton n
occ (IZ _ n)     = Nm.singleton n
occ (Arrow t t') = occ t <> occ t'
occ (Arr _ a)    = occ a -- shouldn't need shape?
occ I            = IS.empty
occ F            = IS.empty
occ B            = IS.empty
occ Li{}         = IS.empty
occ (P ts)       = occ @<> ts
occ (Ρ n rs)     = Nm.insert n $ occ @<> rs

scalar sv = mapShSubst (insert sv Nil)

σ (Li IEVar{}) = I; σ (IZ IEVar{} t) = TVar t; σ t = t

mgu :: Focus -> (a, E a) -> Subst a -> T a -> T a -> UM a (T a, Subst a)
mgu f l s (Arrow t0 t1) (Arrow t0' t1') = do
    (t0'', s0) <- mgu LF l s t0 t0'
    (t1'', s1) <- mguPrep f l s0 t1 t1'
    pure (Arrow t0'' t1'', s1)
mgu _ _ s I I = pure (I, s)
mgu _ _ s F F = pure (F, s)
mgu _ _ s B B = pure (B, s)
mgu _ _ s Li{} I = pure (I, s)
mgu _ _ s I Li{} = pure (I, s)
mgu _ _ s (IZ _ (Nm _ (U j) _)) I = pure (I, uTS j I s)
mgu _ _ s I (IZ _ (Nm _ (U j) _)) = pure (I, uTS j I s)
mgu _ _ s F (IZ _ (Nm _ (U j) _)) = pure (F, uTS j F s)
mgu _ _ s (IZ _ (Nm _ (U j) _)) F = pure (F, uTS j F s)
mgu f _ s (Li i0) (IZ i1 (Nm _ (U j) _)) = do {(i',iS) <- mguI f (iSubst s) i0 i1; let t=σ$Li i' in pure (t, uTS j t$wI iS s)}
mgu f _ s (IZ i0 (Nm _ (U j) _)) (Li i1) = do {(i',iS) <- mguI f (iSubst s) i0 i1; let t=σ$Li i' in pure (t, uTS j t$wI iS s)}
mgu _ _ s t@(IZ (Ix _ i0) n0) (IZ (Ix _ i1) n1) | i0==i1&&n0==n1 = pure (t, s)
mgu f _ s (IZ i0 n0) (IZ i1 n1@(Nm _ (U u) _)) | n0/=n1 = do {(i',iS) <- mguI f (iSubst s) i0 i1; let t=σ$IZ i' n0 in pure (t, uTS u t$wI iS s)}
mgu _ _ s (IZ _ n0@(Nm _ (U j) _)) t1@(TVar n1) | n0/=n1 = pure (t1, uTS j t1 s)
mgu _ _ s t0@(TVar n0) (IZ _ n1@(Nm _ (U j) _)) | n0/=n1 = pure (t0, uTS j t0 s)
mgu f _ s (Li i0) (Li i1) = do {(i', iS) <- mguI f (iSubst s) i0 i1; pure (σ$Li i', wI iS s)}
mgu _ _ s Li{} (TVar (Nm _ (U u) _)) = pure (I, uTS u I s)
mgu _ _ s (TVar (Nm _ (U u) _)) Li{} = pure (I, uTS u I s)
mgu _ _ s t@(TVar n) (TVar n') | n == n' = pure (t, s)
mgu _ _ s t@(TVar n) (Arr (SVar i) (TVar n')) | n'==n = pure (t, scalar i s)
mgu _ _ s (Arr (SVar i) t@(TVar n)) (TVar n') | n'==n = pure (t, scalar i s)
mgu _ (l, _) s t'@(TVar (Nm _ (U i) _)) t | i `IS.member` occ t = throwError $ OT l t' t
                                          | otherwise = pure (t, uTS i t s)
mgu _ (l, _) s t t'@(TVar (Nm _ (U i) _)) | i `IS.member` occ t = throwError $ OT l t' t
                                          | otherwise = pure (t, uTS i t s)
mgu _ (l, e) _ t0@Arrow{} t1 = throwError $ UF l e t0 t1
mgu _ (l, e) _ t0 t1@Arrow{} = throwError $ UF l e t0 t1
-- TODO: if t' is a TVar, it could be an array! (so sh could eat sh'++sh part(t'))
mgu f l s (Arr sh t) (Arr sh' t') = do
    (t'', s0) <- mgu f l s t t'
    (sh'', s1) <- mgShPrep f (fst l) s0 sh sh'
    pure (Arr sh'' t'', s1)
mgu f l s (Arr (SVar n) t) F = second (scalar n) <$> mgu f l s t F
mgu f l s (Arr (SVar n) t) I = second (scalar n) <$> mgu f l s t I
mgu f l s F (Arr (SVar n) t) = second (scalar n) <$> mgu f l s F t
mgu f l s I (Arr (SVar n) t) = second (scalar n) <$> mgu f l s I t
mgu f l s (Arr (SVar n) t) B = second (scalar n) <$> mgu f l s t B
mgu f l s B (Arr (SVar n) t) = second (scalar n) <$> mgu f l s B t
mgu f l s (Arr (SVar n) t) t'@P{} = second (scalar n) <$> mgu f l s t t'
mgu f l s t'@P{} (Arr (SVar n) t) = second (scalar n) <$> mgu f l s t' t
mgu f l s (Arr (SVar n) t) t'@Ρ{} = second (scalar n) <$> mgu f l s t t'
mgu f l s t'@Ρ{} (Arr (SVar n) t) = second (scalar n) <$> mgu f l s t' t
mgu f l s (Arr (SVar n) t) t'@Li{} = second (scalar n) <$> mgu f l s t t'
mgu f l s t'@Li{} (Arr (SVar n) t) = second (scalar n) <$> mgu f l s t' t
mgu f l s (Arr (SVar n) t) t'@IZ{} = second (scalar n) <$> mgu f l s t t'
mgu f l s t'@IZ{} (Arr (SVar n) t) = second (scalar n) <$> mgu f l s t' t
mgu f l s (P ts) (P ts') | length ts == length ts' = first P <$> zSt (mguPrep f l) s ts ts'
-- TODO: rho occurs check
mgu f l@(lϵ, e) s t@(Ρ n rs) t'@(P ts) | length ts >= fst (IM.findMax rs) && fst (IM.findMin rs) > 0 = first P <$> tS (\sϵ (i, tϵ) -> second (iTS n t') <$> mguPrep f l sϵ (ts!!(i-1)) tϵ) s (IM.toList rs)
                                       | otherwise = throwError $ UF lϵ e t t'
mgu f l s t@P{} t'@Ρ{} = mgu f l s t' t
mgu _ l s (Ρ n rs) (Ρ n' rs') = do
    (_, rss) <- tS (\sϵ (t0,t1) -> mguPrep LF l sϵ t0 t1) s $ IM.elems $ IM.intersectionWith (,) rs rs'
    let t=Ρ n' (rs<>rs') in pure (t, iTS n t rss)
mgu _ (l, e) _ t0@Ρ{} t1 = throwError $ UF l e t0 t1
mgu _ (l, e) _ t0 t1@Ρ{} = throwError $ UF l e t0 t1
mgu _ (l, e) _ t0@Li{} t1 = throwError $ UF l e t0 t1
mgu _ (l, e) _ t0 t1@Li{} = throwError $ UF l e t0 t1
mgu _ (l, e) _ t0@IZ{} t1 = throwError $ UF l e t0 t1
mgu _ (l, e) _ t0 t1@IZ{} = throwError $ UF l e t0 t1
mgu _ (l, e) _ B t1 = throwError $ UF l e B t1
mgu _ (l, e) _ t0 B = throwError $ UF l e t0 B
mgu _ (l, e) _ F t1 = throwError $ UF l e F t1
mgu _ (l, e) _ t0 F = throwError $ UF l e t0 F
mgu _ (l, e) _ I t1 = throwError $ UF l e I t1
mgu _ (l, e) _ t0 I = throwError $ UF l e t0 I
mgu _ (l, e) _ t0@P{} t1 = throwError $ UF l e t0 t1
mgu _ (l, e) _ t0 t1@P{} = throwError $ UF l e t0 t1

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

-- TODO: (+) applied to num(n) should be int(i)->int(j)->int(#n)...
-- Maybe IZ #n a?
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
tyB _ Floor = pure (F ~> I, mempty); tyB _ Ceil = pure (F ~> I, mempty); tyB _ ItoF = pure (I ~> F, mempty)
tyB _ Even = pure (I ~> B, mempty); tyB _ Odd = pure (I ~> B, mempty)
tyB _ Sr = pure (I ~> I ~> I, mempty); tyB _ Sl = pure (I ~> I ~> I, mempty)
tyB l R = do
    n <- fc "a" l IsNum; sh <- fsh "sh"
    pure (n ~> n ~> Arr sh n, mempty)
tyB _ ConsE = do
    a <- ftv "a"; i <- fti "i"
    pure (a ~> vV i a ~> vV (i+:Ix()1) a, mempty)
tyB l Snoc = tyB l ConsE
tyB _ A1 = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (Arr (i `Cons` sh) a ~> I ~> Arr sh a, mempty)
tyB _ I1 = do
    a <- ftv "a"; i <- fti "i"; n <- fti "n"; sh <- fsh "sh"
    pure (vV n I ~> Arr (i `Cons` sh) a ~> Arr (n `Cons` sh) a, mempty)
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
    pure (Arr ((i+:Ix()1) `Cons` sh) a ~> Arr sh a, mempty)
tyB _ Head = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (Arr ((i+:Ix()1) `Cons` sh) a ~> Arr sh a, mempty)
tyB _ Init = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (Arr ((i+:Ix()1) `Cons` sh) a ~> Arr (i `Cons` sh) a, mempty)
tyB _ InitM = do
    a <- ftv "a"; i <- fti "i"; n <- ftie; sh <- fsh "sh"
    pure (Arr (i `Cons` sh) a ~> Arr (n `Cons` sh) a, mempty)
tyB _ Tail = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (Arr ((i+:Ix()1) `Cons` sh) a ~> Arr (i `Cons` sh) a, mempty)
tyB _ Ix'd = do
    a <- ftv "a"; i <- fti "i"
    pure (vV i a ~> vV i I, mempty)
tyB _ TailM = do
    a <- ftv "a"; i <- fti "i"; n <- ftie; sh <- fsh "sh"
    pure (Arr (i `Cons` sh) a ~> Arr (n `Cons` sh) a, mempty)
tyB _ Rot = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (I ~> Arr (i `Cons` sh) a ~> Arr (i `Cons` sh) a, mempty)
tyB _ Cyc = do
    sh <- fsh "sh"; a <- ftv "a"; i <- fti "i"; n <- fti "n"
    pure (Arr (i `Cons` sh) a ~> Li n ~> Arr (StaMul() n i `Cons` sh) a, mempty)
tyB _ HeadM = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (Arr (i `Cons` sh) a ~> Arr sh a, mempty)
tyB _ Re = do
    a <- ftv "a"; n <- fti "n"
    pure (a ~> Li n ~> vV n a, mempty)
tyB _ FRange = do {n <- fti "n"; pure (F ~> F ~> Li n ~> vV n F, mempty)}
tyB _ Fib = do
    n <- fti "n"; m <- fti "m"; k <- fti "k"; a <- ftv "a"
    pure (vV m a ~> (vV k a ~> a) ~> Li n ~> vV (m+:n) a, mempty)
tyB _ IRange = do {n <- ftie; pure (I ~> I ~> I ~> vV n I, mempty)}
tyB l Plus = tyNumBinOp l; tyB l Minus = tyNumBinOp l
tyB l Times = tyNumBinOp l
tyB l Dot = do
    n <- fc "a" l IsNum; i <- fti "i"
    pure (vV i n ~> vV i n ~> n, mempty)
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
    pure (vV i n ~> vV j n ~> vV (i+:j) n, mempty)
tyB _ Scan = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    let arrTy = Arr ((i+:Ix() 1) `Cons` sh) a
    pure ((a ~> a ~> a) ~> arrTy ~> arrTy, mempty)
tyB _ ScanS = do
    a <- ftv "a"; b <- ftv "b"
    i <- fti "i"; sh <- fsh "sh"
    let opTy = b ~> a ~> b
        arrTy = Arr (Cons i sh); rarrTy = Arr ((i+:Ix()1) `Cons` sh)
        -- FIXME: 1+1?
    pure (opTy ~> b ~> arrTy a ~> rarrTy b, mempty)
tyB l (DI n) = tyB l (Conv [(n,Just 1)])
tyB _ (Conv as) = do
    sh <- fsh "sh"
    is <- zipWithM (\_ t -> fti (T.singleton t)) ns ['i'..]
    a <- ftv "a"; b <- ftv "b"
    let nx = Ix () <$> ns
        opTy = Arr (foldr Cons sh nx) a ~> b
        t = Arrow (Arr (foldr Cons sh (zipWith3 (\iϵ dϵ n -> StaMul () iϵ dϵ+:n) is dix nx)) a) (Arr (foldr Cons Nil is) b)
    pure (opTy ~> t, mempty)
  where (ns,ds) = unzip as; dix=Ix ().fromMaybe 1<$>ds
tyB _ (Focus ns) = do
    sh <- fsh "sh"
    is <- zipWithM (\_ t -> fti (T.singleton t)) ns ['i'..]
    a <- ftv "a"; b <- ftv "b"
    let nx = map (Ix ()) ns
        opTy = Arr (foldr Cons sh nx) a ~> b
        t = Arr (foldr Cons sh (zipWith (StaMul ()) nx is)) a ~> Arr (foldr Cons Nil is) b
    pure (opTy~>t, mempty)
tyB _ Succ = do
    i <- fti "i"; sh <- fsh "sh"
    a <- ftv "a"; b <- ftv "b"
    let opTy = a ~> (a ~> b)
    pure (opTy ~> (Arr ((i+:Ix () 1) `Cons` sh) a ~> Arr (i `Cons` sh) b), mempty)
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
    let sh1 = (i+:Ix()1) `Cons` sh
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
    a <- ftv "a"; n <- fti "n"
    pure (a ~> (a ~> a) ~> Li n ~> vV n a, mempty)
tyB _ Ug = do
    a <- ftv "a"; b <- ftv "b"; n <- fti "n"
    pure ((b ~> P [b,a]) ~> b ~> Li n ~> vV n a, mempty)
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
tyB _ S' = do
    a <- ftv "a"; b <- ftv "b"; c <- ftv "c"
    pure ((b ~> b ~> c) ~> (a ~> b) ~> a ~> a ~> c, mempty)
tyB _ S = do
    a <- ftv "a"; b <- ftv "b"; c <- ftv "c"
    pure ((a ~> b ~> c) ~> (a ~> b) ~> a ~> c, mempty)
tyB _ K = do
    a <- ftv "a"; b <- ftv "b"
    pure (b ~> (a ~> b), mempty)

liftCloneTy :: T b -> TyM a (T b, IM.IntMap Int)
liftCloneTy t = do
    i<- gets maxU
    let (u,t',vs) = cloneT i t
    setMaxU u $> (t',vs)

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
        (i, Ix _ 1)       -> i
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
rwSh (Π s) | Nil <- rwSh s = Nil
rwSh (Π s) | Just i <- iunroll (rwSh s) = rwI i `Cons` Nil
           | otherwise = Π (rwSh s)

rwArr :: T a -> T a
rwArr (Arrow t t')  = Arrow (rwArr t) (rwArr t')
rwArr I             = I
rwArr B             = B
rwArr F             = F
rwArr t@Li{}        = t
rwArr t@TVar{}      = t
rwArr t@IZ{}        = t
rwArr (P ts)        = P (rwArr<$>ts)
rwArr (Arr sh t)    | Nil <- rwSh sh = rwArr t
rwArr (Arr ixes arr) | (is, Nil) <- unroll (rwSh ixes), Arr sh t <- rwArr arr = Arr (roll sh is) t
rwArr (Arr sh t) | Arr shϵ t' <- rwArr t = Arr (rwSh$Cat sh shϵ) t'
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
checkTy IZ{} (IsNum, _)       = pure Nothing
checkTy Li{} (IsNum, _)       = pure Nothing
checkTy I (IsNum, _)          = pure Nothing
checkTy F (IsNum, _)          = pure Nothing
checkTy I (IsOrd, _)          = pure Nothing
checkTy I (HasBits, _)        = pure Nothing
checkTy Li{} (HasBits, _)     = pure Nothing
checkTy B (HasBits, _)        = pure Nothing
checkTy F (IsOrd, _)          = pure Nothing
checkTy Li{} (IsOrd, _)       = pure Nothing
checkTy IZ{} (IsOrd, _)       = pure Nothing
checkTy I (IsEq, _)           = pure Nothing
checkTy F (IsEq, _)           = pure Nothing
checkTy B (IsEq, _)           = pure Nothing
checkTy Li{} (IsEq, _)        = pure Nothing
checkTy IZ{} (IsEq, _)        = pure Nothing
checkTy t (c@IsNum, l)        = Left$ Doesn'tSatisfy l t c
checkTy t (c@HasBits, l)      = Left$ Doesn'tSatisfy l t c
checkTy t@Arrow{} (c, l)      = Left$ Doesn'tSatisfy l t c
checkTy (Arr _ t) c@(IsEq, _) = checkTy t c

substI :: Subst a -> Int -> Maybe (T a)
substI s@(Subst ts _ _) i =
    case IM.lookup i ts of
        Just ty@TVar{} -> Just $ aT (s\-i) ty
        Just ty        -> Just $ aT s ty
        Nothing        -> Nothing

checkClass :: Subst a -> Int -> (C, a) -> Either (TyE a) (Maybe (Nm a, C))
checkClass s i c =
    case substI s i of
        Just ty -> checkTy (rwArr ty) c
        Nothing -> pure Nothing

tyClosed :: Int -> E a -> Either (TyE a) (E (T ()), [(Nm a, C)], Int)
tyClosed u e = do
    ((eS, scs), i) <- runTyM u (do { (e', s) <- tyE mempty e; cvs <- gets varConstr; scs <- liftEither $ catMaybes <$> traverse (uncurry$checkClass s) (IM.toList cvs); pure (rwArr.(s@@)<$>e', scs) })
    let vs = occ (eAnn eS); scs' = filter (\(Nm _ (U iϵ) _, _) -> iϵ `IS.member` vs) scs
    chkE (eAnn eS) $> (eS, nubOrd scs', i)

tyE :: Subst a -> E a -> TyM a (E (T ()), Subst a)
tyE s (EApp _ (EApp _ (EApp _ (Builtin l IRange) lb) ub) n) = do
    (lbϵ,s0) <- tyE s lb; (ubϵ,s1) <- tyE s0 ub; (nϵ,s2) <- tyE s1 n
    let lbTy0=eAnn lbϵ; ubTy0=eAnn ubϵ; nTy0=eAnn nϵ
        iLoc sϵ t lϵ = second void$iv sϵ (aT sϵ (t$>eAnn lϵ))
        (s3,lbTy) = iLoc s2 lbTy0 lb; (s4,ubTy) = iLoc s3 ubTy0 ub; (s5,niTy) = iLoc s4 nTy0 n
    m <- case (lbTy, ubTy, niTy) of
        (Li (Ix _ lbi), Li (Ix _ ubi), Li (Ix _ ni)) -> do
            let m=(ubi-lbi) `quot` ni+1
            when (m<0) $ throwError (NegIx l m)
            pure (Ix () m)
        _ -> ftie
    let arrTy = vV m I
    pure (EApp arrTy (EApp (nTy0 ~> arrTy) (EApp (ubTy0 ~> nTy0 ~> arrTy) (Builtin (lbTy0 ~> ubTy0 ~> nTy0 ~> arrTy) IRange) lbϵ) ubϵ) nϵ, s5)
  where iv sϵ (IZ i nm)   = let t=Li i in (iTS nm t sϵ, t)
        iv sϵ t@(TVar nm) = (iTS nm I sϵ, t)
        iv sϵ _           = (sϵ, I)
tyE s (FLit _ x) = pure (FLit F x, s)
tyE s (BLit _ x) = pure (BLit B x, s)
tyE s (ILit l m) = do
    n <- fn l m
    pure (ILit n m, s)
tyE s (Builtin l b) = do {(t,sϵ) <- tyB l b ; pure (Builtin t b, sϵ<>s)}
tyE s (Lam _ nϵ e) = do
    n <- ftv "a"
    addStaEnv nϵ n
    (e', s') <- tyE s e
    let lamTy = n ~> eAnn e'
    pure (Lam lamTy (nϵ { loc = n }) e', s')
tyE s (Let _ (n, e') e) = do
    (e'Res, s') <- tyE s e'
    let e'Ty = eAnn e'Res
    addStaEnv n (s'@@e'Ty)
    (eRes, s'') <- tyE s' e
    pure (Let (eAnn eRes) (n { loc = e'Ty }, e'Res) eRes, s'')
tyE s (Def _ (n, e') e) = do
    (e'Res, s') <- tyE s e'
    let e'Ty = eAnn e'Res
    modify (addPolyEnv n (s'@@e'Ty))
    (eRes, s'') <- tyE s' e
    pure (Def (eAnn eRes) (n { loc = e'Ty }, e'Res) eRes, s'')
tyE s (LLet _ (n, e') e) = do
    (e'Res, s') <- tyE s e'
    let e'Ty = eAnn e'Res
    addStaEnv n (s'@@e'Ty)
    (eRes, s'') <- tyE s' e
    pure (LLet (eAnn eRes) (n { loc = e'Ty }, e'Res) eRes, s'')
tyE s e@(ALit l es) = do
    a <- ftv "a"
    (es', s') <- tS tyE s es
    let eTys = a : fmap eAnn es'
        uHere sϵ t t' = mp (l,e) RF sϵ (t$>l) (t'$>l)
    ss' <- liftU $ zS uHere s' eTys (tail eTys)
    pure (ALit (vV (Ix () $ length es) a) es', ss')
tyE s (EApp l e0 e1) = do
    a <- ft "a" l; b <- ft "b" l
    (e0', s0) <- tyE s e0
    (e1', s1) <- tyE s0 e1
    let e0Ty = a ~> b
    s2 <- liftU $ mp (l,e0) LF s1 (eAnn e0'$>l) e0Ty
    s3 <- liftU $ mp (l,e1) RF s2 (eAnn e1'$>l) a
    pure (EApp (void b) e0' e1', s3)
tyE s (Cond l p e0 e1) = do
    (p',sP) <- tyE s p
    (e0',s0) <- tyE sP e0
    (e1',s1) <- tyE s0 e1
    sP' <- liftU $ mp (eAnn p,p) RF s1 B (eAnn p'$>eAnn p); (tB, s0') <- liftU $ mguPrep RF (l,e0) sP' (eAnn e0'$>l) (eAnn e1'$>eAnn e1)
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
    (es', s') <- tS tyE s es
    let eTys = eAnn<$>es'
    pure (Tup (P eTys) es', s')
tyE s (Ann l e t) = do
    (e', s') <- tyE s e
    s'' <- liftEither $ maM LF (aT s'$fmap ($>l) eAnn e') (aT s' (t$>l))
    pure (e', s'<>s'')
tyE _ Dfn{} = desugar
tyE _ ResVar{} = desugar
tyE _ Parens{} = desugar

desugar :: a
desugar = error "Internal error. Should have been desugared by now."
