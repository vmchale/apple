{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Ty ( TyE
          , tyClosed
          , match
          -- * Substitutions
          , Subst
          , aT
          ) where

import           A
import           Control.DeepSeq            (NFData)
import           Control.Exception          (Exception, throw)
import           Control.Monad              (zipWithM)
import           Control.Monad.Except       (liftEither, throwError)
import           Control.Monad.State.Strict (StateT (runStateT), gets, modify)
import           Data.Bifunctor             (first, second)
import           Data.Containers.ListUtils  (nubOrd)
import           Data.Foldable              (fold, traverse_)
import           Data.Functor               (void, ($>))
import qualified Data.IntMap                as IM
import qualified Data.IntSet                as IS
import           Data.Maybe                 (catMaybes)
import           Data.Semigroup             (Semigroup (..))
import qualified Data.Text                  as T
import           Data.Typeable              (Typeable)
import           GHC.Generics               (Generic)
import           Name
import           Prettyprinter              (Doc, Pretty (..), hardline, indent, squotes, tupled, vsep, (<+>))
import           Prettyprinter.Ext
import           Ty.Clone
import           U

data TySt a = TySt { maxU      :: !Int
                   , staEnv    :: IM.IntMap (T ())
                   , polyEnv   :: IM.IntMap (T ())
                   , varConstr :: IM.IntMap (C, a)
                   }

data Subst a = Subst { tySubst :: IM.IntMap (T a)
                     , iSubst  :: IM.IntMap (I a) -- ^ Index variables
                     , sSubst  :: IM.IntMap (Sh a) -- ^ Shape variables
                     } deriving (Functor)

data TyE a = IllScoped a (Name a)
           | UF a (E a) (T a) (T a)
           | UI a (I a) (I a)
           | USh a (Sh a) (Sh a)
           | OT a (T a) (T a)
           | OSh a (Sh a) (Sh a)
           | OI a (I a) (I a)
           | ExistentialArg (T ())
           | MatchFailed (T ()) (T ())
           | MatchShFailed (Sh ()) (Sh ())
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

prettyBind :: (Pretty c, Pretty b) => (c, b) -> Doc a
prettyBind (i, j) = pretty i <+> "→" <+> pretty j

prettyDumpBinds :: Pretty b => IM.IntMap b -> Doc a
prettyDumpBinds b = vsep (prettyBind <$> IM.toList b)

type TyM a = StateT (TySt a) (Either (TyE a))

mI :: I a -> I a -> Either (TyE b) (Subst a)
mI (Ix _ i) (Ix _ j) | i == j = Right mempty
mI (IVar _ (Name _ (U i) _)) ix = Right $ Subst IM.empty (IM.singleton i ix) IM.empty
mI (IEVar _ n) (IEVar _ n') | n == n' = Right mempty
mI (StaPlus _ i j) (StaPlus _ i' j') = (<>) <$> mI i i' <*> mI j j'

mSh :: Sh a -> Sh a -> Either (TyE b) (Subst a)
mSh (SVar (Name _ (U i) _)) sh = Right $ Subst IM.empty IM.empty (IM.singleton i sh)
mSh Nil Nil                    = Right mempty
mSh (IxA i) (IxA i')           = mI i i'
mSh (Cons i sh) (Cons i' sh')  = (<>) <$> mI i i' <*> mSh sh sh'
mSh sh sh'                     = Left $ MatchShFailed (void sh) (void sh')

match :: T a -> T a -> Subst a
match t t' = either (throw :: TyE () -> Subst a) id (maM t t')

maM :: T a -> T a -> Either (TyE b) (Subst a)
maM I I                           = Right mempty
maM F F                           = Right mempty
maM B B                           = Right mempty
maM (TVar n) (TVar n') | n == n'  = Right mempty
maM (TVar (Name _ (U i) _)) t     = Right $ Subst (IM.singleton i t) IM.empty IM.empty
maM (Arrow t0 t1) (Arrow t0' t1') = (<>) <$> maM t0 t0' <*> maM t1 t1' -- FIXME: use <\> over <>
maM (Arr sh t) (Arr sh' t')       = (<>) <$> mSh sh sh' <*> maM t t'
maM (P ts) (P ts')                = mconcat <$> zipWithM maM ts ts'
maM Ρ{} Ρ{}                       = undefined
maM t t'                          = Left $ MatchFailed (void t) (void t')

shSubst :: Subst a -> Sh a -> Sh a
shSubst s (IxA i)       = IxA (iSubst s !> i)
shSubst _ Nil           = Nil
shSubst s (Cons i sh)   = Cons (iSubst s !> i) (shSubst s sh)
shSubst s (Cat sh0 sh1) = Cat (shSubst s sh0) (shSubst s sh1)
shSubst s (Rev sh)      = Rev (shSubst s sh)
shSubst s@(Subst ts is ss) sh'@(SVar (Name _ (U u) _)) =
    case IM.lookup u ss of
        Just sh''@SVar{} -> shSubst (Subst ts is (IM.delete u ss)) sh''
        Just sh          -> shSubst s sh
        Nothing          -> sh'

infixr 4 !>
(!>) :: IM.IntMap (I a) -> I a -> I a
(!>) ixes ix'@(IVar _ (Name _ (U u) _)) =
    case IM.lookup u ixes of
        Just ix@IVar{} -> IM.delete u ixes !> ix
        Just ix        -> ixes !>ix
        Nothing        -> ix'
(!>) ixes (StaPlus l ix ix') = StaPlus l (ixes !> ix) (ixes !> ix')
(!>) _ ix@Ix{} = ix
(!>) _ ix@IEVar{} = ix

aT :: Subst a -> T a -> T a
aT s (Arr sh ty) = Arr (shSubst s sh) (aT s ty)
aT s (Arrow t₁ t₂) = Arrow (aT s t₁) (aT s t₂)
aT s@(Subst ts is ss) ty'@(TVar n) =
    let u = unU $ unique n in
    case IM.lookup u ts of
        Just ty@TVar{}   -> aT (Subst (IM.delete u ts) is ss) ty
        Just ty@(Ρ nr _) -> aT (Subst (IM.delete u ts) is ss) ty
        Just ty          -> aT s ty
        Nothing          -> ty'
aT s (P ts) = P (aT s <$> ts)
aT s@(Subst ts is ss) ty'@(Ρ n rs) =
    let u = unU (unique n) in
    case IM.lookup u ts of
        Just ty@(Ρ n' _) -> aT (Subst (IM.delete u ts) is ss) ty
        Just ty@TVar{}   -> undefined
        Just ty          -> aT s ty
        Nothing          -> Ρ n (aT s<$>rs)
aT _ ty = ty

runTyM :: Int -> TyM a b -> Either (TyE a) (b, Int)
runTyM i = fmap (second maxU) . flip runStateT (TySt i IM.empty IM.empty IM.empty)

mapMaxU :: (Int -> Int) -> TySt a -> TySt a
mapMaxU f (TySt u l v vcs) = TySt (f u) l v vcs

setMaxU :: Int -> TySt a -> TySt a
setMaxU i (TySt _ l v vcs) = TySt i l v vcs

addStaEnv :: Name a -> T () -> TySt a -> TySt a
addStaEnv (Name _ (U i) _) t (TySt u l v vcs) = TySt u (IM.insert i t l) v vcs

addPolyEnv :: Name a -> T () -> TySt a -> TySt a
addPolyEnv (Name _ (U i) _) t (TySt u l v vcs) = TySt u l (IM.insert i t v) vcs

addVarConstrI :: Int -> a -> C -> TySt a -> TySt a
addVarConstrI i ann c (TySt u l v vcs) = TySt u l v (IM.insert i (c, ann) vcs)

addVarConstr :: TyName a -> a -> C -> TySt a -> TySt a
addVarConstr tn = addVarConstrI (unU$unique tn)

pushVarConstraint :: TyName a -> a -> C -> TyM a ()
pushVarConstraint tn l c = modify (addVarConstr tn l c)

freshName :: T.Text -> b -> TyM a (Name b)
freshName n l = do
    st <- gets maxU
    Name n (U$st+1) l
        <$ modify (mapMaxU (+1))

mapTySubst f (Subst t i sh) = Subst (f t) i sh

mapShSubst f (Subst t i sh) = Subst t i (f sh)

mguIPrep :: IM.IntMap (I a) -> I a -> I a -> Either (TyE a) (IM.IntMap (I a))
mguIPrep is i0 i1 =
    let i0' = is !> i0
        i1' = is !> i1
    in mguI is i0' i1'

mguI :: IM.IntMap (I a) -> I a -> I a -> Either (TyE a) (IM.IntMap (I a))
mguI inp i0@(Ix l i) i1@(Ix _ j) | i == j = Right inp
                                 | otherwise = Left$ UI l i0 i1
mguI inp ix0@(IEVar l i) ix1@(IEVar _ j) | i == j = Right inp
                                         | otherwise = Left $ UI l ix0 ix1
mguI inp (IVar _ i) (IVar _ j) | i == j = Right inp
mguI inp iix@(IVar l (Name _ (U i) _)) ix | i `IS.member` occI ix = Left $ OI l iix ix
                                          | otherwise = Right $ IM.insert i ix inp
mguI inp ix iix@(IVar l (Name _ (U i) _)) | i `IS.member` occI ix = Left$ OI l ix iix
                                          | otherwise = Right $ IM.insert i ix inp
mguI inp (StaPlus _ i0 (Ix _ k0)) (StaPlus _ i1 (Ix _ k1)) | k0 == k1 = mguI inp i0 i1

mgShPrep :: a -> Subst a -> Sh a -> Sh a -> Either (TyE a) (Subst a)
mgShPrep l s sh0 sh1 =
    let sh0' = shSubst s sh0
        sh1' = shSubst s sh1
    in mgSh l s sh0' sh1'

mgSh :: a -> Subst a -> Sh a -> Sh a -> Either (TyE a) (Subst a)
mgSh _ inp Nil Nil = Right inp
mgSh _ inp (IxA i) (IxA i') = do {iSubst' <- mguIPrep (iSubst inp) i i' ; pure inp { iSubst = iSubst' }}
mgSh l inp (Cons i sh) (Cons i' sh') = do
    sI <- mguIPrep (iSubst inp) i i'
    mgShPrep l (inp { iSubst = sI }) sh sh'
mgSh _ inp (SVar sh) (SVar sh') | sh == sh' = Right inp
mgSh l inp s@(SVar (Name _ (U i) _)) sh | i `IS.member` occSh sh = Left$ OSh l s sh
                                        | otherwise = Right$ mapShSubst (IM.insert i sh) inp
mgSh l inp sh s@(SVar (Name _ (U i) _)) | i `IS.member` occSh sh = Left$ OSh l sh s
                                        | otherwise = Right$ mapShSubst (IM.insert i sh) inp
mgSh l _ sh@Nil sh'@Cons{} = Left $ USh l sh sh'
mgSh l _ sh@Cons{} sh'@Nil{} = Left $ USh l sh' sh

mguPrep :: (a, E a) -> Subst a -> T a -> T a -> Either (TyE a) (Subst a)
mguPrep l s t0 t1 =
    let t0' = aT s t0
        t1' = aT s t1
    in mgu l s (rwArr t0') (rwArr t1')

occSh :: Sh a -> IS.IntSet
occSh (SVar (Name _ (U i) _)) = IS.singleton i
occSh (Cat sh0 sh1)           = occSh sh0 <> occSh sh1
occSh (_ `Cons` sh)           = occSh sh
occSh IxA{}                   = IS.empty
occSh Nil{}                   = IS.empty

occI :: I a -> IS.IntSet
occI Ix{}                      = IS.empty
occI (IVar _ (Name _ (U i) _)) = IS.singleton i
occI (StaPlus _ i j)           = occI i <> occI j
occI IEVar{}                   = IS.empty

occ :: T a -> IS.IntSet
occ (TVar (Name _ (U i) _)) = IS.singleton i
occ (Arrow t t')            = occ t <> occ t'
occ (Arr _ a)               = occ a -- shouldn't need shape?
occ I                       = IS.empty
occ F                       = IS.empty
occ B                       = IS.empty
occ (P ts)                  = foldMap occ ts
occ (Ρ (Name _ (U i) _) rs) = IS.insert i $ foldMap occ rs

mgu :: (a, E a) -> Subst a -> T a -> T a -> Either (TyE a) (Subst a)
mgu l s (Arrow t0 t1) (Arrow t0' t1') = do
    s0 <- mguPrep l s t0 t0'
    mguPrep l s0 t1 t1'
mgu _ s I I = Right s
mgu _ s F F = Right s
mgu _ s B B = Right s
mgu _ s (TVar n) (TVar n') | n == n' = Right s
mgu (l, _) s t'@(TVar (Name _ (U i) _)) t | i `IS.member` occ t = Left$ OT l t' t
                                          | otherwise = Right $ mapTySubst (IM.insert i t) s
mgu (l, _) s t t'@(TVar (Name _ (U i) _)) | i `IS.member` occ t = Left$ OT l t' t
                                          | otherwise = Right $ mapTySubst (IM.insert i t) s
mgu (l, e) _ t0@Arrow{} t1 = Left $ UF l e t0 t1
mgu (l, e) _ t0 t1@Arrow{} = Left $ UF l e t0 t1
mgu l s (Arr sh t) (Arr sh' t') = do
    s0 <- mguPrep l s t t'
    mgShPrep (fst l) s0 sh sh'
mgu (l, e) _ F I = Left$ UF l e F I
mgu (l, e) _ I F = Left$ UF l e I F
mgu l s (Arr (SVar (Name _ (U i) _)) t) F = mapShSubst (IM.insert i Nil) <$> mguPrep l s t F
mgu l s (Arr (SVar (Name _ (U i) _)) t) I = mapShSubst (IM.insert i Nil) <$> mguPrep l s t I
mgu l s F (Arr (SVar (Name _ (U i) _)) t) = mapShSubst (IM.insert i Nil) <$> mguPrep l s F t
mgu l s I (Arr (SVar (Name _ (U i) _)) t) = mapShSubst (IM.insert i Nil) <$> mguPrep l s I t
mgu l s (P ts) (P ts') | length ts == length ts' = zS (mguPrep l) s ts ts'
-- TODO: rho occurs check
mgu l@(lϵ, e) s t@(Ρ n rs) t'@(P ts) | length ts >= fst (IM.findMax rs) = tS (\sϵ (i, t) -> mapTySubst (IM.insert (unU$unique n) t') <$> mguPrep l sϵ (ts!!(i-1)) t) s (IM.toList rs)
                                     | otherwise = Left$UF lϵ e t t'
mgu l s t@P{} t'@Ρ{} = mgu l s t' t
mgu l s (Ρ n rs) (Ρ n' rs') = do
    rss <- tS (\s (t0,t1) -> mguPrep l s t0 t1) s $ IM.elems $ IM.intersectionWith (,) rs rs'
    pure $ mapTySubst (IM.insert (unU$unique n) (Ρ n' (rs<>rs'))) rss

zS _ s [] _           = pure s
zS _ s _ []           = pure s
zS op s (x:xs) (y:ys) = do{next <- op s x y; zS op next xs ys}

tS :: Monad m => (Subst a -> b -> m (Subst a)) -> Subst a -> [b] -> m (Subst a)
tS _ s []     = pure s
tS f s (t:ts) = do{next <- f s t; tS f next ts}

vx i = Cons i Nil

tyNumBinOp :: a -> TyM a (T (), Subst a)
tyNumBinOp l = do
    n <- freshName "a" l
    let n' = TVar (void n)
    pushVarConstraint n l IsNum
    pure (Arrow n' (Arrow n' n'), mempty)

mm :: a -> TyM a (T (), Subst a)
mm l = do
    n <- freshName "o" l
    let n' = TVar (void n)
    pushVarConstraint n l IsOrd
    pure (Arrow n' (Arrow n' n'), mempty)

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

unroll (Cons i shϵ) = first (i :) $ unroll shϵ
unroll s            = ([], s)

roll :: Sh a -> [I a] -> Sh a
roll = foldr Cons

tyB :: a -> Builtin -> TyM a (T (), Subst a)
tyB _ Floor = pure (Arrow F I, mempty)
tyB _ ItoF = pure (Arrow I F, mempty)
tyB _ RF = pure (Arrow F (Arrow F F), mempty)
tyB _ RI = pure (Arrow I (Arrow I I), mempty)
tyB _ Iter = do{a <- TVar<$>freshName "a"(); let s = Arrow a a in pure (Arrow s (Arrow I s), mempty)}
tyB _ ConsE = do
    a <- TVar <$> freshName "a" ()
    i <- IVar () <$> freshName "i" ()
    pure (Arrow a (Arrow (Arr (i `Cons` Nil) a) (Arr (StaPlus () i (Ix()1) `Cons` Nil) a)), mempty)
tyB l Snoc = tyB l ConsE
tyB _ LastM = do
    a <- TVar <$> freshName "a" ()
    i <- IVar () <$> freshName "i" ()
    pure (Arrow (Arr (i `Cons` Nil) a) a, mempty)
tyB _ Last = do
    a <- TVar <$> freshName "a" ()
    i <- IVar () <$> freshName "i" ()
    pure (Arrow (Arr (StaPlus () i (Ix()1) `Cons` Nil) a) a, mempty)
tyB _ Re = do
    a <- TVar <$> freshName "a" ()
    n <- IEVar () <$> freshName "n" ()
    pure (Arrow I (Arrow a (Arr (n `Cons` Nil) a)), mempty)
tyB _ FRange = do
    n <- IEVar () <$> freshName "n" ()
    pure (Arrow F (Arrow F (Arrow I (Arr (n `Cons` Nil) F))), mempty)
tyB _ Fib = do
    n <- IEVar () <$> freshName "n" ()
    a <- freshName "a" ()
    let a' = TVar a
        arrTy = Arr (n `Cons` Nil) a'
    pure (Arrow a' (Arrow a' (Arrow (Arrow a' (Arrow a' a')) (Arrow I arrTy))), mempty)
tyB _ IRange = do
    n <- IEVar () <$> freshName "n" ()
    pure (Arrow I (Arrow I (Arrow I (Arr (n `Cons` Nil) I))), mempty)
tyB l Plus = tyNumBinOp l
tyB l Minus = tyNumBinOp l
tyB l Times = tyNumBinOp l
tyB _ Exp = pure (Arrow F (Arrow F F), mempty)
tyB l Min = mm l
tyB l Max = mm l
tyB l IntExp = do
    n <- freshName "a" l
    let n' = TVar (void n)
    pushVarConstraint n l IsNum
    pure (Arrow n' (Arrow I n'), mempty)
tyB l Neg = do
    n <- freshName "a" l
    let n' = TVar (void n)
    pushVarConstraint n l IsNum
    pure (Arrow n' n', mempty)
tyB _ Sqrt = pure (Arrow F F, mempty)
tyB _ Log = pure (Arrow F F, mempty)
tyB _ Div = pure (Arrow F (Arrow F F), mempty)
tyB _ Outer = do
    sh0 <- SVar <$> freshName "sh0" (); sh1 <- SVar <$> freshName "sh1" ()
    a <- TVar <$> freshName "a" (); b <- TVar <$> freshName "b" (); c <- TVar <$> freshName "c" ()
    pure (Arrow (Arrow a (Arrow b c)) (Arrow (Arr sh0 a) (Arrow (Arr sh1 b) (Arr (Cat sh0 sh1) c))), mempty)
tyB _ Transpose = do
    sh <- SVar <$> freshName "sh" (); a <- TVar <$> freshName "a" ()
    pure (Arrow (Arr sh a) (Arr (Rev sh) a), mempty)
tyB _ Concat = do
    i <- freshName "i" (); j <- freshName "j" ()
    n <- freshName "a" ()
    let i' = IVar () i; j' = IVar () j; n' = TVar n
    pure (Arrow (Arr (vx i') n') (Arrow (Arr (vx j') n') (Arr (vx $ StaPlus () i' j') n')), mempty)
tyB _ Scan = do
    a <- TVar <$> freshName "a" (); b <- TVar <$> freshName "b" ()
    i <- IVar () <$> freshName "i" ()
    sh <- SVar <$> freshName "sh" ()
    let opTy = Arrow b (Arrow a b)
        arrTy = Arr (Cons i sh)
    pure (Arrow opTy (Arrow b (Arrow (arrTy a) (arrTy b))), mempty)
tyB l (DI n) = tyB l (Conv [n])
tyB _ (Conv ns) = do
    sh <- SVar <$> freshName "sh" ()
    is <- zipWithM (\_ t -> IVar () <$> freshName (T.singleton t) ()) ns ['i'..]
    a <- TVar <$> freshName "a" (); b <- TVar <$> freshName "b" ()
    let nx = Ix () <$> ns
        opTy = Arrow (Arr (foldr Cons sh nx) a) b
        t = Arrow (Arr (foldr Cons sh (zipWith (StaPlus ()) is nx)) a) (Arr (foldr Cons Nil is) b)
    pure (Arrow opTy t, mempty)
tyB _ Succ = do
    sh <- SVar <$> freshName "sh" ()
    i <- IVar () <$> freshName "i" ()
    a <- TVar <$> freshName "a" (); b <- TVar <$> freshName "b" ()
    let opTy = Arrow a (Arrow a b)
    pure (Arrow opTy (Arrow (Arr (StaPlus () i (Ix () 1) `Cons` sh) a) (Arr (i `Cons` sh) b)), mempty)
tyB _ (TAt i) = do
    ρ <- freshName "ρ" ()
    a <- freshName "a" ()
    let aT = TVar a
    pure (Arrow (Ρ ρ (IM.singleton i aT)) aT, mempty)
tyB _ (Map n) = do
    -- for n the shape is i1,i2,...in `Cons` Nil (this forces it to have
    -- enough indices)
    ixList <- zipWithM (\_ c -> freshName (T.singleton c) ()) [1..n] ['i'..]
    a <- freshName "a" (); b <- freshName "b" ()
    let arrSh = foldr Cons Nil (IVar () <$> ixList)
        a' = TVar a; b' = TVar b
        fTy = Arrow a' b'
        gTy = Arrow (Arr arrSh a') (Arr arrSh b')
    pure (Arrow fTy gTy, mempty)
tyB _ Zip = do
    i <- freshName "i" ()
    a <- freshName "a" (); b <- freshName "b" (); c <- freshName "c" ()
    let arrSh = IVar () i `Cons` Nil
        a' = TVar a; b' = TVar b; c' = TVar c
        fTy = Arrow a' (Arrow b' c')
        gTy = Arrow (Arr arrSh a') (Arrow (Arr arrSh b') (Arr arrSh c'))
    pure (Arrow fTy gTy, mempty)
tyB l (Rank as) = do
    let ixN n = zipWithM (\_ c -> freshName (T.singleton c) ()) [1..n] ['i'..]
    shs <- traverse (\(i,ax) -> do {is <- ixN (maybe i maximum ax); sh <- SVar <$> freshName "sh" (); pure $ foldr Cons sh (IVar () <$> is)}) as
    vs <- zipWithM (\_ c -> TVar <$> freshName (T.singleton c) ()) as ['a'..]
    codSh <- freshName "sh" ()
    cod <- TVar <$> freshName "c" ()
    let mArrs = zipWith Arr shs vs
        codTy = Arr (SVar codSh) cod
        fTy = foldr Arrow cod $ zipWith3 (\ax sh t -> case ax of {(_,Nothing) -> Arr (trim sh) t;(_,Just axs) -> Arr (sel axs sh) t}) as shs vs
        rTy = foldr Arrow codTy mArrs
        shsU = zipWith (\ax sh -> case ax of {(n,Nothing) -> tydrop n sh;(_,Just axs) -> del axs sh}) as shs
        shUHere sh sh' = liftEither $ mgShPrep l mempty (sh$>l) (sh'$>l)
    s <- zipWithM shUHere shsU (tail shsU++[SVar codSh])
    pure (Arrow fTy rTy, mconcat s)
tyB _ (Fold n) = do
    ixList <- zipWithM (\_ c -> freshName (T.singleton c) ()) [1..n] ['i'..]
    shV <- freshName "sh" ()
    a <- freshName "a" ()
    let sh = foldr Cons (SVar shV) (IVar () <$> ixList)
        a' = TVar a
    pure (Arrow (Arrow a' (Arrow a' a')) (Arrow a' (Arrow (Arr sh a') (Arr (SVar shV) a'))), mempty)
tyB _ Dim = do
    iV <- IVar () <$> freshName "i" ()
    shV <- SVar <$> freshName "sh" ()
    a <- TVar <$> freshName "a" ()
    pure (Arrow (Arr (iV `Cons` shV) a) I, mempty)
tyB _ Size = do
    shV <- SVar <$> freshName "sh" ()
    a <- TVar <$> freshName "a" ()
    pure (Arrow (Arr shV a) I, mempty)
tyB _ Gen = do
    a <- TVar <$> freshName "a" ()
    n <- IEVar () <$> freshName "n" ()
    let arrTy = Arr (n `Cons` Nil) a
    pure (Arrow a (Arrow (Arrow a a) (Arrow I arrTy)), mempty)
tyB l Mul = do
    a <- freshName "a" l
    i <- IVar () <$> freshName "i" (); j <- IVar () <$> freshName "j" (); k <- IVar () <$> freshName "k" ()
    pushVarConstraint a l IsNum
    let a' = TVar (void a)
    pure (Arrow (Arr (i `Cons` j `Cons` Nil) a') (Arrow (Arr (j `Cons` k `Cons` Nil) a') (Arr (i `Cons` k `Cons` Nil) a')), mempty)

liftCloneTy :: T b -> TyM a (T b, IM.IntMap Int)
liftCloneTy t = do
    i<- gets maxU
    let (u,t',vs) = cloneTClosed i t
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

rwSh :: Sh a -> Sh a
rwSh s@SVar{}     = s
rwSh s@Nil        = s
rwSh s@IxA{}      = s
rwSh (i `Cons` s) = i `Cons` rwSh s
rwSh (Cat s0 s1) | (is, Nil) <- unroll (rwSh s0), (js, Nil) <- unroll (rwSh s1) = roll Nil (is++js)
                 | otherwise = Cat (rwSh s0) (rwSh s1)
rwSh (Rev s) | (is, Nil) <- unroll (rwSh s) = roll Nil (reverse is)
             | otherwise = Rev (rwSh s)

rwArr :: T a -> T a
rwArr (Arrow t t') = Arrow (rwArr t) (rwArr t')
rwArr I            = I
rwArr B            = B
rwArr F            = F
rwArr t@TVar{}     = t
rwArr (P ts)       = P (rwArr<$>ts)
rwArr (Arr Nil t)  = rwArr t
rwArr (Arr ixes arr) | (is, Nil) <- unroll ixes, Arr sh t <- rwArr arr = Arr (roll sh is) t
rwArr (Arr sh t)   = Arr (rwSh sh) (rwArr t)
rwArr (Ρ n fs)     = Ρ n (rwArr<$>fs)

hasEI :: I a -> Bool
hasEI IEVar{}            = True
hasEI (StaPlus _ ix ix') = hasEI ix || hasEI ix'
hasEI _                  = False

hasESh :: Sh a -> Bool
hasESh (IxA i)     = hasEI i
hasESh (Cons i sh) = hasEI i || hasESh sh
hasESh _           = False

hasE :: T a -> Bool
hasE (Arrow t t'@Arrow{}) = hasE t || hasE t'
hasE (Arr sh t)           = hasESh sh || hasE t
hasE _                    = False

-- {-# SCC chkE #-}
chkE :: T () -> Either (TyE a) ()
chkE t@Arrow{} | hasE t = Left (ExistentialArg t)
chkE _ = Right ()

checkTy :: T a -> (C, a) -> Either (TyE a) (Maybe (Name a, C))
checkTy (TVar n) (c, _) = pure $ Just(n, c)
checkTy I (IsNum, _)    = pure Nothing
checkTy F (IsNum, _)    = pure Nothing
checkTy I (IsOrd, _)    = pure Nothing
checkTy F (IsOrd, _)    = pure Nothing
checkTy t (c@IsNum, l)  = Left$ Doesn'tSatisfy l t c

substI :: Subst a -> Int -> Maybe (T a)
substI s@(Subst ts is sh) i =
    case IM.lookup i ts of
        Just ty@TVar{} -> Just $ aT (Subst (IM.delete i ts) is sh) ty
        Just ty        -> Just $ aT s ty
        Nothing        -> Nothing


checkClass :: Subst a -> Int -> (C, a) -> Either (TyE a) (Maybe (Name a, C))
checkClass s i c =
    case substI s i of
        Just ty -> checkTy (rwArr ty) c
        Nothing -> pure Nothing

tyClosed :: Int -> E a -> Either (TyE a) (E (T ()), [(Name a, C)], Int)
tyClosed u e = do
    (((e', s), scs), i) <- runTyM u (do { res@(_, s) <- tyE mempty e ; cvs <- gets varConstr ; scs <- liftEither $ catMaybes <$> traverse (uncurry$checkClass s) (IM.toList cvs) ; pure (res, scs) })
    let eS = {-# SCC "applySubst" #-} fmap (rwArr.aT (void s)) e'
    eS' <- do {(e'', s') <- {-# SCC "match" #-} rAn eS; pure (fmap (rwArr.aT s') e'') }
    chkE (eAnn eS') $> (eS', nubOrd scs, i)

rAn :: E (T ()) -> Either (TyE a) (E (T ()), Subst ())
rAn (Ann _ e t) = do
    s <- maM (eAnn e) t
    pure (e, s)
rAn (EApp t e0 e1) = do
    (e0', s0) <- rAn e0
    (e1', s1) <- rAn e1
    pure (EApp t e0' e1', s0<>s1)
rAn e@Builtin{} = pure (e, mempty)
rAn e@FLit{} = pure (e, mempty)
rAn e@ILit{} = pure (e, mempty)
rAn e@Var{} = pure (e, mempty)
rAn (Let t (n, e0) e1) = do
    (e0', s) <- rAn e0;
    (e1', s') <- rAn e1
    pure (Let t (n, e0') e1', s<>s')
rAn (LLet t (n, e0) e1) = do
    (e0', s) <- rAn e0
    (e1', s') <- rAn e1
    pure (LLet t (n, e0') e1', s<>s')
rAn (Def t (n, e0) e1) = do
    (e0', s) <- rAn e0
    (e1', s') <- rAn e1
    pure (Def t (n, e0') e1', s<>s')
rAn (Lam t n e) = do
    (e', s) <- rAn e
    pure (Lam t n e', s)
rAn (ALit t es) = do
    (es', ss) <- unzip <$> traverse rAn es
    pure (ALit t es', mconcat ss)
rAn (Tup t es) = do
    (es', ss) <- unzip <$> traverse rAn es
    pure (Tup t es', mconcat ss)
rAn (Cond t p e0 e1) = do
    (p',sP) <- rAn p
    (e0',s0) <- rAn e0
    (e1',s1) <- rAn e1
    pure (Cond t p' e0' e1', sP<>s0<>s1)

tyE :: Subst a -> E a -> TyM a (E (T ()), Subst a)
tyE s (EApp _ (Builtin _ Re) (ILit _ n)) = do
    a <- TVar <$> freshName "a" ()
    let arrTy = Arrow a (Arr (vx $ Ix () (fromInteger n)) a)
    pure (EApp arrTy (Builtin (Arrow I arrTy) Re) (ILit I n), s)
tyE s (EApp _ (EApp _ (EApp _ (Builtin _ FRange) e0) e1) (ILit _ n)) = do
    (e0',s0) <- tyE s e0; (e1',s1) <- tyE s0 e1
    let tyE0 = eAnn e0'; tyE1 = eAnn e1'
        arrTy = Arr (vx (Ix () (fromInteger n))) F
        l0 = eAnn e0; l1 = eAnn e1
    s0' <- liftEither $ mguPrep (l0,e0) s1 F (eAnn e0' $> l0); s1' <- liftEither $ mguPrep (l1,e1) s0' F (eAnn e1' $> l1)
    pure (EApp arrTy (EApp (Arrow I arrTy) (EApp (Arrow tyE1 (Arrow I arrTy)) (Builtin (Arrow tyE0 (Arrow tyE1 (Arrow I arrTy))) FRange) e0') e1') (ILit I n), s1')
tyE s (EApp _ (EApp _ (EApp _ (Builtin _ IRange) (ILit _ b)) (ILit _ e)) (ILit _ si)) = do
    let arrTy = Arr (vx (Ix () (fromInteger ((e-b+si) `div` si)))) I
    pure (EApp arrTy (EApp (Arrow I arrTy) (EApp (Arrow I (Arrow I arrTy)) (Builtin (Arrow I (Arrow I (Arrow I arrTy))) IRange) (ILit I b)) (ILit I e)) (ILit I si), s)
tyE s (FLit _ x) = pure (FLit F x, s)
tyE s (BLit _ x) = pure (BLit B x, s)
tyE s (ILit l m) = do
    n <- freshName "a" l
    pushVarConstraint n l IsNum
    pure (ILit (TVar (void n)) m, s)
tyE s (Builtin l b) = do {(t,sϵ) <- tyB l b ; pure (Builtin t b, sϵ<>s)}
tyE s (Lam _ nϵ e) = do
    n <- TVar <$> freshName "a" ()
    modify (addStaEnv nϵ n)
    (e', s') <- tyE s e
    let lamTy = Arrow n (eAnn e')
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
    a <- TVar <$> freshName "a" ()
    (es', s') <- sSt s es
    let eTys = a : fmap eAnn es'
        uHere t t' = liftEither $ mguPrep (l,e) s' (t$>l) (t'$>l)
    -- FIXME: not stateful enough... apply substs forward?
    ss' <- zipWithM uHere eTys (tail eTys)
    pure (ALit (Arr (vx (Ix () $ length es)) a) es', mconcat ss')
tyE s (EApp l e0 e1) = do
    a <- TVar <$> freshName "a" l
    b <- TVar <$> freshName "b" l
    (e0', s0) <- tyE s e0
    (e1', s1) <- tyE s0 e1
    let e0Ty = Arrow a b
    s2 <- liftEither $ mguPrep (l,e0) s1 (eAnn e0'$>l) e0Ty
    s3 <- liftEither $ mguPrep (l,e1) s2 (eAnn e1'$>l) a
    pure (EApp (void b) e0' e1', s3)
tyE s (Cond l p e0 e1) = do
    (p',sP) <- tyE s p
    (e0',s0) <- tyE sP e0
    (e1',s1) <- tyE s0 e1
    sP' <- liftEither $ mguPrep (eAnn p,p) s1 B (eAnn p'$>eAnn p); s0' <- liftEither $ mguPrep (l,e0) sP' (eAnn e0'$>l) (eAnn e1'$>eAnn e1)
    pure (Cond (eAnn e0') p' e0' e1', s0')
tyE s (Var l n@(Name _ (U u) _)) = do
    lSt<- gets staEnv
    case IM.lookup u lSt of
        Just t  -> pure (Var t (n $> t), s)
        -- TODO: polymorphic let
        Nothing -> do
            vSt<- gets polyEnv
            case IM.lookup u vSt of
                Just t  -> do {t'<- cloneWithConstraints t; pure (Var t' (n$>t'), s)}
                Nothing -> throwError $ IllScoped l n
tyE s (Tup _ es) = do
    (es', s') <- sSt s es
    let eTys = eAnn<$>es'
    pure (Tup (P eTys) es', s')
tyE s (Ann _ e t) = do
    (e', s') <- tyE s e
    pure (Ann (eAnn e') e' t, s')

sSt :: Subst a -> [E a] -> TyM a ([E (T ())], Subst a)
sSt s []     = pure([], s)
sSt s (e:es) = do{(e',s') <- tyE s e; first (e':) <$> sSt s' es} -- TODO: recurse other way idk
