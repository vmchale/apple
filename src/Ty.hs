{-# LANGUAGE DeriveGeneric #-}

module Ty ( TyE
          , tyClosed
          , match
          -- * Substitutions
          , aT, rwArr
          ) where

import           A
import           Control.DeepSeq                  (NFData (rnf), rwhnf)
import           Control.Exception                (Exception, throw)
import           Control.Monad                    (when, zipWithM)
import           Control.Monad.Except             (liftEither, throwError)
import           Control.Monad.Trans.State.Strict (StateT (runStateT), gets, modify, state)
import           Data.Bifunctor                   (first, second)
import           Data.Function                    (on)
import           Data.Functor                     (void, ($>))
import qualified Data.IntMap                      as IM
import qualified Data.IntSet                      as IS
import           Data.Maybe                       (fromMaybe, listToMaybe, mapMaybe)
import qualified Data.Set                         as S
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
infixr 5 <||
infixr 5 <|

(<|) = Cons

data TySt a = TySt { maxU :: !Int, staEnv, polyEnv :: IM.IntMap (T ()) }

type ISubst a = IM.IntMap (I a)

data Subst a = Subst { tySubst :: IM.IntMap (T a)
                     , iSubst  :: IM.IntMap (I a) -- ^ Index variables
                     , sSubst  :: IM.IntMap (Sh a) -- ^ Shape variables
                     } deriving (Functor)

data TyE a = IllScoped a !(Nm a)
           | UF a (E a) !(T a) !(T a)
           | USh a !(Sh a) !(Sh a)
           | UShD a !(Sh a) !(Sh a)
           | OT a !(T a) !(T a)
           | OSh a !(Sh a) !(Sh a)
           | OI a !(I a) !(I a)
           | ExistentialArg (T ())
           | MF !(T a) !(T a)
           | MS !(Sh a) !(Sh a)
           | MI !F !(I a) !(I a)
           | Doesn'tSatisfy a (T a) !C
           | CV a (T a) !C
           | NegIx a Int
           | AF !a !(I a) !(I a)
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
    pretty (OT l ty ty')           = located l$ "occurs check failed when unifying" <+> squotes (pretty ty) <+> "and" <+> squotes (pretty ty')
    pretty (OI l i j)              = located l$ "occurs check failed when unifying indices" <+> squotes (pretty i) <+> "and" <+> squotes (pretty j)
    pretty (OSh l s0 s1)           = located l$ "occurs check failed when unifying shapes" <+> squotes (pretty s0) <+> "and" <+> squotes (pretty s1)
    pretty (ExistentialArg ty)     = "Existential occurs as an argument in" <+> squotes (pretty ty)
    pretty (MF t t')               = "Failed to match" <+> squotes (pretty t) <+> "against type" <+> squotes (pretty t')
    pretty (MS sh sh')             = "Failed to match" <+> squotes (pretty sh) <+> "against shape" <+> squotes (pretty sh')
    pretty (MI f i i')             = pretty f <+> "Failed to match" <+> squotes (pretty i) <+> "against index" <+> squotes (pretty i')
    pretty (Doesn'tSatisfy l ty c) = located l$ squotes (pretty ty) <+> "is not a member of class" <+> pretty c
    pretty (NegIx l i)             = located l$ "negative index" <+> pretty i
    pretty (CV l t c)              = located l$squotes (pretty t) <+> "violates constraint" <+> pretty c
    pretty (AF l i i')             = located l$squotes (pretty i) <+> "is not an acceptable argument to a function expecting" <+> squotes (pretty i')

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
nIe l = IEV l <$> nI l

liftU :: UM a x -> TyM a x
liftU a = do
    i <- gets maxU
    (b, j) <- liftEither$runStateT a i
    setMaxU j $> b

wI iS (Subst t i sh) = Subst t (iS<>i) sh

mI :: F -> I a -> I a -> Either (TyE a) (Subst a)
mI f i0@(Ix _ i) i1@(Ix _ j) | i == j = Right mempty
                             | otherwise = Left $ MI f i0 i1
mI _ (IVar _ (Nm _ (U i) _)) ix = Right $ Subst IM.empty (IM.singleton i ix) IM.empty
mI _ ix (IVar _ (Nm _ (U i) _)) = Right $ Subst IM.empty (IM.singleton i ix) IM.empty
mI _ (IEV _ n) (IEV _ n') | n == n' = Right mempty
mI LF i0@IEV{} i1@IEV{} = Left $ MI LF i0 i1
mI LF i0@IEV{} i1@Ix{} = Left $ MI LF i0 i1
mI LF i0@Ix{} i1@IEV{} = Left $ MI LF i0 i1
mI _ Ix{} IEV{} = Right mempty
mI _ IEV{} IEV{} = Right mempty
mI f (StaPlus _ i (Ix _ iϵ)) (Ix l j) | j >= iϵ = mI f i (Ix l (j-iϵ))
mI f (Ix l iϵ) (StaPlus _ i (Ix _ j)) | iϵ >= j = mI f i (Ix l (iϵ-j))
mI f (StaPlus _ (Ix _ iϵ) i) (Ix l j) | j >= iϵ = mI f i (Ix l (j-iϵ))
mI f (Ix l iϵ) (StaPlus _ (Ix _ j) i) | iϵ >= j = mI f i (Ix l (iϵ-j))
mI f (StaPlus _ i j) (StaPlus _ i' j') = (<>) <$> mI f i i' <*> mI f j j' -- FIXME: stringent (TODO confessional error context)
mI f (StaMul _ i j) (StaMul _ i' j') = (<>) <$> mI f i i' <*> mI f j j' -- FIXME: stringent

mSh :: F -> Sh a -> Sh a -> Either (TyE a) (Subst a)
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
mSh _ sh sh'                        = Left $ MS sh sh'

-- I think 'focus' for match is ??
match :: (Typeable a, Pretty a) => T a -> T a -> Subst a
match t t' = either throw id (maM RF t t')

inv RF=LF; inv _=RF

maM :: F -> T a -> T a -> Either (TyE a) (Subst a)
maM f (Li n) (Li m)              = mI f m n
maM _ (IZ _ (Nm _ (U u) _)) I    = Right $ Subst (IM.singleton u I) IM.empty IM.empty
maM _ (IZ _ (Nm _ (U u) _)) F    = Right $ Subst (IM.singleton u F) IM.empty IM.empty
maM _ I I                        = Right mempty
maM _ F F                        = Right mempty
maM _ B B                        = Right mempty
maM _ (TV (Nm _ (U i) l) c) t    | Just e <- (l,t) `enforcesn't` c = Left e
                                 | otherwise = Right $ Subst (IM.singleton i t) IM.empty IM.empty
                                 -- FIXME invert focus
maM f (Arrow t0 t1) (Arrow t0' t1') = (<>) <$> maM (inv f) t0 t0' <*> maM f t1 t1' -- TODO: use <\> over <>
maM f (Arr sh t) (Arr sh' t')       = (<>) <$> mSh f sh sh' <*> maM f t t'
maM f (Arr sh t) t'                 = (<>) <$> mSh f sh Nil <*> maM f t t'
maM f (P ts) (P ts')                = mconcat <$> zipWithM (maM f) ts ts'
maM _ (Ρ n _) (Ρ n' _) | n == n'    = Right mempty
maM f (Ρ n rs) t@(Ρ _ rs') | IM.keysSet rs' `IS.isSubsetOf` IM.keysSet rs = iTS n t . mconcat <$> traverse (uncurry (maM f)) (IM.elems (IM.intersectionWith (,) rs rs'))
maM f (Ρ n rs) t@(P ts) | length ts >= fst (IM.findMax rs) = iTS n t . mconcat <$> traverse (uncurry (maM f)) [ (ts!!(i-1),tϵ) | (i,tϵ) <- IM.toList rs ]
maM _ t t'                          = Left $ MF t t'

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
(!>) _ ix@IEV{} = ix

(\-) :: Subst a -> Int -> Subst a
(\-) (Subst ts is ss) u = Subst (IM.delete u ts) is ss

s@@t= aT (void s) t

aT :: Subst a -> T a -> T a
aT s (Arr sh ty) = Arr (shSubst s sh) (aT s ty)
aT s (Arrow t₁ t₂) = Arrow (aT s t₁) (aT s t₂)
aT s (P ts) = P (aT s <$> ts)
aT s@(Subst ts _ _) (Ρ n rs) =
    let u = unU (unique n) in
    case IM.lookup u ts of
        Just ty@Ρ{}  -> aT (s\-u) ty
        Just ty@TV{} -> aT (s\-u) ty
        Just ty@IZ{} -> aT (s\-u) ty
        Just ty      -> aT s ty
        Nothing      -> Ρ n (aT s<$>rs)
aT s ty'@(TV n _) = aTi ty' n s
aT s ty'@(IZ _ n) = aTi ty' n s
aT _ ty = ty

-- don't need preferential substitution b/c that's how we pick keys already?
aTi ty' n s@(Subst ts _ _) =
    let u=unU$unique n in
    case IM.lookup u ts of
        Just ty@TV{} -> aT (s\-u) ty
        Just ty@IZ{} -> aT (s\-u) ty
        Just ty@Ρ{}  -> aT (s\-u) ty
        Just ty      -> aT s ty
        Nothing      -> ty'

runTyM :: Int -> TyM a b -> Either (TyE a) (b, Int)
runTyM i = fmap (second maxU) . flip runStateT (TySt i IM.empty IM.empty)

tickMaxU :: TyM a ()
tickMaxU = modify (\(TySt u l v) -> TySt (u+1) l v)

setMaxU :: Int -> TyM a ()
setMaxU i = modify (\(TySt _ l v) -> TySt i l v)

infixr 2 <~

(<~) :: Nm a -> T () -> TyM a ()
n <~ t = modify (\(TySt u l v) -> TySt u (insert n t l) v)

addPolyEnv :: Nm a -> T () -> TyM a ()
addPolyEnv n t = modify (\(TySt u l v) -> TySt u l (insert n t v))

nN :: T.Text -> b -> TyM a (Nm b)
nN n l = do {tickMaxU; st <- gets maxU; pure (Nm n (U st) l)}

ft :: T.Text -> b -> TyM a (T b)
ft n l = TV <$> nN n l <*> pure S.empty

fsh :: T.Text -> TyM a (Sh ())
fsh n = SVar <$> nN n ()

fc :: T.Text -> C -> TyM a (T ())
fc n c = do {nϵ <- nN n (); pure $ TV nϵ (S.singleton c)}

fz, fb, fo :: TyM a (T ())
fz = fc "a" IsZ; fb = fc "a" HasBits; fo = fc "o" IsOrd

fn :: Integer -> TyM a (T ())
fn n = IZ (Ix()$fromInteger n)<$>nN "n" ()

ftv :: T.Text -> TyM a (T ())
ftv n = ft n ()

fti :: T.Text -> TyM a (I ())
fti n = IVar () <$> nN n ()

ftie :: TyM a (I ())
ftie = IEV () <$> nN "n" ()

mapTySubst f (Subst t i sh) = Subst (f t) i sh

iTS n t = mapTySubst (insert n t)
uTS u n = mapTySubst (IM.insert u n)
iSh u sh s = s { sSubst = IM.insert u sh (sSubst s) }

data F = LF | RF | CF

instance NFData F where rnf=rwhnf

-- LF narrow among type argument types
-- CF check argument suitability
-- Φ branch out (conditionals)

instance Pretty F where pretty LF="⦠"; pretty RF="∢"; pretty CF="≬"

ctx'ize u is = u is `on` rwI.(is!>)

cuc = ctx'ize uc; nc = ctx'ize ni

uc :: ISubst a
   -> I a -- ^ supplied
   -> I a -- ^ argument accepted by function
   -> UM a (ISubst a)
uc s i0@(Ix l i) i1@(Ix _ j) | i==j = pure s
                             | otherwise = throwError $ AF l i0 i1
uc s i0@(IEV l n) i1@(IEV _ m) | n==m = pure s
                               | otherwise = throwError $ AF l i0 i1
uc s (IVar _ n0) (IVar _ n1) | n0==n1 = pure s
uc s ix@(IVar l (Nm _ (U i) _)) ix' | i `IS.member` occI ix' = throwError $ OI l ix ix'
                                    | otherwise = pure (IM.insert i ix' s)
uc s ix ix'@(IVar l (Nm _ (U i) _)) | i `IS.member` occI ix = throwError $ OI l ix' ix
                                    | otherwise = pure (IM.insert i ix s)
uc s i0@(StaPlus l i (Ix _ k)) i1@(Ix lk j) | j >= k = uc s i (Ix lk (j-k))
                                            | otherwise = throwError $ AF l i0 i1
uc s i0@(Ix l i) i1@(StaPlus _ j (Ix _ k)) | i >= k = uc s j (Ix l (i-k))
                                           | otherwise = throwError $ AF l i0 i1
uc s (StaPlus _ i0 i1) (StaPlus _ j0 j1) = do {s' <- uc s i0 j0; cuc s' i1 j1}
uc s (StaMul _ (Ix _ m) j) (Ix l n) | (k,0) <- n `quotRem` m = uc s j (Ix l k)
uc s (Ix l n) (StaMul _ (Ix _ m) j) | (k,0) <- n `quotRem` m = uc s (Ix l k) j
uc _ i0@(Ix l _) i1@IEV{} = throwError$AF l i0 i1
uc _ i0@(IEV l _) i1@Ix{} = throwError$AF l i0 i1
uc _ i0@(StaPlus l _ _) i1@IEV{} = throwError$AF l i0 i1
uc _ i0@(IEV l _) i1@StaPlus{} = throwError$AF l i0 i1
uc _ i0@(StaMul l _ _) i1@IEV{} = throwError$AF l i0 i1
uc _ i0@(IEV l _) i1@StaMul{} = throwError$AF l i0 i1

-- fan out (not necessary to propagate back)
φ :: ISubst a -> I a -> I a -> UM a (I a, ISubst a)
φ inp (Ix l _) Ix{} = do {m <- nIe l; pure (m, inp)}
φ inp (IEV l _) IEV{} = do {m <- nIe l; pure (m, inp)}
φ inp i@IEV{} Ix{} = pure (i, inp)
φ inp Ix{} j@IEV{} = pure (j, inp)
φ inp (IEV l _) StaPlus{} = (,inp) <$> nIe l
φ inp (IEV l _) StaMul{} = (,inp) <$> nIe l
φ inp (Ix _ i) (StaPlus _ (IVar _ n) (Ix l j)) | i>=j = let t=Ix l (i-j) in pure (t, insert n t inp)
φ inp (StaPlus _ (IVar _ n) (Ix l i)) (Ix _ j) | j>=i = let t=Ix l (j-i) in pure (t, insert n t inp)
φ inp i@(IVar _ n0) (IVar _ n1) | n0==n1 = pure (i, inp)
φ inp i0@(IVar l (Nm _ (U u) _)) i1 | u `IS.member` occI i1 = throwError$OI l i0 i1
                                    | otherwise = pure (i1, IM.insert u i1 inp)
φ inp i0 i1@(IVar l (Nm _ (U u) _)) | u `IS.member` occI i0 = throwError$OI l i0 i1
                                    | otherwise = pure (i0, IM.insert u i0 inp)
φ inp (StaPlus _ i0 (Ix _ n)) (StaPlus _ i1 (Ix _ m)) | n==m = φ inp i0 i1

-- not necessary to propagate substitutions back? (currently we do)
ni :: ISubst a -> I a -> I a -> UM a (I a, ISubst a)
ni inp i@(Ix _ n) (Ix _ m) | n==m = pure (i, inp)
ni inp i@(IVar _ n0) (IVar _ n1) | n0==n1 = pure (i, inp)
ni inp i0 i1@(IVar l (Nm _ (U u) _)) | u `IS.notMember` occI i0 = pure (i0, IM.insert u i0 inp)
                                     | otherwise = throwError$OI l i0 i1
ni inp i0@(IVar l (Nm _ (U u) _)) i1 | u `IS.notMember` occI i1 = pure (i1, IM.insert u i1 inp)
                                     | otherwise = throwError$OI l i0 i1
ni inp (StaPlus _ (IVar l n) (Ix _ i)) (Ix _ j) | j>=i = let t=Ix l (j-i) in pure (t, insert n t inp)
ni inp (Ix _ i) (StaPlus _ (IVar l n) (Ix _ j)) | i>=j = let t=Ix l (i-j) in pure (t, insert n t inp)
ni inp (StaPlus l i₀ j₀) (StaPlus _ i₁ j₁) = do
    (i',s) <- ni inp i₀ i₁
    (j',s') <- nc s j₀ j₁
    pure (StaPlus l i' j', s')
ni inp i@(IEV _ n0) (IEV _ n1) | n0==n1 = pure (i, inp)
ni inp (Ix l n) (StaMul _ (Ix _ m) i) | (k,0) <- n `quotRem` m = ni inp (Ix l k) i
ni inp (StaMul _ (Ix _ n) i) (Ix l m) | (k,0) <- n `quotRem` m = ni inp i (Ix l k)
ni s (StaMul l n mi@(Ix l₀ m)) (StaPlus _ i (Ix l₁ j)) = do
    k <- IVar l <$> nI l
    (_,s0) <- ni s n (k+:Ix l₀ (c`div`m))
    (_,s1) <- nc s0 i (StaMul l₀ mi k+:Ix l₁ (c-j))
    pure (StaMul l mi k+:Ix l₀ c, s1)
  where
    c=lcm m j
-- n*m, i+j, (m,j known) then must be divisible by m and >=j
-- unify to m*k+lcm(m,j)
-- Then n=k+(lcm(m,j)/m), i=m*k+(lcm(m,j)-j)

mguI :: F -> ISubst a -> I a -> I a -> UM a (I a, ISubst a)
mguI CF = \s i0 i1 -> (i0,)<$>cuc s i0 i1; mguI RF = φ; mguI LF = ni

splitFromLeft :: Int -> [a] -> ([a], [a])
splitFromLeft n xs | nl <- length xs = splitAt (nl-n) xs

mgShPrep :: F -> a -> Subst a -> Sh a -> Sh a -> UM a (Sh a, Subst a)
mgShPrep f l s = mgSh f l s `on` shSubst s.rwSh

mgSh :: F -> a -> Subst a -> Sh a -> Sh a -> UM a (Sh a, Subst a)
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
    mgSh f l inp sh (iroll$reverse is)
mgSh f l inp sh (Rev sh') | (is, Nil) <- unroll sh =
    mgSh f l inp (iroll$reverse is) sh'
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
    sh <- vx<$>nIe l
    (_, s') <- mgSh f l inp sh sh0
    (_, s'') <- mgShPrep f l s' sh sh1
    pure (sh, s'')
mgSh f l inp sh0@Π{} sh1@Rev{} = do
    sh <- vx<$>nIe l
    (_, s') <- mgSh f l inp sh sh0
    (_, s'') <- mgShPrep f l s' sh sh1
    pure (sh, s'')
mgSh f l s sh0@Cons{} sh1@(Cat shh shϵ) | (is, Nil) <- unroll sh0, (isϵ, Nil) <- unroll shϵ, n <- length is, nϵ <- length isϵ =
    if n<nϵ
      then throwError $ USh l sh0 sh1
      else let (ish, isϵ') = splitFromLeft (n-nϵ) is
           in do
              (_, s0) <- mgSh f l s shh (ish<||Nil)
              (_, s1) <- mgShPrep f l s0 (isϵ'<||Nil) shϵ
              pure (sh0, s1)
mgSh f l s sh0@Cat{} sh1@Cons{} = mgSh f l s sh1 sh0
mgSh _ l _ sh0@Cons{} sh1 = throwError $ UShD l sh0 sh1
mgSh _ l _ sh0 sh1@Cons{} = throwError $ UShD l sh0 sh1
mgSh _ l _ sh0@Π{} sh1@Cat{} = throwError $ UShD l sh0 sh1
mgSh _ l _ sh0@Cat{} sh1@Π{} = throwError $ UShD l sh0 sh1
mgSh _ l _ sh0@Rev{} sh1@Cat{} = throwError $ UShD l sh0 sh1
mgSh _ l _ sh0@Cat{} sh1@Rev{} = throwError $ UShD l sh0 sh1
-- TODO: confessional context (error messages)

mguPrep :: F -> (a, E a) -> Subst a -> T a -> T a -> UM a (T a, Subst a)
mguPrep f l s t0 t1 =
    let t0' = aT s t0
        t1' = aT s t1
    in mgu f l s ({-# SCC "rwArr" #-} rwArr t0') ({-# SCC "rwArr" #-} rwArr t1')

mp :: (a, E a) -> F -> Subst a -> T a -> T a -> UM a (Subst a)
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
occI IEV{}           = IS.empty

occ :: T a -> IS.IntSet
occ (TV n _)     = Nm.singleton n
occ (IZ _ n)     = Nm.singleton n
occ (Arrow t t') = occ t <> occ t'
occ (Arr _ a)    = occ a -- shouldn't need shape
occ I            = IS.empty
occ F            = IS.empty
occ B            = IS.empty
occ Li{}         = IS.empty
occ (P ts)       = occ @<> ts
occ (Ρ n rs)     = Nm.insert n $ occ @<> rs

scalar f (l,_) s n = snd <$> mgSh f l s n Nil

scalarStep f l s n t t' = do {s'<- scalar f l s n; mguPrep f l s' t t'}

σ RF (Li IEV{}) = I; σ RF (IZ IEV{} t) = TV t (S.singleton IsZ); σ _ t = t

φv (n0,c0) (n1,c1) s = do {n <- nI (loc n0); let t=TV n (c0<>c1) in pure (t, iTS n0 t$iTS n1 t s)}

mgu :: F -> (a, E a) -> Subst a -> T a -> T a -> UM a (T a, Subst a)
mgu RF l s (Arrow t0 t1) (Arrow t0' t1') = do
    (t0'',s0) <- mgu LF l s t0 t0'
    (t1'',s1) <- mguPrep RF l s0 t1 t1'
    pure (Arrow t0'' t1'', s1)
mgu CF l s (Arrow t0 t1) (Arrow t0' t1') = do
    (t0'', s0) <- mgu CF l s t0' t0
    (t1'', s1) <- mguPrep CF l s0 t1 t1'
    pure (Arrow t0'' t1'', s1)
mgu _ _ s I I = pure (I, s)
mgu _ _ s F F = pure (F, s)
mgu _ _ s B B = pure (B, s)
-- FIXME: frange allowed have I argument; I is Li #n
-- but also this should be rewritten
-- (∃a. t a) → r ≡ ∀a. t a → r
-- mgu LF (l,e) _ t@Li{} I = throwError $ UF l e t I
-- mgu LF (l,e) _ I t@Li{} = throwError $ UF l e I t
mgu _ _ s Li{} I = pure (I, s)
mgu _ _ s I Li{} = pure (I, s)
-- mgu f (l,_) s (Li i) I = do {n <- nIe l; (i',is) <- mguI f (iSubst s) i n; pure (σ f$Li i',wI is s)}
-- mgu f (l,_) s I (Li i) = do {n <- nIe l; (i',is) <- mguI f (iSubst s) i n; pure (σ f$Li i',wI is s)}
-- IZ always from literals
mgu _ _ s (IZ _ n) I = pure (I, iTS n I s)
mgu _ _ s I (IZ _ n) = pure (I, iTS n I s)
mgu _ _ s (IZ _ n) F = pure (F, iTS n F s)
mgu _ _ s F (IZ _ n) = pure (F, iTS n F s)
mgu _ _ s I (TV n _) = pure (I, iTS n I s)
mgu _ _ s (TV n _) I = pure (I, iTS n I s)
mgu _ (l,_) s t@(TV n c) F = if HasBits `S.member` c then throwError$Doesn'tSatisfy l t HasBits else pure (F, iTS n F s)
mgu _ (l,_) s F t@(TV n c) = if HasBits `S.member` c then throwError$Doesn'tSatisfy l t HasBits else pure (F, iTS n F s)
mgu _ _ s t@(IZ (Ix _ i0) n0) (IZ (Ix _ i1) n1) | i0==i1&&n0==n1 = pure (t, s)
mgu f _ s (Li i0) (Li i1) = do {(i', iS) <- mguI f (iSubst s) i0 i1; pure (σ f$Li i', wI iS s)}
                              -- constraints arise from >, +, &. so we should not propagate index constraints
mgu f _ s (Li i0) (IZ i1 n) = do {(i',iS) <- mguI f (iSubst s) i0 i1; let t=σ f$Li i' in pure (t, iTS n t$wI iS s)}
mgu f _ s (IZ i0 n0) (Li i1) = do {(i',iS) <- mguI f (iSubst s) i0 i1; let t=σ f$Li i' in pure (t, iTS n0 t$wI iS s)}
mgu _ _ s (TV n c) t1@Li{} = if S.null c then pure (t1, iTS n t1 s) else pure (I, iTS n I s)
mgu _ _ s t0@Li{} (TV n c) = if S.null c then pure (t0, iTS n t0 s) else pure (I, iTS n I s)
mgu f _ s (IZ i0 n0) (IZ i1 n1) | n0/=n1 = do {(i',iS) <- mguI f (iSubst s) i0 i1; let t=σ f$IZ i' n0 in pure (t, iTS n1 t$wI iS s)}
-- TODO: if C HasBits, force I (Li (?))
mgu _ _ s t0@(IZ _ n0) (TV n1 c) | n0/=n1 = if S.null c then pure (t0, iTS n1 t0 s) else let t=TV n1 (S.insert IsZ c) in pure (t, iTS n0 t s)
                                 | otherwise = error "unexpected."
mgu _ _ s (TV n0 c) t1@(IZ _ n1) | n0/=n1 = if S.null c then pure (t1, iTS n0 t1 s) else let t=TV n0 (S.insert IsZ c) in pure (t, iTS n1 t s)
                                 | otherwise = error "unexpected."
-- FIXME ug. is higher-rank on indices 😬
-- "LF" for universal variables should be for function argument (à la ug.)... go with the type var
mgu _ _ s (TV n c) (TV n' c') | n == n' = do {m <- nI (loc n); let t'=TV m (c<>c') in pure (t', iTS n t' s)}
mgu _ _ s t@(TV n0 c) t'@(TV n1 c')
    | c' `S.isSubsetOf` c = pure (t, iTS n1 t s)
    | c `S.isSubsetOf` c' = pure (t', iTS n0 t' s)
    | otherwise = φv (n0,c) (n1,c') s
mgu f l s (TV n c) (Arr i (TV n' c')) | n'==n = scalar f l s i >>= φv (n,c) (n',c')
mgu f l s (Arr i (TV n c)) (TV n' c') | n'==n = scalar f l s i >>= φv (n,c) (n',c')
mgu f l s (TV n c) (Arr i (TV n' c')) | IsZ `S.member` c = scalar f l s i >>= φv (n,c) (n',c')
mgu f l s (Arr i (TV n c)) (TV n' c') | IsZ `S.member` c' = scalar f l s i >>= φv (n,c) (n',c')
mgu _ (l,_) s t'@(TV (Nm _ (U i) _) c) t | i `IS.member` occ t = throwError $ OT l t' t
                                         | otherwise = case (l,t) `satisfiesn't` c of Nothing -> pure (t, uTS i t s); Just e -> throwError e
mgu _ (l,_) s t t'@(TV (Nm _ (U i) _) c) | i `IS.member` occ t = throwError $ OT l t' t
                                         | otherwise = case (l,t) `satisfiesn't` c of Nothing -> pure (t, uTS i t s); Just e -> throwError e
mgu _ (l,e) _ t0@Arrow{} t1 = throwError $ UF l e t0 t1
mgu _ (l,e) _ t0 t1@Arrow{} = throwError $ UF l e t0 t1
-- TODO: if t' is a TV, it could be an array (sh could eat sh'++sh part of t')
mgu f l s (Arr sh t) (Arr sh' t') = do
    (t'', s0) <- mgu f l s t t'
    (sh'', s1) <- mgShPrep f (fst l) s0 sh sh'
    pure (Arr sh'' t'', s1)
mgu f l s (Arr n t) F = scalarStep f l s n t F
mgu f l s (Arr n t) I = scalarStep f l s n t I
mgu f l s F (Arr n t) = scalarStep f l s n F t
mgu f l s I (Arr n t) = scalarStep f l s n I t
mgu f l s (Arr n t) B = scalarStep f l s n t B
mgu f l s B (Arr n t) = scalarStep f l s n B t
mgu f l s (Arr n t) t'@P{} = scalarStep f l s n t t'
mgu f l s t'@P{} (Arr n t) = scalarStep f l s n t' t
mgu f l s (Arr n t) t'@Ρ{} = scalarStep f l s n t t'
mgu f l s t'@Ρ{} (Arr n t) = scalarStep f l s n t' t
mgu f l s (Arr n t) t'@Li{} = scalarStep f l s n t t'
mgu f l s t'@Li{} (Arr n t) = scalarStep f l s n t' t
mgu f l s (Arr n t) t'@IZ{} = scalarStep f l s n t t'
mgu f l s t'@IZ{} (Arr n t) = scalarStep f l s n t' t
mgu f l s (P ts) (P ts') | length ts == length ts' = first P <$> zSt (mguPrep f l) s ts ts'
mgu f l@(lϵ, e) s t@(Ρ (Nm _ (U j) _) rs) t'@(P ts) | j `IS.member` (occ@<>ts) = throwError $ OT lϵ t t'
                                                    | length ts >= fst (IM.findMax rs) && fst (IM.findMin rs) > 0 = first P <$> tS (\sϵ (i, tϵ) -> second (uTS j t') <$> mguPrep f l sϵ (ts!!(i-1)) tϵ) s (IM.toList rs)
                                                    | otherwise = throwError $ UF lϵ e t t'
mgu f l s t@P{} t'@Ρ{} = mgu f l s t' t
mgu _ l@(lϵ,_) s t@(Ρ (Nm _ (U i) x) rs) t'@(Ρ (Nm _ (U j) _) rs') | i `IS.notMember` (occ@<>rs') && j `IS.notMember` (occ@<>rs) = do
    (_, rss) <- tS (\sϵ (t0,t1) -> mguPrep LF l sϵ t0 t1) s $ IM.elems $ IM.intersectionWith (,) rs rs'
    n<-nI x
    let t''=Ρ n (rs<>rs')
    pure (t'', uTS i t'$uTS j t'' rss)
                                                                   | otherwise = throwError $ OT lϵ t t'
mgu _ (l,e) _ t0@Ρ{} t1 = throwError $ UF l e t0 t1
mgu _ (l,e) _ t0 t1@Ρ{} = throwError $ UF l e t0 t1
mgu _ (l,e) _ t0@Li{} t1 = throwError $ UF l e t0 t1
mgu _ (l,e) _ t0 t1@Li{} = throwError $ UF l e t0 t1
mgu _ (l,e) _ t0@IZ{} t1 = throwError $ UF l e t0 t1
mgu _ (l,e) _ t0 t1@IZ{} = throwError $ UF l e t0 t1
mgu _ (l,e) _ B t1 = throwError $ UF l e B t1
mgu _ (l,e) _ t0 B = throwError $ UF l e t0 B
mgu _ (l,e) _ F t1 = throwError $ UF l e F t1
mgu _ (l,e) _ t0 F = throwError $ UF l e t0 F
mgu _ (l,e) _ I t1 = throwError $ UF l e I t1
mgu _ (l,e) _ t0 I = throwError $ UF l e t0 I
mgu _ (l,e) _ t0@P{} t1 = throwError $ UF l e t0 t1

zSt _ s [] _           = pure ([], s)
zSt _ s _ []           = pure ([], s)
zSt op s (x:xs) (y:ys) = do{(t, next) <- op s x y; first (t:) <$> zSt op next xs ys}

zS _ s [] _           = pure s
zS _ s _ []           = pure s
zS op s (x:xs) (y:ys) = do {next <- op s x y; zS op next xs ys}

tS :: Monad m => (Subst a -> b -> m (x, Subst a)) -> Subst a -> [b] -> m ([x], Subst a)
tS _ s []     = pure ([], s)
tS f s (t:ts) = do{(tϵ, next) <- f s t; first (tϵ:) <$> tS f next ts}

vx = (<| Nil)
vV i = Arr (vx i)

tyNumBinOp :: TyM a (T (), Subst a)
tyNumBinOp = do {n<-fz; pure (n~>n~>n, mempty)}

mm, tyBoo, tyOrdBinRel, tyEqBinRel :: TyM a (T (), Subst a)
mm = do {n <- fo;  pure (n ~> n ~> n, mempty)}
tyBoo = do {n <- fb; pure (n ~> n ~> n, mempty)}
tyOrdBinRel = do {n <- fo; pure (n ~> n ~> B, mempty)}
tyEqBinRel = do {n <- fc "e" IsEq; pure (n ~> n ~> B, mempty)}

del, sel :: [Int] -> Sh a -> Sh a
sel axes sh = iroll (fmap snd (filter ((`elem` axes) . fst) (zip [1..] unrolled))) where
    (unrolled, _) = unroll sh

tydrop :: Int -> Sh a -> Sh a
tydrop 0 sh            = sh
tydrop n (_ `Cons` sh) = tydrop (n-1) sh

del axes sh = roll t (fmap snd (filter ((`notElem` axes) . fst) (zip [1..] unrolled))) where
    (unrolled, t) = unroll sh

trim :: Sh a -> Sh a
trim = iroll . fst . unroll

iunroll (Cons i Nil) = Just i
iunroll (Cons i shϵ) = StaMul (ia i) i <$> iunroll shϵ
iunroll _            = Nothing

unroll (Cons i shϵ) = first (i :) $ unroll shϵ
unroll s            = ([], s)

iroll :: [I a] -> Sh a
iroll = roll Nil

roll :: Sh a -> [I a] -> Sh a
roll = foldr Cons

i <|| sh = foldr Cons sh i

tyB :: a -> Builtin -> TyM a (T (), Subst a)
tyB _ Floor = pure (F ~> I, mempty); tyB _ Ceil = pure (F ~> I, mempty); tyB _ ItoF = pure (I ~> F, mempty)
tyB _ Even = pure (I ~> B, mempty); tyB _ Odd = pure (I ~> B, mempty)
tyB _ Sr = pure (I ~> I ~> I, mempty); tyB _ Sl = pure (I ~> I ~> I, mempty)
tyB _ R = do
    n <- fz; sh <- fsh "sh"
    pure (n ~> n ~> Arr sh n, mempty)
tyB _ ConsE = do
    a <- ftv "a"; i <- fti "i"
    pure (a ~> vV i a ~> vV (i+:Ix()1) a, mempty)
tyB l Snoc = tyB l ConsE
tyB _ Sort = do {o <- fo; i <- fti "i"; pure (vV i o~>vV i o, mempty)}
tyB _ A1 = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (Arr (i <| sh) a ~> I ~> Arr sh a, mempty)
tyB _ I1 = do
    a <- ftv "a"; i <- fti "i"; n <- fti "n"; sh <- fsh "sh"
    pure (vV n I ~> Arr (i <| sh) a ~> Arr (n <| sh) a, mempty)
tyB _ IOf = do
    a <- ftv "a"; i <- fti "i"
    pure ((a ~> B) ~> vV i a ~> I, mempty)
tyB _ Di = do
    a <- ftv "a"; i <- fti "i"
    pure (Arr (i <| i <| Nil) a ~> vV i a, mempty)
tyB _ LastM = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (Arr (i <| sh) a ~> Arr sh a, mempty)
tyB _ Head = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (Arr ((i+:Ix()1) <| sh) a ~> Arr sh a, mempty)
tyB l Last = tyB l Head
tyB _ Init = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (Arr ((i+:Ix()1) <| sh) a ~> Arr (i <| sh) a, mempty)
tyB _ InitM = do
    a <- ftv "a"; i <- fti "i"; n <- ftie; sh <- fsh "sh"
    pure (Arr (i <| sh) a ~> Arr (n <| sh) a, mempty)
tyB l Tail = tyB l Init
tyB _ Take = do
    a <- ftv "a"; k <- fti "k"; n <- ftie; sh <- fsh "sh"
    pure (I ~> Arr (k <| sh) a ~> Arr (n <| sh) a, mempty)
tyB _ Drop = do
    a <- ftv "a"; k <- fti "k"; n <- ftie; sh <- fsh "sh"
    pure (I ~> Arr (k <| sh) a ~> Arr (n <| sh) a, mempty)
tyB _ Del = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (Arr ((i+:Ix()1) <| sh) a ~> I ~> Arr (i <| sh) a, mempty)
tyB _ DelM = do
    a <- ftv "a"; i <- fti "i"; n <- ftie; sh <- fsh "sh"
    pure (Arr (i <| sh) a ~> I ~> Arr (n <| sh) a, mempty)
tyB _ Ix'd = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (Arr (i <| sh) a ~> vV i I, mempty)
tyB _ TailM = do
    a <- ftv "a"; i <- fti "i"; n <- ftie; sh <- fsh "sh"
    pure (Arr (i <| sh) a ~> Arr (n <| sh) a, mempty)
tyB _ Rot = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (I ~> Arr (i <| sh) a ~> Arr (i <| sh) a, mempty)
tyB _ Cyc = do
    sh <- fsh "sh"; a <- ftv "a"; i <- fti "i"; n <- fti "n"
    pure (Arr (i <| sh) a ~> Li n ~> Arr (StaMul() n i <| sh) a, mempty)
tyB _ HeadM = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    pure (Arr (i <| sh) a ~> Arr sh a, mempty)
tyB _ Re = do
    a <- ftv "a"; n <- fti "n"
    pure (a ~> Li n ~> vV n a, mempty)
tyB _ FRange = do {n <- fti "n"; pure (F ~> F ~> Li n ~> vV n F, mempty)}
-- could require m>0 then k>0
tyB _ Fib = do
    n <- fti "n"; m <- fti "m"; k <- fti "k"; a <- ftv "a"
    pure (vV m a ~> (vV k a ~> a) ~> Li n ~> vV (m+:n) a, mempty)
tyB _ Io = do {n <- fti "n"; pure (Li n ~> vV (n+:Ix()1) I, mempty)}
tyB _ Range = do {n <- ftie; pure (I ~> I ~> vV n I, mempty)}
tyB _ Plus = tyNumBinOp; tyB _ Minus = tyNumBinOp
tyB _ Times = tyNumBinOp
tyB _ Dot = do
    n <- fz; i <- fti "i"
    pure (vV i n ~> vV i n ~> n, mempty)
    -- FIXME: need to do something like Z type... eugh
tyB _ Gte = tyOrdBinRel; tyB _ Gt = tyOrdBinRel; tyB _ Lt = tyOrdBinRel
tyB _ Lte = tyOrdBinRel; tyB _ Eq = tyEqBinRel; tyB _ Neq = tyEqBinRel
tyB _ And = tyBoo; tyB _ Or = tyBoo; tyB _ Xor = tyBoo
tyB _ N = do {n <- fb; pure (n ~> n, mempty)}
tyB _ Min = mm; tyB _ Max = mm
tyB _ IntExp = do {n <- fz; pure (n~>I~>n, mempty)}
tyB _ Neg = do {n <- fz; pure (n~>n, mempty)}; tyB _ Abs = do {n <- fz; pure (n~>n, mempty)}
tyB _ Sqrt = pure (F~>F, mempty); tyB _ Log = pure (F~>F, mempty)
tyB _ Div = pure (F~>F~>F, mempty); tyB _ Mod = pure (I~>I~>I, mempty)
tyB _ Exp = pure (F~>F~>F, mempty); tyB _ IDiv = pure (I~>I~>I, mempty)
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
    pure (Arr sh a ~> Arr (Ix()1 <| sh) a, mempty)
tyB _ CatE = do
    i <- fti "i"; j <- fti "j"
    n <- ftv "a"
    pure (vV i n ~> vV j n ~> vV (i+:j) n, mempty)
tyB _ Scan = do
    a <- ftv "a"; i <- fti "i"; sh <- fsh "sh"
    let arrTy = Arr ((i+:Ix() 1) <| sh) a
    pure ((a ~> a ~> a) ~> arrTy ~> arrTy, mempty)
tyB _ ScanS = do
    a <- ftv "a"; b <- ftv "b"
    i <- fti "i"; sh <- fsh "sh"
    let rarrTy = Arr ((i+:Ix()1) <| sh)
    pure ((b~>a~>b) ~> b ~> Arr (i <| sh) a ~> rarrTy b, mempty)
tyB l (DI n) = tyB l (Conv [(n,Just 1)])
tyB _ (Conv as) = do
    sh <- fsh "sh"
    is <- zipWithM (\_ t -> fti (T.singleton t)) ns ['i'..]
    a <- ftv "a"; b <- ftv "b"
    let nx = Ix () <$> ns
        opTy = Arr (nx <|| sh) a ~> b
        t = Arrow (Arr (zipWith3 (\dϵ iϵ n -> StaMul () dϵ (iϵ+:n)) dix is nx <|| sh) a) (Arr (((+:Ix()1)<$>is) <|| Nil) b)
    pure (opTy ~> t, mempty)
  where (ns,ds) = unzip as; dix=Ix ().fromMaybe 1<$>ds
tyB _ (Focus ns) = do
    sh <- fsh "sh"
    is <- zipWithM (\_ t -> fti (T.singleton t)) ns ['i'..]
    a <- ftv "a"; b <- ftv "b"
    let nx = map (Ix ()) ns
        opTy = Arr (nx <|| sh) a ~> b
        t = Arr (zipWith (StaMul ()) nx is <|| sh) a ~> Arr (is <|| Nil) b
    pure (opTy~>t, mempty)
tyB _ Succ = do
    i <- fti "i"; sh <- fsh "sh"
    a <- ftv "a"; b <- ftv "b"
    let opTy = a ~> (a ~> b)
    pure (opTy ~> (Arr ((i+:Ix () 1) <| sh) a ~> Arr (i <| sh) b), mempty)
tyB _ (TAt i) = do
    ρ <- nN "ρ" ()
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
    shs <- traverse (\(i,ax) -> do {is <- ixN (maybe i maximum ax); sh <- fsh "sh"; pure (is <|| sh)}) as
    vs <- zipWithM (\_ c -> ftv (T.singleton c)) as ['a'..]
    codSh <- fsh "sh"
    cod <- ftv "c"
    let mArrs = zipWith Arr shs vs
        codTy = Arr codSh cod
        fTy = foldr (~>) cod $ zipWith3 (\ax sh t -> case ax of {(_,Nothing) -> Arr (trim sh) t;(_,Just axs) -> Arr (sel axs sh) t}) as shs vs
        rTy = foldr (~>) codTy mArrs
        shsU = zipWith (\ax sh -> case ax of {(n,Nothing) -> tydrop n sh;(_,Just axs) -> del axs sh}) as shs
        shUHere sh sh' = fmap snd (liftU $ mgShPrep RF l mempty (sh$>l) (sh'$>l))
    s <- zipWithM shUHere shsU (tail shsU++[codSh])
    pure (fTy ~> rTy, mconcat s)
tyB _ Fold = do
    i <- fti "i"; sh <- fsh "sh"; a <- ftv "a"
    let sh1 = (i+:Ix()1) <| sh
    pure ((a ~> a ~> a) ~> Arr sh1 a ~> Arr sh a, mempty)
tyB _ FoldS = do
    i <- fti "i"; sh <- fsh "sh"; a <- ftv "a"; b <- ftv "b"
    pure ((a ~> b ~> b) ~> b ~> Arr (i <| sh) a ~> Arr sh b, mempty)
tyB _ Foldl = do
    ix <- fti "i"; sh <- fsh "sh"; a <- ftv "a"; b <- ftv "b"
    pure ((b ~> a ~> b) ~> b ~> Arr (ix <| sh) a ~> Arr sh b, mempty)
tyB _ FoldA = do
    sh <- fsh "sh"; a <- ftv "a"
    pure ((a ~> a ~> a) ~> a ~> Arr sh a ~> a, mempty)
tyB _ Dim = do
    iV <- fti "i"; shV <- fsh "sh"; a <- ftv "a"
    pure (Arr (iV <| shV) a ~> Li iV, mempty)
tyB _ RevE = do
    iV <- fti "i"; shV <- fsh "sh"; a <- ftv "a"
    let aTy = Arr (iV <| shV) a
    pure (aTy ~> aTy, mempty)
tyB _ Size = do
    shV <- fsh "sh"; a <- ftv "a"
    pure (Arr shV a ~> I, mempty)
tyB _ Gen = do
    a <- ftv "a"; n <- fti "n"
    pure (a ~> (a ~> a) ~> Li n ~> vV n a, mempty)
tyB _ Ug = do
    a <- ftv "a"; b <- ftv "b"; n <- fti "n"
    pure ((b ~> P [b,a]) ~> b ~> Li n ~> vV n a, mempty)
tyB _ Mul = do
    a <- fz; i <- fti "i"; j <- fti "j"; k <- fti "k"
    pure (Arr (i <| j <| Nil) a ~> Arr (j <| k <| Nil) a ~> Arr (i <| k <| Nil) a, mempty)
tyB _ VMul = do
    a <- fz; i <- fti "i"; j <- fti "j"
    pure (Arr (i <| j <| Nil) a ~> vV j a ~> vV i a, mempty)
tyB _ Sin = pure (F ~> F, mempty)
tyB _ Cos = pure (F ~> F, mempty)
tyB _ Tan = pure (F ~> F, mempty)
tyB _ Ices = do
    a <- ftv "a"; i <- fti "i"; n <- ftie
    pure ((a ~> B) ~> vV i a ~> vV n I, mempty)
tyB _ Filt = do
    a <- ftv "a"; i <- fti "i"; n <- ftie
    pure ((a ~> B) ~> vV i a ~> vV n a, mempty)
tyB _ Part = do
    a <- ftv "a"; i <- fti "i"; n <- ftie; m <- ftie
    pure ((a ~> B) ~> vV i a ~> P [vV n a, vV m a], mempty)
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

cl :: T b -> TyM a (T b)
cl t = do {i<- gets maxU; let (u,t') = cloneT i t in setMaxU u $> t'}

rwI :: I a -> I a
rwI (StaPlus l i0 i1) =
    case (rwI i0, rwI i1) of
        (Ix lϵ i, Ix _ j) -> Ix lϵ (i+j)
        (i0', i1')        -> StaPlus l i0' i1'
rwI (StaMul l i0 i1) =
    case (rwI i0, rwI i1) of
        (i, Ix _ 1)       -> i
        (Ix _ 1, i)       -> i
        (Ix lϵ i, Ix _ j) -> Ix lϵ (i*j)
        (i0', i1'@Ix{})   -> StaMul l i1' i0'
        (i0', i1')        -> StaMul l i0' i1'
rwI i = i

rwSh :: Sh a -> Sh a
rwSh s@SVar{}     = s
rwSh s@Nil        = s
rwSh (i `Cons` s) = rwI i <| rwSh s
rwSh (Cat s0 s1) | (is, Nil) <- unroll (rwSh s0), (js, Nil) <- unroll (rwSh s1) = roll Nil (is++js)
                 | otherwise = Cat (rwSh s0) (rwSh s1)
rwSh (Rev s) | (is, Nil) <- unroll (rwSh s) = roll Nil (reverse is)
             | otherwise = Rev (rwSh s)
rwSh (Π s) | Nil <- rwSh s = Nil
rwSh (Π s) | Just i <- iunroll (rwSh s) = rwI i <| Nil
           | otherwise = Π (rwSh s)

rwArr :: T a -> T a
rwArr (Arrow t t')  = Arrow (rwArr t) (rwArr t')
rwArr I             = I
rwArr B             = B
rwArr F             = F
rwArr t@Li{}        = t
rwArr t@TV{}        = t
rwArr t@IZ{}        = t
rwArr (P ts)        = P (rwArr<$>ts)
rwArr (Arr sh t)    | Nil <- rwSh sh = rwArr t
rwArr (Arr ixes arr) | (is, Nil) <- unroll (rwSh ixes), Arr sh t <- rwArr arr = Arr (roll sh is) t
rwArr (Arr sh t) | Arr shϵ t' <- rwArr t = Arr (rwSh$Cat sh shϵ) t'
rwArr (Arr sh t)   = Arr (rwSh sh) (rwArr t)
rwArr (Ρ n fs)     = Ρ n (rwArr<$>fs)

hasEI :: I a -> Bool
hasEI IEV{}              = True
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

enforcesn't :: (a, T a) -> S.Set C -> Maybe (TyE a)
enforcesn't (l,t@(TV _ cϵ)) c = if c `S.isSubsetOf` cϵ then Nothing else Just (CV l t (S.findMin c))
enforcesn't (l,t@IZ{}) c      = if HasBits `S.member` c then Just$CV l t HasBits else Nothing
enforcesn't x c               = x `satisfiesn't` c

satisfiesn't :: (a, T a) -- ^ Not a type variable
             -> S.Set C -> Maybe (TyE a)
satisfiesn't (l,t) = listToMaybe . mapMaybe s . S.toList
    where s = case t of
            I       -> const Nothing
            Li{}    -> const Nothing
            B       -> const Nothing
            F       -> \case {HasBits -> Just$Doesn'tSatisfy l t HasBits; _ -> Nothing}
            P{}     -> \case {HasBits -> Just$Doesn'tSatisfy l t HasBits; _ -> Nothing}
            Arr{}   -> \case {IsZ -> Just$Doesn'tSatisfy l t IsZ; _ -> Nothing}
            Arrow{} -> Just . Doesn'tSatisfy l t

tyClosed :: Int -> E a -> Either (TyE a) (E (T ()), Int)
tyClosed u e = do
    (eS, i) <- runTyM u (do {(e', s) <- tyE mempty e; pure (rwArr.(s@@)<$>e')})
    chkE (eAnn eS) $> (eS, i)

tyE :: Subst a -> E a -> TyM a (E (T ()), Subst a)
tyE s (EApp _ (EApp _ (Builtin l Range) lb) ub) = do
    (lbϵ,s0) <- tyE s lb; (ubϵ,s1) <- tyE s0 ub
    let lbTy0=eAnn lbϵ; ubTy0=eAnn ubϵ
        iLoc sϵ t lϵ = second void$iv sϵ (aT sϵ (t$>eAnn lϵ)); x=eAnn lb
        (s3,lbTy) = iLoc s1 lbTy0 lb; (s4,ubTy) = iLoc s3 ubTy0 ub
    (m,s5) <- case (lbTy, ubTy) of
        (Li (Ix _ lbi), Li (Ix _ ubi)) -> do
            let m=ubi-lbi+1
            when (m<0) $ throwError (NegIx l m)
            pure (Ix () m,s4)
        (Li (Ix _ 0), TV n _) -> do
            k <- fti "n"
            pure (k+:Ix()1, iTS n (Li$k$>x) s4)
        _ -> (,s4)<$>ftie
    let arrTy = vV m I
    pure (EApp arrTy (EApp (ubTy0 ~> arrTy) (Builtin (lbTy0 ~> ubTy0 ~> arrTy) Range) lbϵ) ubϵ, s5)
  where iv sϵ (IZ i nm)   = let t=Li i in (iTS nm t sϵ, t)
        iv sϵ t@(TV nm _) = (iTS nm I sϵ, t) -- int satisfies all constraints
        iv sϵ _           = (sϵ, I)
tyE s (FLit _ x) = pure (FLit F x, s)
tyE s (BLit _ x) = pure (BLit B x, s)
tyE s (ILit _ m) = do {n <- fn m; pure (ILit n m, s)}
tyE s (Builtin l b) = do {(t,sϵ) <- tyB l b ; pure (Builtin t b, sϵ<>s)}
tyE s (Lam _ nϵ e) = do
    n <- ftv "a"
    nϵ <~ n
    (e', s') <- tyE s e
    pure (Lam (n~>eAnn e') (nϵ { loc = n }) e', s')
tyE s (Let _ (n, b) e) = do
    (b', s') <- tyE s b
    let t = eAnn b'
    n <~ s'@@t
    (e', s'') <- tyE s' e
    pure (Let (eAnn e') (n { loc = t }, b') e', s'')
tyE s (Def _ (n, b) e) = do
    (b', s') <- tyE s b
    let t = eAnn b'
    addPolyEnv n (s'@@t)
    (e', s'') <- tyE s' e
    pure (Def (eAnn e') (n { loc = t }, b') e', s'')
tyE s (LLet _ (n, b) e) = do
    (b', s') <- tyE s b
    let t = eAnn b'
    n <~ s'@@t
    (e', s'') <- tyE s' e
    pure (LLet (eAnn e') (n { loc = t }, b') e', s'')
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
    s2 <- liftU $ mp (l,e0) RF s1 (eAnn e0'$>l) (a~>b)
    s3 <- liftU $ mp (l,e1) CF s2 (eAnn e1'$>l) a
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
                Just t  -> do {t'<- cl t; pure (Var t' (n$>t'), s)}
                Nothing -> throwError $ IllScoped l n
tyE s (Tup _ es) = do
    (es', s') <- tS tyE s es
    let eTys = eAnn<$>es'
    pure (Tup (P eTys) es', s')
tyE s (Ann l e t) = do
    (e', s') <- tyE s e
    s'' <- liftEither $ maM RF (aT s'$fmap ($>l) eAnn e') (aT s' (t$>l))
    pure (e', s'<>s'')
tyE _ Dfn{} = desugar; tyE _ ResVar{} = desugar; tyE _ Parens{} = desugar

desugar :: a
desugar = error "Internal error. Should have been desugared by now."
