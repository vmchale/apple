module QC ( gas, Val (..) ) where

import           A
import           Control.Monad.State.Strict (StateT, evalStateT, get, gets, modify, put, runStateT)
import           Control.Monad.Trans.Class  (lift)
import           Data.Bifunctor             (bimap)
import           Data.Functor               (($>))
import           Data.Int                   (Int64)
import qualified Data.IntMap                as IM
import           Foreign.C.Types            (CDouble (..))
import           Foreign.LibFFI             (Arg, argCDouble, argInt64, argPtr)
import           Foreign.Marshal.Alloc      (mallocBytes)
import           Foreign.Ptr                (Ptr)
import           Foreign.Storable           (poke, sizeOf)
import           Hs.A
import           Nm
import           Prettyprinter              (Pretty (..))
import           Test.QuickCheck.Gen        (Gen, chooseAny, chooseInt64, frequency, genDouble, generate, vectorOf)
import           U

rnk :: Gen Int64
rnk = frequency [(8, pure 1), (3, pure 2), (2, pure 3)]

dim :: Gen Int64
dim = chooseInt64 (0, 20)

data RSubst = RSubst { iS :: IM.IntMap Int64, sS :: IM.IntMap (Int64, [Int64]) }

mapI f (RSubst i s) = RSubst (f i) s
mapS g (RSubst i s) = RSubst i (g s)

type ShM = StateT RSubst Gen

gg :: Sh a -> ShM (Int64, [Int64])
gg Nil = pure (0, [])
gg (Ix _ i `Cons` sh) = bimap (+1) (fromIntegral i:)<$>gg sh
gg (IVar _ (Nm _ (U n) _) `Cons` sh) = do
    iSt <- gets iS
    case IM.lookup n iSt of
        Nothing -> do {d <- lift dim; modify (mapI (IM.insert n d)); bimap (+1) (d:)<$>gg sh}
        Just d  -> bimap (+1) (d:)<$>gg sh
gg (StaPlus _ (IVar _ (Nm _ (U n) _)) (Ix _ i) `Cons` sh) | i' <- fromIntegral i = do
    iSt <- gets iS
    case IM.lookup n iSt of
        Nothing -> do {d <- lift$chooseInt64 (0,10); modify (mapI (IM.insert n d)); bimap (+1) ((d+i'):)<$>gg sh}
        Just d  -> bimap (+1) ((d+i'):)<$>gg sh
gg (SVar (Nm _ (U n) _)) = do
    sSt <- gets sS
    case IM.lookup n sSt of
        Nothing -> do {r <- lift rnk; ds <- lift$vectorOf (fromIntegral r) dim; modify (mapS (IM.insert n (r,ds))) $> (r,ds)}
        Just s  -> pure s

data ValP = ArrDp (Ptr (Apple Double))

gas :: [T a] -> IO [(Arg, Val, Maybe (Ptr (Apple Double)))]
gas = flip evalStateT (RSubst IM.empty IM.empty).traverse ga

data Val = ArrD !(Apple Double) | II !Int64 | D !Double

instance Pretty Val where
    pretty (ArrD a) = pretty a
    pretty (II i)   = pretty i
    pretty (D d)    = pretty d

ga :: T a -> StateT RSubst IO (Arg, Val, Maybe (Ptr (Apple Double)))
ga (Arr sh A.F) = do
    st <- get
    (a, st') <- lift $ generate $ runStateT (gD sh) st
    put st'
    p <- lift $ mallocBytes (sizeOf a)
    lift (poke p a $> (argPtr p, ArrD a, Just p))
ga I = do
    i <- lift $ generate chooseAny
    pure (argInt64 i, II i, Nothing)
ga A.F = do
    x <- lift $ generate chooseAny
    pure (argCDouble (CDouble x), D x, Nothing)

gD :: Sh a -> ShM (Apple Double)
gD sh = do
    (r, ds) <- gg sh
    let n=fromIntegral$product ds
    es <- lift$vectorOf n genDouble
    pure (AA r ds es)
