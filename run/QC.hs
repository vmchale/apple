module QC ( gas ) where

import           A
import           Control.Monad.State.Strict (StateT, evalStateT, get, gets, modify, put, runStateT)
import           Control.Monad.Trans.Class  (lift)
import           Data.Bifunctor             (bimap)
import           Data.Functor               (($>))
import           Data.Int                   (Int64)
import qualified Data.IntMap                as IM
import           Foreign.Marshal.Alloc      (mallocBytes)
import           Foreign.Ptr                (Ptr)
import           Foreign.Storable           (poke, sizeOf)
import           Hs.A
import           Nm
import           Test.QuickCheck.Gen        (Gen, chooseInt64, frequency, genDouble, generate, vectorOf)
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
        Nothing -> do {d <- lift$dim; modify (mapI (IM.insert n d)); bimap (+1) (d:)<$>gg sh}
        Just d  -> bimap (+1) (d:)<$>gg sh
gg (StaPlus _ (IVar _ (Nm _ (U n) _)) (Ix _ i) `Cons` sh) | i' <- fromIntegral i = do
    iSt <- gets iS
    case IM.lookup n iSt of
        Nothing -> do {d <- lift$chooseInt64 (0,10); modify (mapI (IM.insert n d)); bimap (+1) ((d+i'):)<$>gg sh}
        Just d  -> bimap (+1) ((d+i'):)<$>gg sh
gg (SVar (Nm _ (U n) _)) = do
    sSt <- gets sS
    case IM.lookup n sSt of
        Nothing -> do {r <- lift$rnk; ds <- lift$vectorOf (fromIntegral r) dim; modify (mapS (IM.insert n (r,ds))) $> (r,ds)}
        Just s  -> pure s

gas :: [T a] -> IO [Ptr (Apple Double)]
gas = flip evalStateT (RSubst IM.empty IM.empty).traverse ga

ga :: T a -> StateT RSubst IO (Ptr (Apple Double))
ga (Arr sh F) = do
    st <- get
    (a, st') <- lift $ generate $ runStateT (gD sh) st
    put st'
    p <- lift $ mallocBytes (sizeOf a)
    lift (poke p a $> p)

gD :: Sh a -> ShM (Apple Double)
gD sh = do
    (r, ds) <- gg sh
    let n=fromIntegral$product ds
    es <- lift$vectorOf n genDouble
    pure (AA r ds es)
