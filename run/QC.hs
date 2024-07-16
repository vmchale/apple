module QC ( gD ) where

import           A
import           Control.Monad.State.Strict (StateT, gets, modify, runStateT)
import           Control.Monad.Trans.Class  (lift)
import           Data.Bifunctor             (bimap)
import           Data.Functor               (($>))
import           Data.Int                   (Int64)
import qualified Data.IntMap                as IM
import           Foreign.Marshal.Alloc      (mallocBytes)
import           Foreign.Ptr                (Ptr, castPtr)
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

mapI :: (IM.IntMap Int64 -> IM.IntMap Int64) -> RSubst -> RSubst
mapI f (RSubst i s) = RSubst (f i) s

mapS :: (IM.IntMap (Int64, [Int64]) -> IM.IntMap (Int64, [Int64])) -> RSubst -> RSubst
mapS g (RSubst i s) = RSubst i (g s)

type ShM = StateT RSubst Gen

gg :: Sh () -> ShM (Int64, [Int64])
gg Nil = pure (0, [])
gg (Ix _ i `Cons` sh) = bimap (+1) (fromIntegral i:)<$>gg sh
gg (IVar _ (Nm _ (U n) _) `Cons` sh) = do
    iSt <- gets iS
    case IM.lookup n iSt of
        Nothing -> do {d <- lift$dim; modify (mapI (IM.insert n d)); bimap (+1) (d:)<$>gg sh}
        Just d  -> bimap (+1) (d:)<$>gg sh
gg (SVar (Nm _ (U n) _)) = do
    sSt <- gets sS
    case IM.lookup n sSt of
        Nothing -> do {r <- lift$rnk; ds <- lift$vectorOf (fromIntegral r) dim; modify (mapS (IM.insert n (r,ds))) $> (r,ds)}
        Just s  -> pure s

ga :: RSubst -> T () -> IO (RSubst, Ptr ())
ga st (Arr sh F) = do
    (st', a) <- generate (gD st sh)
    p <- mallocBytes (sizeOf a)
    poke p a $> (st', castPtr p)

gD :: RSubst -> Sh () -> Gen (RSubst, Apple Double)
gD st sh = do
    ((r, ds), st') <- runStateT (gg sh) st
    let n=fromIntegral$product ds
    es <- vectorOf n genDouble
    pure (st', AA r ds es)
