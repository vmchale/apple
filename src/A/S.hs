module A.S ( isF, isI, isB, isArr, isΠ, nind
           , bT, szT, nSz, rSz
           , nr, rr
           , staRnk, tRnk
           , staIx, tIx
           , mAA, aB, aN, aBs
           ) where

import           A
import           Data.Bifunctor (bimap)
import           Data.Int       (Int64)
import           Data.List      (scanl')
import           Sh

isF, isI, isB :: T a -> Bool
isF F = True; isF _ = False
isI I = True; isI _ = False
isB B = True; isB _ = False
isArr Arr{}=True; isArr _=False
nind I=True; nind F=True; nind P{}=True; nind B{}=True; nind _=False
isΠ P{}=True; isΠ _=False

staRnk :: Integral b => Sh a -> Maybe b
staRnk Nil           = Just 0
staRnk (_ `Cons` sh) = (1+) <$> staRnk sh
staRnk _             = Nothing

tRnk :: T a -> Maybe (T a, Int64)
tRnk (Arr sh t) = (t,) <$> staRnk sh
tRnk _          = Nothing

staIx :: Sh a -> Maybe [Int64]
staIx Nil=Just[]; staIx (Ix _ i `Cons` s) = (fromIntegral i:)<$>staIx s; staIx _=Nothing

tIx :: T a -> Maybe (T a, [Int64])
tIx (Arr sh t) = (t,)<$>staIx sh; tIx _=Nothing

bT :: Integral b => T a -> b
bT (P ts)=sum (bT<$>ts); bT F=8; bT I=8; bT B=1; bT Arr{}=8

mAA :: T a -> Maybe ((T a, Int64), (T a, Int64))
mAA (Arrow t0 t1) = (,) <$> tRnk t0 <*> tRnk t1; mAA _ = Nothing

rSz, nSz :: T a -> Maybe Int64
rSz F=Just 8; rSz I=Just 8; rSz B=Just 1; rSz _=Nothing
nSz F=Just 8; nSz I=Just 8; nSz B=Just 1; nSz (P ts)=sum<$>traverse nSz ts; nSz _=Nothing

aB (Arr (_ `Cons` Nil) t) = nSz t; aB _ = Nothing
aBs (Arr (_ `Cons` Nil) t) = nr t; aBs _ = Nothing
aN (Arr _ t) = nr t; aN _=Nothing

nr, rr :: T a -> Maybe (T a, Int64)
nr (P ts) = bimap P sum . unzip <$> traverse nr ts; nr t = rr t

rr I=Just (I,8); rr F=Just (F,8); rr B=Just (B,1); rr _=Nothing

szT = scanl' (\o ty -> o+bT ty::Int64) 0
