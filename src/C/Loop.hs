module C.Loop ( scan ) where

import           C
import Data.List (scanl')
import qualified Data.Set as S

uAA :: ArrAcc -> S.Set Temp
uAA (AElem t e0 e1 _ _) = S.insert t (uET e0<>uET e1)

uET :: CE -> S.Set Temp
uET (Bin _ e0 e1) = uET e0 <> uET e1
uET (Tmp t)       = S.singleton t
uET (DP t _)      = S.singleton t
uET (EAt a)       = uAA a

uT :: CS -> S.Set Temp
uT (_ := e) = uET e

dT :: CS -> S.Set Temp
dT (t := _)         = S.singleton t
dT (Cmov _ t _)     = S.singleton t
dT (Cset _ t)       = S.singleton t
dT (PlProd t _)     = S.singleton t
dT (Rnd t)          = S.singleton t
dT (Sa t _)         = S.singleton t
dT (SZ _ t _ _)     = S.singleton t
dT (Ma _ t _ _ _)   = S.singleton t
dT (MaΠ _ t _)      = S.singleton t
dT (For t _ _ _ ss) = S.singleton t <> foldMap dT ss
dT (While _ _ _ ss) = foldMap dT ss
dT (If _ s0 s1)     = foldMap dT s0<>foldMap dT s1
dT (Ifn't _ ss)     = foldMap dT ss

dX :: CS -> S.Set FTemp
dX (MX t _)         = S.singleton t
dX (Fcmov _ t _)    = S.singleton t
dX (For _ _ _ _ ss) = foldMap dX ss
dX (While _ _ _ ss) = foldMap dX ss
dX (If _ s0 s1)     = foldMap dX s0 <> foldMap dX s1

data An = An { used :: S.Set Temp, def'd :: S.Set Temp }

scan :: [CS] -> [(An, CS)]
scan css = zip ls css where ls = scanl' (\(An u d) s -> (An (u<>uT s) (d<>dT s))) (An S.empty S.empty) css

-- don't bother if FOR has G or whatever
-- sanity check: if constant is written to BEFORE in loop ??
