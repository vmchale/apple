module C.Loop ( lift ) where

import           C
import           Data.Bifunctor (first, second)
import           Data.List      (scanl')
import qualified Data.Set       as S

class HasT a where
    uT :: a -> S.Set Temp

class HasX a where
    uX :: a -> S.Set FTemp

instance HasT ArrAcc where
    uT (AElem t e0 e1 _ _) = S.insert t (uT e0<>uT e1)
    uT (ARnk t _)          = S.singleton t
    uT (ADim t e _)        = S.insert t (uT e)
    uT (Raw t e _ _)       = S.insert t (uT e)
    uT (TupM t _)          = S.singleton t
    uT (At t ss ds _ _)    = S.insert t (foldMap uT ss<>foldMap uT ds)

instance HasX ArrAcc where
    uX (AElem _ e0 e1 _ _) = uX e0<>uX e1
    uX ARnk{}              = S.empty
    uX (ADim _ e _)        = uX e
    uX (Raw _ e _ _)       = uX e
    uX TupM{}              = S.empty
    uX (At _ ss ds _ _)    = foldMap uX ss<>foldMap uX ds

instance HasT CFE where
    uT (IE e)         = uT e
    uT (FAt a)        = uT a
    uT (FBin _ e0 e1) = uT e0<>uT e1
    uT (FUn _ e0)     = uT e0
    uT FTmp{}         = S.empty
    uT ConstF{}       = S.empty

instance HasT CE where
    uT (Bin _ e0 e1) = uT e0 <> uT e1
    uT (Tmp t)       = S.singleton t
    uT (DP t _)      = S.singleton t
    uT (EAt a)       = uT a
    uT LA{}          = S.empty
    uT ConstI{}      = S.empty
    uT (CFloor e)    = uT e

instance HasX CE where
    uX (CFloor e)    = uX e
    uX (Bin _ e0 e1) = uX e0<>uX e1
    uX Tmp{}         = S.empty
    uX (EAt a)       = uX a
    uX ConstI{}      = S.empty
    uX LA{}          = S.empty
    uX (DP _ e)      = uX e

instance HasT PE where
    uT (IRel _ e0 e1) = uT e0<>uT e1
    uT (IUn _ e)      = uT e
    uT (Is t)         = S.singleton t
    uT (FRel _ e0 e1) = uT e0<>uT e1

instance HasX PE where
    uX (FRel _ e0 e1) = uX e0<>uX e1
    uX (IRel _ e0 e1) = uX e0<>uX e1
    uX Is{}           = S.empty
    uX (IUn _ e)      = uX e

instance HasT CS where
    uT (_ := e)         = uT e
    uT (Ma _ _ e0 e1 _) = uT e0<>uT e1
    uT (MaΠ _ _ e)      = uT e
    uT (CpyE a0 a1 e _) = uT a0<>uT a1<>uT e
    uT (CpyD a0 a1 e)   = uT a0<>uT a1<>uT e
    uT (Ifn't p ss)     = uT p<>foldMap uT ss
    uT (If p s0 s1)     = uT p<>foldMap uT s0<>foldMap uT s1
    uT (Pop e)          = uT e
    uT (Sa _ e)         = uT e
    uT (Cmov p _ e)     = uT p<>uT e
    uT (Cset p _)       = uT p
    uT (MX _ e)         = uT e
    uT (Wr a e)         = uT a<>uT e
    uT (WrF a e)        = uT a<>uT e
    uT (SZ _ t e _)     = S.insert t (uT e)
    uT (Fcmov p _ e)    = uT p<>uT e
    uT (PlProd _ es)    = foldMap uT es
    uT (For t l _ u ss) = S.insert t$uT l<>uT u<>foldMap uT ss
    uT (While t _ c ss) = S.insert t$uT c<>foldMap uT ss
    uT Rnd{}            = S.empty
    uT RA{}             = S.empty

instance HasX CFE where
    uX (FTmp i)       = S.singleton i
    uX (FAt a)        = uX a
    uX (FBin _ e0 e1) = uX e0<>uX e1
    uX (FUn _ e)      = uX e
    uX ConstF{}       = S.empty
    uX (IE e)         = uX e

instance HasX CS where
    uX (MX _ e)         = uX e
    uX (_ := e)         = uX e
    uX Rnd{}            = S.empty
    uX (Wr a e)         = uX a<>uX e
    uX (WrF a e)        = uX a<>uX e
    uX (Fcmov p _ e)    = uX p<>uX e
    uX (Cset p _)       = uX p
    uX (SZ _ _ e _)     = uX e
    uX (Ma _ _ e0 e1 _) = uX e0<>uX e1
    uX (MaΠ _ _ e)      = uX e
    uX (CpyE a0 a1 e _) = uX a0<>uX a1<>uX e
    uX (CpyD a0 a1 e)   = uX a0<>uX a1<>uX e
    uX (Ifn't p ss)     = uX p<>foldMap uX ss
    uX (If p s0 s1)     = uX p<>foldMap uX s0<>foldMap uX s1
    uX (Sa _ e)         = uX e
    uX (Pop e)          = uX e
    uX (Cmov p _ e)     = uX p<>uX e
    uX (PlProd _ es)    = foldMap uX es
    uX (For _ l _ u ss) = uX l<>uX u<>foldMap uX ss
    uX (While _ _ c ss) = uX c<>foldMap uX ss

data An = An { used :: !(S.Set Temp), uF :: !(S.Set FTemp) }

lift :: [CS] -> [CS]
lift (For t lb rel ub ss:cs) = let (consts, ss') = liftBody ss in consts++For t lb rel ub ss':lift cs
lift (c:cs)                  = c:lift cs
lift []                      = []

liftBody :: [CS] -> ([CS], [CS])
liftBody css | not (hasF css) = let ann=scan css in pick ann | otherwise = ([], css)
  where
    pick :: [(An, CS)] -> ([CS], [CS])
    pick ((l, c@(MX t ConstF{})):cs) | t `S.notMember` uF l = first (c:) (pick cs)
    -- pick ((l, c@(t := ConstI{})):cs) | t `S.notMember` used l = first (c:) (pick cs)
    pick ((_,c):cs) = second (c:) (pick cs)
    pick [] = ([], [])
    hasF = any isF
    isF G{}=True; isF Def{}=True; isF _=False

scan :: [CS] -> [(An, CS)]
scan css = zip ls css where ls = scanl' (\(An u ux) s -> (An (u<>uT s) (ux<>uX s))) (An S.empty S.empty) css

-- don't bother if FOR has G or whatever
-- sanity check: if constant is written to BEFORE in loop ??
