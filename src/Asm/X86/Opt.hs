module Asm.X86.Opt ( optX86 ) where

import           Asm.X86     hiding (toInt)
import           Class.E
import qualified Data.IntSet as IS

{-# SCC optAddr #-}
optAddr :: Addr reg -> Addr reg
optAddr (RC r 0)      = R r
optAddr (RSD b s i 0) = RS b s i
optAddr a             = a

occ :: E reg => reg -> Addr reg -> Bool
occ r a = toInt r `IS.member` foldMap (IS.singleton.toInt) a

-- remove noops
optX86 :: (E reg, Eq reg, Eq freg) => [X86 reg freg a] -> [X86 reg freg a]
optX86 [] = []
optX86 (ISubRI _ _ 0:asms) = optX86 asms
optX86 ((MovqAX l a r):asms) = MovqAX l (optAddr a) r:optX86 asms
optX86 ((MovqXA l r a):asms) = MovqXA l r (optAddr a):optX86 asms
optX86 (isn@(MovRA _ r0 a0):MovAR _ a1 r1:asms) | r0 == r1 && not (occ r0 a0) && optAddr a0 == optAddr a1 = optX86 (isn:asms)
optX86 (isn@(MovRA _ r0 a0):MovRA _ r1 a1:asms) | r0 == r1 && not (occ r0 a0) && optAddr a0 == optAddr a1 = optX86 (isn:asms)
optX86 (isn@(MovAR _ a0 r0):MovRA l r1 a1:asms) | optAddr a0 == optAddr a1 = optX86 (isn:MovRR l r1 r0:asms)
optX86 ((MovAR l a r):asms) = MovAR l (optAddr a) r:optX86 asms
optX86 ((MovRA l r a):asms) = MovRA l r (optAddr a):optX86 asms
optX86 ((MovRR _ r0 r1):asms) | r0 == r1 = optX86 asms
optX86 ((Movapd _ r0 r1):asms) | r0 == r1 = optX86 asms
optX86 (isn@(Movapd _ r0 r1):(Movapd _ r0' r1'):asms) | r0 == r1' && r1 == r0' = optX86 (isn:asms)
optX86 (Vmulsd _ xr0 xr1 xr2:Vaddsd l xr0' xr1' xr2':asms) | xr0 == xr2 && xr0 == xr0' && xr0' == xr2' = Vfmadd213sd l xr0 xr1 xr1':optX86 asms
optX86 (asm:asms) = asm : optX86 asms
