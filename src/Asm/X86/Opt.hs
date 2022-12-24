module Asm.X86.Opt ( optX86 ) where

import           Asm.X86

{-# SCC optAddr #-}
optAddr :: Addr reg -> Addr reg
optAddr (RC r 0)      = R r
optAddr (RSD b s i 0) = RS b s i
optAddr a             = a

-- remove noops
optX86 :: (Eq reg, Eq freg) => [X86 reg freg a] -> [X86 reg freg a]
optX86 [] = []
optX86 (ISubRI _ _ 0:asms) = optX86 asms
optX86 ((MovqAX l a r):asms) = MovqAX l (optAddr a) r:optX86 asms
optX86 ((MovqXA l r a):asms) = MovqXA l r (optAddr a):optX86 asms
optX86 ((MovAR l a r):asms) = MovAR l (optAddr a) r:optX86 asms
optX86 ((MovRA l r a):asms) = MovRA l r (optAddr a):optX86 asms
optX86 ((MovRR _ r0 r1):asms) | r0 == r1 = optX86 asms
optX86 ((Movapd _ r0 r1):asms) | r0 == r1 = optX86 asms
optX86 (isn@(Movapd _ r0 r1):(Movapd _ r0' r1'):asms) | r0 == r1' && r1 == r0' = optX86 (isn:asms)
optX86 (asm:asms) = asm : optX86 asms
