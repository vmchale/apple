module Asm.X86.Opt ( optX86 ) where

import           Asm.X86

-- remove noops
optX86 :: (Eq reg, Eq freg) => [X86 reg freg a] -> [X86 reg freg a]
optX86 [] = []
optX86 ((MovRR _ r0 r1):asms) | r0 == r1 = optX86 asms
optX86 ((Movapd _ r0 r1):asms) | r0 == r1 = optX86 asms
optX86 (isn@(Movapd _ r0 r1):(Movapd _ r0' r1'):asms) | r0 == r1' && r1 == r0' = optX86 (isn:asms)
optX86 (Vaddsd l xr0 xr1 xr2:asms) | xr0 == xr2 = Addsd l xr0 xr1:optX86 asms
optX86 (Vdivsd l xr0 xr1 xr2:asms) | xr0 == xr1 = Divsd l xr0 xr2:optX86 asms
optX86 (asm:asms) = asm : optX86 asms
