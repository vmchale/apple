module Asm.X86.Opt ( optX86 ) where

import           Asm.X86

-- remove noops
optX86 :: Eq reg => [X86 reg a] -> [X86 reg a]
optX86 [] = []
optX86 ((MovRR _ r0 r1):asms) | r0 == r1 = optX86 asms
optX86 ((Movapd _ r0 r1):asms) | r0 == r1 = optX86 asms
optX86 (isn@(Movapd _ r0 r1):(Movapd _ r0' r1'):asms) | r0 == r1' && r1 == r0' = optX86 (isn:asms)
optX86 (asm:asms) = asm : optX86 asms
