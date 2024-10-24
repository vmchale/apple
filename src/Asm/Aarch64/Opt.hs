module Asm.Aarch64.Opt ( opt ) where

import           Asm.Aarch64

opt :: (Eq reg, Eq freg) => [AArch64 reg freg ()] -> [AArch64 reg freg ()]
opt (AddRC _ r0 r0' 0:asms) | r0==r0' = opt asms
opt (SubRC _ r0 r0' 0:asms) | r0==r0' = opt asms
opt (Str _ r0 (R ar0):Str _ r1 (RP ar1 8):asms) | ar0 == ar1 = Stp () r0 r1 (R ar0):opt asms
opt (SubRC _ r r1 w:CmpRC _ r' 0:asms) | r==r' = SubsRC () r r1 w:opt asms
opt ((MovRC _ r 0):asms) = opt (ZeroR () r:asms)
opt ((ZeroR _ r0):(MovK _ r1 u s):asms) | r0 == r1 = opt (MovZ () r1 u s:asms)
opt ((MovRR _ r0 r1):asms) | r0 == r1 = opt asms
opt ((MovQQ _ v0 v1):asms) | v0 == v1 = opt asms
opt ((FMovXX _ r0 r1):asms) | r0 == r1 = opt asms
opt (asm:asms) = asm : opt asms
opt [] = []
