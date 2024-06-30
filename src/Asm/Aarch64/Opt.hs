module Asm.Aarch64.Opt ( opt ) where

import           Asm.Aarch64

opt :: (Eq reg, Eq freg) => [AArch64 reg freg ()] -> [AArch64 reg freg ()]
opt ((MovRC _ r 0):asms) = opt (ZeroR () r:asms)
opt ((ZeroR _ r0):(MovK _ r1 u s):asms) | r0 == r1 = opt (MovZ () r1 u s:asms)
opt ((MovRR _ r0 r1):asms) | r0 == r1 = opt asms
opt ((FMovXX _ r0 r1):asms) | r0 == r1 = opt asms
opt (asm:asms) = asm : opt asms
opt [] = []
