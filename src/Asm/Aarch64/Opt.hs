module Asm.Aarch64.Opt ( opt ) where

import           Asm.Aarch64

opt :: (Eq reg, Eq freg) => [AArch64 reg freg a] -> [AArch64 reg freg a]
opt ((MovRR _ r0 r1):asms) | r0 == r1 = opt asms
opt ((FMovXX _ r0 r1):asms) | r0 == r1 = opt asms
opt (asm:asms) = asm : opt asms
opt [] = []
