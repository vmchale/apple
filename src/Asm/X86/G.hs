module Asm.X86.G ( mI, mf ) where

import Asm.X86

-- | result: src, dest
mI :: X86 reg freg a -> Maybe (reg, reg)
mI (MovRR _ r0 r1) = Just (r1, r0)
mI _               = Nothing

mf :: X86 reg freg a -> Maybe (freg, freg)
mf (Movapd _ r0 r1) = Just (r1, r0)
mf _                = Nothing
