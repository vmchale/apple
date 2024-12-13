module Asm.Aarch64.Opt ( opt ) where

import           Asm.Aarch64

opt :: (Eq reg, Eq freg) => [AArch64 reg freg ()] -> [AArch64 reg freg ()]
opt=pe.mvs
  where
    pe (AddRC _ r0 r0' 0 _:asms) | r0==r0' = pe asms
    pe (SubRC _ r0 r0' 0 _:asms) | r0==r0' = pe asms
    pe (Ldr _ r0 (RP b0 i0):Ldr _ r1 (RP b1 i1):asms) | b0==b1 && i1==i0+8 = Ldp () r0 r1 (RP b0 i0):pe asms
    pe (Stp x r0 r1 (RP ar 0):AddRC _ r2 r3 u IZero:asms) | ar==r2&&r2==r3 = Stp x r0 r1 (Po ar (fromIntegral u)):pe asms
    pe (Str x r0 (RP ar 0):AddRC _ r1 r2 u IZero:asms) | ar==r1&&r1==r2 = Str x r0 (Po ar (fromIntegral u)):pe asms
    pe (Str x r0 (R ar):AddRC _ r1 r2 u IZero:asms) | ar==r1&&r1==r2 = Str x r0 (Po ar (fromIntegral u)):pe asms
    pe (Ldr x r0 (R ar):AddRC _ r1 r2 u IZero:asms) | ar==r1&&r1==r2 = Ldr x r0 (Po ar (fromIntegral u)):pe asms
    pe (LdrD x d0 (R ar):AddRC _ r1 r2 u IZero:asms) | ar==r1&&r1==r2 = LdrD x d0 (Po ar (fromIntegral u)):pe asms
    pe (LdrS x q0 (R ar):AddRC _ r1 r2 u IZero:asms) | ar==r1&&r1==r2 = LdrS x q0 (Po ar (fromIntegral u)):pe asms
    pe (StrD x d0 (R ar):AddRC _ r1 r2 u IZero:asms) | ar==r1&&r1==r2 = StrD x d0 (Po ar (fromIntegral u)):pe asms
    pe (StrS x q0 (R ar):AddRC _ r1 r2 u IZero:asms) | ar==r1&&r1==r2 = StrS x q0 (Po ar (fromIntegral u)):pe asms
    pe (Str _ r0 (R ar0):Str _ r1 (RP ar1 8):asms) | ar0 == ar1 = Stp () r0 r1 (R ar0):pe asms
    pe (SubRC _ r r1 w IZero:CmpRC _ r' 0:asms) | r==r' = SubsRC () r r1 w:pe asms
    pe ((MovRC _ r 0):asms) = pe (ZeroR () r:asms)
    pe ((ZeroR _ r0):(MovK _ r1 u s):asms) | r0 == r1 = pe (MovZ () r1 u s:asms)
    pe (asm:asms) = asm : pe asms
    pe [] = []

mvs :: (Eq reg, Eq freg) => [AArch64 reg freg a] -> [AArch64 reg freg a]
mvs = filter (not.isMv) where isMv (MovRR _ r0 r1) | r0==r1 = True; isMv (MovQQ _ v0 v1) | v0==v1 = True; isMv (FMovXX _ r0 r1) | r0==r1 = True; isMv _=False
