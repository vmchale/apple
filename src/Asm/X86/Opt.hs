module Asm.X86.Opt ( optX86 ) where

import           Asm.L
import           Asm.X86      hiding (toInt)
import           Asm.X86.CF
import           CF
import           Class.E
import           Data.Functor (void)
import qualified Data.IntSet  as IS

{-# SCC optAddr #-}
optAddr :: Addr reg -> Addr reg
optAddr (RC r 0)      = R r
optAddr (RSD b s i 0) = RS b s i
optAddr a             = a

occ :: E reg => reg -> Addr reg -> Bool
occ r a = toInt r `IS.member` foldMap (IS.singleton.toInt) a

-- remove noops
optX86 :: (E reg, E freg, Eq reg, Eq freg) => [X86 reg freg ()] -> [X86 reg freg ()]
optX86 = opt.mkLive

opt :: (E reg, E freg, Eq reg, Eq freg) => [X86 reg freg Liveness] -> [X86 reg freg ()]
opt [] = []
opt (ISubRI _ _ 0:asms) = opt asms
opt (MovqXA _ xrϵ a:Vfmadd231sd l xr0 xr1 xr2:asms) | xr2 == xrϵ && (toInt xr2 `IS.notMember` fout l) = Vfmadd231sdA () xr0 xr1 (optAddr a):opt asms
opt (MovqXA _ xrϵ a:Vmaxsd l xr0 xr1 xr2:asms) | xr2 == xrϵ && (toInt xr2 `IS.notMember` fout l) = VmaxsdA () xr0 xr1 (optAddr a):opt asms
opt (MovqXA _ xrϵ a:Vaddsd l xr0 xr1 xr2:asms) | xr2 == xrϵ && (toInt xr2 `IS.notMember` fout l) = VaddsdA () xr0 xr1 (optAddr a):opt asms
opt (MovqXA _ xr0 a:Vaddsd _ xr1 xr2 xr3:asms) | xr0 == xr1 && xr0 == xr3 = VaddsdA () xr1 xr2 (optAddr a):opt asms
opt (MovqXA _ xr0 a:asm:Vaddsd _ xr1 xr2 xr3:asms) | xr0 == xr2 && (toInt xr0 `IS.notMember` defsF asm) = void asm:VaddsdA () xr1 xr3 (optAddr a):opt asms
opt ((MovqAX _ a r):asms) = MovqAX () (optAddr a) r:opt asms
opt ((MovqXA _ r a):asms) = MovqXA () r (optAddr a):opt asms
opt (isn@(MovRA _ r0 a0):MovAR _ a1 r1:asms) | r0 == r1 && not (occ r0 a0) && optAddr a0 == optAddr a1 = opt (isn:asms)
opt (isn@(MovRA _ r0 a0):MovRA _ r1 a1:asms) | r0 == r1 && not (occ r0 a0) && optAddr a0 == optAddr a1 = opt (isn:asms)
opt (isn@(MovAR _ a0 r0):MovRA _ r1 a1:asms) | optAddr a0 == optAddr a1 = opt (isn:MovRR undefined r1 r0:asms)
opt ((MovAR _ a r):asms) = MovAR () (optAddr a) r:opt asms
opt ((MovRA _ r a):asms) = MovRA () r (optAddr a):opt asms
opt ((MovRR _ r0 r1):asms) | r0 == r1 = opt asms
opt ((Movapd _ r0 r1):asms) | r0 == r1 = opt asms
opt (isn@(Movapd _ r0 r1):(Movapd _ r0' r1'):asms) | r0 == r1' && r1 == r0' = opt (isn:asms)
-- opt (Vmulsd _ xr0 xr1 xr2:Vaddsd _ xr0' xr1' xr2':asms) | xr0 == xr2 && xr0 == xr0' && xr0' == xr2' = Vfmadd213sd () xr0 xr1 xr1':opt asms
opt (asm:asms) = void asm : opt asms
