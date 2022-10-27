module Asm.X86.P ( gallocFrame ) where

import           Asm.G
import           Asm.X86
import           Asm.X86.Frame
import           Asm.X86.LI
import qualified Data.IntMap    as IM
import           Data.Semigroup
import qualified Data.Set       as S

(@$) :: (areg -> reg) -> Addr areg -> Addr reg
f @$ (R r)           = R (f r)
f @$ (RC r i)        = RC (f r) i
f @$ (RS r0 s r1)    = RS (f r0) s (f r1)
f @$ (RSD r0 s r1 i) = RSD (f r0) s (f r1) i

mapR :: (areg -> reg) -> X86 areg afreg a -> X86 reg afreg a
mapR f (MovRR l r0 r1)             = MovRR l (f r0) (f r1)
mapR _ (Jg x l)                    = Jg x l
mapR _ (Je x l)                    = Je x l
mapR _ (Jge x l)                   = Jge x l
mapR _ (Jne x l)                   = Jne x l
mapR _ (J x l)                     = J x l
mapR _ (Jl x l)                    = Jl x l
mapR _ (Jle x l)                   = Jle x l
mapR _ (Label x l)                 = Label x l
mapR f (IMulRR l r0 r1)            = IMulRR l (f r0) (f r1)
mapR f (IAddRR l r0 r1)            = IAddRR l (f r0) (f r1)
mapR f (ISubRR l r0 r1)            = ISubRR l (f r0) (f r1)
mapR f (CmpRR l r0 r1)             = CmpRR l (f r0) (f r1)
mapR f (ISubRI l r0 i)             = ISubRI l (f r0) i
mapR f (IAddRI l r0 i)             = IAddRI l (f r0) i
mapR f (MovRI l r0 i)              = MovRI l (f r0) i
mapR f (MovRA l r a)               = MovRA l (f r) (f@$a)
mapR f (MovAR l a r)               = MovAR l (f@$a) (f r)
mapR f (MovAI32 l a i)             = MovAI32 l (f@$a) i
mapR f (MovqXR l xr r)             = MovqXR l xr (f r)
mapR f (MovqXA l xr a)             = MovqXA l xr (f@$a)
mapR f (MovqAX l a xr)             = MovqAX l (f@$a) xr
mapR f (Fld l a)                   = Fld l (f@$a)
mapR _ (Fldl2e l)                  = Fldl2e l
mapR _ (Fldln2 l)                  = Fldln2 l
mapR _ (Fld1 l)                    = Fld1 l
mapR _ (Fyl2x l)                   = Fyl2x l
mapR f (Fstp l a)                  = Fstp l (f@$a)
mapR _ (F2xm1 l)                   = F2xm1 l
mapR _ (Fmulp l)                   = Fmulp l
mapR _ (Fprem l)                   = Fprem l
mapR _ (Faddp l)                   = Faddp l
mapR _ (Fscale l)                  = Fscale l
mapR f (CmpRI l r i)               = CmpRI l (f r) i
mapR _ (Ret l)                     = Ret l
mapR _ (Vdivsd l xr0 xr1 xr2)      = Vdivsd l xr0 xr1 xr2
mapR _ (Movapd l xr0 xr1)          = Movapd l xr0 xr1
mapR _ (FldS l s)                  = FldS l s
mapR _ (Fxch l s)                  = Fxch l s
mapR _ (Mulsd l xr0 xr1)           = Mulsd l xr0 xr1
mapR _ (Addsd l xr0 xr1)           = Addsd l xr0 xr1
mapR _ (Subsd l xr0 xr1)           = Subsd l xr0 xr1
mapR _ (Divsd l xr0 xr1)           = Divsd l xr0 xr1
mapR _ (Vmulsd l xr0 xr1 xr2)      = Vmulsd l xr0 xr1 xr2
mapR _ (Vaddsd l xr0 xr1 xr2)      = Vaddsd l xr0 xr1 xr2
mapR _ (Vsubsd l xr0 xr1 xr2)      = Vsubsd l xr0 xr1 xr2
mapR f (Cvttsd2si l r xr)          = Cvttsd2si l (f r) xr
mapR f (Push l r)                  = Push l (f r)
mapR f (Pop l r)                   = Pop l (f r)
mapR _ (Call l f)                  = Call l f
mapR f (IDiv l r)                  = IDiv l (f r)
mapR _ (Roundsd l xr0 xr1 m)       = Roundsd l xr0 xr1 m
mapR f (Cvtsi2sd l xr r)           = Cvtsi2sd l xr (f r)
mapR _ (Vfmadd231sd l xr0 xr1 xr2) = Vfmadd231sd l xr0 xr1 xr2
mapR f (Sal l r i)                 = Sal l (f r) i
mapR f (Sar l r i)                 = Sar l (f r) i
mapR _ (Sqrtsd l xr0 xr1)          = Sqrtsd l xr0 xr1
mapR _ (Maxsd l xr0 xr1)           = Maxsd l xr0 xr1
mapR _ (Minsd l xr0 xr1)           = Minsd l xr0 xr1
mapR _ (Vmaxsd l xr0 xr1 xr2)      = Vmaxsd l xr0 xr1 xr2
mapR _ (Vminsd l xr0 xr1 xr2)      = Vminsd l xr0 xr1 xr2
mapR f (Not l r)                   = Not l (f r)
mapR f (And l r0 r1)               = And l (f r0) (f r1)
mapR f (Rdrand l r)                = Rdrand l (f r)
mapR f (Cmovnle l r0 r1)           = Cmovnle l (f r0) (f r1)

mapFR :: (afreg -> freg) -> X86 areg afreg a -> X86 areg freg a
mapFR _ (Jg x l)                    = Jg x l
mapFR _ (J x l)                     = J x l
mapFR _ (Label x l)                 = Label x l
mapFR _ (MovRI l r i)               = MovRI l r i
mapFR _ (MovRR l r0 r1)             = MovRR l r0 r1
mapFR _ (IAddRI l r i)              = IAddRI l r i
mapFR f (Movapd l r0 r1)            = Movapd l (f r0) (f r1)
mapFR f (Mulsd l xr0 xr1)           = Mulsd l (f xr0) (f xr1)
mapFR f (MovqXR l xr r)             = MovqXR l (f xr) r
mapFR f (Roundsd l xr0 xr1 s)       = Roundsd l (f xr0) (f xr1) s
mapFR f (Cvttsd2si l r xr)          = Cvttsd2si l r (f xr)
mapFR f (Vsubsd l xr0 xr1 xr2)      = Vsubsd l (f xr0) (f xr1) (f xr2)
mapFR f (Vaddsd l xr0 xr1 xr2)      = Vaddsd l (f xr0) (f xr1) (f xr2)
mapFR f (Vdivsd l xr0 xr1 xr2)      = Vdivsd l (f xr0) (f xr1) (f xr2)
mapFR _ (CmpRR l r0 r1)             = CmpRR l r0 r1
mapFR f (Addsd l xr0 xr1)           = Addsd l (f xr0) (f xr1)
mapFR _ (IAddRR l r0 r1)            = IAddRR l r0 r1
mapFR _ (ISubRR l r0 r1)            = ISubRR l r0 r1
mapFR _ (IMulRR l r0 r1)            = IMulRR l r0 r1
mapFR _ (ISubRI l r i)              = ISubRI l r i
mapFR _ (MovRA l r a)               = MovRA l r a
mapFR _ (MovAI32 l r i)             = MovAI32 l r i
mapFR _ (MovAR l a r)               = MovAR l a r
mapFR f (MovqXA l xr a)             = MovqXA l (f xr) a
mapFR f (MovqAX l a xr)             = MovqAX l a (f xr)
mapFR _ (Fld l a)                   = Fld l a
mapFR _ (FldS l s)                  = FldS l s
mapFR _ (Fldl2e l)                  = Fldl2e l
mapFR _ (Fldln2 l)                  = Fldln2 l
mapFR _ (Fld1 l)                    = Fld1 l
mapFR _ (Fyl2x l)                   = Fyl2x l
mapFR _ (Fstp l a)                  = Fstp l a
mapFR _ (F2xm1 l)                   = F2xm1 l
mapFR _ (Fmulp l)                   = Fmulp l
mapFR _ (Fprem l)                   = Fprem l
mapFR _ (Faddp l)                   = Faddp l
mapFR _ (Fscale l)                  = Fscale l
mapFR _ (Fxch l s)                  = Fxch l s
mapFR _ (Je x l)                    = Je x l
mapFR _ (Jge x l)                   = Jge x l
mapFR _ (Jne x l)                   = Jne x l
mapFR _ (Jl x l)                    = Jl x l
mapFR _ (Jle x l)                   = Jle x l
mapFR _ (CmpRI l r i)               = CmpRI l r i
mapFR _ (Ret l)                     = Ret l
mapFR f (Subsd l xr0 xr1)           = Subsd l (f xr0) (f xr1)
mapFR f (Divsd l xr0 xr1)           = Divsd l (f xr0) (f xr1)
mapFR f (Vmulsd l xr0 xr1 xr2)      = Vmulsd l (f xr0) (f xr1) (f xr2)
mapFR _ (Push l r)                  = Push l r
mapFR _ (Pop l r)                   = Pop l r
mapFR _ (IDiv l r)                  = IDiv l r
mapFR _ (Call l f)                  = Call l f
mapFR _ (Sal l r i)                 = Sal l r i
mapFR _ (Sar l r i)                 = Sar l r i
mapFR f (Maxsd l xr0 xr1)           = Maxsd l (f xr0) (f xr1)
mapFR f (Vmaxsd l xr0 xr1 xr2)      = Vmaxsd l (f xr0) (f xr1) (f xr2)
mapFR f (Minsd l xr0 xr1)           = Minsd l (f xr0) (f xr1)
mapFR f (Vminsd l xr0 xr1 xr2)      = Vminsd l (f xr0) (f xr1) (f xr2)
mapFR _ (Not l r)                   = Not l r
mapFR f (Cvtsi2sd l xr r)           = Cvtsi2sd l (f xr) r
mapFR f (Vfmadd231sd l xr0 xr1 xr2) = Vfmadd231sd l (f xr0) (f xr1) (f xr2)
mapFR f (Sqrtsd l xr0 xr1)          = Sqrtsd l (f xr0) (f xr1)
mapFR _ (And l r0 r1)               = And l r0 r1
mapFR _ (Cmovnle l r0 r1)           = Cmovnle l r0 r1
mapFR _ (Rdrand l r)                = Rdrand l r

gallocFrame :: [X86 AbsReg FAbsReg ()] -> [X86 X86Reg FX86Reg ()]
gallocFrame = frameC . mkIntervals . galloc

{-# SCC galloc #-}
galloc :: [X86 AbsReg FAbsReg ()] -> [X86 X86Reg FX86Reg ()]
galloc isns = frame clob'd (fmap (mapR ((regs IM.!).toInt).mapFR ((fregs IM.!).fToInt)) isns)
    where (regs, fregs) = gallocOn isns
          clob'd = S.fromList $ IM.elems regs

{-# SCC frame #-}
frame :: S.Set X86Reg -> [X86 X86Reg FX86Reg ()] -> [X86 X86Reg FX86Reg ()]
frame clob asms = pre++asms++post++[Ret()] where
    pre = Push () <$> clobs
    post = Pop () <$> reverse clobs
    clobs = S.toList (clob `S.intersection` S.fromList [R12 .. Rbx])

gallocOn :: [X86 AbsReg FAbsReg ()] -> (IM.IntMap X86Reg, IM.IntMap FX86Reg)
gallocOn isns = (regs, fregs)
    where regs = alloc isns [Rcx .. Rax] (IM.keysSet pres) pres
          fregs = allocF isns [XMM1 .. XMM15] (IM.keysSet preFs) preFs

pres :: IM.IntMap X86Reg
pres = IM.fromList [(0, Rdi), (1, Rsi), (2, Rdx), (3, Rcx), (4, R8), (5, R9), (6, Rax), (7, Rsp)]

preFs :: IM.IntMap FX86Reg
preFs = IM.fromList [(8, XMM0), (9, XMM1), (10, XMM2), (11, XMM3), (12, XMM4), (13, XMM5), (14, XMM6), (15, XMM7)]
