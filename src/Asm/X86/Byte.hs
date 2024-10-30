-- https://defuse.ca/online-x86-assembler.htm
-- https://disasm.pro/
--
-- https://wiki.osdev.org/X86-64_Instruction_Encoding

module Asm.X86.Byte ( allFp, assembleCtx, dbgFp ) where

import           Asm.M
import           Asm.X86
import           Data.Bifunctor   (second)
import           Data.Bits        (Bits, rotateR, shiftL, (.&.), (.|.))
import qualified Data.ByteString  as BS
import           Data.Functor     (($>))
import           Data.Int         (Int32, Int64, Int8)
import qualified Data.IntMap      as IM
import qualified Data.Map.Strict  as M
import           Data.Word
import           Foreign.Ptr      (FunPtr, IntPtr (..), Ptr, ptrToIntPtr)
import           Foreign.Storable (Storable, sizeOf)
import           Hs.FFI
import           Sys.DL

pI :: Ptr a -> Int
pI = (\(IntPtr i) -> i) . ptrToIntPtr

prepAddrs :: [X86 reg freg a] -> IO (Maybe CCtx)
prepAddrs ss = if hasMa ss then Just <$> mem' else pure Nothing

dbgFp asmϵ = do
    (bs, _, ps) <- allFp asmϵ
    mFree ps $> bs

assembleCtx :: CCtx -> (IM.IntMap [Word64], [X86 X86Reg FX86Reg a]) -> IO (BS.ByteString, FunPtr b, Maybe (Ptr Word64))
assembleCtx ctx (ds, isns) = do
    let (sz, lbls) = mkIx 0 isns
    p <- if hasMa isns then allocNear (fst4 ctx) (fromIntegral sz) else allocExec (fromIntegral sz)
    arrs <- aArr ds
    let b = BS.pack.concat$asm 0 (pI p, arrs, Just ctx, lbls) isns
        mP = snd<$>IM.lookupMin arrs
    (b,,mP)<$>finish b p

allFp :: (IM.IntMap [Word64], [X86 X86Reg FX86Reg a]) -> IO ([BS.ByteString], FunPtr b, Maybe (Ptr Word64))
allFp (ds, instrs) = do
    let (sz, lbls) = mkIx 0 instrs
    (fn, p) <- do
        res <- prepAddrs instrs
        case res of
            Just (m, _, _, _) -> (res,) <$> allocNear m (fromIntegral sz)
            _                 -> (res,) <$> allocExec (fromIntegral sz)
    arrs <- aArr ds
    let is = asm 0 (pI p, arrs, fn, lbls) instrs; b = BS.pack.concat$is; bs = BS.pack<$>is
        mP = snd<$>IM.lookupMin arrs
    (bs,,mP)<$>finish b p

data VEXM = F | F38 | F3A

data PP = S6 | F3 | F2

rrNoPre :: RMB reg
        => [Word8]
        -> reg -- ^ r/m
        -> reg -- ^ reg
        -> [Word8]
rrNoPre opc r0 r1 =
    let (_, b0) = modRM r0
        (_, b1) = modRM r1
        modRMB = (0x3 `shiftL` 6) .|. (b1 `shiftL` 3) .|. b0
        in opc++[modRMB]

mkRR opc = mkAR opc 3

mkAR :: (RMB reg0, RMB reg1)
     => [Word8]
     -> Word8 -- ^ mod
     -> reg0 -- ^ r/m
     -> reg1 -- ^ reg
     -> [Word8]
mkAR opc m r0 r1 =
    let (e0, b0) = modRM r0
        (e1, b1) = modRM r1
        prefix = 0x48 .|. (e1 `shiftL` 2) .|. e0
        modRMB = (m `shiftL` 6) .|. (b1 `shiftL` 3) .|. b0
        in prefix:opc++[modRMB]

-- movapd xmm9, xmm5 -> 66 44 0f 28 cd
-- movapd xmm1, xmm5 -> 66 0f 28 cd
--
-- addsd xmm8,xmm10 -> f2 45 0f 58 c2
extSse :: Word8 -> Word8 -> FX86Reg -> FX86Reg -> [Word8]
extSse pre opc r0 r1 =
    let (e0, b0) = modRM r0
        (e1, b1) = modRM r1
        b = 0x40 .|. (e1 `shiftL` 2) .|. e0
        modRMB = (0x3 `shiftL` 6) .|. (b1 `shiftL` 3) .|. b0
        in [pre,b,0xf,opc,modRMB]

vexV4 :: FX86Reg -> Word8
vexV4 XMM0  = 0xf
vexV4 XMM1  = 0xe
vexV4 XMM2  = 0xd
vexV4 XMM3  = 0xc
vexV4 XMM4  = 0xb
vexV4 XMM5  = 0xa
vexV4 XMM6  = 0x9
vexV4 XMM7  = 0x8
vexV4 XMM8  = 0x7
vexV4 XMM9  = 0x6
vexV4 XMM10 = 0x5
vexV4 XMM11 = 0x4
vexV4 XMM12 = 0x3
vexV4 XMM13 = 0x2
vexV4 XMM14 = 0x1
vexV4 XMM15 = 0x0

bitC :: Word8 -> Word8
bitC 0x0 = 0x1
bitC 0x1 = 0x0

bitsm :: VEXM -> Word8
bitsm F   = 0x1
bitsm F38 = 0x2
bitsm F3A = 0x3

ppbits :: PP -> Word8
ppbits S6 = 0x1
ppbits F3 = 0x2
ppbits F2 = 0x3

mkVex :: Word8 -> PP -> FX86Reg -> FX86Reg -> FX86Reg -> [Word8]
mkVex opc pp r0 r1 r2 =
    [0xc5,b,opc,modRMB]
    where b = (bitC e0 `shiftL` 7) .|. (vexV4 r1 `shiftL` 3) .|. ppbits pp
          (e0, b0) = modRM r0
          (_, b2) = modRM r2
          modRMB = (0x3 `shiftL` 6) .|. b0 `shiftL` 3 .|. b2

mkVex3 :: Word8 -> PP -> VEXM -> FX86Reg -> FX86Reg -> FX86Reg -> [Word8]
mkVex3 opc pp mm r0 r1 r2 =
    [0xc4,by0,by1,opc,modRMB]
    where by0 = (bitC e0 `shiftL` 7) .|. (0x1 `shiftL` 6) .|. (bitC e2 `shiftL` 5) .|. bitsm mm
          by1 = 1 `shiftL` 7 .|. (vexV4 r1 `shiftL` 3) .|. ppbits pp
          (e0, b0) = modRM r0
          (e2, b2) = modRM r2
          modRMB = (0x3 `shiftL` 6) .|. b0 `shiftL` 3 .|. b2

mkVexA :: Word8 -> PP -> VEXM -> FX86Reg -> FX86Reg -> Addr X86Reg -> [Word8]
mkVexA opc pp mm xr0 xr1 (RSD b s i d)=
    [0xc4,by0,by1,opc,modRMB,sib]++le d
    where by0 = (bitC e0 `shiftL` 7) .|. (bitC ei `shiftL` 6) .|. (bitC eb `shiftL` 5) .|. bitsm mm
          by1 = 1 `shiftL` 7 .|. vexV4 xr1 `shiftL` 3 .|. ppbits pp
          (e0, b0) = modRM xr0
          (eb, bb) = modRM b
          (ei, bi) = modRM i
          modRMB = 0x1 `shiftL` 6 .|. b0 `shiftL` 3 .|. 0x4
          sib = encS s `shiftL` 6 .|. bi `shiftL` 3 .|. bb
mkVexA opc pp mm xr0 xr1 (RC b@Rsp i) =
    [0xc4,by0,by1,opc,modRMB,sib]++le i
    where by0 = bitC e0 `shiftL` 7 .|. bitC eb `shiftL` 6 .|. bitC eb `shiftL` 5 .|. bitsm mm
          by1 = 1 `shiftL` 7 .|. vexV4 xr1 `shiftL` 3 .|. ppbits pp
          (e0,b0) = modRM xr0
          (eb,bb) = modRM b
          modRMB = 0x1 `shiftL` 6 .|. b0 `shiftL` 3 .|. 0x4
          sib = bb `shiftL` 3 .|. bb

mkIx :: Int -> [X86 X86Reg FX86Reg a] -> (Int, M.Map Label Int)
mkIx ix (Pop _ r:asms) | fits r               = mkIx (ix+1) asms
                       | otherwise            = mkIx (ix+2) asms
mkIx ix (Push _ r:asms) | fits r              = mkIx (ix+1) asms
                        | otherwise           = mkIx (ix+2) asms
mkIx ix (Label _ l:asms)                      = second (M.insert l ix) $ mkIx ix asms
mkIx ix (MovRR{}:asms)                        = mkIx (ix+3) asms
mkIx ix (Movapd _ r0 r1:asms) | fits r0 && fits r1 = mkIx (ix+4) asms
                              | otherwise = mkIx (ix+5) asms
mkIx ix (IAddRR{}:asms)                       = mkIx (ix+3) asms
mkIx ix (And{}:asms)                          = mkIx (ix+3) asms
mkIx ix (ISubRR{}:asms)                       = mkIx (ix+3) asms
mkIx ix (Addsd _ r0 r1:asms) | fits r0 && fits r1 = mkIx (ix+4) asms
                             | otherwise = mkIx (ix+5) asms
mkIx ix (Mulsd _ r0 r1:asms) | fits r0 && fits r1 = mkIx (ix+4) asms
                             | otherwise = mkIx (ix+5) asms
mkIx ix (Divsd _ r0 r1:asms) | fits r0 && fits r1 = mkIx (ix+4) asms
                             | otherwise = mkIx (ix+5) asms
mkIx ix (Vsubsd _ _ _ r:asms) | fits r        = mkIx (ix+4) asms
                              | otherwise     = mkIx (ix+5) asms
mkIx ix (Vaddsd _ _ _ r:asms) | fits r        = mkIx (ix+4) asms
                              | otherwise     = mkIx (ix+5) asms
mkIx ix (Vdivsd _ _ _ r:asms) | fits r        = mkIx (ix+4) asms
                              | otherwise     = mkIx (ix+5) asms
mkIx ix (Vmulsd _ _ _ r:asms) | fits r        = mkIx (ix+4) asms
                              | otherwise     = mkIx (ix+5) asms
mkIx ix (Vmaxsd _ _ _ r:asms) | fits r        = mkIx (ix+4) asms
                              | otherwise     = mkIx (ix+5) asms
mkIx ix (VaddsdA{}:asms)                      = mkIx (ix+7) asms
mkIx ix (VmaxsdA{}:asms)                      = mkIx (ix+7) asms
mkIx ix (Vcmppd _ _ _ r _ :asms) | fits r     = mkIx (ix+5) asms
                                 | otherwise  = mkIx (ix+6) asms
mkIx ix (Vfmadd231sd{}:asms)                  = mkIx (ix+5) asms
mkIx ix (Vfmnadd231sd{}:asms)                 = mkIx (ix+5) asms
mkIx ix (Vfmadd213sd{}:asms)                  = mkIx (ix+5) asms
mkIx ix (Vfmsub213sd{}:asms)                  = mkIx (ix+5) asms
mkIx ix (Vfmsub231sd{}:asms)                  = mkIx (ix+5) asms
mkIx ix (Vfmsub132sd{}:asms)                  = mkIx (ix+5) asms
mkIx ix (Vfmadd231sdA{}:asms)                 = mkIx (ix+7) asms
mkIx ix (CmpRR{}:asms)                        = mkIx (ix+3) asms
mkIx ix (IMulRR{}:asms)                       = mkIx (ix+4) asms
mkIx ix (IMulRA{}:asms)                       = mkIx (ix+6) asms
mkIx ix (XorRR{}:asms)                        = mkIx (ix+3) asms
mkIx ix (MovqXR{}:asms)                       = mkIx (ix+5) asms
mkIx ix (MovqRX{}:asms)                       = mkIx (ix+5) asms
mkIx ix (TestI{}:asms)                        = mkIx (ix+7) asms
mkIx ix ((CmpRI _ _ i):asms) | Just{} <- mi64i8 (fromIntegral i) = mkIx (ix+4) asms
                             | otherwise      = mkIx (ix+7) asms
mkIx ix ((IAddRI _ _ i):asms) | Just{} <- mi64i8 i = mkIx (ix+4) asms
mkIx ix ((IAddRI _ _ i):asms) | Just{} <- mi64i32 i = mkIx (ix+7) asms
mkIx ix ((ISubRI _ _ i):asms) | Just{} <- mi64i8 i = mkIx (ix+4) asms
                              | Just{} <- mi64i32 i = mkIx (ix+7) asms
mkIx ix (MovRI _ r i:asms) | Just{} <- mi64i32 i, i >= 0 && fits r = mkIx (ix+5) asms
mkIx ix (MovRI{}:asms)                        = mkIx (ix+10) asms
mkIx ix (MovRL{}:asms)                        = mkIx (ix+10) asms
mkIx ix (Roundsd _ r0 r1 _:asms) | fits r0 && fits r1 = mkIx (ix+6) asms
mkIx ix (Roundsd{}:asms)                      = mkIx (ix+7) asms
mkIx ix (Cvttsd2si{}:asms)                    = mkIx (ix+5) asms
mkIx ix (Cvtsi2sd{}:asms)                     = mkIx (ix+5) asms
mkIx ix (Ret{}:asms)                          = mkIx (ix+1) asms
mkIx ix (RetL{}:asms)                         = mkIx (ix+1) asms
mkIx ix (Je{}:asms)                           = mkIx (ix+6) asms
mkIx ix (Jne{}:asms)                          = mkIx (ix+6) asms
mkIx ix (Jg{}:asms)                           = mkIx (ix+6) asms
mkIx ix (Jge{}:asms)                          = mkIx (ix+6) asms
mkIx ix (Jl{}:asms)                           = mkIx (ix+6) asms
mkIx ix (Jle{}:asms)                          = mkIx (ix+6) asms
mkIx ix (J{}:asms)                            = mkIx (ix+5) asms
mkIx ix (C{}:asms)                            = mkIx (ix+5) asms
mkIx ix (MovqAX _ (R Rsp) r:asms) | fits r = mkIx (ix+5) asms
mkIx ix (MovqAX _ (R rb) r:asms) | fits rb && fits r = mkIx (ix+4) asms
                                 | otherwise = mkIx (ix+5) asms
mkIx ix (MovqAX _ (RC Rsp _) r1:asms) | fits r1 = mkIx (ix+6) asms
mkIx ix (MovqAX _ (RC R12 _) _:asms)            = mkIx (ix+7) asms
mkIx ix (MovqAX _ (RC rb _) r:asms) | fits rb && fits r = mkIx (ix+5) asms
                                    | otherwise = mkIx (ix+6) asms
mkIx ix (MovqAX _ (RSD b _ i _) r:asms) | fits r && fits b && fits i = mkIx (ix+6) asms
mkIx ix (MovqAX _ RSD{} _:asms)               = mkIx (ix+7) asms
mkIx ix (MovqXA _ _ (R R13):asms)             = mkIx (ix+6) asms
mkIx ix (MovqXA _ r0 (R Rsp):asms) | fits r0  = mkIx (ix+5) asms
                                   | otherwise = mkIx (ix+6) asms
mkIx ix (MovqXA _ r0 (R r1):asms) | fits r0 && fits r1 = mkIx (ix+4) asms
                                  | otherwise = mkIx (ix+5) asms
mkIx ix (MovqXA _ _ (RS R13 _ _):asms)        = mkIx (ix+7) asms
mkIx ix (MovqXA _ _ RSD{}:asms)               = mkIx (ix+7) asms
mkIx ix (MovqXA _ _ RS{}:asms)                = mkIx (ix+6) asms
mkIx ix (MovqXA _ r0 (RC Rsp _):asms) | fits r0 = mkIx (ix+6) asms
                                      | otherwise = mkIx (ix+7) asms
mkIx ix (MovqXA _ xr (RC r _):asms) | fits xr && fits r = mkIx (ix+5) asms
mkIx ix (MovqXA _ _ (RC R12 _):asms)          = mkIx (ix+7) asms
mkIx ix (MovqXA _ _ RC{}:asms)                = mkIx (ix+6) asms
mkIx ix (Fldl2e{}:asms)                       = mkIx (ix+2) asms
mkIx ix (Fldln2{}:asms)                       = mkIx (ix+2) asms
mkIx ix (Fld1{}:asms)                         = mkIx (ix+2) asms
mkIx ix (Fsin{}:asms)                         = mkIx (ix+2) asms
mkIx ix (Fcos{}:asms)                         = mkIx (ix+2) asms
mkIx ix (FldS{}:asms)                         = mkIx (ix+2) asms
mkIx ix (Fld _ (RC Rsp _):asms)               = mkIx (ix+4) asms
mkIx ix (Fyl2x{}:asms)                        = mkIx (ix+2) asms
mkIx ix (Fmulp{}:asms)                        = mkIx (ix+2) asms
mkIx ix (F2xm1{}:asms)                        = mkIx (ix+2) asms
mkIx ix (Fprem{}:asms)                        = mkIx (ix+2) asms
mkIx ix (Faddp{}:asms)                        = mkIx (ix+2) asms
mkIx ix (Fscale{}:asms)                       = mkIx (ix+2) asms
mkIx ix (Fxch{}:asms)                         = mkIx (ix+2) asms
mkIx ix (Fstp _ (RC Rsp _):asms)              = mkIx (ix+4) asms
mkIx ix (Sal{}:asms)                          = mkIx (ix+4) asms
mkIx ix (Sar{}:asms)                          = mkIx (ix+4) asms
mkIx ix (Call{}:asms)                         = mkIx (ix+5) asms
mkIx ix (MovAI32 _ (R Rsp) _:asms)            = mkIx (ix+8) asms
mkIx ix (MovAI32 _ (R Rbp) _:asms)            = mkIx (ix+8) asms
mkIx ix (MovAI32 _ (R R13) _:asms)            = mkIx (ix+8) asms
mkIx ix (MovAI32 _ (R R12) _:asms)            = mkIx (ix+8) asms
mkIx ix (MovAI32 _ R{} _:asms)                = mkIx (ix+7) asms
mkIx ix (MovAI32 _ RC{} _:asms)               = mkIx (ix+8) asms
mkIx ix (MovAR _ (RC Rsp _) _:asms)           = mkIx (ix+5) asms
mkIx ix (MovAR _ (RC R12 _) _:asms)           = mkIx (ix+5) asms
mkIx ix (MovAR _ RC{} _:asms)                 = mkIx (ix+4) asms
mkIx ix (MovAR _ (RC32 Rsp _) _:asms)         = mkIx (ix+8) asms
mkIx ix (MovAR _ RC32{} _:asms)               = mkIx (ix+7) asms
mkIx ix (MovAR _ RSD{} _:asms)                = mkIx (ix+5) asms
mkIx ix (MovAR _ RS{} _:asms)                 = mkIx (ix+4) asms
mkIx ix (MovAR _ (R Rsp) _:asms)              = mkIx (ix+4) asms
mkIx ix (MovAR _ (R Rbp) _:asms)              = mkIx (ix+4) asms
mkIx ix (MovAR _ (R R13) _:asms)              = mkIx (ix+4) asms
mkIx ix (MovAR _ R{} _:asms)                  = mkIx (ix+3) asms
mkIx ix (MovRA _ _ (RS Rbp _ _):asms)         = mkIx (ix+5) asms
mkIx ix (MovRA _ _ (RS R13 _ _):asms)         = mkIx (ix+5) asms
mkIx ix (MovRA _ _ RS{}:asms)                 = mkIx (ix+4) asms
mkIx ix (MovRA _ _ RSD{}:asms)                = mkIx (ix+5) asms
mkIx ix (MovRA _ _ (R Rsp):asms)              = mkIx (ix+4) asms
mkIx ix (MovRA _ _ (R Rbp):asms)              = mkIx (ix+4) asms
mkIx ix (MovRA _ _ (R R13):asms)              = mkIx (ix+4) asms
mkIx ix (MovRA _ _ R{}:asms)                  = mkIx (ix+3) asms
mkIx ix (MovRA _ _ (RC Rsp _):asms)           = mkIx (ix+5) asms
mkIx ix (MovRA _ _ (RC R12 _):asms)           = mkIx (ix+5) asms
mkIx ix (MovRA _ _ RC{}:asms)                 = mkIx (ix+4) asms
mkIx ix (MovRA _ _ (RC32 Rsp _):asms)         = mkIx (ix+8) asms
mkIx ix (MovRA _ _ RC32{}:asms)               = mkIx (ix+7) asms
mkIx ix (Sqrtsd _ r0 r1:asms) | fits r0 && fits r1 = mkIx (ix+4) asms
                              | otherwise     = mkIx (ix+5) asms
mkIx ix (Not{}:asms)                          = mkIx (ix+3) asms
mkIx ix (Rdrand{}:asms)                       = mkIx (ix+4) asms
mkIx ix (Cmovnle{}:asms)                      = mkIx (ix+4) asms
mkIx ix (Cmovnl{}:asms)                       = mkIx (ix+4) asms
mkIx ix (Cmovne{}:asms)                       = mkIx (ix+4) asms
mkIx ix (Cmove{}:asms)                        = mkIx (ix+4) asms
mkIx ix (Cmovl{}:asms)                        = mkIx (ix+4) asms
mkIx ix (Cmovle{}:asms)                       = mkIx (ix+4) asms
mkIx ix (Fninit{}:asms)                       = mkIx (ix+2) asms
mkIx ix (IDiv{}:asms)                         = mkIx (ix+3) asms
mkIx ix (Neg{}:asms)                          = mkIx (ix+3) asms
mkIx ix []                                    = (ix, M.empty)
mkIx _ (instr:_) = error (show instr)

fits :: RMB reg => reg -> Bool
fits r = let (e, _) = modRM r in e == 0

asm :: Int -> (Int, IM.IntMap (Ptr Word64), Maybe CCtx, M.Map Label Int) -> [X86 X86Reg FX86Reg a] -> [[Word8]]
asm _ _ [] = []
asm ix st (Push _ r:asms) | fits r =
    let (_, b0) = modRM r
        isn = 0x50 .|. b0
    in [isn]:asm (ix+1) st asms
                          | otherwise =
    let (_, b0) = modRM r
        instr = [0x41, 0x50 .|. b0]
    in instr:asm (ix+2) st asms
asm ix st (Pop _ r:asms) | (0, b0) <- modRM r=
    let isn = 0x58 .|. b0
    in [isn]:asm (ix+1) st asms
                         | otherwise =
    let (_, b0) = modRM r
        instr = [0x41, 0x58 .|. b0]
    in instr:asm (ix+2) st asms
asm ix st (Label{}:asms) =
    asm ix st asms
asm ix st (MovRR _ r0 r1:asms) =
    mkRR [0x89] r0 r1:asm (ix+3) st asms
asm ix st (MovRA _ r0 (RC r1@Rsp i8):asms) =
    let (e0, b0) = modRM r0
        (0, b1) = modRM r1
        pref = 0x48 .|. e0 `shiftL` 2
        modB = 0x1 `shiftL` 6 .|. b0 `shiftL` 3 .|. 4
        sib = b1 `shiftL` 3 .|. b1
        opc=0x8b; instr = pref:opc:modB:sib:le i8
    in instr:asm (ix+5) st asms
asm ix st (MovRA _ r0 (RC r1@R12 i8):asms) =
    let (e0, b0) = modRM r0
        (e1, b1) = modRM r1
        pref = 0x48 .|. e0 `shiftL` 2 .|. e1
        modB = 0x1 `shiftL` 6 .|. b0 `shiftL` 3 .|. 4
        sib = b1 `shiftL` 3 .|. b1
        opc=0x8b; instr = pref:opc:modB:sib:le i8
    in instr:asm (ix+5) st asms
asm ix st (MovRA _ r0 (RC32 r1@Rsp i32):asms) =
    let (e0, b0) = modRM r0
        (0, b1) = modRM r1
        pref = 0x48 .|. e0 `shiftL` 2
        modB = 0x2 `shiftL` 6 .|. b0 `shiftL` 3 .|. 4
        sib = b1 `shiftL` 3 .|. b1
        opc=0x8b; instr = pref:opc:modB:sib:le i32
    in instr:asm (ix+8) st asms
asm ix st (MovRA _ r0 (RC r1 i8):asms) =
    let (e0, b0) = modRM r0
        (e1, b1) = modRM r1
        pref = 0x48 .|. (e0 `shiftL` 2) .|. e1
        modB = 0x1 `shiftL` 6 .|. (b0 `shiftL` 3) .|. b1
        opc=0x8b; instr = pref:opc:modB:le i8
    in instr:asm (ix+4) st asms
asm ix st (MovRA _ r0 (RC32 r1 i32):asms) =
    let (e0, b0) = modRM r0
        (e1, b1) = modRM r1
        pref = 0x48 .|. (e0 `shiftL` 2) .|. e1
        modB = 0x2 `shiftL` 6 .|. (b0 `shiftL` 3) .|. b1
        opc=0x8b; instr = pref:opc:modB:le i32
    in instr:asm (ix+7) st asms
asm ix st (MovqXA _ r0 (RC r1@Rsp i8):asms) | fits r0 =
    let (_, b0) = modRM r0
        (_, b1) = modRM r1
        modB = 0x1 `shiftL` 6 .|. b0 `shiftL` 3 .|. 0x4
        sib = b1 `shiftL` 3 .|. b1
        instr = 0xf3:0xf:0x7e:modB:sib:le i8
    in instr:asm (ix+6) st asms
-- https://stackoverflow.com/questions/52522544/rbp-not-allowed-as-sib-base
asm ix st (MovqXA l r0 (R R13):asms) = asm ix st (MovqXA l r0 (RC R13 0):asms)
asm ix st (MovqXA _ r0 (R r1@Rsp):asms) | (0, b0) <- modRM r0 =
    let (0, b1) = modRM r1
        modB = b0 `shiftL` 3 .|. 4
        sib = b1 `shiftL` 3 .|. b1
        isn = [0xf3,0x0f,0x7e,modB,sib]
    in isn:asm (ix+5) st asms
asm ix st (MovqXA _ r0 (R r1):asms) | (0, b0) <- modRM r0, (0, b1) <- modRM r1 =
    let modB = b0 `shiftL` 3 .|. b1
        instr = [0xf3, 0x0f, 0x7e, modB]
    in instr:asm (ix+4) st asms
asm ix st (MovqXA _ r0 (RC r1 i8):asms) | (0, b0) <- modRM r0, (0, b1) <- modRM r1 =
    let modB = 0x1 `shiftL` 6 .|. b0 `shiftL` 3 .|. b1
        instr = 0xf3:0x0f:0x7e:modB:le i8
    in instr:asm (ix+5) st asms
asm ix st (MovqXA _ r (RC rb@R12 i8):asms) =
    let (e, b) = modRM r
        (eb, bb) = modRM rb
        modB = 0x1 `shiftL` 6 .|. b `shiftL` 3 .|. 0x4
        sib = 0x4 `shiftL` 3 .|. bb
        pre = 0x48 .|. e `shiftL` 2 .|. eb
        isn = 0x66:pre:0xf:0x6e:modB:sib:le i8
    in isn:asm (ix+7) st asms
asm ix st (MovqXA _ r (RC rb@Rsp i8):asms) =
    let (e, b) = modRM r
        (eb, bb) = modRM rb
        modB = 0x1 `shiftL` 6 .|. b `shiftL` 3 .|. 0x4
        sib = 0x4 `shiftL` 3 .|. bb
        pre = 0x48 .|. e `shiftL` 2 .|. eb
        isn = 0x66:pre:0xf:0x6e:modB:sib:le i8
    in isn:asm (ix+7) st asms
asm ix st (MovqXA _ r0 (RC r1 i8):asms) =
    let (e0, b0) = modRM r0
        (e1, b1) = modRM r1
        modB = 0x1 `shiftL` 6 .|. b0 `shiftL` 3 .|. b1
        pre = 0x48 .|. e0 `shiftL` 2 .|. e1
        instr = 0x66:pre:0xf:0x6e:modB:le i8
    in instr:asm (ix+6) st asms
asm ix st (MovqXA _ r0 (R r1@Rsp):asms) =
    let (e0, b0) = modRM r0
        (0, b1) = modRM r1
        modB = b0 `shiftL` 3 .|. 4
        pre = 0x48 .|. e0 `shiftL` 2
        sib = b1 `shiftL` 3 .|. b1
        instr = [0x66, pre, 0xf, 0x6e, modB, sib]
    in instr:asm (ix+6) st asms
asm ix st (MovqXA _ r0 (R r1):asms) =
    let (e0, b0) = modRM r0
        (e1, b1) = modRM r1
        modB = b0 `shiftL` 3 .|. b1
        pre = 0x48 .|. e0 `shiftL` 2 .|. e1
        instr = [0x66, pre, 0xf, 0x6e, modB]
    in instr:asm (ix+5) st asms
-- https://stackoverflow.com/questions/52522544/rbp-not-allowed-as-sib-base
asm ix st (MovqAX _ (RC r0@Rsp i8) r1:asms) | (0, b1) <- modRM r1 =
    let (0, b0) = modRM r0
        modB = 0x1 `shiftL` 6 .|. b1 `shiftL` 3 .|. 0x4
        sib = b0 `shiftL` 3 .|. b0
        instr = 0x66:0x0f:0xd6:modB:sib:le i8
    in instr:asm (ix+6) st asms
asm ix st (MovqAX _ (R rb@Rsp) r:asms) | (0, b) <- modRM r =
    let (0, bb) = modRM rb
        modB = b `shiftL` 3 .|. 0x4
        sib = bb `shiftL` 3 .|. bb
        isn = [0x66,0x0f,0xd6,modB,sib]
    in isn:asm (ix+5) st asms
asm ix st (MovqAX _ (R rb) r:asms) | (0, bb) <- modRM rb, (0, b) <- modRM r =
    let modB = b `shiftL` 3 .|. bb
        isn = [0x66,0x0f,0xd6,modB]
    in isn:asm (ix+4) st asms
asm ix st (MovqAX _ (R rb) r:asms) =
    let (eb, bb) = modRM rb; (e, b) = modRM r
        pre = 0x48 .|. e `shiftL` 3 .|. eb
        modB = b `shiftL` 3 .|. bb
        isn = [0x66,pre,0x0f,0xd6,modB]
    in isn:asm (ix+5) st asms
asm ix st (MovqAX _ (RC rb i8) r:asms) | (0, bb) <- modRM rb, (0, b) <- modRM r =
    let modB = 0x1 `shiftL` 6 .|. b `shiftL` 3 .|. bb
        isn = 0x66:0x0f:0xd6:modB:le i8
    in isn:asm (ix+5) st asms
asm ix st (MovqAX _ (RC rb@R12 i8) r:asms) =
    let (eb, bb) = modRM rb
        (e, b) = modRM r
        modB = 0x1 `shiftL` 6 .|. b `shiftL` 3 .|. 0x4
        pre = 0x48 .|. e `shiftL` 2 .|. eb
        sib = 0x4 `shiftL` 3 .|. bb
        isn = 0x66:pre:0x0f:0xd6:modB:sib:le i8
    in isn:asm (ix+7) st asms
asm ix st (MovqAX _ (RC rb i8) r:asms) =
    let (eb, bb) = modRM rb
        (e, b) = modRM r
        modB = 0x1 `shiftL` 6 .|. b `shiftL` 3 .|. bb
        pre = 0x48 .|. e `shiftL` 2 .|. eb
        isn = 0x66:pre:0x0f:0xd6:modB:le i8
    in isn:asm (ix+6) st asms
asm ix st (MovqAX _ (RSD rb s ri d) r:asms) | (0, b) <- modRM r, (0, bi) <- modRM ri, (0, bb) <- modRM rb =
    let modB = 0x1 `shiftL` 6 .|. b `shiftL` 3 .|. 0x4
        sib = encS s `shiftL` 6 .|. bi `shiftL` 3 .|. bb
        instr = 0x66:0x0f:0xd6:modB:sib:le d
    in instr:asm (ix+6) st asms
asm ix st (MovqAX _ (RSD rb s ri d) r:asms) =
    let (e, b) = modRM r; (eb, bb) = modRM rb; (ei, bi) = modRM ri
        modB = 0x1 `shiftL` 6 .|. b `shiftL` 3 .|. 0x4
        rex = 0x48 .|. e `shiftL` 2 .|. ei `shiftL` 1 .|. eb
        sib = encS s `shiftL` 6 .|. bi `shiftL` 3 .|. bb
        instr = 0x66:rex:0x0f:0x7e:modB:sib:le d
    in instr:asm (ix+7) st asms
asm ix st (MovqXA l r (RS R13 s ri):asms) = asm ix st (MovqXA l r (RSD R13 s ri 0):asms)
asm ix st (MovqXA _ r (RS rb s ri):asms) =
    let (e, b) = modRM r; (eb, bb) = modRM rb; (ei, bi) = modRM ri
        modB = b `shiftL` 3 .|. 4
        rex = 0x48 .|. e `shiftL` 2 .|. ei `shiftL` 1 .|. eb
        sib = encS s `shiftL` 6 .|. bi `shiftL` 3 .|. bb
        instr = [0x66,rex,0x0f,0x6e,modB,sib]
    in instr:asm (ix+6) st asms
asm ix st (MovqXA _ r (RSD rb s ri d):asms) =
    let (e, b) = modRM r; (eb, bb) = modRM rb; (ei, bi) = modRM ri
        modB = 1 `shiftL` 6 .|. b `shiftL` 3 .|. 4
        rex = 0x48 .|. e `shiftL` 2 .|. ei `shiftL` 1 .|. eb
        sib = encS s `shiftL` 6 .|. bi `shiftL` 3 .|. bb
        instr = 0x66:rex:0x0f:0x6e:modB:sib:le d
    in instr:asm (ix+7) st asms
asm ix st (Movapd _ r0 r1:asms) | fits r0 && fits r1 =
    rrNoPre [0x66,0x0f,0x28] r1 r0:asm (ix+4) st asms
                                | otherwise =
    extSse 0x66 0x28 r1 r0:asm (ix+5) st asms
asm ix st (IAddRR _ r0 r1:asms) =
    mkRR [0x01] r0 r1:asm (ix+3) st asms
asm ix st (And _ r0 r1:asms) =
    mkRR [0x21] r0 r1:asm (ix+3) st asms
asm ix st (ISubRR _ r0 r1:asms) =
    mkRR [0x29] r0 r1:asm (ix+3) st asms
asm ix st (IDiv _ r:asms) =
    let (e, b) = modRM r
        modB = 3 `shiftL` 6 .|. 7 `shiftL` 3 .|. b
        pre = 0x48 .|. e
        isn = [pre,0xf7,modB]
    in isn:asm (ix+3) st asms
asm ix st (Addsd _ r0 r1:asms) | fits r0 && fits r1 =
    rrNoPre [0xf2,0x0f,0x58] r1 r0:asm (ix+4) st asms
                               | otherwise =
    extSse 0xf2 0x58 r1 r0:asm (ix+5) st asms
asm ix st (Mulsd _ r0 r1:asms) | fits r0 && fits r1 =
    rrNoPre [0xf2,0x0f,0x59] r1 r0:asm (ix+4) st asms
                               | otherwise =
    extSse 0xf2 0x59 r1 r0:asm (ix+5) st asms
asm ix st (Divsd _ r0 r1:asms) | fits r0 && fits r1 =
    rrNoPre [0xf2,0x0f,0x5e] r1 r0:asm (ix+4) st asms
                               | otherwise =
    extSse 0xf2 0x5e r1 r0:asm (ix+5) st asms
asm ix st (Vsubsd _ r0 r1 r2:asms) | fits r2 =
    mkVex 0x5c F2 r0 r1 r2:asm (ix+4) st asms
                                   | otherwise =
    mkVex3 0x5c F2 F r0 r1 r2:asm (ix+5) st asms
asm ix st (Vaddsd _ r0 r1 r2:asms) | fits r2 =
    mkVex 0x58 F2 r0 r1 r2:asm (ix+4) st asms
                                   | otherwise =
    mkVex3 0x58 F2 F r0 r1 r2:asm (ix+5) st asms
asm ix st (Vdivsd _ r0 r1 r2:asms) | fits r2 =
    mkVex 0x5e F2 r0 r1 r2:asm (ix+4) st asms
                                   | otherwise =
    mkVex3 0x5e F2 F r0 r1 r2:asm (ix+5) st asms
asm ix st (Vmulsd _ r0 r1 r2:asms) | fits r2 =
    mkVex 0x59 F2 r0 r1 r2:asm (ix+4) st asms
                                   | otherwise =
    mkVex3 0x59 F2 F r0 r1 r2:asm (ix+5) st asms
asm ix st (Vmaxsd _ r0 r1 r2:asms) | fits r2 =
    mkVex 0x5f F2 r0 r1 r2:asm (ix+4) st asms
                                   | otherwise =
    mkVex3 0x5f F2 F r0 r1 r2:asm (ix+5) st asms
asm ix st (VaddsdA _ r0 r1 a:asms) =
    mkVexA 0x58 F2 F r0 r1 a:asm (ix+7) st asms
asm ix st (VmaxsdA _ r0 r1 a:asms) =
    mkVexA 0x5f F2 F r0 r1 a:asm (ix+7) st asms
asm ix st (Vcmppd _ r0 r1 r2 p:asms) | fits r2 =
    (mkVex 0xc2 S6 r0 r1 r2 ++ le (imm8 p)):asm (ix+5) st asms
                                     | otherwise =
    (mkVex3 0xc2 S6 F r0 r1 r2 ++ le (imm8 p)):asm (ix+6) st asms
asm ix st (Vfmadd231sd _ r0 r1 r2:asms) =
    mkVex3 0xb9 S6 F38 r0 r1 r2:asm (ix+5) st asms
asm ix st (Vfmnadd231sd _ r0 r1 r2:asms) =
    mkVex3 0xbd S6 F38 r0 r1 r2:asm (ix+5) st asms
asm ix st (Vfmadd213sd _ r0 r1 r2:asms) =
    mkVex3 0xa9 S6 F38 r0 r1 r2:asm (ix+5) st asms
asm ix st (Vfmsub132sd _ r0 r1 r2:asms) =
    mkVex3 0x9b S6 F38 r0 r1 r2:asm (ix+5) st asms
asm ix st (Vfmsub213sd _ r0 r1 r2:asms) =
    mkVex3 0xab S6 F38 r0 r1 r2:asm (ix+5) st asms
asm ix st (Vfmsub231sd _ r0 r1 r2:asms) =
    mkVex3 0xbb S6 F38 r0 r1 r2:asm (ix+5) st asms
asm ix st (Vfmadd231sdA _ r0 r1 a:asms) =
    mkVexA 0xb9 S6 F38 r0 r1 a:asm (ix+7) st asms
asm ix st (Roundsd _ r0 r1 i:asms) | fits r0 && fits r1 =
    (rrNoPre [0x66,0x0f,0x3a,0x0b] r1 r0++le (roundMode i)):asm (ix+6) st asms
asm ix st (Roundsd _ r0 r1 i:asms) =
    (0x66:mkAR [0xf,0x3a,0xb] 0 r1 r0++le (roundMode i)):asm (ix+7) st asms
asm ix st (Cvttsd2si _ r0 r1:asms) =
    (0xf2:mkRR [0x0f,0x2c] r1 r0):asm (ix+5) st asms
asm ix st (Cvtsi2sd _ fr r:asms) =
    (0xf2:mkRR [0x0f,0x2a] r fr):asm (ix+5) st asms
asm ix st (Sqrtsd _ r0 r1:asms) | fits r0 && fits r1 =
    rrNoPre [0xf2,0x0f,0x51] r1 r0:asm (ix+4) st asms
                                | otherwise =
    extSse 0xf2 0x51 r1 r0:asm (ix+5) st asms
asm ix st (CmpRR _ r0 r1:asms) =
    mkRR [0x39] r0 r1:asm (ix+3) st asms
asm ix st (MovqXR _ fr r:asms) =
    (0x66:mkRR [0x0f,0x6e] r fr):asm (ix+5) st asms
asm ix st (MovqRX _ r fr:asms) =
    (0x66:mkRR [0x0f,0x7e] r fr):asm (ix+5) st asms
asm ix st (IMulRR _ r0 r1:asms) =
    -- flip r0,r1 as instr. uses them differently from sub, etc.
    mkRR [0x0f, 0xaf] r1 r0:asm (ix+4) st asms
asm ix st (IMulRA _ r (RSD rb s ri d):asms) =
    let (e, b) = modRM r
        (eb, bb) = modRM rb
        (ei, bi) = modRM ri
        pre = 0x48 .|. e `shiftL` 2 .|. ei `shiftL` 1 .|. eb
        modB = 0x1 `shiftL` 6 .|. b `shiftL` 3 .|. 0x4
        sib = encS s `shiftL` 6 .|. bi `shiftL` 3 .|. bb
    in (pre:0x0f:0xaf:modB:sib:le d):asm (ix+6) st asms
asm ix st (XorRR _ r0 r1:asms) =
    mkRR [0x31] r0 r1:asm (ix+3) st asms
asm ix st (TestI _ r i:asms) =
    let (e, b) = modRM r
        prefix = 0x48 .|. e
        modB = 0x3 `shiftL` 6 .|. b
    in (prefix:0xf7:modB:le i):asm (ix+7) st asms
asm ix st (CmpRI _ r i:asms) | Just i8 <- mi64i8 (fromIntegral i) =
    let (e, b) = modRM r
        prefix = 0x48 .|. e
        modRMB = (0x3 `shiftL` 6) .|. (0o7 `shiftL` 3) .|. b
    in (prefix:0x83:modRMB:le i8):asm (ix+4) st asms
asm ix st (CmpRI _ r i32:asms) =
    let (e, b) = modRM r
        prefix = 0x48 .|. e
        modRMB = (0x3 `shiftL` 6) .|. (0o7 `shiftL` 3) .|. b
    in (prefix:0x81:modRMB:le i32):asm (ix+7) st asms
asm ix st (IAddRI _ r i:asms) | Just i8 <- mi64i8 i =
    let (e, b) = modRM r
        prefix = 0x48 .|. e
        modRMB = (0x3 `shiftL` 6) .|. b
    in (prefix:0x83:modRMB:le i8):asm (ix+4) st asms
asm ix st (IAddRI _ r i:asms) | Just i32 <- mi64i32 i =
    let (e, b) = modRM r
        prefix = 0x48 .|. e
        modRMB = (0x3 `shiftL` 6) .|. b
    in (prefix:0x81:modRMB:le i32):asm (ix+7) st asms
asm ix st (ISubRI _ r i:asms) | Just i8 <- mi64i8 i =
    let (e, b) = modRM r
        prefix = 0x48 .|. e
        modRMB = (0x3 `shiftL` 6) .|. (0x5 `shiftL` 3) .|. b
    in (prefix:0x83:modRMB:le i8):asm (ix+4) st asms
asm ix st (ISubRI _ r i:asms) | Just i32 <- mi64i32 i =
    let (e, b) = modRM r
        prefix = 0x48 .|. e
        modRMB = (0x3 `shiftL` 6) .|. (0x5 `shiftL` 3) .|. b
    in (prefix:0x81:modRMB:le i32):asm (ix+7) st asms
                           | otherwise = error "Not implemented yet: handling 64-bit immediates"
-- TODO: r32<-i32 like nasm does (note r32<-i32 (zero-extended) vs.
-- sign-extended
-- https://stackoverflow.com/questions/40315803/difference-between-movq-and-movabsq-in-x86-64
asm ix st (MovRI _ r i:asms) | Just i32 <- mi64i32 i, i >= 0 && fits r =
    let (_, b) = modRM r
        opc = 0xb8 .|. b
    in (opc:cd i32):asm (ix+5) st asms
    -- TODO: 0xc7 for case i<0
asm ix st (MovRI _ r i:asms) =
    let (e, b) = modRM r
        pre = (0x48 .|. e:) . (0xB8 .|. b:)
    in pre (le i):asm (ix+10) st asms
asm ix st (MovRL x r l:asms) =
    let p=arr l st
    in asm ix st (MovRI x r (fromIntegral$pI p):asms)
asm ix st (Ret{}:asms) =
    [0xc3]:asm (ix+1) st asms
asm ix st (RetL{}:asms) =
    [0xc3]:asm (ix+1) st asms
asm ix st (Je _ l:asms) =
    let lIx = get l st
        instr = let offs = lIx-ix-6 in 0x0f:0x84:cd (fromIntegral offs :: Int32)
    in instr:asm (ix+6) st asms
asm ix st (Jne _ l:asms) =
    let lIx = get l st
        instr = let offs = lIx-ix-6 in 0x0f:0x85:cd (fromIntegral offs :: Int32)
    in instr:asm (ix+6) st asms
asm ix st (Jg _ l:asms) =
    let lIx = get l st
        instr = let offs = lIx-ix-6 in 0x0f:0x8f:cd (fromIntegral offs :: Int32)
    in instr:asm (ix+6) st asms
asm ix st (Jge _ l:asms) =
    let lIx = get l st
        instr = let offs = lIx-ix-6 in 0x0f:0x8d:cd (fromIntegral offs :: Int32)
    in instr:asm (ix+6) st asms
asm ix st (Jl _ l:asms) =
    let lIx = get l st
        instr = let offs = lIx-ix-6 in 0x0f:0x8c:cd (fromIntegral offs :: Int32)
    in instr:asm (ix+6) st asms
asm ix st (Jle _ l:asms) =
    let lIx = get l st
        instr = let offs = lIx-ix-6 in 0x0f:0x8e:cd (fromIntegral offs :: Int32)
    in instr:asm (ix+6) st asms
asm ix st (J _ l:asms) =
    let lIx = get l st
        instr = let offs = lIx-ix-5 in 0xe9:cd (fromIntegral offs :: Int32)
    in instr:asm (ix+5) st asms
asm ix st (C _ l:asms) =
    let lIx = get l st
        instr = let offs = lIx-ix-5 in 0xe8:cd (fromIntegral offs :: Int32)
    in instr:asm (ix+5) st asms
asm ix st (Fmulp{}:asms) =
    [0xde,0xc9]:asm (ix+2) st asms
asm ix st (F2xm1{}:asms) =
    [0xd9,0xf0]:asm (ix+2) st asms
asm ix st (Fldl2e{}:asms) =
    [0xd9,0xea]:asm (ix+2) st asms
asm ix st (Fldln2{}:asms) =
    [0xd9,0xed]:asm (ix+2) st asms
asm ix st (Fld1{}:asms) =
    [0xd9,0xe8]:asm (ix+2) st asms
asm ix st (Fsin{}:asms) =
    [0xd9,0xfe]:asm (ix+2) st asms
asm ix st (Fcos{}:asms) =
    [0xd9,0xff]:asm (ix+2) st asms
asm ix st (FldS _ (ST i):asms) =
    let isn = [0xd9, 0xc0+fromIntegral i] in isn:asm (ix+2) st asms
asm ix st (Fprem{}:asms) =
    [0xd9,0xf8]:asm (ix+2) st asms
asm ix st (Faddp{}:asms) =
    [0xde,0xc1]:asm (ix+2) st asms
asm ix st (Fscale{}:asms) =
    [0xd9,0xfd]:asm (ix+2) st asms
asm ix st (Fninit{}:asms) =
    [0xdb,0xe3]:asm (ix+2) st asms
asm ix st (Fxch _ (ST i):asms) =
    let isn = [0xd9, 0xc9+fromIntegral i] in isn:asm (ix+2) st asms
asm ix st (Fyl2x{}:asms) =
    [0xd9,0xf1]:asm (ix+2) st asms
asm ix st (Fld _ (RC r@Rsp i8):asms) =
    let (_, b) = modRM r
        modB = 0x1 `shiftL` 6 .|. 0x4
        sib = b `shiftL` 3 .|. b
        instr = 0xdd:modB:sib:le i8
    in instr:asm (ix+4) st asms
asm ix st (Fstp _ (RC r@Rsp i8):asms) =
    let (_, b) = modRM r
        modB = 0x1 `shiftL` 6 .|. 0x3 `shiftL` 3 .|. 0x4
        sib = b `shiftL` 3 .|. b
        instr = 0xdd:modB:sib:le i8
    in instr:asm (ix+4) st asms
asm ix st (Sal _ r i:asms) =
    let (e, b) = modRM r
        modRMB = (0x3 `shiftL` 6) .|. (0x4 `shiftL` 3) .|. b
        pre = 0x48 .|. e
        instr = pre:0xc1:modRMB:le i
    in instr:asm (ix+4) st asms
asm ix st (Sar _ r i:asms) =
    let (e, b) = modRM r
        modRMB = (0x3 `shiftL` 6) .|. (0x7 `shiftL` 3) .|. b
        pre = 0x48 .|. e
        instr = pre:0xc1:modRMB:le i
    in instr:asm (ix+4) st asms
asm ix st (MovAI32 l (R R13) i32:asms) = asm ix st (MovAI32 l (RC R13 0) i32:asms)
asm ix st (MovAI32 l (R Rbp) i32:asms) = asm ix st (MovAI32 l (RC Rbp 0) i32:asms)
asm ix st (MovAI32 _ (R Rsp) i32:asms) =
    let (0, b) = modRM Rsp
        modB = 0x4
        sib = b `shiftL` 3 .|. b
        instr = 0x48:0xc7:modB:sib:le i32
    in instr:asm (ix+8) st asms
asm ix st (MovAI32 _ (R R12) i32:asms) =
    let (e, b) = modRM R12
        modB = 0x4
        sib = 0x4 `shiftL` 3 .|. b
        pre = 0x48 .|. e
        isn = pre:0xc7:modB:sib:le i32
    in isn:asm (ix+8) st asms
asm ix st (MovAI32 _ (R r) i32:asms) =
    let (e, b) = modRM r
        modRMB = b
        pre = 0x48 .|. e
        instr = pre:0xc7:modRMB:le i32
    in instr:asm (ix+7) st asms
asm ix st (MovAI32 _ (RC r i8) i32:asms) =
    let (e, b) = modRM r
        modB = 0x1 `shiftL` 6 .|. b
        pre = 0x48 .|. e
        isn = pre:0xc7:modB:le i8 ++ le i32
    in isn:asm (ix+8) st asms
asm ix st (MovAR _ (RC Rsp i8) r:asms) =
    let (e, b) = modRM r
        (0, bi) = modRM Rsp
        pre = 0x48 .|. e `shiftL` 2
        modB = 0x1 `shiftL` 6 .|. b `shiftL` 3 .|. 0x4
        sib = bi `shiftL` 3 .|. bi
        instr = pre:0x89:modB:sib:le i8
    in instr:asm (ix+5) st asms
asm ix st (MovAR _ (RC32 Rsp i32) r:asms) =
    let (e, b) = modRM r
        (0, bi) = modRM Rsp
        pre = 0x48 .|. e `shiftL` 2
        modB = 0x2 `shiftL` 6 .|. b `shiftL` 3 .|. 0x4
        sib = bi `shiftL` 3 .|. bi
        instr = pre:0x89:modB:sib:le i32
    in instr:asm (ix+8) st asms
asm ix st (MovAR _ (RC R12 i8) r:asms) =
    let (e, b) = modRM r
        (ei, bi) = modRM R12
        pre = 0x48 .|. e `shiftL` 2 .|. ei
        modB = 0x1 `shiftL` 6 .|. b `shiftL` 3 .|. 0x4
        sib = bi `shiftL` 3 .|. bi
        instr = pre:0x89:modB:sib:le i8
    in instr:asm (ix+5) st asms
asm ix st (MovAR _ (RC ar i8) r:asms) =
    (mkAR [0x89] 1 ar r++le i8):asm (ix+4) st asms
asm ix st (MovAR _ (RC32 ar i32) r:asms) =
    (mkAR [0x89] 2 ar r++le i32):asm (ix+7) st asms
asm ix st (MovRA l r (RS b@Rbp s i):asms) = asm ix st (MovRA l r (RSD b s i 0):asms)
asm ix st (MovRA l r (RS b@R13 s i):asms) = asm ix st (MovRA l r (RSD b s i 0):asms)
asm ix st (MovRA _ r (RS b s i):asms) =
    let (e0, b0) = modRM r
        (eb, bb) = modRM b
        (ei, bi) = modRM i
        pre = 0x48 .|. e0 `shiftL` 2 .|. ei `shiftL` 1 .|. eb
        modB = b0 `shiftL` 3 .|. 4
        sib = encS s `shiftL` 6 .|. bi `shiftL` 3 .|. bb
        instr = [pre,0x8b,modB,sib]
    in instr:asm (ix+4) st asms
asm ix st (MovAR _ (RSD b s i i8) r:asms) =
    let (eb, bb) = modRM b
        (ei, bi) = modRM i
        (e0, b0) = modRM r
        pre = 0x48 .|. e0 `shiftL` 2 .|. ei `shiftL` 1 .|. eb
        modRMB = 1 `shiftL` 6 .|. b0 `shiftL` 3 .|. 4
        sib = encS s `shiftL` 6 .|. bi `shiftL` 3 .|. bb
        instr = pre:0x89:modRMB:sib:le i8
    in instr:asm (ix+5) st asms
asm ix st (MovRA _ r (RSD b s i i8):asms) =
    let (eb, bb) = modRM b
        (ei, bi) = modRM i
        (e0, b0) = modRM r
        pre = 0x48 .|. e0 `shiftL` 2 .|. ei `shiftL` 1 .|. eb
        modRMB = 1 `shiftL` 6 .|. b0 `shiftL` 3 .|. 4
        sib = encS s `shiftL` 6 .|. bi `shiftL` 3 .|. bb
        instr = pre:0x8b:modRMB:sib:le i8
    in instr:asm (ix+5) st asms
asm ix st (MovAR l (R Rbp) r:asms) = asm ix st (MovAR l (RC Rbp 0) r:asms)
asm ix st (MovAR l (R R13) r:asms) = asm ix st (MovAR l (RC R13 0) r:asms)
asm ix st (MovAR _ (R ar@Rsp) r:asms) =
    let (0, bi) = modRM ar
        (e0, b0) = modRM r
        pre = 0x48 .|. e0 `shiftL` 2
        modRMB = b0 `shiftL` 3 .|. 4; sib = bi `shiftL` 3 .|. bi
        isn = [pre,0x89,modRMB,sib]
    in isn:asm (ix+4) st asms
asm ix st (MovRA _ r (R ar@Rsp):asms) =
    let (0, bi) = modRM ar
        (e0, b0) = modRM r
        pre = 0x48 .|. e0 `shiftL` 2
        modRMB = b0 `shiftL` 3 .|. 4; sib = bi `shiftL` 3 .|. bi
        isn = [pre,0x8b,modRMB,sib]
    in isn:asm (ix+4) st asms
asm ix st (MovAR _ (R ar) r:asms) =
    mkAR [0x89] 0 ar r:asm (ix+3) st asms
asm ix st (MovRA l r (R Rbp):asms) = asm ix st (MovRA l r (RC Rbp 0):asms)
asm ix st (MovRA l r (R R13):asms) = asm ix st (MovRA l r (RC R13 0):asms)
asm ix st (MovRA _ r (R ar):asms) =
    mkAR [0x8b] 0 ar r:asm (ix+3) st asms
asm ix st (Cmovne _ r0 r1:asms) =
    mkRR [0xf,0x45] r1 r0:asm (ix+4) st asms
asm ix st (Cmovnle _ r0 r1:asms) =
    mkRR [0xf,0x4f] r1 r0:asm (ix+4) st asms
asm ix st (Cmovnl _ r0 r1:asms) =
    mkRR [0xf,0x4d] r1 r0:asm (ix+4) st asms
asm ix st (Cmovle _ r0 r1:asms) =
    mkRR [0xf,0x4e] r1 r0:asm (ix+4) st asms
asm ix st (Cmovl _ r0 r1:asms) =
    mkRR [0xf,0x4c] r1 r0:asm (ix+4) st asms
asm ix st (Cmove _ r0 r1:asms) =
    mkRR [0xf,0x44] r1 r0:asm (ix+4) st asms
asm ix st (MovAR _ (RS rb s ri) r:asms) =
    let (eb, bb) = modRM rb
        (ei, bi) = modRM ri
        (e, b) = modRM r
        modRMB = b `shiftL` 3 .|. 4
        sib = encS s `shiftL` 6 .|. bi `shiftL` 3 .|. bb
        pre = 0x48 .|. e `shiftL` 2 .|. ei `shiftL` 1 .|. eb
    in [pre,0x89,modRMB,sib]:asm (ix+4) st asms
asm ix st (Not _ r:asms) =
    let (e, b) = modRM r
        pre = 0x48 .|. e
        modB = 3 `shiftL` 6 .|. 2 `shiftL` 3 .|. b
    in [pre,0xf7,modB]:asm (ix+3) st asms
asm ix st (Neg _ r:asms) =
    let (e, b) = modRM r
        pre = 0x48 .|. e
        modB = 3 `shiftL` 6 .|. 3 `shiftL` 3 .|. b
    in [pre,0xf7,modB]:asm(ix+3) st asms
asm ix st (Rdrand _ r:asms) =
    let (e, b) = modRM r
        pre = 0x48 .|. e
        modB = 3 `shiftL` 6 .|. 6 `shiftL` 3 .|. b
    in [pre,0xf,0xc7,modB]:asm(ix+4) st asms
asm ix st@(self, _, Just (m, _, _, _), _) (Call _ Malloc:asms) | Just i32 <- mi32 (m-(self+ix+5)) =
    let instr = 0xe8:le i32
    in instr:asm (ix+5) st asms
asm ix st@(self, _, Just (_, f, _, _), _) (Call _ Free:asms) | Just i32 <- mi32 (f-(self+ix+5)) =
    let instr = 0xe8:le i32
    in instr:asm (ix+5) st asms
asm ix st@(self, _, Just (_, _, d, _), _) (Call _ DR:asms) | Just i32 <- mi32 (d-(self+ix+5)) =
    let isn=0xe8:le i32
    in isn:asm (ix+5) st asms
asm _ (_, _, Nothing, _) (Call{}:_) = error "Internal error? no dynlibs"
asm _ _ (instr:_) = error (show instr)

encS :: Scale -> Word8
encS One   = 0
encS Two   = 1
encS Four  = 2
encS Eight = 3

get :: Label -> (Int, IM.IntMap (Ptr Word64), Maybe CCtx, M.Map Label Int) -> Int
get l =
    M.findWithDefault (error "Internal error: label not found") l . fth where fth (_,_,_,z) = z

arr :: Int -> (Int, IM.IntMap (Ptr Word64), Maybe CCtx, M.Map Label Int) -> Ptr Word64
arr n = IM.findWithDefault (error "Internal error: array not found during assembler stage") n . snd4 where snd4 (_,y,_,_) = y

mi64i8 :: Int64 -> Maybe Int8
mi64i8 i | i > fromIntegral (maxBound :: Int8) || i < fromIntegral (minBound :: Int8) = Nothing
         | otherwise = Just $ fromIntegral i

mi32 :: Int -> Maybe Int32
mi32 i | i > fromIntegral (maxBound :: Int32) || i < fromIntegral (minBound :: Int32) = Nothing
       | otherwise = Just $ fromIntegral i

mi64i32 :: Int64 -> Maybe Int32
mi64i32 i | i > fromIntegral (maxBound :: Int32) || i < fromIntegral (minBound :: Int32) = Nothing
          | otherwise = Just $ fromIntegral i

class RMB a where
    -- extra is 1 bit, ModR/M is 3 bits; I store them as bytes for ease of
    -- manipulation
    modRM :: a -> (Word8, Word8)

instance RMB X86Reg where
    modRM Rax = (0, 0o0)
    modRM Rcx = (0, 0o1)
    modRM Rdx = (0, 0o2)
    modRM Rbx = (0, 0o3)
    modRM Rsp = (0, 0o4)
    modRM Rbp = (0, 0o5)
    modRM Rsi = (0, 0o6)
    modRM Rdi = (0, 0o7)
    modRM R8  = (1, 0o0)
    modRM R9  = (1, 0o1)
    modRM R10 = (1, 0o2)
    modRM R11 = (1, 0o3)
    modRM R12 = (1, 0o4)
    modRM R13 = (1, 0o5)
    modRM R14 = (1, 0o6)
    modRM R15 = (1, 0o7)

instance RMB FX86Reg where
    modRM XMM0  = (0, 0o0)
    modRM XMM1  = (0, 0o1)
    modRM XMM2  = (0, 0o2)
    modRM XMM3  = (0, 0o3)
    modRM XMM4  = (0, 0o4)
    modRM XMM5  = (0, 0o5)
    modRM XMM6  = (0, 0o6)
    modRM XMM7  = (0, 0o7)
    modRM XMM8  = (1, 0o0)
    modRM XMM9  = (1, 0o1)
    modRM XMM10 = (1, 0o2)
    modRM XMM11 = (1, 0o3)
    modRM XMM12 = (1, 0o4)
    modRM XMM13 = (1, 0o5)
    modRM XMM14 = (1, 0o6)
    modRM XMM15 = (1, 0o7)

cd :: (Integral a) => a -> [Word8]
cd x = le (fromIntegral x :: Word32)

-- little endian
le :: (Storable a, Integral a, Bits a) => a -> [Word8]
le x = fromIntegral <$> zipWith (\m e -> (x .&. m) `rotateR` e) masks ee
    where ee = [0,8..(8*(sizeOf x-1))]
          masks = iterate (*0x100) 0xff

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x
