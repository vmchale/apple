{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MagicHash      #-}
{-# LANGUAGE TupleSections  #-}

-- https://developer.arm.com/documentation/ddi0602/2021-12/Base-Instructions
module Asm.Aarch64.Byte ( allFp, assembleCtx, dbgFp ) where

import           Asm.Aarch64
import           Asm.M
import           Control.Monad   (when)
import           Data.Bifunctor  (second)
import           Data.Bits       (Bits (..))
import qualified Data.ByteString as BS
import qualified Data.Map        as M
import           Data.Word       (Word8)
import           Foreign.Ptr     (FunPtr, nullPtr)
import           GHC.Base        (Int (I#), iShiftRL#)
import           Hs.FFI
import           Sys.DL

hasMa :: [AArch64 reg freg a] -> Bool
hasMa = any g where g MovRCf{}=True; g _=False

prepAddrs :: [AArch64 reg freg a] -> IO (Maybe (Int, Int))
prepAddrs ss = if hasMa ss then Just <$> mem' else pure Nothing

assembleCtx :: (Int, Int) -> [AArch64 AReg FAReg ()] -> IO (BS.ByteString, FunPtr b)
assembleCtx ctx isns = do
    let (sz, lbls) = mkIx 0 isns
    p <- if hasMa isns then allocNear (fst ctx) (fromIntegral sz) else allocExec (fromIntegral sz)
    when (p==nullPtr) $ error "failed to allocate memory for JIT"
    let b = BS.pack.concatMap reverse$asm 0 (pI p, Just ctx, lbls) isns
    (b,)<$>finish b p

dbgFp = fmap fst . allFp
allFp :: [AArch64 AReg FAReg ()] -> IO ([BS.ByteString], FunPtr b)
allFp instrs = do
    let (sz, lbls) = mkIx 0 instrs
    (fn, p) <- do
        res <- prepAddrs instrs
        case res of
            Just (m, _) -> (res,) <$> allocNear m (fromIntegral sz)
            _           -> (res,) <$> allocExec (fromIntegral sz)
    let is = asm 0 (pI p, fn, lbls) instrs; b = BS.pack.concatMap reverse$is; bs = BS.pack.reverse<$>is
    (bs,)<$>finish b p

mkIx :: Int -> [AArch64 AReg FAReg a] -> (Int, M.Map Label Int)
mkIx ix (Label _ l:asms) = second (M.insert l ix) $ mkIx ix asms
mkIx ix (MovRCf{}:asms)  = mkIx (ix+16) asms
mkIx ix (_:asms)         = mkIx (ix+4) asms
mkIx ix []               = (ix, M.empty)

be :: Enum a => a -> Word8
be = fromIntegral.fromEnum

-- https://developer.arm.com/documentation/ddi0406/c/Application-Level-Architecture/Instruction-Details/Conditional-execution?lang=en
bp :: Cond -> Word8
bp Eq=0b0000; bp Neq=0b0001; bp Gt=0b1100; bp Leq=0b1101; bp Geq=0b1010; bp Lt=0b1011

bs :: Shift -> Word8
bs Zero=0b0; bs Three=0b1

lsr :: Int -> Int -> Int
lsr (I# n) (I# k) = I# (iShiftRL# n k)

lb r rD = (0x7 .&. be r) `shiftL` 5 .|. be rD

asm :: Int -> (Int, Maybe (Int, Int), M.Map Label Int) -> [AArch64 AReg FAReg ()] -> [[Word8]]
asm _ _ [] = []
asm ix st (MovRC _ r i:asms) = [0b11010010, 0b100 `shiftL` 5 .|. fromIntegral (i `shiftR` 11), fromIntegral (0xff .&. (i `shiftR` 3)), (fromIntegral (0x7 .&. i)) `shiftL` 5 .|. be r]:asm (ix+4) st asms
asm ix st (MovK _ r i s:asms) = [0b11110010, 0b1 `shiftL` 7 .|. (fromIntegral (s `quot` 16)) `shiftL` 5 .|. fromIntegral (i `shiftR` 11), fromIntegral ((i `shiftR` 3)), (fromIntegral (0x7 .&. i) `shiftL` 5) .|. be r]:asm (ix+4) st asms
asm ix st (FMovDR _ d r:asms) = [0b10011110, 0b01100111, be r `shiftR` 3, lb r d]:asm (ix+4) st asms
asm ix st (FMovXX _ d0 d1:asms) = [0b00011110, 0x1 `shiftL` 6 .|. 0b100000, 0b10000 `shiftL` 2 .|. be d1 `shiftR` 3, lb d1 d0]:asm (ix+4) st asms
asm ix st (Fadd _ d0 d1 d2:asms) = [0b00011110, 0x3 `shiftL` 5 .|. be d2, 0b001010 `shiftL` 2 .|. (be d1 `shiftR` 3), lb d1 d0]:asm (ix+4) st asms
asm ix st (Fmul _ d0 d1 d2:asms) = [0b00011110, 0x3 `shiftL` 5 .|. be d2, 0x2 `shiftL` 2 .|. (be d1 `shiftR` 3), lb d1 d0]:asm (ix+4) st asms
asm ix st (Fsub _ d0 d1 d2:asms) = [0b00011110, 0x3 `shiftL` 5 .|. be d2, 0b1110 `shiftL` 2 .|. (be d1 `shiftR` 3), lb d1 d0]:asm (ix+4) st asms
asm ix st (Fdiv _ d0 d1 d2:asms) = [0b00011110, 0x3 `shiftL` 5 .|. be d2, 0b110 `shiftL` 2 .|. (be d1 `shiftR` 3), lb d1 d0]:asm (ix+4) st asms
asm ix st (Fmax _ d0 d1 d2:asms) = [0b00011110, 0x3 `shiftL` 5 .|. be d2, 0b10010 `shiftL` 2 .|. be d1 `shiftR` 3, lb d0 d0]:asm (ix+4) st asms
asm ix st (Fmadd _ d0 d1 d2 d3:asms) = [0b00011111, 0x2 `shiftL` 5 .|. be d2, be d3 `shiftL` 2 .|. be d1 `shiftR` 3, lb d1 d0]:asm (ix+4) st asms
asm ix st (Label{}:asms) = asm ix st asms
asm ix st (Ret{}:asms) = [0b11010110, 0b01011111, be X30 `shiftR` 3, (0x7 .&. (be X30)) `shiftL` 5]:asm (ix+4) st asms
asm ix st (MrsR _ r:asms) = [0b11010101, 0b00111011, 0b00100100, be r]:asm (ix+4) st asms
asm ix st (SubRR _ r0 r1 r2:asms) = [0b11001011, be r2, be r1 `shiftR` 3, lb r1 r0]:asm (ix+4) st asms
asm ix st (Neg _ r0 r1:asms) = [0b11001011, be r1, 0x3, 0x7 `shiftL` 5 .|. be r0]:asm (ix+4) st asms
asm ix st (MulRR _ r0 r1 r2:asms) = [0b10011011, be r2, 0b11111 `shiftL` 2 .|. be r1 `shiftR` 3, lb r1 r0]: asm (ix+4) st asms
asm ix st (AddRR _ r0 r1 r2:asms) = [0b10001011, be r2, be r1 `shiftR` 3, lb r1 r0]:asm (ix+4) st asms
asm ix st (AddRC _ r0 r1 i:asms) = [0b10010001, fromIntegral (i `shiftR` 6), fromIntegral (0b111111 .&. i) `shiftL` 2 .|. (be r1 `shiftR` 3), lb r1 r0]:asm (ix+4) st asms
asm ix st (SubRC _ r0 r1 i:asms) = [0b11010001, fromIntegral (i `shiftR` 6), fromIntegral (0b111111 .&. i) `shiftL` 2 .|. be r1 `shiftR` 3, lb r1 r0]:asm (ix+4) st asms
asm ix st (MovRR _ r0 r1:asms) = [0b10101010, be r1, 0x3, 0x7 `shiftL` 5 .|. be r0]:asm (ix+4) st asms
asm ix st (Ldr _ r (RP rb u):asms) | (uϵ, 0) <- u `quotRem` 8 = [0b11111001, 0x1 `shiftL` 6 .|. fromIntegral (uϵ `shiftR` 6), fromIntegral (0b111111 .&. uϵ) `shiftL` 2 .|. be rb `shiftR` 3, lb rb r]:asm (ix+4) st asms
asm ix st (Ldr _ r (R rb):asms) = [0xf9, 0x1 `shiftL` 6, be rb `shiftR` 3, lb rb r]:asm (ix+4) st asms
asm ix st (Ldr _ r (BI rb ri s):asms) = [0b11111000, 0x3 `shiftL` 5 .|. be ri, 0x3 `shiftL` 5 .|. bs s `shiftL` 4 .|. 0x2 `shiftL` 2 .|. (be rb `shiftR` 3), lb rb r]:asm (ix+4) st asms
asm ix st (Ldp _ r0 r1 (R rb):asms) = [0b10101000, 0x3 `shiftL` 6, be r1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb r0]:asm (ix+4) st asms
asm ix st (Str x r (R rb):asms) = asm ix st (Str x r (RP rb 0):asms)
asm ix st (Str _ r (RP rb u):asms) | (uu, 0) <- u `quotRem` 8 = [0xf9, fromIntegral (uu `shiftR` 6), fromIntegral (0b111111 .&. uu) `shiftL` 2 .|. be rb `shiftR` 3, lb rb r]:asm (ix+4) st asms --  [0b11111000, fromIntegral (u `shiftR` 4), fromIntegral (0xf .&. u) `shiftL` 4 .|. 0x1 `shiftL` 2 .|. be rb `shiftR` 2, lb rb r]:asm (ix+4) st asms
asm ix st (Str _ r (BI rb ri s):asms) = [0b11111000, 0x1 `shiftL` 5 .|. be ri, 0x3 `shiftL` 5 .|. bs s `shiftL` 4 .|. 0x2 `shiftL` 2 .|. be rb `shiftR` 3, lb rb r]:asm (ix+4) st asms
asm ix st (StrD _ d (BI rb ri s):asms) = [0xfc, 0x1 `shiftL` 5 .|. be ri, 0x3 `shiftL` 5 .|. bs s `shiftL` 4 .|. 0x2 `shiftL` 2 .|. be rb `shiftR` 3, lb rb d]:asm (ix+4) st asms
asm ix st (StrD _ d (R rb):asms) = [0b11111101, 0x0, be rb `shiftR` 3, lb rb d]:asm (ix+4) st asms
asm ix st (Stp _ r0 r1 (R rb):asms) = [0b10101000, 0x1 `shiftL` 7, be r1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb r0]:asm (ix+4) st asms
asm ix st (StpD _ d0 d1 (R rb):asms) = [0b01101100, 0x1 `shiftL` 7, be d1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb d0]:asm (ix+4) st asms
asm ix st (LdrD x d (R rb):asms) = asm ix st (LdrD x d (RP rb 0):asms)
asm ix st (LdrD _ d (RP rb u):asms) | (uϵ, 0) <- u `quotRem` 8 = [0b11111101, 0x1 `shiftL` 6 .|. fromIntegral (uϵ `shiftR` 6), fromIntegral (0b111111 .&. uϵ) `shiftL` 2 .|. be rb `shiftR` 3, lb rb d]:asm (ix+4) st asms
asm ix st (LdrD _ d (BI rb ri s):asms) = [0b11111100, 0x3 `shiftL` 5 .|. be ri, 0x3 `shiftL` 5 .|. bs s `shiftL` 4 .|. 0x2 `shiftL` 2 .|. be rb `shiftR` 3, lb rb d]:asm (ix+4) st asms
asm ix st (LdpD _ d0 d1 (R rb):asms) = [0x6d, 1 `shiftL` 6, be d1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb d0]:asm (ix+4) st asms
asm ix st (CmpRR _ r0 r1:asms) = [0b11101011, be r1, be r0 `shiftR` 3, (0x7 .&. be r0) `shiftL` 5 .|. 0b11111]:asm (ix+4) st asms
asm ix st (CmpRC _ r u:asms) = [0b11110001, fromIntegral (u `shiftR` 6), (0b111111 .&. fromIntegral u) `shiftL` 2 .|. (be r `shiftR` 3), (0x7 .&. be r) `shiftL` 5 .|. 0b11111]:asm (ix+4) st asms
asm ix st (Scvtf _ d r:asms) = [0b10011110, 0b01100010, be r `shiftR` 3, lb r d]:asm (ix+4) st asms
asm ix st (Fcvtms _ r d:asms) = [0b10011110, 0x1 `shiftL` 6 .|. 0b1100000, be d `shiftR` 3, lb d r]:asm (ix+4) st asms
asm ix st (Fsqrt _ d0 d1:asms) = [0b00011110, 0b01100001, 0x3 `shiftL` 6 .|. be d1 `shiftR` 3, lb d1 d0]:asm (ix+4) st asms
asm ix st (Lsl _ r0 r1 s:asms) =
    let immr= (-s) `mod` 64
        imms=63-s
    in [0b11010011, 0x1 `shiftL` 6 .|. immr, imms `shiftL` 2 .|. be r1 `shiftR` 3, lb r1 r0]:asm (ix+4) st asms
asm ix st (Bc _ p l:asms) =
    let lIx=get l st
        offs=(lIx-ix) `quot` 4
        isn=[0b01010100, fromIntegral (offs `lsr` 11), fromIntegral (0xff .&. (offs `lsr` 3)), (fromIntegral (0x7 .&. offs) `shiftL` 5) .|. bp p]
    in isn:asm (ix+4) st asms
asm ix st (B _ l:asms) =
    let lIx=get l st
        offs=(lIx-ix) `quot` 4
        isn=[0x5 `shiftL` 2 .|. fromIntegral (0x3 .&. (offs `lsr` 24)), fromIntegral (0xff .&. (offs `lsr` 16)), fromIntegral (0xff .&. (offs `lsr` 8)), fromIntegral (0xff .&. offs)]
    in isn:asm (ix+4) st asms
asm ix st (Blr _ r:asms) = [0b11010110, 0b00111111, be r `shiftR` 3, (0x7 .&. be r) `shiftL` 5]:asm (ix+4) st asms
asm ix st@(_, Just (m, _), _) (MovRCf _ r Malloc:asms) =
    let w0=m .&. 0xffff; w1=(m .&. 0xffff0000) `lsr` 16; w2=(m .&. 0xFFFF00000000) `lsr` 32; w3=(m .&. 0xFFFF000000000000) `lsr` 48
    in asm ix st (MovRC () r (fromIntegral w0):MovK () r (fromIntegral w1) 16:MovK () r (fromIntegral w2) 32:MovK () r (fromIntegral w3) 48:asms)
asm ix st@(_, Just (_, f), _) (MovRCf _ r Free:asms) =
    let w0=f .&. 0xffff; w1=(f .&. 0xffff0000) `lsr` 16; w2=(f .&. 0xFFFF00000000) `lsr` 32; w3=(f .&. 0xFFFF000000000000) `lsr` 48
    in asm ix st (MovRC () r (fromIntegral w0):MovK () r (fromIntegral w1) 16:MovK () r (fromIntegral w2) 32:MovK () r (fromIntegral w3) 48:asms)
asm _ _ (isn:_) = error(show isn)

get :: Label -> (Int, Maybe (Int, Int), M.Map Label Int) -> Int
get l =
    M.findWithDefault (error "Internal error: label not found") l . thd where thd (_, _, z) = z
