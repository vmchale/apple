{-# LANGUAGE BinaryLiterals #-}
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
import           Hs.FFI
import           Sys.DL

hasMa :: [AArch64 reg freg a] -> Bool
hasMa = any g where g Bl{}=True; g _=False

prepAddrs :: [AArch64 reg freg a] -> IO (Maybe (Int, Int))
prepAddrs ss = if hasMa ss then Just <$> mem' else pure Nothing

assembleCtx :: (Int, Int) -> [AArch64 AReg FAReg a] -> IO (BS.ByteString, FunPtr b)
assembleCtx ctx isns = do
    let (sz, lbls) = mkIx 0 isns
    p <- if hasMa isns then allocNear (fst ctx) (fromIntegral sz) else allocExec (fromIntegral sz)
    when (p==nullPtr) $ error "failed to allocate memory for JIT"
    let b = BS.pack.concatMap reverse$asm 0 (pI p, Just ctx, lbls) isns
    (b,)<$>finish b p

dbgFp = fmap fst . allFp
allFp :: [AArch64 AReg FAReg a] -> IO ([BS.ByteString], FunPtr b)
allFp instrs = do
    let (sz, lbls) = mkIx 0 instrs
    (fn, p) <- do
        res <- prepAddrs instrs
        case res of
            Just (m, _) -> (res,) <$> allocNear m (fromIntegral sz)
            _           -> (res,) <$> allocExec (fromIntegral sz)
    let is = asm 0 (pI p, fn, lbls) instrs; b = BS.pack.concatMap reverse$is; bs = BS.pack<$>is
    (bs,)<$>finish b p

mkIx :: Int -> [AArch64 AReg FAReg a] -> (Int, M.Map Label Int)
mkIx ix (Label _ l:asms) = second (M.insert l ix) $ mkIx ix asms
mkIx ix (_:asms)         = mkIx (ix+4) asms
mkIx ix []               = (ix, M.empty)

be :: Enum a => a -> Word8
be = fromIntegral.fromEnum

bp :: Cond -> Word8
bp Eq=0b0000; bp Neq=0b0001; bp Gt=0b1100; bp Leq=0b1101

asm :: Int -> (Int, Maybe (Int, Int), M.Map Label Int) -> [AArch64 AReg FAReg a] -> [[Word8]]
asm _ _ [] = []
asm ix st (MovRC _ r i:asms) = [0b11010010, 0b100 `shiftL` 5 .|. fromIntegral (i `shiftR` 11), fromIntegral (0xf .&. (i `shiftR` 3)), (fromIntegral (0x7 .&. i)) `shiftL` 5 .|. be r]:asm (ix+4) st asms
asm ix st (MovK _ r i s:asms) = [0b11110010, 0b1 `shiftL` 7 .|. (fromIntegral (s `quot` 16)) `shiftL` 5 .|. fromIntegral (i `shiftR` 11), fromIntegral ((i `shiftR` 3)), (fromIntegral (0x7 .&. i) `shiftL` 5) .|. be r]:asm (ix+4) st asms
asm ix st (FMovDR _ d r:asms) = [0b10011110, 0b01100111, be r `shiftR` 3, ((0x7 .&. be r) `shiftL` 5) .|. be d]:asm (ix+4) st asms
asm ix st (FMovXX _ d0 d1:asms) = [0b00011110, 0x1 `shiftL` 6 .|. 0b100000, 0b10000 `shiftL` 2 .|. be d1 `shiftR` 3, (0x7 .&. be d1) `shiftL` 5 .|. be d0]:asm (ix+4) st asms
asm ix st (Fadd _ d0 d1 d2:asms) = [0b00011110, 0x3 `shiftL` 5 .|. be d2, 0b001010 `shiftL` 2 .|. (be d1 `shiftR` 3), (0x7 .&. be d1) `shiftL` 5 .|. be d0]:asm (ix+4) st asms
asm ix st (Fmul _ d0 d1 d2:asms) = [0b00011110, 0x3 `shiftL` 5 .|. be d2, 0x2 `shiftL` 2 .|. (be d1 `shiftR` 3), (0x7 .&. be d1) `shiftL` 5 .|. be d0]:asm (ix+4) st asms
asm ix st (Fsub _ d0 d1 d2:asms) = [0b00011110, 0x3 `shiftL` 5 .|. be d2, 0b1110 `shiftL` 2 .|. (be d1 `shiftR` 3), (0x7 .&. be d1) `shiftL` 5 .|. be d0]:asm (ix+4) st asms
asm ix st (Fdiv _ d0 d1 d2:asms) = [0b00011110, 0x3 `shiftL` 5 .|. be d2, 0b110 `shiftL` 2 .|. (be d1 `shiftR` 3), (0x7 .&. be d1) `shiftL` 5 .|. be d0]:asm (ix+4) st asms
asm ix st (Label{}:asms) = asm ix st asms
asm ix st (Ret{}:asms) = [0b11010110, 0b01011111, 0x0, 0x0]:asm (ix+4) st asms
asm ix st (SubRR _ r0 r1 r2:asms) = [0b11001011, be r2, be r1 `shiftR` 3, (0x7 .&. be r1) `shiftL` 5 .|. be r0]:asm (ix+4) st asms
asm ix st (MulRR _ r0 r1 r2:asms) = [0b10011011, be r2, 0b11111 `shiftL` 2 .|. be r1 `shiftR` 5, (0x7 .&. be r1) `shiftL` 5 .|. be r0]: asm (ix+4) st asms
asm ix st (AddRR _ r0 r1 r2:asms) = [0b10001011, be r2, be r1 `shiftR` 5, (0x7 .&. be r1) `shiftL` 5 .|. be r0]:asm (ix+4) st asms
asm ix st (AddRC _ r0 r1 i:asms) = [0b10010001, 0b100 `shiftL` 5 .|. fromIntegral (i `shiftR` 6), fromIntegral (0b111111 .&. i) .|. (be r1 `shiftR` 3), (0x7 .&. be r1) `shiftL` 5 .|. be r0]:asm (ix+4) st asms
asm ix st (MovRR _ r0 r1:asms) = [0b10101010, be r1, 0x3, 0x7 `shiftL` 5 .|. be r0]:asm (ix+4) st asms
asm ix st (Ldr _ r (RP rb u):asms) = [0b11111000, 0x2 `shiftL` 5 .|. fromIntegral (u `shiftR` 5), fromIntegral (0xf .&. u) `shiftL` 4 .|. 0x1 `shiftL` 2 .|. be rb `shiftR` 2, (0x7 .&. be rb) `shiftL` 5 .|. be r]:asm (ix+4) st asms
asm ix st (LdrD _ d (RP rb u):asms) = [0b11111100, 0x2 `shiftL` 5 .|. fromIntegral (u `shiftR` 5), fromIntegral (0xf .&. u) `shiftL` 4 .|. 0x1 `shiftL` 2 .|. be rb `shiftR` 2, (0x7 .&. be rb) `shiftL` 5 .|. be d]:asm (ix+4) st asms
asm ix st (CmpRR _ r0 r1:asms) = [0b11101011, be r1, be r0 `shiftR` 3, (0x7 .&. be r1) `shiftL` 5 .|. 0b11111]:asm (ix+4) st asms
asm ix st (CmpRC _ r u:asms) = [0b11110001, fromIntegral (u `shiftR` 6), (0b111111 .&. fromIntegral u) `shiftL` 2 .|. (be r `shiftR` 5), (0x7 .&. be r) `shiftL` 5 .|. 0b11111]:asm (ix+4) st asms
asm ix st (Scvtf _ d r:asms) = [0b10011110, 0b01100010, be r `shiftR` 3, (0x7 .&. be r) `shiftL` 5 .|. be d]:asm (ix+4) st asms
asm ix st (Fcvtms _ r d:asms) = [0b10011110, 0x1 `shiftL` 6 .|. 0b1100000, be d `shiftR` 3, (0x7 .&. be d) `shiftL` 5  .|. be r]:asm (ix+4) st asms
asm ix st (Bc _ p l:asms) =
    let lIx = get l st
        isn = let offs = lIx-ix-4 `quot` 4 in [0b01010100, fromIntegral (offs `shiftR` 11), fromIntegral (offs `shiftR` 5) .&. 0xff, (fromIntegral (0x7 .&. offs) `shiftL` 5) .|. bp p]
    in isn:asm (ix+4) st asms
asm ix st (B _ l:asms) =
    let lIx = get l st
        isn = let offs = lIx-ix-4 `quot` 4 in [0x5 `shiftL` 2 .|. fromIntegral (offs `shiftR` 24), fromIntegral (0xff .&. (offs `shiftR` 16)), fromIntegral (0xff .&. (offs `shiftR` 8)), fromIntegral (0xff .&. offs)]
    in isn:asm (ix+4) st asms
asm _ _ (isn:_) = error(show isn)

get :: Label -> (Int, Maybe (Int, Int), M.Map Label Int) -> Int
get l =
    M.findWithDefault (error "Internal error: label not found") l . thd where thd (_, _, z) = z
