{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MagicHash      #-}
{-# LANGUAGE TupleSections  #-}

-- https://developer.arm.com/documentation/ddi0602/2021-12/Base-Instructions
module Asm.Aarch64.Byte ( allFp, assembleCtx, dbgFp ) where

import           Asm.Aarch64
import           Asm.M
import           Control.Monad              (when)
import           Control.Monad.Tardis       (Tardis, getFuture, getPast, evalTardis)
import           Control.Monad.Tardis.Class (modifyBackwards, modifyForwards)
import           Data.Bifunctor             (second)
import           Data.Bits                  (Bits (..))
import qualified Data.ByteString            as BS
import           Data.Functor               (($>))
import qualified Data.IntMap                as IM
import qualified Data.Map                   as M
import           Data.Word                  (Word64, Word8)
import           Foreign.Ptr                (FunPtr, IntPtr (..), Ptr, nullPtr, ptrToIntPtr)
import           GHC.Base                   (Int (I#), iShiftRL#)
import           Hs.FFI
import           Sys.DL

hasMa :: [AArch64 reg freg a] -> Bool
hasMa = any g where g MovRCf{}=True; g _=False

prepAddrs :: [AArch64 reg freg a] -> IO (Maybe (Int, Int))
prepAddrs ss = if hasMa ss then Just <$> mem' else pure Nothing

assembleCtx :: (Int, Int) -> (IM.IntMap [Word64], [AArch64 AReg FAReg ()]) -> IO (BS.ByteString, FunPtr b, Maybe (Ptr Word64))
assembleCtx ctx (ds, isns) = do
    let sz=iSz isns
    p <- if hasMa isns then allocNear (fst ctx) (fromIntegral sz) else allocExec (fromIntegral sz)
    when (p==nullPtr) $ error "failed to allocate memory for JIT"
    ps <- aArr ds
    let b = BS.pack.concatMap reverse$asm (ps, Just ctx) isns
    (b,,snd<$>IM.lookupMin ps)<$>finish b p

dbgFp asmϵ = do
    (bss,_,ps) <- allFp asmϵ
    mFree ps $> bss
allFp :: (IM.IntMap [Word64], [AArch64 AReg FAReg ()]) -> IO ([BS.ByteString], FunPtr b, Maybe (Ptr Word64))
allFp (ds, instrs) = do
    let sz=iSz instrs
    (fn, p) <- do
        res <- prepAddrs instrs
        case res of
            Just (m, _) -> (res,) <$> allocNear m (fromIntegral sz)
            _           -> (res,) <$> allocExec (fromIntegral sz)
    ps <- aArr ds
    let is = asm (ps, fn) instrs; b = BS.pack.concatMap reverse$is; bsϵ = BS.pack.reverse<$>is
    (bsϵ,,snd<$>IM.lookupMin ps)<$>finish b p

type A = Tardis (M.Map Label Int) (M.Map Label Int)

asm c is=evalTardis (asmA 0 c is) (M.empty, M.empty)
-- runA = evalTardis

get :: Label -> A Int
get l = do
    s0 <- getFuture
    s1 <- getPast
    pure $ M.findWithDefault (error "Internal error: label not found") l (s0<>s1)

iSz :: [AArch64 AReg FAReg a] -> Int
iSz (Label _ l:asms) = iSz asms
iSz (MovRCf{}:asms)  = 16+iSz asms
iSz (MovRL{}:asms)   = 16+iSz asms
iSz (_:asms)         = 4+iSz asms
iSz []               = 0

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

asmA :: Int -> (IM.IntMap (Ptr Word64), Maybe (Int, Int)) -> [AArch64 AReg FAReg ()] -> A [[Word8]]
asmA _ _ [] = pure []
asmA ix st (Label _ l:asms) = do {modifyBackwards (M.insert l ix); modifyForwards (M.insert l ix); asmA ix st asms}
asmA ix st (MovRC _ r i:asms) = ([0b11010010, 0b100 `shiftL` 5 .|. fromIntegral (i `shiftR` 11), fromIntegral (0xff .&. (i `shiftR` 3)), fromIntegral (0x7 .&. i) `shiftL` 5 .|. be r]:)<$>asmA (ix+4) st asms
asmA ix st (MovK _ r i s:asms) = ([0b11110010, 0b1 `shiftL` 7 .|. fromIntegral (s `quot` 16) `shiftL` 5 .|. fromIntegral (i `shiftR` 11), fromIntegral (i `shiftR` 3), (fromIntegral (0x7 .&. i) `shiftL` 5) .|. be r]:)<$>asmA (ix+4) st asms
asmA ix st (FMovDR _ d r:asms) = ([0b10011110, 0b01100111, be r `shiftR` 3, lb r d]:)<$>asmA (ix+4) st asms
asmA ix st (FMovXX _ d0 d1:asms) = ([0b00011110, 0x1 `shiftL` 6 .|. 0b100000, 0b10000 `shiftL` 2 .|. be d1 `shiftR` 3, lb d1 d0]:)<$>asmA (ix+4) st asms
asmA ix st (Fadd _ d0 d1 d2:asms) = ([0b00011110, 0x3 `shiftL` 5 .|. be d2, 0b001010 `shiftL` 2 .|. (be d1 `shiftR` 3), lb d1 d0]:)<$>asmA (ix+4) st asms
asmA ix st (Fmul _ d0 d1 d2:asms) = ([0b00011110, 0x3 `shiftL` 5 .|. be d2, 0x2 `shiftL` 2 .|. (be d1 `shiftR` 3), lb d1 d0]:)<$>asmA (ix+4) st asms
asmA ix st (Fsub _ d0 d1 d2:asms) = ([0b00011110, 0x3 `shiftL` 5 .|. be d2, 0b1110 `shiftL` 2 .|. (be d1 `shiftR` 3), lb d1 d0]:)<$>asmA (ix+4) st asms
asmA ix st (Fdiv _ d0 d1 d2:asms) = ([0b00011110, 0x3 `shiftL` 5 .|. be d2, 0b110 `shiftL` 2 .|. (be d1 `shiftR` 3), lb d1 d0]:)<$>asmA (ix+4) st asms
asmA ix st (Fmax _ d0 d1 d2:asms) = ([0b00011110, 0x3 `shiftL` 5 .|. be d2, 0b10010 `shiftL` 2 .|. be d1 `shiftR` 3, lb d0 d0]:)<$>asmA (ix+4) st asms
asmA ix st (Fabs _ d0 d1:asms) = ([0b00011110, 0x60, 0x3 `shiftL` 6 .|. be d1 `shiftR` 3, lb d1 d0]:)<$>asmA (ix+4) st asms
asmA ix st (Fmadd _ d0 d1 d2 d3:asms) = ([0b00011111, 0x2 `shiftL` 5 .|. be d2, be d3 `shiftL` 2 .|. be d1 `shiftR` 3, lb d1 d0]:)<$>asmA (ix+4) st asms
asmA ix st (Fmsub _ d0 d1 d2 d3:asms) = ([0b00011111, 0x2 `shiftL` 5 .|. be d2, 0x1 `shiftL` 7 .|. be d3 `shiftL` 2 .|. be d1 `shiftR` 3, lb d1 d0]:)<$>asmA (ix+4) st asms
asmA ix st (Ret{}:asms) = ([0b11010110, 0b01011111, be X30 `shiftR` 3, (0x7 .&. be X30) `shiftL` 5]:)<$>asmA (ix+4) st asms
asmA ix st (MrsR _ r:asms) = ([0b11010101, 0b00111011, 0b00100100, be r]:)<$>asmA (ix+4) st asms
asmA ix st (SubRR _ r0 r1 r2:asms) = ([0b11001011, be r2, be r1 `shiftR` 3, lb r1 r0]:)<$>asmA (ix+4) st asms
asmA ix st (Neg _ r0 r1:asms) = ([0b11001011, be r1, 0x3, 0x7 `shiftL` 5 .|. be r0]:)<$>asmA (ix+4) st asms
asmA ix st (MulRR _ r0 r1 r2:asms) = ([0b10011011, be r2, 0b11111 `shiftL` 2 .|. be r1 `shiftR` 3, lb r1 r0]:)<$> asmA (ix+4) st asms
asmA ix st (Madd _ r0 r1 r2 r3:asms) = ([0b10011011, be r2, be r3 `shiftL` 2 .|. be r1 `shiftR` 3, lb r1 r0]:)<$> asmA (ix+4) st asms
asmA ix st (AddRR _ r0 r1 r2:asms) = ([0b10001011, be r2, be r1 `shiftR` 3, lb r1 r0]:)<$>asmA (ix+4) st asms
asmA ix st (AddRC _ r0 r1 i:asms) = ([0b10010001, fromIntegral (i `shiftR` 6), fromIntegral (0b111111 .&. i) `shiftL` 2 .|. (be r1 `shiftR` 3), lb r1 r0]:)<$>asmA (ix+4) st asms
asmA ix st (SubRC _ r0 r1 i:asms) = ([0b11010001, fromIntegral (i `shiftR` 6), fromIntegral (0b111111 .&. i) `shiftL` 2 .|. be r1 `shiftR` 3, lb r1 r0]:)<$>asmA (ix+4) st asms
asmA ix st (MovRR x r0 SP:asms) = asmA ix st (AddRC x r0 SP 0:asms)
asmA ix st (MovRR x SP r1:asms) = asmA ix st (AddRC x SP r1 0:asms)
asmA ix st (MovRR _ r0 r1:asms) = ([0b10101010, be r1, 0x3, 0x7 `shiftL` 5 .|. be r0]:)<$>asmA (ix+4) st asms
asmA ix st (Csel _ r0 r1 r2 p:asms) = ([0b10011010, 0x1 `shiftL` 7 .|. be r2, bp p `shiftL` 4 .|. be r1 `shiftR` 3, lb r1 r0]:)<$>asmA (ix+4) st asms
asmA ix st (Cset _ r p:asms) = ([0b10011010, 0b10011111, bp p `shiftL` 4 .|. 0x7, 0x7 `shiftL` 5 .|. be r]:)<$>asmA (ix+4) st asms
asmA ix st (TstI _ r 1:asms) = ([0b11110010, 0x1 `shiftL` 6, be r `shiftR` 3, (0x7 .&. be r) `shiftL` 5 .|. 0b11111]:)<$>asmA (ix+4) st asms
asmA ix st (Fcsel _ d0 d1 d2 p:asms) = ([0b00011110, 0x3 `shiftL` 5 .|. be d2, bp p `shiftL` 4 .|. 0x3 `shiftL` 2 .|. be d1 `shiftR` 3, lb d1 d0]:)<$>asmA (ix+4) st asms
asmA ix st (Ldr _ r (RP rb u):asms) | (uϵ, 0) <- u `quotRem` 8 = ([0b11111001, 0x1 `shiftL` 6 .|. fromIntegral (uϵ `shiftR` 6), fromIntegral (0b111111 .&. uϵ) `shiftL` 2 .|. be rb `shiftR` 3, lb rb r]:)<$>asmA (ix+4) st asms
asmA ix st (Ldr _ r (R rb):asms) = ([0xf9, 0x1 `shiftL` 6, be rb `shiftR` 3, lb rb r]:)<$>asmA (ix+4) st asms
asmA ix st (Ldr _ r (BI rb ri s):asms) = ([0b11111000, 0x3 `shiftL` 5 .|. be ri, 0x3 `shiftL` 5 .|. bs s `shiftL` 4 .|. 0x2 `shiftL` 2 .|. (be rb `shiftR` 3), lb rb r]:)<$>asmA (ix+4) st asms
asmA ix st (Ldp x r0 r1 (R rb):asms) = asmA ix st (Ldp x r0 r1 (RP rb 0):asms)
asmA ix st (Ldp _ r0 r1 (RP rb u):asms) | (u', 0) <- u `quotRem` 8, u <= 504 = ([0xa9, 0x1 `shiftL` 6 .|. fromIntegral (u' `shiftR` 1), fromIntegral (0x1 .&. u') `shiftL` 7 .|. be r1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb r0]:)<$>asmA (ix+4) st asms
asmA ix st (Str x r (R rb):asms) = asmA ix st (Str x r (RP rb 0):asms)
asmA ix st (Str _ r (RP rb u):asms) | (uu, 0) <- u `quotRem` 8 = ([0xf9, fromIntegral (uu `shiftR` 6), fromIntegral (0b111111 .&. uu) `shiftL` 2 .|. be rb `shiftR` 3, lb rb r]:)<$>asmA (ix+4) st asms --  [0b11111000, fromIntegral (u `shiftR` 4), fromIntegral (0xf .&. u) `shiftL` 4 .|. 0x1 `shiftL` 2 .|. be rb `shiftR` 2, lb rb r]:asmA (ix+4) st asms
asmA ix st (Str _ r (BI rb ri s):asms) = ([0b11111000, 0x1 `shiftL` 5 .|. be ri, 0x3 `shiftL` 5 .|. bs s `shiftL` 4 .|. 0x2 `shiftL` 2 .|. be rb `shiftR` 3, lb rb r]:)<$>asmA (ix+4) st asms
asmA ix st (StrD _ d (BI rb ri s):asms) = ([0xfc, 0x1 `shiftL` 5 .|. be ri, 0x3 `shiftL` 5 .|. bs s `shiftL` 4 .|. 0x2 `shiftL` 2 .|. be rb `shiftR` 3, lb rb d]:)<$>asmA (ix+4) st asms
asmA ix st (StrD _ d (R rb):asms) = ([0b11111101, 0x0, be rb `shiftR` 3, lb rb d]:)<$>asmA (ix+4) st asms
asmA ix st (Stp x r0 r1 (R rb):asms) = asmA ix st (Stp x r0 r1 (RP rb 0):asms)
asmA ix st (Stp _ r0 r1 (RP rb u):asms) | (u', 0) <- u `quotRem` 8, u <= 504 = ([0xa9, fromIntegral (u' `shiftR` 1), fromIntegral (0x1 .&. u') `shiftL` 7 .|. be r1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb r0]:)<$>asmA (ix+4) st asms
asmA ix st (StpD _ d0 d1 (R rb):asms) = ([0b01101101, 0, be d1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb d0]:)<$>asmA (ix+4) st asms
asmA ix st (LdrD x d (R rb):asms) = asmA ix st (LdrD x d (RP rb 0):asms)
asmA ix st (LdrD _ d (RP rb u):asms) | (uϵ, 0) <- u `quotRem` 8 = ([0b11111101, 0x1 `shiftL` 6 .|. fromIntegral (uϵ `shiftR` 6), fromIntegral (0b111111 .&. uϵ) `shiftL` 2 .|. be rb `shiftR` 3, lb rb d]:)<$>asmA (ix+4) st asms
asmA ix st (LdrD _ d (BI rb ri s):asms) = ([0b11111100, 0x3 `shiftL` 5 .|. be ri, 0x3 `shiftL` 5 .|. bs s `shiftL` 4 .|. 0x2 `shiftL` 2 .|. be rb `shiftR` 3, lb rb d]:)<$>asmA (ix+4) st asms
asmA ix st (LdpD _ d0 d1 (R rb):asms) = ([0x6d, 1 `shiftL` 6, be d1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb d0]:)<$>asmA (ix+4) st asms
asmA ix st (CmpRR _ r0 r1:asms) = ([0b11101011, be r1, be r0 `shiftR` 3, (0x7 .&. be r0) `shiftL` 5 .|. 0b11111]:)<$>asmA (ix+4) st asms
asmA ix st (CmpRC _ r u:asms) = ([0b11110001, fromIntegral (u `shiftR` 6), (0b111111 .&. fromIntegral u) `shiftL` 2 .|. (be r `shiftR` 3), (0x7 .&. be r) `shiftL` 5 .|. 0b11111]:)<$>asmA (ix+4) st asms
asmA ix st (Scvtf _ d r:asms) = ([0b10011110, 0b01100010, be r `shiftR` 3, lb r d]:)<$>asmA (ix+4) st asms
asmA ix st (Fcvtms _ r d:asms) = ([0b10011110, 0x1 `shiftL` 6 .|. 0b110000, be d `shiftR` 3, lb d r]:)<$>asmA (ix+4) st asms
asmA ix st (Fsqrt _ d0 d1:asms) = ([0b00011110, 0b01100001, 0x3 `shiftL` 6 .|. be d1 `shiftR` 3, lb d1 d0]:)<$>asmA (ix+4) st asms
asmA ix st (Asr _ r0 r1 s:asms) = ([0b10010011, 0x1 `shiftL` 6 .|. s, 0b111111 `shiftL` 2 .|. be r1 `shiftR` 3, lb r0 r1]:)<$>asmA (ix+4) st asms
asmA ix st (Lsl _ r0 r1 s:asms) =
    let immr= (-s) `mod` 64
        imms=63-s
    in ([0b11010011, 0x1 `shiftL` 6 .|. immr, imms `shiftL` 2 .|. be r1 `shiftR` 3, lb r1 r0]:)<$>asmA (ix+4) st asms
asmA ix st (Bc _ p l:asms) = do
    lIx <- get l
    let offs=(lIx-ix) `quot` 4
        isn=[0b01010100, fromIntegral (offs `lsr` 11), fromIntegral (0xff .&. (offs `lsr` 3)), (fromIntegral (0x7 .&. offs) `shiftL` 5) .|. bp p]
    (isn:)<$>asmA (ix+4) st asms
asmA ix st (Cbnz _ r l:asms) = do
    lIx <- get l
    let offs=(lIx-ix) `quot` 4
        isn=[0b10110101, fromIntegral (offs `lsr` 11), fromIntegral (0xff .&. (offs `lsr` 3)), fromIntegral (0x7 .&. offs `shiftL` 5) .|. be r]
    (isn:)<$>asmA (ix+4) st asms
asmA ix st (Tbz _ r 0 l:asms) = do
    lIx <- get l
    let offs=(lIx-ix) `quot` 4
        isn=[0b00110110, fromIntegral (offs `lsr` 11), fromIntegral (0xff .&. (offs `lsr` 3)), fromIntegral (0x7 .&. offs) `shiftL` 5 .|. be r]
    (isn:)<$>asmA (ix+4) st asms
asmA ix st (Tbnz _ r 0 l:asms) = do
    lIx <- get l
    let offs=(lIx-ix) `quot` 4
        isn=[0b00110111, fromIntegral (offs `lsr` 11), fromIntegral (0xff .&. (offs `lsr` 3)), fromIntegral (0x7 .&. offs) `shiftL` 5 .|. be r]
    (isn:)<$>asmA (ix+4) st asms
asmA ix st (B _ l:asms) = do
    lIx <- get l
    let offs=(lIx-ix) `quot` 4
        isn=[0x5 `shiftL` 2 .|. fromIntegral (0x3 .&. (offs `lsr` 24)), fromIntegral (0xff .&. (offs `lsr` 16)), fromIntegral (0xff .&. (offs `lsr` 8)), fromIntegral (0xff .&. offs)]
    (isn:)<$>asmA (ix+4) st asms
asmA ix st (Blr _ r:asms) = ([0b11010110, 0b00111111, be r `shiftR` 3, (0x7 .&. be r) `shiftL` 5]:)<$>asmA (ix+4) st asms
asmA ix st@(_, Just (m, _)) (MovRCf _ r Malloc:asms) =
    let w0=m .&. 0xffff; w1=(m .&. 0xffff0000) `lsr` 16; w2=(m .&. 0xFFFF00000000) `lsr` 32; w3=(m .&. 0xFFFF000000000000) `lsr` 48
    in asmA ix st (MovRC () r (fromIntegral w0):MovK () r (fromIntegral w1) 16:MovK () r (fromIntegral w2) 32:MovK () r (fromIntegral w3) 48:asms)
asmA ix st@(_, Just (_, f)) (MovRCf _ r Free:asms) =
    let w0=f .&. 0xffff; w1=(f .&. 0xffff0000) `lsr` 16; w2=(f .&. 0xFFFF00000000) `lsr` 32; w3=(f .&. 0xFFFF000000000000) `lsr` 48
    in asmA ix st (MovRC () r (fromIntegral w0):MovK () r (fromIntegral w1) 16:MovK () r (fromIntegral w2) 32:MovK () r (fromIntegral w3) 48:asms)
asmA ix st (MovRL _ r l:asms) =
    let p = pI$arr l st
        w0=p .&. 0xffff; w1=(p .&. 0xffff0000) `lsr` 16; w2=(p .&. 0xFFFF00000000) `lsr` 32; w3=(p .&. 0xFFFF000000000000) `lsr` 48
    in asmA ix st (MovRC () r (fromIntegral w0):MovK () r (fromIntegral w1) 16:MovK () r (fromIntegral w2) 32:MovK () r (fromIntegral w3) 48:asms)
asmA _ _ (isn:_) = error (show isn)

arr :: Int -> (IM.IntMap (Ptr Word64), Maybe (Int, Int)) -> Ptr Word64
arr n = IM.findWithDefault (error "Internal error: array not found during assembler stage") n . fst

pI :: Ptr a -> Int
pI = (\(IntPtr i) -> i) . ptrToIntPtr
