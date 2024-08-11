{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE MagicHash      #-}

-- https://developer.arm.com/documentation/ddi0602/2021-12/Base-Instructions
module Asm.Aarch64.Byte ( allFp, assembleCtx, dbgFp ) where

import           Asm.Aarch64
import           Asm.M
import           Control.Monad    (when)
import           Data.Bifunctor   (bimap, second)
import           Data.Bits        (Bits (..))
import qualified Data.ByteString  as BS
import           Data.Functor     (($>))
import           Data.Int         (Int16)
import qualified Data.IntMap      as IM
import qualified Data.Map         as M
import           Data.Tuple.Extra (fst3, thd3)
import           Data.Word        (Word64, Word8)
import           Foreign.Ptr      (FunPtr, IntPtr (..), Ptr, nullPtr, ptrToIntPtr)
import           GHC.Base         (Int (I#), iShiftRL#)
import           Hs.FFI
import           Sys.DL

hasMa :: [AArch64 reg freg a] -> Bool
hasMa = any g where g (MovRCf _ _ Malloc)=True; g (MovRCf _ _ Free)=True; g (MovRCf _ _ DR)=True; g _=False

hasMath :: [AArch64 reg freg a] -> Bool
hasMath = any g where g (MovRCf _ _ Exp)=True; g (MovRCf _ _ Log)=True; g (MovRCf _ _ Pow)=True; g _=False

prepAddrs :: [AArch64 reg freg a] -> IO (Maybe CCtx, Maybe MCtx)
prepAddrs ss = case (hasMa ss, hasMath ss) of
    (True, False)  -> do {m <- mem'; pure (Just m, Nothing)}
    (False, False) -> pure (Nothing, Nothing)
    (False, True)  -> do {m <- math'; pure (Nothing, Just m)}
    (True, True)   -> do {c <- mem'; m <- math'; pure (Just c, Just m)}

assembleCtx :: (CCtx, MCtx) -> (IM.IntMap [Word64], [AArch64 AReg FAReg ()]) -> IO (BS.ByteString, FunPtr b, Maybe (Ptr Word64))
assembleCtx ctx (ds, isns) = do
    let (sz, lbls) = mkIx 0 isns
    p <- if hasMa isns then allocNear (fst4 (fst ctx)) (fromIntegral sz) else allocExec (fromIntegral sz)
    when (p==nullPtr) $ error "failed to allocate memory for JIT"
    ps <- aArr ds
    let b = BS.pack.concatMap reverse$asm 0 (ps, bimap Just Just ctx, lbls) isns
    (b,,snd<$>IM.lookupMin ps)<$>finish b p

dbgFp asmϵ = do
    (bss,_,ps) <- allFp asmϵ
    mFree ps $> bss
allFp :: (IM.IntMap [Word64], [AArch64 AReg FAReg ()]) -> IO ([BS.ByteString], FunPtr b, Maybe (Ptr Word64))
allFp (ds, instrs) = do
    let (sz, lbls) = mkIx 0 instrs
    (fn, p) <- do
        res <- prepAddrs instrs
        case res of
            (Just (m, _, _, _),_) -> (res,) <$> allocNear m (fromIntegral sz)
            _                     -> (res,) <$> allocExec (fromIntegral sz)
    ps <- aArr ds
    let is = asm 0 (ps, fn, lbls) instrs; b = BS.pack.concatMap reverse$is; bsϵ = BS.pack.reverse<$>is
    (bsϵ,,snd<$>IM.lookupMin ps)<$>finish b p

mkIx :: Int -> [AArch64 AReg FAReg a] -> (Int, M.Map Label Int)
mkIx ix (Label _ l:asms) = second (M.insert l ix) $ mkIx ix asms
mkIx ix (C{}:asms)       = mkIx (ix+12) asms
mkIx ix (MovRCf{}:asms)  = mkIx (ix+16) asms
mkIx ix (LdrRL{}:asms)   = mkIx (ix+16) asms
mkIx ix (_:asms)         = mkIx (ix+4) asms
mkIx ix []               = (ix, M.empty)

be :: Enum a => a -> Word8
be = fromIntegral.fromEnum

-- https://developer.arm.com/documentation/ddi0406/c/Application-Level-Architecture/Instruction-Details/Conditional-execution?lang=en
bp :: Cond -> Word8
bp Eq=0b0000; bp Neq=0b0001; bp Gt=0b1100; bp Leq=0b1101; bp Geq=0b1010; bp Lt=0b1011

ip :: Cond -> Word8
ip = bp.inv where inv Eq=Neq; inv Neq=Eq; inv Gt=Leq; inv Geq=Lt; inv Lt=Geq; inv Leq=Gt

bs :: Shift -> Word8
bs Zero=0b0; bs Three=0b1; bs Four=0b1

lsr :: Int -> Int -> Int
lsr (I# n) (I# k) = I# (iShiftRL# n k)

i7, i9 :: Int16 -> (Word8, Word8)
i7 i = (fromIntegral ((i.&.0b1111111) `shiftR` 1), 0b1 .&. fromIntegral i)
i9 i = (fromIntegral ((i.&.0b111111111) `shiftR` 4), 0b1111 .&. fromIntegral i)

lb r rD = (0x7 .&. be r) `shiftL` 5 .|. be rD

asm :: Int -> (IM.IntMap (Ptr Word64), (Maybe CCtx, Maybe MCtx), M.Map Label Int) -> [AArch64 AReg FAReg ()] -> [[Word8]]
asm _ _ [] = []
asm ix st (MovZ _ r i s:asms) = [0b11010010, 0b1 `shiftL` 7 .|. fromIntegral (s `quot` 16) `shiftL` 5 .|. fromIntegral (i `shiftR` 11), fromIntegral (0xff .&. (i `shiftR` 3)), fromIntegral (0x7 .&. i) `shiftL` 5 .|. be r]:asm (ix+4) st asms
asm ix st (MovRC _ r i:asms) = asm ix st (MovZ () r i 0:asms)
asm ix st (MovK _ r i s:asms) = [0b11110010, 0b1 `shiftL` 7 .|. fromIntegral (s `quot` 16) `shiftL` 5 .|. fromIntegral (i `shiftR` 11), fromIntegral (i `shiftR` 3), (fromIntegral (0x7 .&. i) `shiftL` 5) .|. be r]:asm (ix+4) st asms
asm ix st (FMovDR _ d r:asms) = [0b10011110, 0b01100111, be r `shiftR` 3, lb r d]:asm (ix+4) st asms
asm ix st (FMovXX _ d0 d1:asms) = [0b00011110, 0x1 `shiftL` 6 .|. 0b100000, 0b10000 `shiftL` 2 .|. be d1 `shiftR` 3, lb d1 d0]:asm (ix+4) st asms
asm ix st (MovQQ _ q0 q1:asms) = [0b01001110, 0b101 `shiftL` 5 .|. be q1, 0b111 `shiftL` 2 .|. be q1 `shiftR` 3, lb q1 q0]:asm (ix+4) st asms
asm ix st (Fadd _ d0 d1 d2:asms) = [0b00011110, 0x3 `shiftL` 5 .|. be d2, 0b001010 `shiftL` 2 .|. (be d1 `shiftR` 3), lb d1 d0]:asm (ix+4) st asms
asm ix st (Fmul _ d0 d1 d2:asms) = [0b00011110, 0x3 `shiftL` 5 .|. be d2, 0x2 `shiftL` 2 .|. (be d1 `shiftR` 3), lb d1 d0]:asm (ix+4) st asms
asm ix st (Fsub _ d0 d1 d2:asms) = [0b00011110, 0x3 `shiftL` 5 .|. be d2, 0b1110 `shiftL` 2 .|. (be d1 `shiftR` 3), lb d1 d0]:asm (ix+4) st asms
asm ix st (Fdiv _ d0 d1 d2:asms) = [0b00011110, 0x3 `shiftL` 5 .|. be d2, 0b110 `shiftL` 2 .|. (be d1 `shiftR` 3), lb d1 d0]:asm (ix+4) st asms
asm ix st (Fmax _ d0 d1 d2:asms) = [0b00011110, 0x3 `shiftL` 5 .|. be d2, 0b10010 `shiftL` 2 .|. be d1 `shiftR` 3, lb d1 d0]:asm (ix+4) st asms
asm ix st (Fmin _ d0 d1 d2:asms) = [0b00011110, 0x3 `shiftL` 5 .|. be d2, 0b10110 `shiftL` 2 .|. be d1 `shiftR` 3, lb d1 d0]:asm (ix+4) st asms
asm ix st (Fabs _ d0 d1:asms) = [0b00011110, 0x60, 0x3 `shiftL` 6 .|. be d1 `shiftR` 3, lb d1 d0]:asm (ix+4) st asms
-- https://stackoverflow.com/a/57312875/11296354
-- .2D arrangement specifier = two doubles in vector register
asm ix st (Fadd2 _ x0 x1 x2:asms) = [0b01001110, 0x3 `shiftL` 5 .|. be x2, 0b110101 `shiftL` 2 .|. be x1 `shiftR` 3, lb x1 x0]:asm (ix+4) st asms
asm ix st (Fsub2 _ x0 x1 x2:asms) = [0b01001110, 0x7 `shiftL` 5 .|. be x2, 0b110101 `shiftL` 2 .|. be x1 `shiftR` 3, lb x1 x0]:asm (ix+4) st asms
asm ix st (Fmul2 _ x0 x1 x2:asms) = [0b01101110, 0b11 `shiftL` 5 .|. be x2, 0b110111 `shiftL` 2 .|. be x1 `shiftR` 3, lb x1 x0]:asm (ix+4) st asms
asm ix st (Fdiv2 _ x0 x1 x2:asms) = [0b01101110, 0x3 `shiftL` 5 .|. be x2, 0b111111 `shiftL` 2 .|. be x1 `shiftR` 3, lb x1 x0]:asm (ix+4) st asms
asm ix st (Fmax2 _ x0 x1 x2:asms) = [0b01001110, 0x3 `shiftL` 5 .|. be x2, 0b111101 `shiftL` 2 .|. be x1 `shiftR` 3, lb x1 x0]:asm (ix+4) st asms
asm ix st (Fmin2 _ x0 x1 x2:asms) = [0b01001110, 0x7 `shiftL` 5 .|. be x2, 0b111101 `shiftL` 2 .|. be x1 `shiftR` 3, lb x1 x0]:asm (ix+4) st asms
asm ix st (Fmadd _ d0 d1 d2 d3:asms) = [0b00011111, 0x2 `shiftL` 5 .|. be d2, be d3 `shiftL` 2 .|. be d1 `shiftR` 3, lb d1 d0]:asm (ix+4) st asms
asm ix st (Fmsub _ d0 d1 d2 d3:asms) = [0b00011111, 0x2 `shiftL` 5 .|. be d2, 0x1 `shiftL` 7 .|. be d3 `shiftL` 2 .|. be d1 `shiftR` 3, lb d1 d0]:asm (ix+4) st asms
asm ix st (Fsqrt2 _ x0 x1:asms) = [0b01101110, 0b11100001, 0b111110 `shiftL` 2 .|. be x1 `shiftR` 3, lb x1 x0]:asm (ix+4) st asms
asm ix st (Fneg2 _ x0 x1:asms) = [0b01101110, 0x7 `shiftL` 5, 0b111110 `shiftL` 2 .|. be x1 `shiftR` 3, lb x1 x0]:asm (ix+4) st asms
asm ix st (Fmla _ d0 d1 d2:asms) = [0b01001110, 0b11 `shiftL` 5 .|. be d2, 0b110011 `shiftL` 2 .|. be d1 `shiftR` 3, lb d1 d0]:asm (ix+4) st asms
asm ix st (Fmls _ d0 d1 d2:asms) = [0b01001110, 0x7 `shiftL` 5 .|. be d2, 0b110011 `shiftL` 2 .|. be d1 `shiftR` 3, lb d1 d0]:asm (ix+4) st asms
asm ix st (Faddp _ d v:asms) = [0b01111110, 0b1110000, 0b110110 `shiftL` 2 .|. be v `shiftR` 3, lb v d]:asm (ix+4) st asms
asm ix st (Fmaxp _ d v:asms) = [0b01111110, 0b1110000, 0b111110 `shiftL` 2 .|. be v `shiftR` 3, lb v d]:asm (ix+4) st asms
asm ix st (Fminp _ d v:asms) = [0b01111110, 0b11110000, 0b111110 `shiftL` 2 .|. be v `shiftR` 3, lb v d]:asm (ix+4) st asms
asm ix st (EorS _ v0 v1 v2:asms) = [0b01101110, 0x1 `shiftL` 5 .|. be v2, 0x7 `shiftL` 2 .|. be v1 `shiftR` 3, lb v1 v0]:asm (ix+4) st asms
asm ix st (ZeroS x v:asms) = asm ix st (EorS x v v v:asms)
asm ix st (Label{}:asms) = asm ix st asms
asm ix st (Ret{}:asms) = [0b11010110, 0b01011111, be X30 `shiftR` 3, (0x7 .&. be X30) `shiftL` 5]:asm (ix+4) st asms
asm ix st (RetL{}:asms) = [0b11010110, 0b01011111, be X30 `shiftR` 3, (0x7 .&. be X30) `shiftL` 5]:asm (ix+4) st asms
asm ix st (MrsR _ r:asms) = [0b11010101, 0b00111011, 0b00100100, be r]:asm (ix+4) st asms
asm _ _ (SubRR _ _ SP SP:_) = error "encoding not valid/supported"
asm _ _ (AddRR _ _ SP SP:_) = error "encoding not valid/supported."
asm ix st (SubRR _ SP r1 r2:asms) = [0b11001011, 0x1 `shiftL` 5 .|. be r2, 0b111 `shiftL` 5 .|. be r1 `shiftR` 3, lb r1 SP]:asm (ix+4) st asms
asm ix st (SubRR _ r0 SP r2:asms) = [0b11001011, 0x1 `shiftL` 5 .|. be r2, 0b111 `shiftL` 5 .|. be SP `shiftR` 3, lb SP r0]:asm (ix+4) st asms
asm ix st (SubRR _ r0 r1 r2:asms) = [0b11001011, be r2, be r1 `shiftR` 3, lb r1 r0]:asm (ix+4) st asms
asm ix st (AddRR _ SP r1 r2:asms) = [0b10001011, 0b1 `shiftL` 5 .|. be r2, 0b111 `shiftL` 5 .|. be r1 `shiftR` 3, lb r1 SP]:asm (ix+4) st asms
asm ix st (AddRR _ r0 SP r2:asms) = [0b10001011, 0b1 `shiftL` 5 .|. be r2, 0b111 `shiftL` 5 .|. be SP `shiftR` 3, lb SP r0]:asm (ix+4) st asms
asm ix st (AddRR l r0 r1 r2:asms) = asm ix st (AddRRS l r0 r1 r2 0:asms)
asm ix st (AddRRS _ r0 r1 r2 s:asms) = [0b10001011, be r2, s `shiftL` 2 .|. be r1 `shiftR` 3, lb r1 r0]:asm (ix+4) st asms
asm ix st (AndRR _ r0 r1 r2:asms) = [0b10001010, be r2, be r1 `shiftR` 3, lb r1 r0]:asm (ix+4) st asms
asm ix st (Eor _ r0 r1 r2:asms) = [0b11001010, be r2, be r1 `shiftR` 3, lb r1 r0]:asm (ix+4) st asms
asm ix st (Eon _ r0 r1 r2:asms) = [0b11001010, 0b1 `shiftL` 5 .|. be r2, be r1 `shiftR` 3, lb r1 r0]:asm (ix+4) st asms
asm ix st (OrRR _ r0 r1 r2:asms) = [0b10101010, be r2, be r1 `shiftR` 3, lb r1 r0]:asm (ix+4) st asms
asm ix st (Neg _ r0 r1:asms) = [0b11001011, be r1, 0x3, 0x7 `shiftL` 5 .|. be r0]:asm (ix+4) st asms
asm ix st (MulRR _ r0 r1 r2:asms) = [0b10011011, be r2, 0b11111 `shiftL` 2 .|. be r1 `shiftR` 3, lb r1 r0]:asm (ix+4) st asms
asm ix st (Sdiv _ r0 r1 r2:asms) = [0b10011010, 0b110 `shiftL` 5 .|. be r2, 0b11 `shiftL` 2 .|. be r1 `shiftR` 3, lb r1 r0]:asm (ix+4) st asms
asm ix st (Madd _ r0 r1 r2 r3:asms) = [0b10011011, be r2, be r3 `shiftL` 2 .|. be r1 `shiftR` 3, lb r1 r0]:asm (ix+4) st asms
asm ix st (Msub _ r0 r1 r2 r3:asms) = [0b10011011, be r2, 0b1 `shiftL` 7 .|. be r3 `shiftL` 2 .|. be r1 `shiftR` 3, lb r1 r0]:asm (ix+4) st asms
asm ix st (AddRC _ r0 r1 i:asms) = [0b10010001, fromIntegral (i `shiftR` 6), fromIntegral (0b111111 .&. i) `shiftL` 2 .|. (be r1 `shiftR` 3), lb r1 r0]:asm (ix+4) st asms
asm ix st (SubRC _ r0 r1 i:asms) = [0b11010001, fromIntegral (i `shiftR` 6), fromIntegral (0b111111 .&. i) `shiftL` 2 .|. be r1 `shiftR` 3, lb r1 r0]:asm (ix+4) st asms
asm ix st (MovRR x r0 SP:asms) = asm ix st (AddRC x r0 SP 0:asms)
asm ix st (MovRR x SP r1:asms) = asm ix st (AddRC x SP r1 0:asms)
asm ix st (MovRR _ r0 r1:asms) = [0b10101010, be r1, 0x3, 0x7 `shiftL` 5 .|. be r0]:asm (ix+4) st asms
asm ix st (ZeroR _ r:asms) = [0b11001010, be r, be r `shiftR` 3, lb r r]:asm (ix+4) st asms
asm ix st (Csel _ r0 r1 r2 p:asms) = [0b10011010, 0x1 `shiftL` 7 .|. be r2, bp p `shiftL` 4 .|. be r1 `shiftR` 3, lb r1 r0]:asm (ix+4) st asms
asm ix st (Cset _ r p:asms) = [0b10011010, 0b10011111, ip p `shiftL` 4 .|. 0x7, 0x7 `shiftL` 5 .|. be r]:asm (ix+4) st asms
asm ix st (TstI _ r (BM 1 ro):asms) | ro <= 0b111111 = [0b11110010, 0x1 `shiftL` 6 .|. (0b111111 .&. (-ro)), be r `shiftR` 3, (0x7 .&. be r) `shiftL` 5 .|. 0b11111]:asm (ix+4) st asms
asm ix st (EorI _ r0 r1 (BM 1 0):asms) = [0b11010010, 0x1 `shiftL` 6, be r1 `shiftR` 5, lb r1 r0]:asm (ix+4) st asms
asm ix st (Fcsel _ d0 d1 d2 p:asms) = [0b00011110, 0x3 `shiftL` 5 .|. be d2, bp p `shiftL` 4 .|. 0x3 `shiftL` 2 .|. be d1 `shiftR` 3, lb d1 d0]:asm (ix+4) st asms
asm ix st (Fcmp _ d0 d1:asms) = [0b00011110, 0x3 `shiftL` 5 .|. be d1, 0b1000 `shiftL` 2 .|. be d0 `shiftR` 5, be d0 `shiftL` 5]:asm (ix+4) st asms
asm ix st (Ldr _ r (RP rb u):asms) | (uϵ, 0) <- u `quotRem` 8 = [0xf9, 0x1 `shiftL` 6 .|. (fromIntegral uϵ `shiftR` 6), fromIntegral (0b111111 .&. uϵ) `shiftL` 2 .|. be rb `shiftR` 3, lb rb r]:asm (ix+4) st asms
asm ix st (Ldr _ r (Po rb i):asms) | i >= -256 && i < 255 = let (ub,lub)=i9 i in [0xf8, 0x2 `shiftL` 5 .|. ub, lub `shiftL` 4 .|. 0x1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb r]:asm (ix+4) st asms
asm ix st (Ldr x r (R rb):asms) = asm ix st (Ldr x r (RP rb 0):asms)
asm ix st (Ldr _ r (BI rb ri s):asms) = [0b11111000, 0x3 `shiftL` 5 .|. be ri, 0x3 `shiftL` 5 .|. bs s `shiftL` 4 .|. 0x2 `shiftL` 2 .|. (be rb `shiftR` 3), lb rb r]:asm (ix+4) st asms
asm ix st (LdrB x r (R rb):asms) = asm ix st (LdrB x r (RP rb 0):asms)
asm ix st (LdrB _ r (RP rb u):asms) | u <= 4095 = [0b00111001, 0x1 `shiftL` 6 .|. (fromIntegral u `shiftR` 6), fromIntegral (0b111111 .&. u) `shiftL` 2 .|. be rb `shiftR` 3, lb rb r]:asm (ix+4) st asms
asm ix st (LdrB _ r (BI rb ri s):asms) = [0b00111000, 0x3 `shiftL` 5 .|. be ri, 0x3 `shiftL` 5 .|. bs s `shiftL` 4 .|. 0x2 `shiftL` 2 .|. (be rb `shiftR` 3), lb rb r]:asm (ix+4) st asms
asm ix st (Ldp x r0 r1 (R rb):asms) = asm ix st (Ldp x r0 r1 (RP rb 0):asms)
asm ix st (Ldp _ r0 r1 (RP rb u):asms) | (u', 0) <- u `quotRem` 8, u <= 504 = [0xa9, 0x1 `shiftL` 6 .|. fromIntegral (u' `shiftR` 1), fromIntegral (0x1 .&. u') `shiftL` 7 .|. be r1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb r0]:asm (ix+4) st asms
asm ix st (Ldp _ r0 r1 (Po rb i):asms) | (i', 0) <- i `quotRem` 8, i >= -512 && i <= 504 = let (ub,lub)=i7 i' in [0xa8, 0x3 `shiftL` 6 .|. ub, lub `shiftL` 7 .|. be r1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb r0]:asm (ix+4) st asms
asm ix st (Str x r (R rb):asms) = asm ix st (Str x r (RP rb 0):asms)
asm ix st (Str _ r (RP rb u):asms) | (uu, 0) <- u `quotRem` 8 = [0xf9, fromIntegral (uu `shiftR` 6), fromIntegral (0b111111 .&. uu) `shiftL` 2 .|. be rb `shiftR` 3, lb rb r]:asm (ix+4) st asms
asm ix st (Str _ r (Pr rb i):asms) | i >= -256 && i < 255 = [0xf8, fromIntegral ((i `shiftR` 4) .&. 0b11111), (0b1111 .&. fromIntegral i) `shiftL` 4 .|. 0x3 `shiftL` 2 .|. be rb `shiftR` 3, lb rb r]:asm (ix+4) st asms
asm ix st (Str _ r (Po rb i):asms) | i >= -256 && i < 255 = [0xf8, fromIntegral ((i `shiftR` 4) .&. 0b11111), (0b1111 .&. fromIntegral i) `shiftL` 4 .|. 0x1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb r]:asm (ix+4) st asms
asm ix st (Str _ r (BI rb ri s):asms) = [0b11111000, 0x1 `shiftL` 5 .|. be ri, 0x3 `shiftL` 5 .|. bs s `shiftL` 4 .|. 0x2 `shiftL` 2 .|. be rb `shiftR` 3, lb rb r]:asm (ix+4) st asms
asm ix st (StrB x r (R rb):asms) = asm ix st (StrB x r (RP rb 0):asms)
asm ix st (StrB _ r (RP rb u):asms) | u <= 4095 = [0b00111001, fromIntegral (u `shiftR` 6), fromIntegral (0b111111 .&. u) `shiftL` 2 .|. be rb `shiftR` 3, lb rb r]:asm (ix+4) st asms
asm ix st (StrB _ r (BI rb ri s):asms) = [0b00111000, 0x1 `shiftL` 5 .|. be ri, 0x3 `shiftL` 5 .|. bs s `shiftL` 4 .|. 0x2 `shiftL` 2 .|. be rb `shiftR` 3, lb rb r]:asm (ix+4) st asms
asm ix st (StrD _ d (BI rb ri s):asms) = [0xfc, 0x1 `shiftL` 5 .|. be ri, 0x3 `shiftL` 5 .|. bs s `shiftL` 4 .|. 0x2 `shiftL` 2 .|. be rb `shiftR` 3, lb rb d]:asm (ix+4) st asms
asm ix st (StrD _ d (RP rb u):asms) | (uϵ, 0) <- u `quotRem` 8, u <= 32760 = [0b11111101, fromIntegral (uϵ `shiftR` 6), fromIntegral (uϵ .&. 0b111111) `shiftL` 2 .|. be rb `shiftR` 3, lb rb d]:asm (ix+4) st asms
asm ix st (StrD _ d (Pr rb i):asms) | i >= -256 && i <= 255 = let (ub,lub)=i9 i in [0xfc, ub, lub `shiftL` 4 .|. 0x3 `shiftL` 2 .|. be rb `shiftR` 3, lb rb d]:asm (ix+4) st asms
asm ix st (StrD _ d (Po rb i):asms) | i >= -256 && i <= 255 = let (ub,lub)=i9 i in [0xfc, ub, lub `shiftL` 4 .|. 0x1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb d]:asm (ix+4) st asms
asm ix st (StrD x d (R rb):asms) = asm ix st (StrD x d (RP rb 0):asms)
asm ix st (Stp x r0 r1 (R rb):asms) = asm ix st (Stp x r0 r1 (RP rb 0):asms)
asm ix st (Stp _ r0 r1 (RP rb u):asms) | (u', 0) <- u `quotRem` 8, u <= 504 = [0xa9, fromIntegral (u' `shiftR` 1), fromIntegral (0x1 .&. u') `shiftL` 7 .|. be r1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb r0]:asm (ix+4) st asms
asm ix st (Stp _ r0 r1 (Pr rb i):asms) | (i', 0) <- i `quotRem` 8, i >= -512 && i <= 504 = let (ub,lub)=i7 i' in [0xa9, 0x2 `shiftL` 6 .|. ub, lub `shiftL` 7 .|. be r1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb r0]:asm (ix+4) st asms
asm ix st (Stp _ r0 r1 (Po rb i):asms) | (i', 0) <- i `quotRem` 8, i >= -512 && i <= 504 = let (ub,lub)=i7 i' in [0xa8, 0x2 `shiftL` 6 .|. ub, lub `shiftL` 7 .|. be r1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb r0]:asm (ix+4) st asms
asm ix st (Stp2 _ q0 q1 (RP rb u):asms) | (u', 0) <- u `quotRem` 16, u <= 1008 = [0b10101101, fromIntegral (u' `shiftR` 1), fromIntegral (0x1 .&. u') `shiftL` 7 .|. be q1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb q0]:asm (ix+4) st asms
asm ix st (Stp2 _ q0 q1 (Po rb i):asms) | (i', 0) <- i `quotRem` 16, i >= -1024 && i <= 1008 = let (ub,lub)=i7 i' in [0b10101100, 0x2 `shiftL` 6 .|. ub, lub `shiftL` 7 .|. be q1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb q0]:asm (ix+4) st asms
asm ix st (Stp2 _ q0 q1 (Pr rb i):asms) | (i', 0) <- i `quotRem` 16, i >= -1024 && i <= 1008 = let (ub,lub)=i7 i' in [0b10101101, 0x2 `shiftL` 6 .|. ub, lub `shiftL` 7 .|. be q1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb q0]:asm (ix+4) st asms
asm ix st (Stp2 x q0 q1 (R rb):asms) = asm ix st (Stp2 x q0 q1 (RP rb 0):asms)
asm ix st (StpD x d0 d1 (R rb):asms) = asm ix st (StpD x d0 d1 (RP rb 0):asms)
asm ix st (StpD _ d0 d1 (RP rb u):asms) | (uϵ, 0) <- u `quotRem` 8, u <= 504 = [0b01101101, fromIntegral (uϵ `shiftR` 1), fromIntegral (uϵ .&. 0b1) `shiftL` 7 .|. be d1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb d0]:asm (ix+4) st asms
asm ix st (StpD _ d0 d1 (Pr rb i):asms) | i >= -512 && i <= 504, (i',0) <- i `quotRem` 8 = let (ub,lub)=i7 i' in [0b01101101, 0x2 `shiftL` 6 .|. ub, lub `shiftL` 7 .|. be d1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb d0]:asm (ix+4) st asms
asm ix st (LdrD x d (R rb):asms) = asm ix st (LdrD x d (RP rb 0):asms)
asm ix st (LdrD _ d (RP rb u):asms) | (uϵ, 0) <- u `quotRem` 8, u < 32760 = [0b11111101, 0x1 `shiftL` 6 .|. fromIntegral (uϵ `shiftR` 6), fromIntegral (0b111111 .&. uϵ) `shiftL` 2 .|. be rb `shiftR` 3, lb rb d]:asm (ix+4) st asms
asm ix st (LdrD _ d (Po rb i):asms) | i >= -256 && i < 255 = let (ub,lub)=i9 i in [0xfc, 0x2 `shiftL` 5 .|. ub, lub `shiftL` 4 .|. 0x1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb d]:asm (ix+4) st asms
asm ix st (LdrD _ d (BI rb ri s):asms) = [0b11111100, 0x3 `shiftL` 5 .|. be ri, 0x3 `shiftL` 5 .|. bs s `shiftL` 4 .|. 0x2 `shiftL` 2 .|. be rb `shiftR` 3, lb rb d]:asm (ix+4) st asms
asm ix st (LdpD _ d0 d1 (RP rb u):asms) | (uϵ, 0) <- u `quotRem` 8, u <= 504 = [0x6d, 0x1 `shiftL` 6 .|. fromIntegral (uϵ `shiftR` 1), fromIntegral (uϵ .&. 0b1) `shiftL` 7 .|. be d1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb d0]:asm (ix+4) st asms
asm ix st (LdpD _ d0 d1 (Po rb i):asms) | (i', 0) <- i `quotRem` 8, i >= -512 && i <= 504 = let (ub,lub)=i7 i' in [0b01101100, 0x3 `shiftL` 6 .|. ub, lub `shiftL` 7 .|. be d1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb d0]:asm (ix+4) st asms
asm ix st (LdpD x d0 d1 (R rb):asms) = asm ix st (LdpD x d0 d1 (RP rb 0):asms)
asm ix st (LdrS _ q (BI rb ri s):asms) = [0b00111100, 0b111 `shiftL` 5 .|. be ri, 0b11 `shiftL` 5 .|. bs s `shiftL` 4 .|. 0b10 `shiftL` 2 .|. be rb `shiftR` 3, lb rb q]:asm (ix+4) st asms
asm ix st (StrS x q (R rb):asms) = asm ix st (StrS x q (RP rb 0):asms)
asm ix st (StrS _ q (RP rb u):asms) | (u',0) <- u `quotRem` 16, u <= 65520 = [0b00111101, 0x2 `shiftL` 6 .|. fromIntegral (u' `shiftR` 6), fromIntegral (0b111111 .&. u') `shiftL` 2 .|. be rb `shiftR` 3, lb rb q]:asm (ix+4) st asms
asm ix st (LdrS x q (R rb):asms) = asm ix st (LdrS x q (RP rb 0):asms)
asm ix st (LdrS _ q (RP rb u):asms) | (u',0) <- u `quotRem` 16, u <= 65520 = [0b00111101, 0b11 `shiftL` 6 .|. fromIntegral (u' `shiftR` 6), fromIntegral (0b11111 .&. u') `shiftL` 2 .|. be rb `shiftR` 3, lb rb q]:asm (ix+4) st asms
asm ix st (StrS _ q (BI rb ri s):asms) = [0b00111100, 0b101 `shiftL` 5 .|. be ri, 0b011 `shiftL` 5 .|. bs s `shiftL` 4 .|. 0b10 `shiftL` 2 .|. be rb `shiftR` 3, lb rb q]:asm (ix+4) st asms
asm ix st (Dup _ q r:asms) = [0b01001110, 0b01000, 0x3 `shiftL` 2 .|. be r `shiftR` 3, lb r q]:asm (ix+4) st asms
asm ix st (DupD _ q r:asms) = [0b1001110, 0b01000, 0x1 `shiftL` 2 .|. be r `shiftR` 3, lb r q]:asm (ix+4) st asms
asm ix st (LdpD _ d0 d1 (R rb):asms) = [0x6d, 1 `shiftL` 6, be d1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb d0]:asm (ix+4) st asms
asm ix st (Ldp2 _ q0 q1 (RP rb u):asms) | (uϵ, 0) <- u `quotRem` 16, u <= 1008 = [0b10101101, 0x1 `shiftL` 6 .|. fromIntegral (uϵ `shiftR` 1), fromIntegral (0x1 .&. uϵ) `shiftL` 7 .|. be q1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb q0]:asm (ix+4) st asms
asm ix st (Ldp2 _ q0 q1 (Po rb i):asms) | (i',0) <- i `quotRem` 16, i >= -1024 && i <= 1004 = let (ub,lub)=i7 i' in [0b10101100, 0x3 `shiftL` 6 .|. ub, lub `shiftL` 7 .|. be q1 `shiftL` 2 .|. be rb `shiftR` 3, lb rb q0]:asm (ix+4) st asms
asm ix st (Ldp2 x q0 q1 (R rb):asms) = asm ix st (Ldp2 x q0 q1 (RP rb 0):asms)
asm ix st (CmpRR _ r0 r1:asms) = [0b11101011, be r1, be r0 `shiftR` 3, (0x7 .&. be r0) `shiftL` 5 .|. 0b11111]:asm (ix+4) st asms
asm ix st (CmpRC _ r u:asms) = [0b11110001, fromIntegral (u `shiftR` 6), (0b111111 .&. fromIntegral u) `shiftL` 2 .|. (be r `shiftR` 3), (0x7 .&. be r) `shiftL` 5 .|. 0b11111]:asm (ix+4) st asms
asm ix st (SubsRC _ r0 r1 u:asms) = [0b11110001, fromIntegral (u `shiftR` 6), (0b111111 .&. fromIntegral u) `shiftL` 2 .|. be r1 `shiftR` 3, lb r1 r0]:asm (ix+4) st asms
asm ix st (Bfc _ r l w:asms) | l >= 0 && l <= 64 && w >= 1 && w <= 63 = [0b10110011, 0x1 `shiftL` 6 .|. ((-l) `mod` 64), (w-1) `shiftL` 2 .|. 0b11, 0b111 `shiftL` 5 .|. be r]:asm (ix+4) st asms
asm ix st (Scvtf _ d r:asms) = [0b10011110, 0b01100010, be r `shiftR` 3, lb r d]:asm (ix+4) st asms
asm ix st (Fcvtms _ r d:asms) = [0x9e, 0b01110000, be d `shiftR` 3, lb d r]:asm (ix+4) st asms
asm ix st (Fcvtas _ r d:asms) = [0x9e, 0b01100100, be d `shiftR` 3, lb d r]:asm (ix+4) st asms
asm ix st (Fsqrt _ d0 d1:asms) = [0b00011110, 0b01100001, 0x3 `shiftL` 6 .|. be d1 `shiftR` 3, lb d1 d0]:asm (ix+4) st asms
asm ix st (Fneg _ d0 d1:asms) = [0b00011110, 0b01100001, 0x1 `shiftL` 6 .|. be d1 `shiftR` 3, lb d1 d0]:asm (ix+4) st asms
asm ix st (Frintm _ d0 d1:asms) = [0b00011110, 0b01100101, 0x1 `shiftL` 6  .|. be d1 `shiftR` 3, lb d1 d0]:asm (ix+4) st asms
asm ix st (Asr _ r0 r1 s:asms) = [0b10010011, 0x1 `shiftL` 6 .|. s, 0b111111 `shiftL` 2 .|. be r1 `shiftR` 3, lb r0 r1]:asm (ix+4) st asms
asm ix st (Lsl _ r0 r1 s:asms) =
    let immr= (-s) `mod` 64
        imms=63-s
    in [0b11010011, 0x1 `shiftL` 6 .|. immr, imms `shiftL` 2 .|. be r1 `shiftR` 3, lb r1 r0]:asm (ix+4) st asms
asm ix st (Bc _ p l:asms) =
    let lIx=get l st
        offs=(lIx-ix) `quot` 4
        isn=[0b01010100, fromIntegral (offs `lsr` 11), fromIntegral (0xff .&. (offs `lsr` 3)), (fromIntegral (0x7 .&. offs) `shiftL` 5) .|. bp p]
    in isn:asm (ix+4) st asms
asm ix st (Cbnz _ r l:asms) =
    let lIx=get l st
        offs=(lIx-ix) `quot` 4
        isn=[0b10110101, fromIntegral (offs `lsr` 11), fromIntegral (0xff .&. (offs `lsr` 3)), fromIntegral (0x7 .&. offs) `shiftL` 5 .|. be r]
    in isn:asm (ix+4) st asms
asm ix st (Cbz _ r l:asms) =
    let lIx=get l st
        offs=(lIx-ix) `quot` 4
        isn=[0b10110100, fromIntegral (offs `lsr` 11), fromIntegral (0xff .&. (offs `lsr` 3)), fromIntegral (0x7 .&. offs) `shiftL` 5 .|. be r]
    in isn:asm (ix+4) st asms
asm ix st (Tbz _ r b l:asms) | b <= 31 =
    let lIx=get l st
        offs=(lIx-ix) `quot` 4
        isn=[0b00110110, b `shiftL` 3 .|. fromIntegral (offs `lsr` 11), fromIntegral (0xff .&. (offs `lsr` 3)), fromIntegral (0x7 .&. offs) `shiftL` 5 .|. be r]
    in isn:asm (ix+4) st asms
asm ix st (Tbnz _ r b l:asms) | b <= 31 =
    let lIx=get l st
        offs=(lIx-ix) `quot` 4
        isn=[0b00110111, b `shiftL` 3 .|. fromIntegral (offs `lsr` 11), fromIntegral (0xff .&. (offs `lsr` 3)), fromIntegral (0x7 .&. offs) `shiftL` 5 .|. be r]
    in isn:asm (ix+4) st asms
asm ix st (C _ l:asms) =
    let lIx=get l st
        offs=(lIx-(ix+4)) `quot` 4
        isn=[0b100101 `shiftL` 2 .|. fromIntegral (0x3 .&. (offs `lsr` 24)), fromIntegral (0xff .&. (offs `lsr` 16)), fromIntegral (0xff .&. (offs `lsr` 8)), fromIntegral (0xff .&. offs)]
        pro=asm ix undefined [Stp () X29 X30 (Pr SP (-16))]
    in pro++isn:asm (ix+8) st (Ldp () X29 X30 (Po SP 16):asms)
asm ix st (B _ l:asms) =
    let lIx=get l st
        offs=(lIx-ix) `quot` 4
        isn=[0x5 `shiftL` 2 .|. fromIntegral (0x3 .&. (offs `lsr` 24)), fromIntegral (0xff .&. (offs `lsr` 16)), fromIntegral (0xff .&. (offs `lsr` 8)), fromIntegral (0xff .&. offs)]
    in isn:asm (ix+4) st asms
asm ix st (Blr _ r:asms) = [0b11010110, 0b00111111, be r `shiftR` 3, (0x7 .&. be r) `shiftL` 5]:asm (ix+4) st asms
asm ix st@(_, (Just (m, _, _, _), _), _) (MovRCf _ r Malloc:asms) =
    asm ix st (m4 r m++asms)
asm ix st@(_, (Just (_, f, _, _), _), _) (MovRCf _ r Free:asms) =
    asm ix st (m4 r f++asms)
asm ix st@(_, (Just (_, _, d, _), _),_) (MovRCf _ r DR:asms) =
    asm ix st (m4 r d++asms)
asm ix st@(_, (Just (_, _, _, j), _),_) (MovRCf _ r JR:asms) =
    asm ix st (m4 r j++asms)
asm ix st@(_, (_, Just (e, _, _)),_) (MovRCf _ r Exp:asms) =
    asm ix st (m4 r e++asms)
asm ix st@(_, (_, Just (_, l, _)),_) (MovRCf _ r Log:asms) =
    asm ix st (m4 r l++asms)
asm ix st@(_, (_, Just (_, _, p)),_) (MovRCf _ r Pow:asms) =
    asm ix st (m4 r p++asms)
asm ix st (LdrRL _ r l:asms) =
    let p = pI$arr l st
        w0=p .&. 0xffff; w1=(p .&. 0xffff0000) `lsr` 16; w2=(p .&. 0xFFFF00000000) `lsr` 32; w3=(p .&. 0xFFFF000000000000) `lsr` 48
    in asm ix st (MovRC () r (fromIntegral w0):MovK () r (fromIntegral w1) 16:MovK () r (fromIntegral w2) 32:MovK () r (fromIntegral w3) 48:asms)
asm _ _ (isn:_) = error (show isn)

m4 :: AReg -> Int -> [AArch64 AReg FAReg ()]
m4 r a = let w0=a .&. 0xffff; w1=(a .&. 0xffff0000) `lsr` 16; w2=(a .&. 0xFFFF00000000) `lsr` 32; w3=(a .&. 0xFFFF000000000000) `lsr` 48
         in [MovRC () r (fromIntegral w0), MovK () r (fromIntegral w1) 16, MovK () r (fromIntegral w2) 32, MovK () r (fromIntegral w3) 48]

get :: Label -> (IM.IntMap (Ptr Word64), (Maybe CCtx, Maybe MCtx), M.Map Label Int) -> Int
get l =
    M.findWithDefault (error "Internal error: label not found") l . thd3

arr :: Int -> (IM.IntMap (Ptr Word64), (Maybe CCtx, Maybe MCtx), M.Map Label Int) -> Ptr Word64
arr n = IM.findWithDefault (error "Internal error: array not found during assembler stage") n . fst3

fst4 :: (a, b, c, d) -> a
fst4 (x, _, _, _) = x

pI :: Ptr a -> Int
pI = (\(IntPtr i) -> i) . ptrToIntPtr
