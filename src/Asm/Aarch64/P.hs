module Asm.Aarch64.P ( gallocFrame, gallocOn ) where

import           Asm.Aarch64
import           Asm.Aarch64.Fr
import           Asm.Ar.P
import           Asm.G
import           Asm.LI
import qualified Data.IntMap    as IM
import qualified Data.Set       as S

gallocFrame :: Int -- ^ int supply for spilling
            -> [AArch64 AbsReg FAbsReg ()] -> [AArch64 AReg FAReg ()]
gallocFrame u = frameC . mkIntervals . galloc u

galloc :: Int -> [AArch64 AbsReg FAbsReg ()] -> [AArch64 AReg FAReg ()]
galloc u isns = frame clob'd clob'dd (fmap (mapR ((regs IM.!).toInt).mapFR ((fregs IM.!).fToInt)) isns')
    where (regs, fregs, isns') = gallocOn u (isns++[Ret ()])
          clob'd = S.fromList $ IM.elems regs
          clob'dd = S.fromList $ IM.elems fregs

{-# SCC frame #-}
frame :: S.Set AReg -> S.Set FAReg -> [AArch64 AReg FAReg ()] -> [AArch64 AReg FAReg ()]
frame clob clobd asms = pre++asms++post++[Ret ()] where
    pre=pus clobs; post=pos clobs
    -- https://developer.arm.com/documentation/102374/0101/Procedure-Call-Standard
    clobs = S.toList (clob `S.intersection` S.fromList [X18 .. X28])
    clobsd = S.toList (clobd `S.intersection` S.fromList [D8 .. D15])
    -- FIXME: vector registers

gallocOn :: Int -> [AArch64 AbsReg FAbsReg ()] -> (IM.IntMap AReg, IM.IntMap FAReg, [AArch64 AbsReg FAbsReg ()])
gallocOn u = go u 0 pres
    where go uϵ offs pres' isns = rmaps
              where rmaps = case (regsM, fregsM) of
                        (Right regs, Right fregs) -> (regs, fregs, init isns)
                    -- https://developer.apple.com/documentation/xcode/writing-arm64-code-for-apple-platforms#Respect-the-purpose-of-specific-CPU-registers
                    regsM = alloc aIsns (filter (/= X18) [X0 .. X28]) (IM.keysSet pres') pres'
                    fregsM = allocF aFIsns [D0 .. D30] (IM.keysSet preFs) preFs
                    (aIsns, aFIsns) = bundle isns

pres :: IM.IntMap AReg
pres = IM.fromList [(0, X0), (1, X1), (2, X2), (3, X3), (4, X4), (5, X5), (6, X6), (7, X7), (8, X30), (9, SP), (18, X29)]

preFs :: IM.IntMap FAReg
preFs = IM.fromList [(10, D0), (11, D1), (12, D2), (13, D3), (14, D4), (15, D5), (16, D6), (17, D7)]
