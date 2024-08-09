module Asm.X86.P ( gallocFrame, gallocOn ) where

import           Asm.Ar.P
import           Asm.G
import           Asm.LI
import           Asm.X86
import           Asm.X86.Frame
import           Asm.X86.Sp
import           Data.Int      (Int64)
import qualified Data.IntMap   as IM
import qualified Data.Set      as S

-- TODO: don't bother re-analyzing if no Calls
gallocFrame :: Int -- ^ int supply for spilling
            -> [X86 AbsReg FAbsReg ()] -> [X86 X86Reg FX86Reg ()]
gallocFrame u = frameC . mkIntervals . galloc u

{-# SCC galloc #-}
galloc :: Int -> [X86 AbsReg FAbsReg ()] -> [X86 X86Reg FX86Reg ()]
galloc u isns = frame clob'd (fmap (mapR ((regs IM.!).toInt).mapFR ((fregs IM.!).fToInt)) isns')
    where (regs, fregs, isns') = gallocOn u (isns ++ [Ret()])
          clob'd = S.fromList $ IM.elems regs

{-# SCC frame #-}
frame :: S.Set X86Reg -> [X86 X86Reg FX86Reg ()] -> [X86 X86Reg FX86Reg ()]
frame clob asms = pre++asms++post++[Ret()] where
    pre = save$Push () <$> clobs
    post = restore$Pop () <$> reverse clobs
    clobs = S.toList (clob `S.intersection` S.fromList (Rbp:[R12 .. Rbx]))
    scratch=even(length clobs) && hasMa asms; save=if scratch then (++[ISubRI () Rsp 8]) else id; restore=if scratch then (IAddRI () Rsp 8:) else id
    -- TODO: https://eli.thegreenplace.net/2011/09/06/stack-frame-layout-on-x86-64/
    -- https://stackoverflow.com/questions/51523127/why-does-the-compiler-reserve-a-little-stack-space-but-not-the-whole-array-size

{-# INLINE gallocOn #-}
gallocOn :: Int -> [X86 AbsReg FAbsReg ()] -> (IM.IntMap X86Reg, IM.IntMap FX86Reg, [X86 AbsReg FAbsReg ()])
gallocOn u = go u 16 pres True
    where go uϵ offs pres' i isns = rmaps
              where rmaps = case (regsM, fregsM) of
                        (Right regs, Right fregs) -> let saa = saI$8*fromIntegral offs; saaP = if i then init else (++[IAddRI () SP saa]).init.(ISubRI () SP saa:).(ISubRI () BP saa:) in (regs, fregs, saaP isns)
                        (Left s, Right fregs) ->
                            let (uϵ', offs', isns') = spill uϵ offs s isns
                            in go uϵ' offs' (IM.insert (-16) Rbp pres') False isns'
                    regsM = alloc aIsns ((if i then (++[Rbp]) else id) [Rcx .. Rax]) (IM.keysSet pres') pres'
                    fregsM = allocF aFIsns [XMM1 .. XMM15] (IM.keysSet preFs) preFs
                    (aIsns, aFIsns) = bundle isns

saI :: Int64 -> Int64
saI i | i`rem`16 == 0 = i | otherwise = i+8

pres :: IM.IntMap X86Reg
pres = IM.fromList [(0, Rdi), (1, Rsi), (2, Rdx), (3, Rcx), (4, R8), (5, R9), (6, Rax), (7, Rsp)]

preFs :: IM.IntMap FX86Reg
preFs = IM.fromList [(8, XMM0), (9, XMM1), (10, XMM2), (11, XMM3), (12, XMM4), (13, XMM5), (14, XMM6), (15, XMM7)]
