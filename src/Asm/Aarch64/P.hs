module Asm.Aarch64.P ( galloc, gallocOn ) where

import           Asm.Aarch64
import           Asm.Ar.P
import           Asm.G
import qualified Data.IntMap as IM
import qualified Data.Set    as S

galloc :: Int -> [AArch64 AbsReg FAbsReg ()] -> [AArch64 AReg FAReg ()]
galloc u isns = frame clob'd (fmap (mapR ((regs IM.!).toInt).mapFR ((fregs IM.!).fToInt)) isns')
    where (regs, fregs, isns') = gallocOn u (isns ++ [Ret ()])
          clob'd = S.fromList $ IM.elems regs

{-# SCC frame #-}
frame :: S.Set AReg -> [AArch64 AReg FAReg ()] -> [AArch64 AReg FAReg ()]
frame clob asms = pre++asms++post++[Ret ()] where
    pre = save $ concatMap pu clobs
    post = restore $ concatMap po (reverse clobs)
    clobs = S.toList (clob `S.intersection` S.fromList [X19 .. X29])
    scratch=odd(length clobs); save=if scratch then (++[SubRC () SP SP 8]) else id; restore=if scratch then (AddRC () SP SP 8:) else id

pu, po :: AReg -> [AArch64 AReg freg ()]
pu r = [SubRC () SP SP 8, Str () r (R SP)]
po r = [Ldr () r (R SP), AddRC () SP SP 8]

gallocOn :: Int -> [AArch64 AbsReg FAbsReg ()] -> (IM.IntMap AReg, IM.IntMap FAReg, [AArch64 AbsReg FAbsReg ()])
gallocOn u = go u 0 pres
    where go uϵ offs pres' isns = rmaps
              where rmaps = case (regsM, fregsM) of
                        (Right regs, Right fregs) -> (regs, fregs, init isns)
                    regsM = alloc aIsns [X0 .. X30] (IM.keysSet pres') pres'
                    fregsM = allocF aFIsns [D0 .. D30] (IM.keysSet preFs) preFs
                    (aIsns, aFIsns) = bundle isns

pres :: IM.IntMap AReg
pres = IM.fromList [(0, X0), (1, X1), (2, X2), (3, X3), (4, X4), (5, X5), (6, X6), (7, X7), (8, X30), (9, SP)]

preFs :: IM.IntMap FAReg
preFs = IM.fromList [(10, D0), (11, D1), (12, D2), (13, D3), (14, D4), (15, D5), (16, D6), (17, D7)]
