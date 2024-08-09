module Asm.Aarch64.Fr ( frameC ) where

import           Asm.Aarch64
import           Asm.M
import           CF
import           Data.Copointed
import           Data.Functor   (void)
import qualified Data.IntSet    as IS
import           Data.Maybe     (mapMaybe)

frameC :: [AArch64 AReg FAReg Live] -> [AArch64 AReg FAReg ()]
frameC = concat.go IS.empty IS.empty
    where go _ _ [] = []
          go _ _ [isn] = [[void isn]]
          go s fs (isn0@(MovRCf _ _ cf):isn1@Blr{}:isns) =
            let i0 = copoint isn0; i1=copoint isn1
                s' = s `IS.union` new i0 `IS.difference` done i0
                s'' = s' `IS.union` new i1 `IS.difference` done i1
                fs' = fs `IS.union` fnew i0 `IS.difference` fdone i0
                fs'' = fs' `IS.union` fnew i1 `IS.difference` fdone i1
                cs = handleX0 cf $ mapMaybe fromInt (IS.toList s)
                ds = handleD0 cf $ mapMaybe fInt (IS.toList fs)
                save = pus cs; restore = pos cs
                saved = puds ds; restored = pods ds
            in (save ++ saved ++ void isn0:void isn1:restored ++ restore) : go s'' fs'' isns
          go s fs (isn:isns) =
            let i = copoint isn
                s' = s `IS.union` new i `IS.difference` done i
                fs' = fs `IS.union` fnew i `IS.difference` fdone i
            in [void isn] : go s' fs' isns
          handleX0 Malloc=filter (/=X0); handleX0 Free=id; handleX0 Exp=id; handleX0 Log=id; handleX0 Pow=id; handleX0 DR=id; handleX0 JR=filter (/=X0)
          handleD0 Exp=filter (/=D0); handleD0 Log=filter (/=D0);handleD0 Malloc=id;handleD0 Free=id; handleD0 Pow=filter (\d->d/=D0 && d/=D1); handleD0 DR=filter (/=D0); handleD0 JR=id

-- https://developer.arm.com/documentation/102374/0101/Procedure-Call-Standard
fromInt :: Int -> Maybe AReg
fromInt 0     = Just X0
fromInt 1     = Just X1
fromInt 2     = Just X2
fromInt 3     = Just X3
fromInt 4     = Just X4
fromInt 5     = Just X5
fromInt 6     = Just X6
fromInt 7     = Just X7
fromInt (-1)  = Just X8
fromInt (-2)  = Just X9
fromInt (-3)  = Just X10
fromInt (-4)  = Just X11
fromInt (-5)  = Just X12
fromInt (-6)  = Just X13
fromInt (-7)  = Just X14
fromInt (-8)  = Just X15
fromInt (-9)  = Just X16
fromInt (-10) = Just X17
fromInt (-11) = Just X18
fromInt _     = Nothing

-- https://learn.microsoft.com/en-us/cpp/build/arm64-windows-abi-conventions?view=msvc-170#floating-pointsimd-registers
fInt :: Int -> Maybe FAReg
fInt 10    = Just D0
fInt 11    = Just D1
fInt 12    = Just D2
fInt 13    = Just D3
fInt 14    = Just D4
fInt 15    = Just D5
fInt 16    = Just D6
fInt 17    = Just D7
fInt (-31) = Just D16
fInt (-32) = Just D17
fInt (-33) = Just D18
fInt (-34) = Just D19
fInt (-35) = Just D20
fInt (-36) = Just D21
fInt (-37) = Just D22
fInt (-38) = Just D23
fInt (-39) = Just D24
fInt (-40) = Just D25
fInt (-41) = Just D26
fInt (-42) = Just D27
fInt (-43) = Just D28
fInt (-44) = Just D29
fInt (-45) = Just D30
fInt (-46) = Just D31
fInt _     = Nothing
