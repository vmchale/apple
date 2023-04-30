module Asm.Aarch64.Fr ( frameC ) where

import           Asm.Aarch64
import           Asm.M
import           CF
import           Data.Copointed
import           Data.Functor   (void)
import qualified Data.IntSet    as IS
import           Data.Maybe     (mapMaybe)

frameC :: [AArch64 AReg FAReg Interval] -> [AArch64 AReg FAReg ()]
frameC = concat.go IS.empty IS.empty
    where go _ _ [] = []
          go _ _ [isn] = [[void isn]]
          go s fs (isn0@(MovRCf _ _ cf):isn1@Blr{}:isns) =
            let i0 = copoint isn0; i1=copoint isn1
                s' = s `IS.union` new i0 `IS.difference` done i0
                s'' = s' `IS.union` new i1 `IS.difference` done i1
                fs' = fs `IS.union` fnew i0 `IS.difference` fdone i0
                fs'' = fs' `IS.union` fnew i1 `IS.difference` fdone i1
                cs = handleX0 cf $ mapMaybe fromInt $ IS.toList s
                ds = mapMaybe fInt $ IS.toList fs
                save = pus cs; restore = pos cs
                saved = puds ds; restored = pods ds
            in (save ++ saved ++ void isn0:void isn1:restored ++ restore) : go s'' fs'' isns
          go s fs (isn:isns) =
            let i = copoint isn
                s' = s `IS.union` new i `IS.difference` done i
                fs' = fs `IS.union` fnew i `IS.difference` fdone i
            in [void isn] : go s' fs' isns
          handleX0 Malloc = filter (/=X0)
          handleX0 Free   = id

-- https://developer.arm.com/documentation/102374/0101/Procedure-Call-Standard
fromInt :: Int -> Maybe AReg
fromInt 0    = Just X0
fromInt 1    = Just X1
fromInt 2    = Just X2
fromInt 3    = Just X3
fromInt 4    = Just X4
fromInt 5    = Just X5
fromInt 6    = Just X6
fromInt 7    = Just X7
fromInt (-1) = Just X8
fromInt (-2) = Just X9
fromInt (-3) = Just X10
fromInt (-4) = Just X11
fromInt (-5) = Just X12
fromInt (-6) = Just X13
fromInt (-7) = Just X14
fromInt (-8) = Just X15
fromInt _    = Nothing

fInt :: Int -> Maybe FAReg
fInt 10 = Just D0
fInt 11 = Just D1
fInt 12 = Just D2
fInt 13 = Just D3
fInt 14 = Just D4
fInt 15 = Just D5
fInt 16 = Just D6
fInt 17 = Just D7
fInt _  = Nothing
