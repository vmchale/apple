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
          go s fs (isn0:isn1:isns) =
            let i = copoint isn0
                s' = s `IS.union` new i `IS.difference` done i
                fs' = fs `IS.union` fnew i `IS.difference` fdone i
            in case (isn0, isn1) of
                -- FIXME: blr
                (MovRCf _ _ cf, Blr{}) ->
                    let
                        cs = handleX0 cf $ mapMaybe fromInt $ IS.toList s
                        ds = mapMaybe fInt $ IS.toList fs
                        save = pus cs; restore = pos cs
                        saved = puds ds; restored = pods ds
                    in (save ++ saved ++ void isn0 : void isn1 : restored ++ restore) : go s' fs' isns
                _ -> [void isn0, void isn1] : go s' fs' isns
          handleX0 Malloc = filter (/=X0)
          handleX0 Free   = id

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
