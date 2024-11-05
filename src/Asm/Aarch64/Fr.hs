module Asm.Aarch64.Fr ( frameC ) where

import           Asm.Aarch64
import           Asm.Aarch64.Guess
import           Asm.M
import           CF
import           Data.Copointed
import           Data.Functor      (void)
import qualified Data.IntSet       as IS
import           Data.List         (partition)
import           Data.Maybe        (mapMaybe)

frameC :: [AArch64 AReg FAReg Live] -> [AArch64 AReg FAReg ()]
frameC = concat.go IS.empty IS.empty IS.empty
    where go _ _ _ [] = []
          go _ _ _ [isn] = [[void isn]]
          go s fs vs (isn0@(MovRCf _ _ cf):isn1@Blr{}:isns) =
            let i0 = copoint isn0; i1=copoint isn1
                vs' = collectV isn0<>vs
                s' = s `IS.union` new i0 `IS.difference` done i0
                s'' = s' `IS.union` new i1 `IS.difference` done i1
                fs' = fs `IS.union` fnew i0 `IS.difference` fdone i0
                fs'' = fs' `IS.union` fnew i1 `IS.difference` fdone i1
                (f2s, f1s) = partition (`IS.member` vs) (IS.toList fs)
                cs = handleX0 cf $ mapMaybe fromInt (IS.toList s)
                ds = handleD0 cf $ mapMaybe fInt f1s
                qs = handleD0 cf $ fmap fi f2s
                save = pus cs; restore = pos cs
                saved = puds ds; restored = pods ds
                saveq = puxs qs; restorex = poxs qs
            in (save ++ saved ++ saveq ++ void isn0:void isn1:restorex ++ restored ++ restore) : go s'' fs'' vs' isns
          go s fs vs (isn:isns) =
            let i = copoint isn
                s' = s `IS.union` new i `IS.difference` done i
                fs' = fs `IS.union` fnew i `IS.difference` fdone i
                vs' = collectV isn <> vs
            in [void isn] : go s' fs' vs' isns
          handleX0 Malloc=filter (/=X0); handleX0 Free=id; handleX0 Exp=id; handleX0 Log=id; handleX0 Pow=id; handleX0 DR=id; handleX0 JR=filter (/=X0)
          handleD0 Exp=filter (/=D0); handleD0 Log=filter (/=D0);handleD0 Malloc=id;handleD0 Free=id; handleD0 Pow=filter (/=D0); handleD0 DR=filter (/=D0); handleD0 JR=id

-- https://developer.arm.com/documentation/102374/0102/Procedure-Call-Standard
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
fi 10 = D0; fi 11 = D1; fi 12 = D2; fi 13 = D3
fi 14 = D4; fi 15 = D5; fi 16 = D6; fi 17 = D7
fi (-23) = D8; fi (-24) = D9; fi (-25) = D10; fi (-26) = D11
fi (-27) = D12; fi (-28) = D13; fi (-29) = D14; fi (-30) = D15
fi (-31) = D16; fi (-32) = D17; fi (-33) = D18; fi (-34) = D19
fi (-35) = D20; fi (-36) = D21; fi (-37) = D22; fi (-38) = D23
fi (-39) = D24; fi (-40) = D25; fi (-41) = D26; fi (-42) = D27
fi (-43) = D28; fi (-44) = D29; fi (-45) = D30; fi (-46) = D31

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
