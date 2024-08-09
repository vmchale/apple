module Asm.X86.Frame ( frameC ) where

import           Asm.X86
import           CF
import           Data.Copointed
import           Data.Functor   (void)
import qualified Data.IntSet    as IS
import           Data.Maybe     (mapMaybe)

frameC :: [X86 X86Reg FX86Reg Live] -> [X86 X86Reg FX86Reg ()]
frameC = concat . go IS.empty IS.empty
    where go _ _ [] = []
          go s fs (isn:isns) =
            let i = copoint isn
                s' = s `IS.union` new i `IS.difference` done i
                fs' = fs `IS.union` fnew i `IS.difference` fdone i
            in case isn of
                Call _ cf ->
                    let
                        cs = handleRax cf $ mapMaybe fromInt $ IS.toList s
                        xms = mx cf $ mapMaybe fInt $ IS.toList fs
                        scratch = odd(length cs+length xms)
                        save = (if scratch then (++[ISubRI () Rsp 8]) else id)$fmap (Push ()) cs
                        restore = (if scratch then (IAddRI () Rsp 8:) else id)$fmap (Pop ()) (reverse cs)
                        savex = concatMap puxmm xms
                        restorex = concatMap poxmm (reverse xms)
                    in (save ++ savex ++ void isn : restorex ++ restore) : go s' fs' isns
                _ -> [void isn] : go s' fs' isns
          handleRax Malloc = filter (/=Rax)
          handleRax Free   = id
          handleRax DR     = id
          puxmm xr = [ISubRI () Rsp 8, MovqAX () (R Rsp) xr]
          poxmm xr = [MovqXA () xr (R Rsp), IAddRI () Rsp 8]
          mx Free   = const []
          mx Malloc = id
          mx DR     = filter (/=XMM0)

fromInt :: Int -> Maybe X86Reg
fromInt 1    = Just Rsi
fromInt 2    = Just Rdx
fromInt 3    = Just Rcx
fromInt 4    = Just R8
fromInt 5    = Just R9
fromInt 6    = Just Rax
fromInt (-1) = Just R10
fromInt (-2) = Just R11
fromInt _    = Nothing

fInt :: Int -> Maybe FX86Reg
fInt 8     = Just XMM0
fInt 9     = Just XMM1
fInt 10    = Just XMM2
fInt 11    = Just XMM3
fInt 12    = Just XMM4
fInt 13    = Just XMM5
fInt 14    = Just XMM6
fInt 15    = Just XMM7
fInt (-5)  = Just XMM8
fInt (-6)  = Just XMM9
fInt (-7)  = Just XMM10
fInt (-8)  = Just XMM11
fInt (-9)  = Just XMM12
fInt (-10) = Just XMM13
fInt (-11) = Just XMM14
fInt (-12) = Just XMM15
fInt _     = Nothing
