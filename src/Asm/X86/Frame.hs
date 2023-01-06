module Asm.X86.Frame ( frameC ) where

import           Asm.X86
import           CF
import           Data.Copointed
import           Data.Functor   (void)
import qualified Data.IntSet    as IS
import           Data.Maybe     (mapMaybe)

frameC :: [X86 X86Reg FX86Reg Interval] -> [X86 X86Reg FX86Reg ()]
frameC = concat . go IS.empty
    where go _ [] = []
          go s (isn:isns) =
            let i = copoint isn
                s' = s `IS.union` new i `IS.difference` done i
            in case isn of
                Call _ cf ->
                    let
                        scratch = odd$IS.size s
                        cs = handleRax cf $ mapMaybe fromInt $ IS.toList s
                        -- PUSH...POP rax destroys the return value!
                        save = (if scratch then (++[ISubRI () Rsp 8]) else id)$fmap (Push ()) cs
                        restore = (if scratch then (IAddRI () Rsp 8:) else id)$fmap (Pop ()) (reverse cs)
                    in (save ++ void isn : restore) : go s' isns
                _ -> [void isn] : go s' isns
          handleRax Malloc = filter (/=Rax)
          handleRax Free   = id

fromInt :: Int -> Maybe X86Reg
fromInt 1    = Just Rsi
fromInt 2    = Just Rdx
fromInt 3    = Just Rcx
fromInt 4    = Just R8
fromInt 5    = Just R9
fromInt 6    = Just Rax
fromInt 7    = Just Rsp
fromInt (-1) = Just R10
fromInt (-2) = Just R11
fromInt _    = Nothing
