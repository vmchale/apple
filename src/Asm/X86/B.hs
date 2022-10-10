module Asm.X86.B ( bb ) where

import           Asm.X86
import           Data.List.Split (split, whenElt)

bb :: [X86 reg freg a] -> [[X86 reg freg a]]
bb = split (whenElt cf)
    where cf J{} = True; cf Jl{} = True; cf Jg{} = True; cf Jge{} = True; cf Jle{} = True; cf Jne{} = True; cf Label{} = True; cf Call{} = True; cf Ret{} = True; cf _ = False
