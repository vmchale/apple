module Asm.X86.B ( bb ) where

import           Asm.BB
import           Asm.X86
import           Data.List.Split (keepDelimsL, keepDelimsR, split, whenElt)

bb :: [X86 reg freg a] -> [BB X86 reg freg a ()]
bb = filter (not.emptyBB).fmap mkBB.concatMap (split (keepDelimsL$whenElt isL)).split (keepDelimsR$whenElt cf)
    where cf J{}=True; cf Jl{}=True; cf Jg{}=True; cf Jge{}=True; cf Jle{}=True; cf Jne{}=True; cf C{}=True; cf RetL{}=True; cf _=False
          isL Label{}=True; isL _=False
          mkBB x = BB x ()
          emptyBB (BB [] _) = True
          emptyBB _         = False
