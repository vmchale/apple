module Asm.Aarch64.B ( bb ) where

import           Asm.Aarch64
import           Asm.BB
import           Data.List.Split (keepDelimsL, split, whenElt)

bb :: [AArch64 reg freg a] -> [BB AArch64 reg freg a ()]
bb = fmap mkBB.split (whenElt cf)
    where cf B{}=True; cf Bc{}=True; cf Label{}=True; cf Blr{}=True; cf Ret{}=True; cf Cbnz{}=True; cf Tbnz{} = True; cf Tbz{} = True; cf _=False
          mkBB x = BB x ()
