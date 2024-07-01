module Asm.Aarch64.B ( bb ) where

import           Asm.Aarch64
import           Asm.BB
import           Data.List.Split (keepDelimsL, keepDelimsR, split, whenElt)

bb :: [AArch64 reg freg a] -> [BB AArch64 reg freg a ()]
bb = fmap mkBB.concatMap (split (keepDelimsL$whenElt isL)).split (keepDelimsR$whenElt cf)
    where cf B{}=True; cf Bc{}=True; cf Ret{}=True; cf Cbnz{}=True; cf Tbnz{}=True; cf Tbz{}=True; cf BlL{}=True; cf RetL{}=True; cf _=False
          isL Label{}=True; isL _=False
          mkBB x = BB x ()
