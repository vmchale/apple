module Asm.L ( mkLive, liveBB ) where

import           Asm.Ar
import           Asm.BB
import           CF
import           LR

mkLive :: (Arch arch reg freg f2) => [arch reg freg f2 ()] -> [arch reg freg f2 Liveness]
mkLive = concatMap expand. liveBB

liveBB :: (Arch arch reg freg f2) => [arch reg freg f2 ()] -> [BB arch reg freg f2 () Liveness]
liveBB = fmap (fmap liveness) . reconstructFlat . cf . bb
