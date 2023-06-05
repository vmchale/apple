module Asm.L ( mkLive, liveBB ) where

import           Asm.Ar
import           Asm.BB
import           CF
import           Class.E
import           LR

mkLive :: (Arch arch reg freg) => [arch reg freg ()] -> [arch reg freg Liveness]
mkLive = concatMap expand. liveBB

liveBB :: (Arch arch reg freg) => [arch reg freg ()] -> [BB arch reg freg () Liveness]
liveBB = fmap (fmap liveness) . reconstruct . cf . bb
