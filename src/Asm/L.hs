module Asm.L ( mkLive ) where

import           Asm.Ar
import           CF
import           Class.E
import           LR

mkLive :: (E reg, E freg, Arch arch reg freg) => [arch reg freg ()] -> [arch reg freg Liveness]
mkLive = concatMap (expand.fmap liveness) . reconstruct . cf . bb
