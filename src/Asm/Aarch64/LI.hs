module Asm.Aarch64.LI ( mkIntervals ) where

import           Asm.Aarch64
import qualified Asm.Aarch64.CF as Aarch64
import           CF
import           Class.E
import           LI
import           LR

mkIntervals :: (E reg, E freg) => [AArch64 reg freg ()] -> [AArch64 reg freg Interval]
mkIntervals = intervals . reconstruct . Aarch64.mkControlFlow
