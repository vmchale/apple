module Asm.X86.LI ( mkIntervals ) where

import           Asm.X86
import qualified Asm.X86.CF as X86
import           CF
import           Class.E
import           LI
import           LR

mkIntervals :: (E reg, E freg) => [X86 reg freg ()] -> [X86 reg freg Interval]
mkIntervals = intervals . reconstruct . X86.mkControlFlow
