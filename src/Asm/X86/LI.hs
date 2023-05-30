module Asm.X86.LI ( mkIntervals, mkLive ) where

import           Asm.X86
import qualified Asm.X86.CF as X86
import           CF
import           Class.E
import           LI
import           LR

mkIntervals :: (E reg, E freg) => [X86 reg freg ()] -> [X86 reg freg Interval]
mkIntervals = intervals . mkLive

mkLive :: (E reg, E freg) => [X86 reg freg ()] -> [X86 reg freg NLiveness]
mkLive = reconstruct . X86.mkControlFlow
