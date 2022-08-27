module IR.Alloc ( live ) where

import           CF
import           IR
import qualified IR.CF as IR
import           LI
import           LR

live :: [Stmt] -> [(Stmt, Interval)]
live  = intervals . reconstruct . IR.mkControlFlow
