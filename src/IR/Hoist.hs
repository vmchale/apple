module IR.Hoist ( hoist ) where

import qualified Data.Array  as A
import qualified Data.IntMap as IM
import           Dom
import           IR          (Stmt)
import           IR.CFA

hoist :: [Stmt] -> (A.Array N [N], IM.IntMap Stmt)
hoist = mkG.mkControlFlow
