module IR.Hoist ( hoist ) where

import           Data.Graph  (Tree)
import qualified Data.IntMap as IM
import           Dom
import           IR          (Stmt)
import           IR.CFA

hoist :: [Stmt] -> ([Tree N], IM.IntMap Stmt)
hoist = mkG.mkControlFlow
