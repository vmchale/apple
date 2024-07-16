module Dom ( mkG ) where

import           CF
import qualified Data.Array as A
import           IR

mkG :: (Int, [(Stmt, ControlAnn)]) -> A.Array Int [Int]
mkG (m, ns)= A.array (0,m) ((\(_,ann) -> (node ann, conn ann))<$>ns)
