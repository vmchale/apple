module Dom ( N, mkG ) where

import           CF
import qualified Data.Array  as A
import           Data.Graph  (Tree, scc)
import qualified Data.IntMap as IM
import           IR

type N=Int

mkG :: ([(Stmt, ControlAnn)], Int) -> ([Tree N], IM.IntMap (Stmt, ControlAnn))
mkG (ns, m)= (scc$A.array (0,m-1) ((\(_,ann) -> (node ann, conn ann))<$>ns), IM.fromList ((\(s, ann) -> (node ann, (s, ann)))<$>ns))
