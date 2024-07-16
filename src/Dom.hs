module Dom ( N, mkG ) where

import           CF
import qualified Data.Array  as A
import           Data.Graph  (transposeG)
import qualified Data.IntMap as IM
import           IR

type N=Int

mkG :: ([(Stmt, ControlAnn)], Int) -> (A.Array N [N], IM.IntMap Stmt)
mkG (ns, m)= (transposeG$A.array (0,m) ((\(_,ann) -> (node ann, conn ann))<$>ns), IM.fromList ((\(s, ann) -> (node ann, s))<$>ns))
