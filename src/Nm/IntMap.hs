module Nm.IntMap ( insert, Nm.IntMap.lookup
                 , findWithDefault
                 ) where

import qualified Data.IntMap as IM
import           Nm
import           U

insert :: Nm a -> b -> IM.IntMap b -> IM.IntMap b
insert (Nm _ (U i) _) = IM.insert i

lookup (Nm _ (U i) _) = IM.lookup i

findWithDefault x (Nm _ (U i) _) = IM.findWithDefault x i
