module Nm.IntMap ( insert
                 , findWithDefault
                 ) where

import qualified Data.IntMap as IM
import           Nm
import           U

insert :: Nm a -> b -> IM.IntMap b -> IM.IntMap b
insert (Nm _ (U i) _) = IM.insert i

findWithDefault x (Nm _ (U i) _) = IM.findWithDefault x i
