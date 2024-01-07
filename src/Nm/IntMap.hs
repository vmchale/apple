module Nm.IntMap ( insert ) where

import qualified Data.IntMap as IM
import           Nm
import           U

insert :: Nm a -> b -> IM.IntMap b -> IM.IntMap b
insert (Nm _ (U i) _) = IM.insert i
