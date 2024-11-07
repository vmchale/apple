module CF.AL (AL (..), insert, singleton, sinsert, notMember) where

import qualified Data.IntMap as IM
import qualified Data.IntSet as IS

newtype AL=AL Int

insert (AL i) = IM.insert i; sinsert (AL i) = IS.insert i
singleton (AL i) = IS.singleton i; notMember (AL i) = IS.notMember i
