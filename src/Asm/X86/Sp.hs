module Asm.X86.Sp ( spillM ) where

import           Asm.X86
import           Control.Monad.Extra        (concatMapM)
import           Control.Monad.State.Strict (State)
import qualified Data.IntMap.Strict         as IM
import qualified Data.IntSet                as IS

type SpM = State Int

spillM :: Int -- ^ Offset (from already spilled)
       -> IS.IntSet
       -> [X86 AbsReg FAbsReg a]
       -> (Int, SpM [X86 AbsReg FAbsReg ()]) -- ^ offset, rewritten
spillM offs m isns = (foffs, concatMapM g isns)
    where g (MovRR _ r0 r1) | toInt r0 `IS.member` m && toInt r1 `IS.member` m = undefined

          ass :: IS.IntSet -> IM.IntMap Int
          ass = IM.fromList . (\k -> zip k [offs..]) . IS.toList

          assgn = ass m

          foffs = offs + 8*IS.size m
