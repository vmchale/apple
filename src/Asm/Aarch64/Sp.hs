module Asm.Aarch64.Sp ( spill ) where

import           Asm.Aarch64
import           Asm.Aarch64.CF
import           Control.Monad.Extra              (concatMapM)
import           Control.Monad.Trans.State.Strict (State, runState, state)
import           Data.Functor                     (void)
import qualified Data.IntMap.Strict               as IM
import qualified Data.IntSet                      as IS
import           Data.Maybe                       (catMaybes)
import           Data.Word                        (Word16)

type SpM = State Int

next :: SpM AbsReg
next = state (\i -> (IReg i,i+1))

spill :: Int -- ^ Unique state
      -> Int
      -> IS.IntSet
      -> [AArch64 AbsReg FAbsReg a]
      -> (Int, Int, [AArch64 AbsReg FAbsReg ()])
spill u offs m isns =
    let (o', ᴍ) = spillM offs m isns
        (nisns, u') = runState ᴍ u
    in (u', o', nisns)

spillM :: Int -- ^ Offset (from already spilled)
       -> IS.IntSet
       -> [AArch64 AbsReg FAbsReg a]
       -> (Int, SpM [AArch64 AbsReg FAbsReg ()]) -- ^ offset, rewritten
spillM offs m isns = (foffs, concatMapM g isns)
    where g isn = do
            let is = [ toInt r | r <- fR pure isn, toInt r `IS.member` m ]
            newRs <- traverse (\_ -> next) is
            let f = thread (zipWith (\i rϵ r -> if toInt r == i then rϵ else r) is newRs)
                ma i = ao (at i); as = ma <$> is
                isn' = mapR f isn
            pure $
                   catMaybes (zipWith (\r a -> if toInt r `IS.member` uses isn' then Just (Ldr () r a) else Nothing) newRs as)
                ++ void isn'
                : catMaybes (zipWith (\a r -> if toInt r `IS.member` defs isn' then Just (Str () r a) else Nothing) as newRs)

          ass :: IS.IntSet -> IM.IntMap Int
          ass = IM.fromList . (\k -> zip k [offs,offs+8..]) . IS.toList

          assgn = ass m
          at k = IM.findWithDefault (error "Internal error.") k assgn

          foffs = offs + 8*IS.size m

          thread = foldr (.) id


ao o | Just w16 <- m16 o = RP FP w16

m16 :: Int -> Maybe Word16
m16 i | i <= fromIntegral (maxBound :: Word16) = Just $ fromIntegral i
    | otherwise = Nothing
