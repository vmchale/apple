module Asm.X86.SpX ( spillX ) where

import           Asm.X86
import           Asm.X86.CF
import           Control.Monad.Extra              (concatMapM)
import           Control.Monad.Trans.State.Strict (State, runState, state)
import           Data.Functor                     (void)
import           Data.Int                         (Int32, Int8)
import qualified Data.IntMap.Strict               as IM
import qualified Data.IntSet                      as IS
import           Data.Maybe                       (catMaybes)

type SpM = State Int

next :: SpM Int
next = state (\i -> (i, i+1))

spillX :: Int -- ^ Unique state
       -> Int
       -> IS.IntSet
       -> [X86 AbsReg FAbsReg a]
       -> (Int, Int, [X86 AbsReg FAbsReg ()])
spillX u offs m isns =
    let (o', ᴍ) = spillM offs m isns
        (nisns, u') = runState ᴍ u
    in (u', o', nisns)

spillM :: Int -- ^ Offset (from already spilled)
       -> IS.IntSet
       -> [X86 AbsReg FAbsReg a]
       -> (Int, SpM [X86 AbsReg FAbsReg ()]) -- ^ offset, rewritten
spillM offs m isns = (foffs, concatMapM g isns)
    where g isn = do
            let is = [ fToInt r | r <- fF pure isn, fToInt r `IS.member` m ]
            newRs <- traverse (\_ -> FReg <$> next) is
            let f = thread (zipWith (\i rϵ r -> if fToInt r == i then rϵ else r) is newRs)
                ma i = ao (at i); as = ma <$> is
                isn' = mapFR f isn
            pure $
                   catMaybes (zipWith (\r a -> if fToInt r `IS.member` usesF isn' then Just (MovqXA () r a) else Nothing) newRs as)
                ++ void isn'
                : catMaybes (zipWith (\a r -> if fToInt r `IS.member` defsF isn' then Just (MovqAX () a r) else Nothing) as newRs)

          ass = IM.fromDistinctAscList . (\k -> zip k [offs,offs+8..]) $ IS.toAscList m
          at k = IM.findWithDefault (error "Internal error.") k ass

          foffs = offs + 8*IS.size m

          thread = foldr (.) id

ao o | Just i8 <- mi8 o = RC BP i8
     | Just i32 <- mi32 o = RC32 BP i32

mi8 :: Int -> Maybe Int8
mi8 i | i <= fromIntegral (maxBound :: Int8) && i >= fromIntegral (minBound :: Int8) = Just $ fromIntegral i
      | otherwise = Nothing

mi32 :: Int -> Maybe Int32
mi32 i | i <= fromIntegral (maxBound :: Int32) && i >= fromIntegral (minBound :: Int32) = Just $ fromIntegral i
       | otherwise = Nothing
