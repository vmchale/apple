module Asm.X86.Sp ( spill ) where

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

spill :: Int -- ^ Unique state
      -> Int
      -> IS.IntSet
      -> [X86 AbsReg FAbsReg a]
      -> (Int, Int, [X86 AbsReg FAbsReg ()])
spill u offs m isns =
    let (o', ᴍ) = spillM offs m isns
        (nisns, u') = runState ᴍ u
    in (u', o', nisns)

spillM :: Int -- ^ Offset (from already spilled)
       -> IS.IntSet
       -> [X86 AbsReg FAbsReg a]
       -> (Int, SpM [X86 AbsReg FAbsReg ()]) -- ^ offset, rewritten
spillM offs m isns = (foffs, concatMapM g isns)
    where g isn = do
            let is = [ toInt r | r <- fR pure isn, toInt r `IS.member` m ]
            newRs <- traverse (\_ -> IReg <$> next) is
            let f = thread (zipWith (\i rϵ r -> if toInt r == i then rϵ else r) is newRs)
                ma i = ao (at i); as = ma <$> is
                isn' = mapR f isn
            pure $
                   catMaybes (zipWith (\r a -> if toInt r `IS.member` uses isn' then Just (MovRA () r a) else Nothing) newRs as)
                ++ void isn'
                : catMaybes (zipWith (\a r -> if toInt r `IS.member` defs isn' then Just (MovAR () a r) else Nothing) as newRs)

          ass :: IS.IntSet -> IM.IntMap Int
          ass = IM.fromList . (\k -> zip k [offs,offs+8..]) . IS.toList

          assgn = ass m
          at k = IM.findWithDefault (error "Internal error.") k assgn

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
