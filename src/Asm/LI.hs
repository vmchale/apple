{-# LANGUAGE FlexibleContexts #-}

module Asm.LI ( mkIntervals ) where

import           Asm.Ar
import           Asm.L
import           CF
import           Data.Copointed
import           LI

mkIntervals :: (Arch arch reg freg f2, Copointed (arch reg freg f2)) => [arch reg freg f2 ()] -> [arch reg freg f2 Live]
mkIntervals = intervals . enliven . mkLive
    where enliven = zipWith (\n a -> fmap (NLiveness n) a) [0..]
