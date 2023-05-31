{-# LANGUAGE FlexibleContexts #-}

module Asm.Aarch64.LI ( mkIntervals ) where

import           Asm.Aarch64
import           Asm.Ar
import           Asm.L
import           CF
import           Class.E
import           LI

mkIntervals :: (E reg, E freg, Arch AArch64 reg freg) => [AArch64 reg freg ()] -> [AArch64 reg freg Interval]
mkIntervals = intervals . enliven . mkLive
    where enliven = zipWith (\n a -> fmap (NLiveness n) a) [0..]
