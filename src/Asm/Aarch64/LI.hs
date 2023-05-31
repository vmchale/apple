{-# LANGUAGE FlexibleContexts #-}

module Asm.Aarch64.LI ( mkIntervals ) where

import           Asm.Aarch64
import           Asm.Aarch64.B
import           Asm.Aarch64.CF
import           Asm.Ar
import           Asm.L
import           CF
import           Class.E
import           LI
import           LR

mkIntervals :: (E reg, E freg, Arch AArch64 reg freg) => [AArch64 reg freg ()] -> [AArch64 reg freg Interval]
mkIntervals = intervals . enliven . mkLive
    where enliven = zipWith (\n a -> fmap (NLiveness n) a) [0..]
