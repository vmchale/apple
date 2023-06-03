{-# LANGUAGE FlexibleContexts #-}

module Asm.LI ( mkIntervals ) where

-- import           Asm.Aarch64
import           Asm.Ar
import           Asm.L
import           CF
import           Class.E
import           Data.Copointed
import           LI

mkIntervals :: (E reg, E freg, Arch arch reg freg, Copointed (arch reg freg)) => [arch reg freg ()] -> [arch reg freg Interval]
mkIntervals = intervals . enliven . mkLive
    where enliven = zipWith (\n a -> fmap (NLiveness n) a) [0..]
