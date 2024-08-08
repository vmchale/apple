{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Asm.Ar ( Arch (..) ) where

import qualified Asm.Aarch64    as AArch64
import qualified Asm.Aarch64.B  as AArch64
import qualified Asm.Aarch64.CF as AArch64
import           Asm.BB
import qualified Asm.X86        as X86
import qualified Asm.X86.B      as X86
import qualified Asm.X86.CF     as X86
import           CF
import           Class.E

class Arch arch reg freg where
    cf :: [BB arch reg freg () ()] -> [BB arch reg freg () ControlAnn]

    -- | result: src, dest
    mI :: arch reg freg a -> Maybe (reg, reg)
    mf :: arch reg freg a -> Maybe (Int, Int)

    bb :: [arch reg freg a] -> [BB arch reg freg a ()]
    expand :: BB arch reg freg () Liveness -> [arch reg freg Liveness]
    udd :: arch reg freg a -> UD

instance (E reg, E freg) => Arch X86.X86 reg freg where
    cf = X86.mkControlFlow

    mI (X86.MovRR _ r0 r1) = Just (r1, r0)
    mI _                   = Nothing

    mf (X86.Movapd _ r0 r1) = Just (toInt r1, toInt r0)
    mf _                    = Nothing

    bb = X86.bb
    expand = X86.expand
    udd = X86.udd

instance (E reg, E freg) => Arch AArch64.AArch64 reg freg where
    cf = AArch64.mkControlFlow

    mI (AArch64.MovRR _ r0 r1) = Just (r0, r1)
    mI _                       = Nothing

    mf (AArch64.FMovXX _ r0 r1) = Just (toInt r0, toInt r1)
    mf (AArch64.MovQQ _ v0 v1)  = Just (toInt v0, toInt v1)
    mf _                        = Nothing

    bb = AArch64.bb
    expand = AArch64.expand
    udd = AArch64.udd
