{-# LANGUAGE MultiParamTypeClasses #-}

module Asm.Ar ( Arch (..) ) where

import qualified Asm.X86          as X86
import qualified Asm.X86.B        as X86
import qualified Asm.X86.CF       as X86
import           CF

class Arch arch reg freg where
    cf :: [arch reg freg ()] -> [arch reg freg ControlAnn]
    -- | result: src, dest
    mI :: arch reg freg a -> Maybe (reg, reg)
    mf :: arch reg freg a -> Maybe (freg, freg)
    bb :: [arch reg freg a] -> [[arch reg freg a]]

instance Arch X86.X86 X86.AbsReg X86.FAbsReg where

    mI (X86.MovRR _ r0 r1)   = Just (r1, r0)
    mI (X86.Cmovnle _ r0 r1) = Just (r0, r1)
    mI _                     = Nothing

    mf (X86.Movapd _ r0 r1) = Just (r1, r0)
    mf _                    = Nothing

    cf = X86.mkControlFlow
    bb = X86.bb
