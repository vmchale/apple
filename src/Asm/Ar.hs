{-# LANGUAGE MultiParamTypeClasses #-}

module Asm.Ar ( Arch (..), E(..), bundle, bundleF ) where

import qualified Asm.X86          as X86
import qualified Asm.X86.B        as X86
import qualified Asm.X86.CF       as X86
import           CF
import           Data.Copointed
import           Data.Tuple.Extra (both)
import           LR

class Arch arch reg freg where
    cf :: [arch reg freg ()] -> [arch reg freg ControlAnn]
    -- | result: src, dest
    mI :: arch reg freg a -> Maybe (reg, reg)
    mf :: arch reg freg a -> Maybe (freg, freg)
    bb :: [arch reg freg a] -> [[arch reg freg a]]

class E a where
    toInt :: a -> Int

instance E X86.AbsReg where
    toInt = X86.toInt

instance E X86.FAbsReg where
    toInt = X86.fToInt

instance Arch X86.X86 X86.AbsReg X86.FAbsReg where

    mI (X86.MovRR _ r0 r1) = Just (r1, r0)
    mI _                   = Nothing

    mf (X86.Movapd _ r0 r1) = Just (r1, r0)
    mf _                    = Nothing

    cf = X86.mkControlFlow
    bb = X86.bb

bundle :: (E reg, Copointed (arch reg freg), Functor (arch reg freg), Arch arch reg freg)
       => [arch reg freg ()]
       -> [arch reg freg (ControlAnn, NLiveness, Maybe (Int,Int))]
bundle isns =
    let cfIsns = cf isns; lIsns = reconstruct cfIsns
        mvIsns = fmap (both toInt).mI<$>isns
        combine x y z = let tup = (copoint x, copoint y, z) in tup <$ y
    in zipWith3 combine cfIsns lIsns mvIsns

-- TODO: this computes reconstruct cfIsns twice?

bundleF :: (E freg, Copointed (arch reg freg), Functor (arch reg freg), Arch arch reg freg)
        => [arch reg freg ()]
        -> [arch reg freg (ControlAnn, NLiveness, Maybe (Int,Int))]
bundleF isns =
    let cfIsns = cf isns; lIsns = reconstruct cfIsns
        mvIsns = fmap (both toInt).mf<$>isns
        combine x y z = let tup = (copoint x, copoint y, z) in tup <$ y
    in zipWith3 combine cfIsns lIsns mvIsns
