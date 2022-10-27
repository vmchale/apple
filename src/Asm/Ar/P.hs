module Asm.Ar.P ( bundle, bundleF ) where

import           Asm.Ar
import qualified Asm.X86.CF as X86

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
