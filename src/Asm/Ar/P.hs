module Asm.Ar.P ( bundle ) where

import           Asm.Ar
import           CF
import           Class.E
import           Data.Copointed
import           Data.Tuple.Extra (both)
import           LR

bundle :: (E reg, E freg, Copointed (arch reg freg), Functor (arch reg freg), Arch arch reg freg)
       => [arch reg freg ()]
       -> ([arch reg freg (ControlAnn, NLiveness, Maybe (Int,Int))], [arch reg freg (ControlAnn, NLiveness, Maybe (Int,Int))])
bundle isns =
    let cfIsns = cf isns; lIsns = reconstruct cfIsns
        mvIsns = fmap (both toInt).mI<$>isns
        mvFIsns = fmap (both toInt).mf<$>isns
        combine x y z = let tup = (copoint x, copoint y, z) in tup <$ y
    in (zipWith3 combine cfIsns lIsns mvIsns, zipWith3 combine cfIsns lIsns mvFIsns)
