module Asm.Ar.P ( bundle ) where

import           Asm.Aarch64.LI
import           Asm.Ar
import           Asm.L
import           CF
import           Class.E
import           Data.Copointed
import           Data.Tuple.Extra (both)
import           LR

bundle :: (E reg, E freg, Copointed (arch reg freg), Functor (arch reg freg), Arch arch reg freg)
       => [arch reg freg ()]
       -> ([arch reg freg (UD, Liveness, Maybe (Int,Int))], [arch reg freg (UD, Liveness, Maybe (Int,Int))])
bundle isns =
    let cfIsns = fmap udd isns; lIsns = mkLive isns
        mvIsns = fmap (both toInt).mI<$>isns
        mvFIsns = fmap (both toInt).mf<$>isns
        combine x y z = let tup = (x, copoint y, z) in tup <$ y
    in (zipWith3 combine cfIsns lIsns mvIsns, zipWith3 combine cfIsns lIsns mvFIsns)
