module Asm.Ar.P ( bundle ) where

import           Asm.Ar
import           Asm.L
import           CF
import           Class.E
import           Data.Copointed
import           Data.Tuple.Extra (both)

bundle :: (E reg, Copointed (arch reg freg), Arch arch reg freg)
       => [arch reg freg ()]
       -> ([arch reg freg (UD, Liveness, Maybe (Int,Int))], [arch reg freg (UD, Liveness, Maybe (Int,Int))])
bundle isns =
    let cfIsns = fmap udd isns; lIsns = mkLive isns
        mvIsns = fmap (both toInt).mI<$>isns
        mvFIsns = mf<$>isns
        combine x y z = let tup = (x, copoint y, z) in tup <$ y
    in (zipWith3 combine cfIsns lIsns mvIsns, zipWith3 combine cfIsns lIsns mvFIsns)
