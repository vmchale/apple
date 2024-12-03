module Asm.Aarch64.Guess ( collectV, collectS ) where

import           Asm.Aarch64 hiding (toInt)
import           Class.E
import qualified Data.IntSet as IS
import qualified Data.Set    as S

ss=S.singleton . simd2

collectV :: (Ord freg, E freg) => AArch64 areg freg a -> IS.IntSet
collectV = IS.fromList . fmap toInt . S.toList . collectS

collectS :: Ord freg => AArch64 areg freg a -> S.Set freg
collectS (Dup _ q _)      = ss q
collectS (Ins _ v 1 _)    = ss v
collectS (DupD _ q _ _)   = ss q
collectS (MovQQ _ q _)    = ss q
collectS (LdrS _ q _)     = ss q
collectS (ZeroS _ v)      = ss v
collectS (EorS _ v _ _)   = ss v
collectS (Fadd2 _ v _ _)  = ss v
collectS (Fsub2 _ v _ _)  = ss v
collectS (Fmul2 _ v _ _)  = ss v
collectS (Fdiv2 _ v _ _)  = ss v
collectS (Fmax2 _ v _ _)  = ss v
collectS (Fmin2 _ v _ _)  = ss v
collectS (Fsqrt2 _ v _)   = ss v
collectS (Fabs2 _ v _)    = ss v
collectS (Fneg2 _ v _)    = ss v
collectS (Ldp2 _ q0 q1 _) = S.fromList (simd2<$>[q0,q1])
collectS (Fmla _ v _ _)   = ss v
collectS (Fmls _ v _ _)   = ss v
collectS _                = S.empty
