module Asm.Aarch64.Guess ( collectV ) where

import           Asm.Aarch64 hiding (toInt)
import           Class.E
import qualified Data.IntSet as IS

singleton :: V2Reg FAReg -> IS.IntSet
singleton = IS.singleton . toInt

fromList = IS.fromList . fmap toInt

collectV :: AArch64 areg FAReg a -> IS.IntSet
collectV (Dup _ q _)      = singleton q
-- TODO: only index 1? (only high bits are volatile lol)
collectV (Ins _ v _ _)    = singleton v
collectV (DupD _ q _)     = singleton q
collectV (MovQQ _ q _)    = singleton q
collectV (LdrS _ q _)     = singleton q
collectV (ZeroS _ v)      = singleton v
collectV (EorS _ v _ _)   = singleton v
collectV (Fadd2 _ v _ _)  = singleton v
collectV (Fsub2 _ v _ _)  = singleton v
collectV (Fmul2 _ v _ _)  = singleton v
collectV (Fdiv2 _ v _ _)  = singleton v
collectV (Fmax2 _ v _ _)  = singleton v
collectV (Fmin2 _ v _ _)  = singleton v
collectV (Fsqrt2 _ v _)   = singleton v
collectV (Fneg2 _ v _)    = singleton v
collectV (Ldp2 _ q0 q1 _) = fromList [q0,q1]
collectV (Fmla _ v _ _)   = singleton v
collectV _                = IS.empty
