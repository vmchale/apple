module Q ((@<>), (#.), (!:)) where

import qualified Data.IntMap as IM

infixr 7 @<>
infixl 4 #.

(@<>) :: (Monoid m, Foldable f) => (a -> m) -> f a -> m
(@<>) = foldMap

(#.) = filter

(!:) k i = IM.alter (\kϵ -> Just$!case kϵ of {Nothing -> [i]; Just is -> i:is}) k
