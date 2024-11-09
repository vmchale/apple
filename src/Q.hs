module Q ((@<>), (#.)) where

infixr 7 @<>
infixl 4 #.

(@<>) :: (Monoid m, Foldable f) => (a -> m) -> f a -> m
(@<>) = foldMap

(#.) = filter
