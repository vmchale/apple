module Data.Copointed ( Copointed (..)
                      ) where

class Functor p => Copointed p where
    copoint :: p a -> a

instance Copointed ((,) a) where
    copoint (_, y) = y
