module Asm.Ar ( Arch (..) ) where

class Arch a where
    isM :: a -> Bool
    isMX :: a -> Bool
