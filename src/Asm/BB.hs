{-# LANGUAGE DeriveFunctor #-}

module Asm.BB ( BB (..) ) where

import           Data.Copointed

data BB arch reg freg f2reg a b = BB { unBB :: [arch reg freg f2reg a], caBB :: b } deriving (Functor)

instance Copointed (BB arch reg freg f2reg a) where copoint=caBB
