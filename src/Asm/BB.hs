{-# LANGUAGE DeriveFunctor #-}

module Asm.BB ( BB (..) ) where

import           Data.Copointed

data BB arch reg freg a b = BB { unBB :: [arch reg freg a], caBB :: b } deriving (Functor)

instance Copointed (BB arch reg freg a) where
    copoint=caBB

-- use[p,n] = use[n] ++ (use[p] \ def[n])
-- def[p,n] = def[p] ++ def[n]

-- gen <-> use
-- kill <-> def
