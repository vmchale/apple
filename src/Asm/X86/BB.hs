-- | Basic blocks.
module Asm.X86.BB ( BB (..)
                  , splitI
                  ) where

import           Asm.X86
import           Data.List.Split (split, whenElt)

data BB reg a = BB { bAnn :: a
                   , isn  :: [X86 reg ()]
                   }

splitI :: [X86 reg ()] -> [BB reg ()]
splitI = fmap (BB ()) . split (whenElt p) where
    p J{}     = True
    p Je{}    = True
    p Jg{}    = True
    p Jl{}    = True
    p Jge{}   = True
    p Jle{}   = True
    p Jne{}   = True
    p Label{} = True
    p Ret{}   = True
    p _       = False
