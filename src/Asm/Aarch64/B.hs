module Asm.Aarch64.B ( bb ) where

import           Asm.Aarch64
import           Data.List.Split (split, whenElt)

bb :: [AArch64 reg freg a] -> [[AArch64 reg freg a]]
bb = split (whenElt cf)
    where cf B{}=True; cf Bc{}=True; cf Label{}=True; cf Bl{}=True; cf Ret{}=True; cf _=False
