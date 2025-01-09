module Ix ( L (..), ip, pr, pc, psh ) where

import           Sh

ip :: I a -> L
ip (Ix _ i) | even i&&i>0 = E | odd i = O
ip (StaPlus _ i0 i1) = sp (ip i0) (ip i1)
ip (StaMul _ i0 i1) = mp (ip i0) (ip i1)
ip _ = U

pr,pc,psh :: Sh a -> L
pr (i `Cons` _) = ip i; pr _ = U
pc (_ `Cons` i `Cons` _) = ip i; pc _ = U
psh (i `Cons` sh) = mp (ip i) (psh sh); psh _ = U

data L = E | O | U

sp,mp :: L -> L -> L
mp E E = E; mp _ O = O; mp O _ = O; mp _ _ = U
sp E E = E; sp E O = O; sp O E = O; sp O O = O; sp _ _ = U
