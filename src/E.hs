module E ( L (..), pr, pr1, pc, psh ) where

import           Sh

ip1 :: I a -> L
ip1 (Ix x i) | i>0 = ip (Ix x (i-1)); ip1 _ = U

ip :: I a -> L
ip (Ix _ i) | even i&&i>0 = E | odd i = O
ip (StaPlus _ i0 i1) = sp (ip i0) (ip i1)
ip (StaMul _ i0 i1) = mp (ip i0) (ip i1)
ip _ = U

pr1, pr,pc,psh :: Sh a -> L
pr1 (i `Cons` _) = ip1 i; pr1 _ = U
pr (i `Cons` _) = ip i; pr _ = U
pc (_ `Cons` i `Cons` _) = ip i; pc _ = U
psh (i `Cons` sh) = mp (ip i) (psh sh); psh _ = U

data L = E | O | U

sp,mp :: L -> L -> L
mp E E = E; mp _ O = O; mp O _ = O; mp _ _ = U
sp E E = E; sp E O = O; sp O E = O; sp O O = O; sp _ _ = U
