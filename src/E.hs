module E ( D (..), L (..), pr, pr1, pc, psh ) where

import           Sh

ip1 :: I a -> L
ip1 (Ix x i) | i>0 = ip (Ix x (i-1)); ip1 _ = U Z

ip :: I a -> L
ip (Ix _ i) | even i = E (dᵢ i) | odd i = O (if i>=3 then S else Z)
ip (StaPlus _ i j) = sp (ip i) (ip j)
-- StaMul 2 case
ip _ = U Z

pr1, pr,pc,psh :: Sh a -> L
pr1 (i `Cons` _) = ip1 i; pr1 _ = U Z
pr (i `Cons` _) = ip i; pr _ = U Z
pc (_ `Cons` i `Cons` _) = ip i; pc _ = U Z
psh (i `Cons` sh) = mp (ip i) (psh sh); psh _ = U Z

dᵢ i | i>0 = S | otherwise = Z

data D = S | Z
data L = E D | O D | U D

dl1 (E d)=d; dl1 O{}=S; dl1 (U d)=d

mp :: L -> L -> L
mp (E d) (E d') = E (d#*d'); mp (E d) l = E (d#*dl1 l); mp l (E d) = E (dl1 l#*d); mp l (O d) = O (dl1 l#*d); mp (O d) l = O (d#*dl1 l); mp l l' = U (dl1 l#*dl1 l')
sp (E d) (E d') = E (d#|d'); sp (E d) (O d') = O (d#|d'); sp (O d) (E d') = O (d#|d'); sp O{} O{} = E S; sp d d' = U (dl1 d#|dl1 d')

(#*), (#|) :: D -> D -> D
S #* S = S; _ #* _ = Z
S #| _ = S; _ #| S = S; _ #| _ = Z
