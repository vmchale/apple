module R.Dfn ( dedfn ) where

import           A
import           Nm
import           U

-- bottom-up
dedfn :: E a -> E a
dedfn e@ILit{} = e
dedfn e@FLit{} = e
dedfn e@BLit{} = e
dedfn e@Var{} = e
dedfn e@Builtin{} = e
dedfn e@ResVar{} = e
dedfn (Ann l e t) = Ann l (dedfn e) t
dedfn (ALit l es) = ALit l (map dedfn es)
dedfn (Tup l es) = Tup l (map dedfn es)
dedfn (EApp l e e') = EApp l (dedfn e) (dedfn e')
dedfn (Cond l e e' e'') = Cond l (dedfn e) (dedfn e') (dedfn e'')
dedfn (LamΠ l ns e) = LamΠ l ns (dedfn e)
dedfn (Lam l n e) = Lam l n (dedfn e)
dedfn (Let l (n, e) eBody) = Let l (n, dedfn e) (dedfn eBody)
dedfn (Def l (n, e) eBody) = Def l (n, dedfn e) (dedfn eBody)
dedfn (LLet l (n, e) eBody) = LLet l (n, dedfn e) (dedfn eBody)
dedfn (Dfn l e) =
    let x=Nm "x" (U (-1))
        y=Nm "y" (U (-2))
        (eDone, hasY) = replaceXY x y (dedfn e)
    in if hasY
        then Lam l (x l) (Lam l (y l) eDone)
        else Lam l (x l) eDone
dedfn (Parens _ e) = dedfn e

-- this approach is criminally inefficient
replaceXY :: (a -> Nm a) -- ^ x
          -> (a -> Nm a) -- ^ y
          -> E a -> (E a, Bool) -- True if it has 'y'
replaceXY _ y (ResVar l Y) = (Var l (y l), True)
replaceXY x _ (ResVar l X) = (Var l (x l), False)
replaceXY _ _ e@FLit{} = (e, False)
replaceXY _ _ e@ILit{} = (e, False)
replaceXY _ _ e@BLit{} = (e, False)
replaceXY _ _ e@Var{} = (e, False)
replaceXY _ _ e@Builtin{} = (e, False)
replaceXY x y (Ann l e t) =
    let (e', b) = replaceXY x y e
        in (Ann l e' t, b)
replaceXY x y (LamΠ l ns e) =
    let (e', b) = replaceXY x y e
        in (LamΠ l ns e', b)
replaceXY x y (Lam l n e) =
    let (e', b) = replaceXY x y e
        in (Lam l n e', b)
replaceXY x y (EApp l e e') =
    let (eR, b) = replaceXY x y e
        (eR', b') = replaceXY x y e'
        in (EApp l eR eR', b || b')
replaceXY x y (Cond l p e e') =
    let (pR, b0) = replaceXY x y p
        (eR, b1) = replaceXY x y e
        (eR', b2) = replaceXY x y e'
    in (Cond l pR eR eR', b0 || b1 || b2)
replaceXY x y (Let l (n, e) e') =
    let (eR, b) = replaceXY x y e
        (eR', b') = replaceXY x y e'
        in (Let l (n, eR) eR', b || b')
replaceXY x y (LLet l (n, e) e') =
    let (eR, b) = replaceXY x y e
        (eR', b') = replaceXY x y e'
        in (LLet l (n, eR) eR', b || b')
replaceXY x y (Def l (n, e) e') =
    let (eR, b) = replaceXY x y e
        (eR', b') = replaceXY x y e'
        in (Def l (n, eR) eR', b || b')
replaceXY x y (ALit l es) =
    let (esR, bs) = unzip (fmap (replaceXY x y) es)
    in (ALit l esR, or bs)
replaceXY x y (Tup l es) =
    let (esR, bs) = unzip (fmap (replaceXY x y) es)
    in (Tup l esR, or bs)
