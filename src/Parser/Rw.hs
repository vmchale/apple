module Parser.Rw ( rewrite
                 ) where

import           A

rewrite = rw

lassoc :: Builtin -> Bool
lassoc IntExp = False
lassoc Exp    = False
lassoc Div    = True
lassoc IDiv   = True
lassoc Mod    = True
lassoc Times  = True
lassoc Max    = True
lassoc Min    = True
lassoc Dot    = False
lassoc Mul    = True
lassoc VMul   = False
lassoc Plus   = True
lassoc Minus  = True
lassoc C      = True
lassoc ConsE  = False
lassoc Snoc   = False
lassoc Del = True; lassoc DelM = True
lassoc A1     = True
lassoc IOf    = False
lassoc Cyc = True; lassoc Rot = True
lassoc Map    = False
lassoc CatE   = False
lassoc Sr     = True
lassoc Sl     = True
lassoc Xor    = True
lassoc Or = False; lassoc And = False
lassoc Filt = False; lassoc Ices = False
lassoc Part = False
lassoc Fold = False; lassoc Succ = False
lassoc Eq = False; lassoc Neq = False
lassoc Gte = False; lassoc Lte = False
lassoc Gt = False; lassoc Lt = False
lassoc Range = False; lassoc Scan = False

shuntl :: Builtin -> Builtin -> Bool
shuntl op0 op1 | Just f0 <- fi op0, Just f1 <- fi op1 = f0>f1 || lassoc op0 && lassoc op1 && f0==f1 | otherwise = False

rw :: E a -> E a
rw (EApp l0 (EApp l1 e0@(Builtin _ op0) e1) e2) | isBinOp op0 =
    case rw e2 of
        (EApp l2 (EApp l3 e3@(Builtin _ op1) e4) e5) | isBinOp op1 && shuntl op0 op1 -> EApp l0 (EApp l1 e3 (rw (EApp l2 (EApp l3 e0 e1) e4))) e5
        e2'                                                                          -> EApp l0 (EApp l1 e0 (rw e1)) e2'
rw (EApp l e0 e') =
    case rw e' of
        (EApp lϵ (EApp lϵϵ e3@(Builtin _ op) e4) e2) | isBinOp op -> EApp l (EApp lϵϵ e3 (rw $ EApp lϵ e0 e4)) e2
        (Ann lϵ e1 t)                                             -> Ann lϵ (rw $ EApp l e0 e1) t
        (EApp lϵ e1@EApp{} e2)                                    -> EApp l (rw $ EApp lϵ e0 e1) e2
        e1@(EApp _ (Builtin _ Ix'd) _)                            -> EApp l (rw e0) e1
        (EApp lϵ e1 e2)                                           -> EApp l (EApp lϵ (rw e0) e1) e2
        eRw                                                       -> EApp l (rw e0) eRw
rw (Let l (n, e') e) = Let l (n, rw e') (rw e)
rw (Def l (n, e') e) = Def l (n, rw e') (rw e)
rw (LLet l (n, e') e) = LLet l (n, rw e') (rw e)
rw (Tup l es) = Tup l (rw<$>es)
rw (ALit l es) = ALit l (rw<$>es)
rw (Lam l n e) = Lam l n (rw e)
rw (Dfn l e) = Dfn l (rw e)
rw (Parens l e) = Parens l (rw e)
rw (Ann l e t) = Ann l (rw e) (rt t)
rw (Cond l p e e') = Cond l (rw p) (rw e) (rw e')
rw e = e

rt :: T a -> T a
rt (Arr sh (Arrow t t')) = Arrow (Arr sh (rt t)) (rt t')
rt t                     = t
