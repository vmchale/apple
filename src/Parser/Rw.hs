module Parser.Rw ( rewrite
                 ) where

import           A

rewrite = rw

isBinOp :: Builtin -> Bool
isBinOp Grade     = False
isBinOp FRange    = False
isBinOp IRange    = False
isBinOp Reverse   = False
isBinOp Transpose = False
isBinOp MapN{}    = False
isBinOp Rank{}    = False
isBinOp Fib       = False
isBinOp Log       = False
isBinOp Size      = False
isBinOp Sqrt      = False
isBinOp Scan{}    = False
isBinOp ItoF      = False
isBinOp _         = True

fi :: Builtin -> Int
fi IntExp = 8
fi Exp = 8
fi Times = 7
fi Div = 7
fi Plus = 6
fi Minus = 6

lassoc :: Builtin -> Bool
lassoc IntExp = False
lassoc Exp = False
lassoc Div = True
lassoc Times = True
lassoc Plus = True
lassoc Minus = True

shuntl :: Builtin -> Builtin -> Bool
shuntl op0 op1 = fi op0 > fi op1 || lassoc op0 && lassoc op1 && fi op0 == fi op1

rw :: E a -> E a
-- TODO: guard against rewriting unary ops (transpose, reverse, floor?)
-- guard against rewriting binary infix
rw (EApp l0 (EApp l1 e0@(Builtin _ op0) e1) e2) | isBinOp op0 =
    case rw e2 of
        (EApp l2 (EApp l3 e3@(Builtin _ op1) e4) e5) | isBinOp op1 && shuntl op0 op1 -> EApp l0 (EApp l1 e3 (rw (EApp l2 (EApp l3 e0 e1) e4))) e5
        e2' -> EApp l0 (EApp l1 e0 (rw e1)) e2'
rw (EApp l e0 e') =
    case rw e' of
        (EApp lϵ e1@EApp{} e2) -> EApp l (rw $ EApp lϵ e0 e1) e2
        (EApp lϵ e1 e2)        -> EApp l (EApp lϵ (rw e0) e1) e2
        eRw                    -> EApp l (rw e0) eRw
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
