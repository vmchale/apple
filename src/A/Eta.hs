module A.Eta ( eta ) where

import           A
import           Control.Monad ((<=<))
import           R.M

-- domains
doms :: T a -> [T a]
doms (Arrow t t') = t:doms t'
doms _            = []

-- count lambdas
cLam :: E a -> Int
cLam (Lam _ _ e) = 1 + cLam e; cLam _ = 0

thread = foldr (.) id

unseam :: [T ()] -> RM (E (T ()) -> E (T ()), E (T ()) -> E (T ()))
unseam ts = do
    lApps <- traverse (\t -> do { n <- nextN t ; pure (\e' -> let t' = eAnn e' in Lam (Arrow t t') n e', \e' -> let Arrow _ cod = eAnn e' in EApp cod e' (Var t n)) }) ts
    let (ls, eApps) = unzip lApps
    pure (thread ls, thread (reverse eApps))

mkLam :: [T ()] -> E (T ()) -> RM (E (T ()))
mkLam ts e = do
    (lam, app) <- unseam ts
    pure $ lam (app e)

eta :: E (T ()) -> RM (E (T ()))
eta = etaM <=< etaAt

tuck :: E a -> (E a -> E a, E a)
tuck (Lam l n e) = let (f, e') = tuck e in (Lam l n.f, e')
tuck e           = (id, e)

etaAt :: E (T ()) -> RM (E (T ()))
etaAt (EApp t ho@(Builtin _ Scan{}) op)  = EApp t ho <$> eta op
etaAt (EApp t ho@(Builtin _ ScanS{}) op) = EApp t ho <$> eta op
etaAt (EApp t ho@(Builtin _ Zip{}) op)   = EApp t ho <$> eta op
etaAt (EApp t ho@(Builtin _ Succ{}) op)  = EApp t ho <$> eta op
etaAt (EApp t ho@(Builtin _ FoldS) op)   = EApp t ho <$> eta op
etaAt (EApp t ho@(Builtin _ Fold) op)    = EApp t ho <$> eta op
etaAt (EApp t ho@(Builtin _ FoldA) op)   = EApp t ho <$> eta op
etaAt (EApp t ho@(Builtin _ Foldl) op)   = EApp t ho <$> eta op
etaAt (EApp t ho@(Builtin _ Map{}) op)   = EApp t ho <$> eta op
etaAt (EApp t ho@(Builtin _ Rank{}) op)  = EApp t ho <$> eta op
etaAt (EApp t ho@(Builtin _ DI{}) op)    = EApp t ho <$> eta op
etaAt (EApp t ho@(Builtin _ Conv{}) op)  = EApp t ho <$> eta op
etaAt (EApp t ho@(Builtin _ Outer) op)   = EApp t ho <$> eta op
etaAt (EApp t e0 e1)                     = EApp t <$> etaAt e0 <*> etaAt e1
etaAt (Lam l n e)                        = Lam l n <$> etaAt e
etaAt (Cond l p e e')                    = Cond l <$> etaAt p <*> etaAt e <*> etaAt e'
etaAt (LLet l (n, e') e)                 = do { e'𝜂 <- etaAt e'; e𝜂 <- etaAt e; pure $ LLet l (n, e'𝜂) e𝜂 }
etaAt (Id l idm)                         = Id l <$> etaIdm idm
etaAt (ALit l es)                        = ALit l <$> traverse etaAt es
etaAt (Tup l es)                         = Tup l <$> traverse etaAt es
etaAt e                                  = pure e

etaIdm (FoldSOfZip seed op es) = FoldSOfZip <$> etaAt seed <*> etaAt op <*> traverse etaAt es
etaIdm (FoldOfZip zop op es)   = FoldOfZip <$> etaAt zop <*> etaAt op <*> traverse etaAt es
etaIdm (AShLit ds es)          = AShLit ds <$> traverse etaAt es

-- outermost only
etaM :: E (T ()) -> RM (E (T ()))
etaM e@FLit{}                = pure e
etaM e@ILit{}                = pure e
etaM e@ALit{}                = pure e
etaM e@(Id _ AShLit{})       = pure e
etaM e@Cond{}                = pure e
etaM e@BLit{}                = pure e
etaM e@Tup{}                 = pure e
etaM e@(Var t@Arrow{} _)     = mkLam (doms t) e
etaM e@Var{}                 = pure e
etaM e@(Builtin t@Arrow{} _) = mkLam (doms t) e
etaM e@Builtin{}             = pure e
etaM e@(EApp t@Arrow{} _ _)  = mkLam (doms t) e
etaM e@EApp{}                = pure e
etaM e@LLet{}                = pure e
etaM e@(Lam t@Arrow{} _ _)   = do
    let l = length (doms t)
        (preL, e') = tuck e
    (lam, app) <- unseam (take (l-cLam e) $ doms t)
    pure (lam (preL (app e')))
-- "\\y. (y*)" -> (λx. (λy. (y * x)))
