module A.Eta ( η ) where

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

η :: E (T ()) -> RM (E (T ()))
η = ηM <=< ηAt

tuck :: E a -> (E a -> E a, E a)
tuck (Lam l n e) = let (f, e') = tuck e in (Lam l n.f, e')
tuck e           = (id, e)

ηAt :: E (T ()) -> RM (E (T ()))
ηAt (EApp t ho@(Builtin _ Scan{}) op)  = EApp t ho <$> η op
ηAt (EApp t ho@(Builtin _ ScanS{}) op) = EApp t ho <$> η op
ηAt (EApp t ho@(Builtin _ Zip{}) op)   = EApp t ho <$> η op
ηAt (EApp t ho@(Builtin _ Succ{}) op)  = EApp t ho <$> η op
ηAt (EApp t ho@(Builtin _ FoldS) op)   = EApp t ho <$> η op
ηAt (EApp t ho@(Builtin _ Fold) op)    = EApp t ho <$> η op
ηAt (EApp t ho@(Builtin _ FoldA) op)   = EApp t ho <$> η op
ηAt (EApp t ho@(Builtin _ Foldl) op)   = EApp t ho <$> η op
ηAt (EApp t ho@(Builtin _ Map{}) op)   = EApp t ho <$> η op
ηAt (EApp t ho@(Builtin _ Rank{}) op)  = EApp t ho <$> η op
ηAt (EApp t ho@(Builtin _ DI{}) op)    = EApp t ho <$> η op
ηAt (EApp t ho@(Builtin _ Conv{}) op)  = EApp t ho <$> η op
ηAt (EApp t ho@(Builtin _ Outer) op)   = EApp t ho <$> η op
ηAt (EApp t e0 e1)                     = EApp t <$> ηAt e0 <*> ηAt e1
ηAt (Lam l n e)                        = Lam l n <$> ηAt e
ηAt (Cond l p e e')                    = Cond l <$> ηAt p <*> ηAt e <*> ηAt e'
ηAt (LLet l (n, e') e)                 = do { e'𝜂 <- ηAt e'; e𝜂 <- ηAt e; pure $ LLet l (n, e'𝜂) e𝜂 }
ηAt (Id l idm)                         = Id l <$> ηIdm idm
ηAt (ALit l es)                        = ALit l <$> traverse ηAt es
ηAt (Tup l es)                         = Tup l <$> traverse ηAt es
ηAt e                                  = pure e

ηIdm (FoldSOfZip seed op es) = FoldSOfZip <$> ηAt seed <*> ηAt op <*> traverse ηAt es
ηIdm (FoldOfZip zop op es)   = FoldOfZip <$> ηAt zop <*> ηAt op <*> traverse ηAt es
ηIdm (AShLit ds es)          = AShLit ds <$> traverse ηAt es

-- outermost only
ηM :: E (T ()) -> RM (E (T ()))
ηM e@FLit{}                = pure e
ηM e@ILit{}                = pure e
ηM e@ALit{}                = pure e
ηM e@(Id _ AShLit{})       = pure e
ηM e@Cond{}                = pure e
ηM e@BLit{}                = pure e
ηM e@Tup{}                 = pure e
ηM e@(Var t@Arrow{} _)     = mkLam (doms t) e
ηM e@Var{}                 = pure e
ηM e@(Builtin t@Arrow{} _) = mkLam (doms t) e
ηM e@Builtin{}             = pure e
ηM e@(EApp t@Arrow{} _ _)  = mkLam (doms t) e
ηM e@EApp{}                = pure e
ηM e@LLet{}                = pure e
ηM e@(Lam t@Arrow{} _ _)   = do
    let l = length (doms t)
        (preL, e') = tuck e
    (lam, app) <- unseam (take (l-cLam e) $ doms t)
    pure (lam (preL (app e')))
-- "\\y. (y*)" -> (λx. (λy. (y * x)))
