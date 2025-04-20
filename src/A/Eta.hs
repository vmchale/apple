module A.Eta ( η ) where

import           A
import           Control.Monad                    ((<=<))
import           Control.Monad.Trans.State.Strict (State, state)
import qualified Data.Text                        as T
import           Nm
import           U

type RM = State Int

nU :: T.Text -> a -> RM (Nm a)
nU n l = state (\i -> let j=i+1 in (Nm n (U j) l, j))

nN :: a -> RM (Nm a)
nN = nU "x"

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
    lApps <- traverse (\t -> do {n <- nN t; pure (\e' -> let t'=eAnn e' in Lam (t~>t') n e', \e' -> let Arrow _ cod=eAnn e' in EApp cod e' (Var t n))}) ts
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

h1 :: Builtin -> Bool
h1 Scan=True; h1 ScanS=True; h1 Zip=True; h1 Map=True
h1 FoldS=True; h1 Fold=True; h1 FoldA=True; h1 Foldl=True
h1 Filt=True; h1 Ices=True; h1 Part=True; h1 Succ=True
h1 Rank{}=True; h1 DI{}=True; h1 Conv{}=True; h1 Focus{}=True
h1 Outer=True; h1 Ug=True; h1 Fib=True; h1 _=False

ηAt :: E (T ()) -> RM (E (T ()))
ηAt (EApp t0 (EApp t1 ho@(Builtin _ Gen) seed) op) = EApp t0 <$> EApp t1 ho <$> ηAt seed <*> η op
ηAt (EApp t0 (EApp t1 ho@(Builtin _ Fib) seed) op) = EApp t0 <$> EApp t1 ho <$> ηAt seed <*> η op
ηAt (EApp t ho@(Builtin _ b) op) | h1 b            = EApp t ho <$> η op
ηAt (EApp t e0 e1)                                 = EApp t <$> ηAt e0 <*> ηAt e1
ηAt (Lam l n e)                                    = Lam l n <$> ηAt e
ηAt (Cond l p e e')                                = Cond l <$> ηAt p <*> ηAt e <*> ηAt e'
ηAt (LLet l (n, e') e)                             = do { e'𝜂 <- ηAt e'; e𝜂 <- ηAt e; pure $ LLet l (n, e'𝜂) e𝜂 }
ηAt (Id l idm)                                     = Id l <$> ηIdm idm
ηAt (ALit l es)                                    = ALit l <$> traverse ηAt es
ηAt (Tup l es)                                     = Tup l <$> traverse ηAt es
ηAt e                                              = pure e

ηIdm (FoldSOfZip seed op es) = FoldSOfZip <$> ηAt seed <*> η op <*> traverse ηAt es
ηIdm (FoldOfZip zop op es)   = FoldOfZip <$> η zop <*> η op <*> traverse ηAt es
ηIdm (FoldGen seed g f n)    = FoldGen <$> ηAt seed <*> η g <*> η f <*> ηAt n
ηIdm (U2 seeds gs c f n)     = U2 <$> traverse ηAt seeds <*> traverse η gs <*> ηAt c <*> η f <*> ηAt n
ηIdm (AShLit ds es)          = AShLit ds <$> traverse ηAt es
ηIdm (Aɴ e ixs)              = Aɴ <$> ηAt e <*> traverse ηAt ixs
ηIdm (Iter g seed n)         = Iter <$> η g <*> ηAt seed <*> ηAt n

-- outermost only
ηM :: E (T ()) -> RM (E (T ()))
ηM e@FLit{}                = pure e
ηM e@ILit{}                = pure e
ηM e@ALit{}                = pure e
ηM e@(Id _ AShLit{})       = pure e
ηM e@(Id _ FoldGen{})      = pure e
ηM e@(Id _ FoldOfZip{})    = pure e
ηM e@(Id _ FoldSOfZip{})   = pure e
ηM e@(Id _ U2{})           = pure e
ηM e@(Id _ Aɴ{})           = pure e
ηM e@(Id _ Iter{})         = pure e
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
