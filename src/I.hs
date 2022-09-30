module I ( inline
         , β
         ) where

import           A
import           Control.Monad.State.Strict (State, gets, modify, runState)
import           Data.Bifunctor             (second)
import qualified Data.IntMap                as IM
import           Name
import           R
import           Ty
import           U

data ISt a = ISt { renames :: !Renames
                 , binds   :: IM.IntMap (E a)
                 }

instance HasRenames (ISt a) where
    rename f s = fmap (\x -> s { renames = x }) (f (renames s))

type M a = State (ISt a)

bind :: Name a -> E a -> ISt a -> ISt a
bind (Name _ (U u) _) e (ISt r bs) = ISt r (IM.insert u e bs)

runI i = second (max_.renames) . flip runState (ISt (Renames i mempty) mempty)

inline :: Int -> E (T ()) -> (E (T ()), Int)
inline i = runI i . iM

β :: Int -> E (T ()) -> (E (T ()), Int)
β i = runI i . bM

hR :: E a -> Bool
hR (EApp _ (EApp _ (Builtin _ RF) _) _) = True
hR (EApp _ (EApp _ (Builtin _ RI) _) _) = True
hR Builtin{}                            = False
hR FLit{}                               = False
hR ILit{}                               = False
hR (ALit _ es)                          = any hR es
hR (Tup _ es)                           = any hR es
hR (Cond _ p e e')                      = hR p||hR e||hR e'
hR (EApp _ e e')                        = hR e||hR e'
hR (Lam _ _ e)                          = hR e
hR (Let _ (_, e') e)                    = hR e'||hR e
hR (Def _ (_, e') e)                    = hR e'||hR e
hR (LLet _ (_, e') e)                   = hR e'||hR e
hR Var{}                                = False

-- assumes globally renamed already
-- | Inlining is easy because we don't have recursion
iM :: E (T ()) -> M (T ()) (E (T ()))
iM e@Builtin{} = pure e
iM e@FLit{} = pure e
iM e@ILit{} = pure e
iM (ALit l es) = ALit l <$> traverse iM es
iM (Tup l es) = Tup l <$> traverse iM es
iM (Cond l p e0 e1) = Cond l <$> iM p <*> iM e0 <*> iM e1
iM (EApp l e0 e1) = EApp l <$> iM e0 <*> iM e1
iM (Lam l n e) = Lam l n <$> iM e
iM (LLet l (n, e') e) = do
    e'I <- iM e'
    eI <- iM e
    pure $ LLet l (n, e'I) eI
iM (Let l (n, e') e) | not(hR e')= do
    eI <- iM e'
    modify (bind n eI) *> iM e
                     | otherwise = iM(LLet l (n,e') e)
iM (Def _ (n, e') e) = do
    eI <- iM e'
    modify (bind n eI) *> iM e
iM e@(Var t (Name _ (U i) _)) = do
    st <- gets binds
    case IM.lookup i st of
        Just e' -> do {er <- rE e'; pure $ fmap (aT (match (eAnn er) t)) er}
        Nothing -> pure e

-- beta reduction
bM :: E (T ()) -> M (T ()) (E (T ()))
bM e@Builtin{} = pure e
bM e@FLit{} = pure e
bM e@ILit{} = pure e
bM (ALit l es) = ALit l <$> traverse bM es
bM (Tup l es) = Tup l <$> traverse bM es
bM (Cond l p e0 e1) = Cond l <$> bM p <*> bM e0 <*> bM e1
bM (EApp _ (Lam _ n e') e) = do
    eI <- bM e
    modify (bind n eI) *> bM e'
bM (EApp l e0 e1) = do
    e0' <- bM e0
    e1' <- bM e1
    case e0' of
        Lam{} -> bM (EApp l e0' e1')
        _     -> pure $ EApp l e0' e1'
bM (Lam l n e) = Lam l n <$> bM e
bM e@(Var _ (Name _ (U i) _)) = do
    st <- gets binds
    case IM.lookup i st of
        Just e' -> rE e'
        Nothing -> pure e
bM (LLet l (n, e') e) = do
    e'B <- bM e'
    eB <- bM e
    pure $ LLet l (n, e'B) eB
bM (Id l idm) = Id l <$> bid idm

bid :: Idiom -> M (T ()) Idiom
bid (FoldOfZip seed op es) = FoldOfZip <$> bM seed <*> bM op <*> traverse bM es
bid (LoopN seed op n)      = LoopN <$> bM seed <*> bM op <*> bM n
