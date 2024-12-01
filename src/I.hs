module I ( inline
         , β
         ) where

import           A
import           Control.Monad.Trans.State.Strict (State, gets, modify, runState)
import           Data.Bifunctor                   (second)
import qualified Data.IntMap                      as IM
import           Nm
import           Nm.IntMap                        as Nm
import           R
import           Ty

data ISt a = ISt { renames :: !Rs
                 , binds   :: IM.IntMap (E a)
                 }

instance HasRs (ISt a) where
    rename f s = fmap (\x -> s { renames = x }) (f (renames s))

type M a = State (ISt a)

bind :: Nm a -> E a -> M a ()
bind n e = modify (\(ISt r bs) -> ISt r (insert n e bs))

runI i = second (max_.renames) . flip runState (ISt (Rs i mempty) mempty)

inline :: Int -> E (T ()) -> (E (T ()), Int)
inline i = runI i.iM

β :: Int -> E (T ()) -> (E (T ()), Int)
β i = runI i.bM

hRi :: Idiom -> Bool
hRi (AShLit _ es) = any hR es; hRi _ = error "Internal error."

hR :: E a -> Bool
hR (EApp _ (EApp _ (Builtin _ R) _) _) = True
hR Builtin{}                           = False
hR FLit{}                              = False
hR ILit{}                              = False
hR BLit{}                              = False
hR (ALit _ es)                         = any hR es
hR (Tup _ es)                          = any hR es
hR (Cond _ p e e')                     = hR p||hR e||hR e'
hR (EApp _ e e')                       = hR e||hR e'
hR (Lam _ _ e)                         = hR e
hR (Let _ (_, e') e)                   = hR e'||hR e
hR (Def _ (_, e') e)                   = hR e'||hR e
hR (LLet _ (_, e') e)                  = hR e'||hR e
hR Var{}                               = False
hR (Id _ i)                            = hRi i
hR Dfn{}                               = desugar
hR ResVar{}                            = desugar
hR Parens{}                            = desugar
hR Ann{}                               = error "Internal error."

-- assumes globally renamed already
-- | Inlining is easy because we don't have recursion
iM :: E (T ()) -> M (T ()) (E (T ()))
iM e@Builtin{} = pure e
iM e@FLit{} = pure e
iM e@ILit{} = pure e
iM e@BLit{} = pure e
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
    bind n eI *> iM e
                     | otherwise = iM(LLet l (n,e') e)
iM (Def _ (n, e') e) = do
    eI <- iM e'
    bind n eI *> iM e
iM e@(Var t n) = do
    st <- gets binds
    case Nm.lookup n st of
        Just e' -> do {er <- rE e'; pure $ fmap (rwArr.aT (match (eAnn er) t)) er}
        Nothing -> pure e
iM Dfn{}=desugar; iM ResVar{}=desugar; iM Parens{}=desugar; iM Ann{}=error "Internal error."

-- beta reduction
bM :: E (T ()) -> M (T ()) (E (T ()))
bM e@Builtin{} = pure e
bM e@FLit{} = pure e
bM e@ILit{} = pure e
bM e@BLit{} = pure e
bM (ALit l es) = ALit l <$> traverse bM es
bM (Tup l es) = Tup l <$> traverse bM es
bM (Cond l p e0 e1) = Cond l <$> bM p <*> bM e0 <*> bM e1
bM (EApp l (Lam _ n e') e) | not(hR e) = do
    eI <- bM e
    bind n eI *> bM e'
                           | otherwise = do
    eI <- bM e
    LLet l (n, eI) <$> bM e'
bM (EApp l e0 e1) = do
    e0' <- bM e0
    e1' <- bM e1
    case e0' of
        Lam{} -> bM (EApp l e0' e1')
        _     -> pure $ EApp l e0' e1'
bM (Lam l n e) = Lam l n <$> bM e
bM e@(Var _ n) = do
    st <- gets binds
    case Nm.lookup n st of
        -- TODO: track if looked up once before (avoid spurious clones?)
        Just e' -> rE e' -- rE vs. match ... t?
        Nothing -> pure e
bM (LLet l (n, e') e) = do
    e'B <- bM e'
    eB <- bM e
    pure $ LLet l (n, e'B) eB
bM (Id l idm) = Id l <$> bid idm
bM Dfn{}=desugar; bM ResVar{}=desugar; bM Parens{}=desugar; bM Ann{} = error "Internal error."

bid :: Idiom -> M (T ()) Idiom
bid (FoldSOfZip seed op es) = FoldSOfZip <$> bM seed <*> bM op <*> traverse bM es
bid (FoldOfZip zop op es)   = FoldOfZip <$> bM zop <*> bM op <*> traverse bM es
bid (AShLit ds es)          = AShLit ds <$> traverse bM es
bid (FoldGen seed f g n)    = FoldGen <$> bM seed <*> bM f <*> bM g <*> bM n
bid (U2 seeds gs c f n)     = U2 <$> traverse bM seeds <*> traverse bM gs <*> bM c <*> bM f <*> bM n

desugar :: a
desugar = error "Internal error. Should have been desugared."
