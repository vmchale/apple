{-# LANGUAGE DeriveGeneric #-}

module Ty.M ( check, RE (..) ) where

import           A
import           Control.Applicative (Alternative (..))
import           Control.DeepSeq     (NFData)
import           Data.Foldable       (asum)
import           GHC.Generics        (Generic)
import           Prettyprinter       (Pretty (..), squotes, (<+>))
import           Sh

data RE = MR (E (T ())) (T ()) | Unflat (E (T ())) (T ()) | UT (E (T ())) (T ()) | IS (Sh ()) deriving (Generic)

instance NFData RE where

instance Pretty RE where
    pretty (MR e t)     = "Type" <+> squotes (pretty t) <+> "of expression" <+> squotes (pretty e) <+> "is not sufficiently monomorphic."
    pretty (Unflat e t) = "Error in expression" <+> squotes (pretty e) <+> "of type" <+> squotes (pretty t) <> ": arrays of functions are not supported."
    pretty (UT e t)     = "Type" <+> squotes (pretty t) <+> "of expression" <+> squotes (pretty e) <+> "tuples of arrays of tuples are not supported"
    pretty (IS s)       = "𝔯 requires statically known dimensions; inferred shape" <+> squotes (pretty s)

check = cM

cM :: E (T ()) -> Maybe RE
cM e | Just t <- mrT (eAnn e) = Just (MR e t)
cM e | Just t <- flT (eAnn e) = Just (Unflat e t)
cM e | Just t <- ata (eAnn e) = Just (UT e t)
cM (Builtin (Arrow _ (Arrow _ (Arr sh _))) R) | dynSh sh = Just (IS sh)
cM (Let _ (_, e) e') = cM e <|> cM e'
cM (LLet _ (_, e) e') = cM e <|> cM e'
cM (Def _ _ e') = cM e' -- FIXME hm
cM (EApp _ e e') = cM e <|> cM e'
cM (ALit _ es) = cM ||> es
cM (Lam _ _ e) = cM e
cM (Cond _ p e e') = cM p <|> cM e <|> cM e'
cM (Tup _ es) = cM ||> es
cM Builtin{} = Nothing
cM ILit{} = Nothing
cM FLit{} = Nothing
cM BLit{} = Nothing
cM Var{} = Nothing
cM Dfn{} = desugar; cM ResVar{} = desugar; cM Parens{} = desugar
cM Id{} = error "Internal error."; cM Ann{} = error "Internal error."

mrT :: T a -> Maybe (T a)
mrT t@TVar{}     = Just t
mrT t@IZ{}       = Just t
mrT (Arr _ t)    = mrT t
mrT (Arrow t t') = mrT t <|> mrT t'
mrT (P ts)       = mrT||>ts
mrT t@Ρ{}        = Just t
mrT _            = Nothing

flT :: T a -> Maybe (T a)
flT t@(Arr _ tϵ) | ha tϵ = Just t
flT (Arrow t t') = flT t <|> flT t'
flT (P ts) = flT ||> ts
flT _ = Nothing

ha :: T a -> Bool
ha Arrow{}   = True
ha (P ts)    = any ha ts
ha (Arr _ t) = ha t
ha _         = False

har :: T a -> Bool
har Arr{} = True; har (P ts) = any har ts; har _ = False

ata :: T a -> Maybe (T a)
ata t@(Arr _ (P ts)) | any har ts = Just t
ata (Arrow t t') = ata t <|> ata t'
ata (P t) = ata ||> t
ata _ = Nothing

dynI :: I a -> Bool
dynI Ix{}      = False
dynI IVar{}    = True
dynI IEVar{}   = True
dynI StaPlus{} = True
dynI StaMul{}  = True

dynSh :: Sh a -> Bool
dynSh SVar{}      = True
dynSh Nil         = False
dynSh (Cons i sh) = dynI i || dynSh sh
dynSh Rev{}       = True
dynSh Cat{}       = True
dynSh Π{}         = True

(||>) :: (Traversable t, Alternative f) => (a -> f b) -> t a -> f b
f ||> xs = asum (f <$> xs)

desugar :: a
desugar = error "Internal error. Should have been desugared."
