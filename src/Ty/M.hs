{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Ty.M ( check, RE ) where

import           A
import           Control.Applicative (Alternative (..), asum)
import           Control.DeepSeq     (NFData)
import           GHC.Generics        (Generic)
import           Prettyprinter       (Pretty (..), squotes, (<+>))

data RE = MR (E (T ())) (T ()) | Unflat (E (T ())) (T ()) deriving (Generic)

instance NFData RE where

instance Pretty RE where
    pretty (MR e t)     = "Type" <+> squotes (pretty t) <+> "of expression" <+> squotes (pretty e) <+> "is not sufficiently monomorphic."
    pretty (Unflat e t) = "Error in expression" <+> squotes (pretty e) <+> "of type" <+> squotes (pretty t) <> ": arrays of functions are not supported."

check = cM

cM :: E (T ()) -> Maybe RE
cM e | Just t <- mrT (eAnn e) = Just (MR e t)
cM e | Just t <- flT (eAnn e) = Just (Unflat e t)
cM (Let _ (_, e) e') = cM e <|> cM e'
cM (LLet _ (_, e) e') = cM e <|> cM e'
cM (Def _ (_, e) e') = cM e <|> cM e'
cM (EApp _ e e') = cM e <|> cM e'
cM (ALit _ es) = foldMapAlternative cM es
cM (Lam _ _ e) = cM e
cM (Cond _ p e e') = cM p <|> cM e <|> cM e'
cM (Tup _ es) = foldMapAlternative cM es
cM Builtin{} = Nothing
cM ILit{} = Nothing
cM FLit{} = Nothing
cM BLit{} = Nothing
cM Var{} = Nothing

mrT :: T a -> Maybe (T a)
mrT t@TVar{}     = Just t
mrT (Arr _ t)    = mrT t
mrT (Arrow t t') = mrT t <|> mrT t'
mrT (P ts)       = foldMapAlternative mrT ts
mrT t@Ρ{}        = Just t
mrT _            = Nothing

flT :: T a -> Maybe (T a)
flT t@(Arr _ tϵ) | ha tϵ = Just t
flT (Arrow t t') = flT t <|> flT t'
flT (P ts) = foldMapAlternative flT ts
flT (Ρ _ ls) = foldMapAlternative flT ls
flT _ = Nothing

ha :: T a -> Bool
ha Arrow{}   = True
ha (P ts)    = any ha ts
ha (Ρ _ ls)  = any ha ls
ha (Arr _ t) = ha t
ha _         = False

foldMapAlternative :: (Traversable t, Alternative f) => (a -> f b) -> t a -> f b
foldMapAlternative f xs = asum (f <$> xs)
