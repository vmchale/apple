{-# LANGUAGE DeriveGeneric #-}

module Sh (I (..), Sh (..), (+:)) where

import           Control.DeepSeq   (NFData)
import           GHC.Generics      (Generic)
import           Nm
import           Prettyprinter     (Pretty (pretty), group, parens, (<+>))
import           Prettyprinter.Ext

instance Pretty (I a) where pretty=ps 0

pg True=group.parens; pg False=id

pv i@Ix{}         = Just (Just i, mempty)
pv i@IVar{}       = Just (Nothing, pretty i)
pv (StaMul _ i j) = do
    (i',vs₀) <- pv i
    (j',vs₁) <- pv j
    case (i',j') of
        (Nothing, Nothing) -> Just (Nothing, vs₀<>vs₁)
        (_, Nothing)       -> Just (i',vs₀<>vs₁)
        (Nothing,_)        -> Just (j',vs₀<>vs₁)
        (Just{}, Just{})   -> Nothing
pv _              = Nothing

instance PS (I a) where
    ps _ (Ix _ i)                   = pretty i
    ps _ (IVar _ n)                 = pretty n
    ps _ ip                         | Just (i,pp) <- pv ip = maybe mempty pretty i <> pp
    ps d (StaPlus _ i j)            = parensp (d>5) (ps 6 i <> "+" <> ps 6 j)
    ps d (StaMul _ i j)             = parensp (d>7) (ps 8 i <> "*" <> ps 8 j)
    ps _ (IEVar _ n)                = "#" <> pretty n

data I a = Ix { ia :: a, ii :: !Int }
         | IVar { ia :: a, ixn :: Nm a }
         | IEVar { ia :: a , ie :: Nm a } -- existential
         | StaPlus { ia :: a, ix0, ix1 :: I a }
         | StaMul { ia :: a, ix0, ix1 :: I a }
         deriving (Functor, Generic)

infixl 6 +:

i0+:i1 = StaPlus (ia i0) i0 i1

infixr 8 `Cons`

data Sh a = Nil
          | SVar (Nm a)
          | Cons (I a) (Sh a)
          | Rev (Sh a)
          | Cat (Sh a) (Sh a)
          | Π (Sh a)
          deriving (Functor, Generic)

unroll Nil         = Just []
unroll (Cons i sh) = (i:)<$>unroll sh
unroll _           = Nothing

instance PS (Sh a) where
    ps _ (SVar n)    = pretty n
    ps _ sh@Cons{}   | Just is <- unroll sh = case is of {[i] -> pretty i; _ -> tupledBy " × " (pretty <$> is)}
    ps d (Cons i sh) = pg (d>6) (pretty i <+> "`Cons`" <+> pretty sh)
    ps _ Nil         = "Nil"
    ps d (Cat s s')  = group (parensp (d>5) (ps 6 s <+> "⧺" <+> ps 6 s'))
    ps d (Rev s)     = parensp (d>appPrec) ("rev" <+> ps (appPrec+1) s)
    ps d (Π s)       = group (parensp (d>appPrec) ("Π" <+> ps (appPrec+1) s))

instance Pretty (Sh a) where pretty=ps 0

instance Show (I a) where show=show.pretty
instance Show (Sh a) where show=show.pretty

instance NFData a => NFData (I a) where
instance NFData a => NFData (Sh a) where
