{-# LANGUAGE DeriveGeneric #-}

module Sh (I (..), Sh (..), PT (..), CT (..), ppt, LC (..), fr, (+:)) where

import           Control.DeepSeq           (NFData)
import           Control.Monad.Trans.State (State, evalState, get, modify, put)
import           Data.Functor              (($>))
import qualified Data.IntMap               as IM
import qualified Data.Set                  as S
import qualified Data.Text                 as T
import           Data.Tuple.Extra          (third3)
import           GHC.Generics              (Generic)
import           Nm
import           Prettyprinter             (Pretty (pretty), group, parens, (<+>))
import           Prettyprinter.Ext
import           U

data CT = CT !Char !Char !Char !Int
data LC = LC (CT->T.Text) (CT->CT)

class PT a where
    pp :: a -> State (S.Set T.Text, IM.IntMap T.Text, CT) a

ppt :: PT a => a -> a
ppt = flip evalState (S.empty, IM.empty, CT 'a' 'i' 'm' 0).pp

fr :: LC -> Nm a -> State (S.Set T.Text, IM.IntMap T.Text, CT) (Nm a)
fr s (Nm t (U i) x) = do
    (ms,u,c) <- get
    case IM.lookup i u of
        Just n                 -> pure (Nm n (U i) x)
        _ | t `S.notMember` ms -> put (S.insert t ms, IM.insert i t u, c) $> Nm t (U i) x
        _                      -> do {t' <- next s; modify (bimap12 (S.insert t') (IM.insert i t')) $> Nm t' (U i) x}

next l@(LC g s) = do
    (ms,_,c) <- get
    let t=g c in if t `S.notMember` ms
                      then pure t
                      else modify (third3 s) *> next l

instance Pretty (I a) where pretty=ps 0.ppt

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

il = LC (\(CT _ y _ _) -> T.singleton y) (\(CT v i e s) -> CT v (succ i) e s)
el = LC (\(CT _ _ z _) -> T.singleton z) (\(CT v i e s) -> CT v i (succ e) s)

instance PT (I a) where
    pp i@Ix{}          = pure i
    pp (IVar x n)      = IVar x<$>fr il n
    pp (IEVar x n)     = IEVar x<$>fr el n
    pp (StaPlus x i j) = StaPlus x<$>pp i<*>pp j
    pp (StaMul x i j)  = StaMul x<$>pp i<*>pp j

instance PS (I a) where
    ps _ (Ix _ i)                   = pretty i
    ps _ (IVar _ n)                 = pretty n
    ps _ ip                         | Just (i,d) <- pv ip = maybe mempty pretty i <> d
    ps d (StaPlus _ i j)            = parensp (d>5) (ps 6 i <+> "+" <+> ps 6 j)
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

sl=LC (\(CT _ _ _ k) -> "sh"<>T.pack (show k)) (\(CT v i e s) -> CT v i e (succ s))

instance PT (Sh a) where
    pp Nil = pure Nil; pp (SVar n) = SVar<$>fr sl n
    pp (Cons i sh) = Cons<$>pp i<*>pp sh; pp (Cat s0 s1) = Cat<$>pp s0<*>pp s1
    pp (Rev s) = Rev<$>pp s; pp (Π s) = Π<$>pp s

instance PS (Sh a) where
    ps _ (SVar n)    = pretty n
    ps _ sh@Cons{}   | Just is <- unroll sh = case is of {[i] -> pretty i; _ -> tupledBy " × " (pretty <$> is)}
    ps d (Cons i sh) = pg (d>6) (pretty i <+> "`Cons`" <+> pretty sh)
    ps _ Nil         = "Nil"
    ps d (Cat s s')  = group (parensp (d>5) (ps 6 s <+> "⧺" <+> ps 6 s'))
    ps d (Rev s)     = parensp (d>appPrec) ("rev" <+> ps (appPrec+1) s)
    ps d (Π s)       = group (parensp (d>appPrec) ("Π" <+> ps (appPrec+1) s))

instance Pretty (Sh a) where pretty=ps 0.ppt

instance Show (I a) where show=show.pretty
instance Show (Sh a) where show=show.pretty

instance NFData a => NFData (I a) where
instance NFData a => NFData (Sh a) where

bimap12 f g ~(x,y,z) = (f x,g y,z)
