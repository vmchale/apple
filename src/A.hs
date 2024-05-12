{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- | AST
module A ( T (..)
         , (~>)
         , I (..)
         , Sh (..)
         , C (..)
         , E (..)
         , Idiom (..)
         , Builtin (..)
         , ResVar (..)
         , prettyTyped
         , prettyC
         , rLi
         ) where

import           Control.DeepSeq   (NFData)
import qualified Data.IntMap       as IM
import           GHC.Generics      (Generic)
import           Nm
import           Prettyprinter     (Doc, Pretty (..), braces, brackets, colon, comma, encloseSep, flatAlt, group, hsep, lbrace, lbracket, parens, pipe, punctuate, rbrace, rbracket,
                                    tupled, (<+>))
import           Prettyprinter.Ext

instance Pretty (I a) where pretty=ps 0

instance PS (I a) where
    ps _ (Ix _ i)        = pretty i
    ps _ (IVar _ n)      = pretty n
    ps d (StaPlus _ i j) = parensp (d>5) (ps 6 i <+> "+" <+> ps 6 j)
    ps d (StaMul _ i j)  = parensp (d>7) (ps 8 i <+> "*" <+> ps 8 j)
    ps _ (IEVar _ n)     = "#" <> pretty n

data I a = Ix { ia :: a, ii :: !Int }
         | IVar { ia :: a, ixn :: Nm a }
         | IEVar { ia :: a , ie :: Nm a } -- existential
         | StaPlus { ia :: a, ix0 :: I a, ix1 :: I a }
         | StaMul { ia :: a, ix0 :: I a, ix1 :: I a }
         deriving (Functor, Generic)

instance Show (I a) where
    show = show . pretty

data C = IsNum | IsOrd -- implies eq
       | HasBits deriving (Generic, Eq, Ord)

instance NFData C where

instance Pretty C where pretty IsNum = "IsNum"; pretty IsOrd = "IsOrd"; pretty HasBits = "HasBits"

instance Show C where show=show.pretty

tupledArr = group . encloseSep (flatAlt "⟨ " "⟨") (flatAlt " ⟩" "⟩") ", "

data Sh a = Nil
          | SVar (Nm a)
          | Cons (I a) (Sh a)
          | Rev (Sh a)
          | Cat (Sh a) (Sh a)
          | Π (Sh a)
          deriving (Functor, Generic)

infixr 8 `Cons`

instance Show (Sh a) where show=show.pretty

instance Pretty (Sh a) where pretty=ps 0

instance PS (Sh a) where
    ps _ (SVar n)    = pretty n
    ps d (Cons i sh) = parensp (d>6) (pretty i <+> "`Cons`" <+> pretty sh)
    ps _ Nil         = "Nil"
    ps d (Cat s s')  = parensp (d>5) (ps 6 s <+> "⧺" <+> ps 6 s')
    ps d (Rev s)     = parensp (d>appPrec) ("rev" <+> ps (appPrec+1) s)
    ps d (Π s)       = parensp (d>appPrec) ("Π" <+> ps (appPrec+1) s)

appPrec=10

infixr 0 ~>
(~>) = Arrow

data T a = Arr (Sh a) (T a)
         | F -- | double
         | I -- | int
         | B -- | bool
         | Li (I a)
         | TVar (Nm a) -- | Kind \(*\), 'F' or 'I'
         | Arrow (T a) (T a)
         | P [T a]
         | Ρ (TyNm a) (IM.IntMap (T a))
         deriving (Functor, Generic)

instance Show (T a) where show=show.pretty

instance Pretty (T a) where pretty=ps 0

instance PS (T a) where
    ps d (Arr (i `Cons` Nil) t) = parensp (d>appPrec) ("Vec" <+> ps (appPrec+1) i <+> pretty t)
    ps d (Arr i t)              = parensp (d>appPrec) ("Arr" <+> ps (appPrec+1) i <+> pretty t)
    ps _ F                      = "float"
    ps _ I                      = "int"
    ps _ (Li i)                 = "int" <> parens (pretty i)
    ps _ B                      = "bool"
    ps _ (TVar n)               = pretty n
    ps d (Arrow t0 t1)          = parensp (d>0) (ps 1 t0 <+> "→" <+> ps 0 t1)
    ps _ (P ts)                 = tupledBy " * " (pretty <$> ts)
    ps _ (Ρ n fs)               = braces (pretty n <+> pipe <+> prettyFields (IM.toList fs))

rLi :: T a -> T a
rLi Li{}          = I
rLi (Arrow t0 t1) = Arrow (rLi t0) (rLi t1)
rLi (Arr sh t)    = Arr sh (rLi t)
rLi (Ρ n ts)      = Ρ n (rLi <$> ts)
rLi (P ts)        = P (rLi <$> ts)
rLi t             = t

prettyFields :: [(Int, T a)] -> Doc ann
prettyFields = mconcat . punctuate "," . fmap g where g (i, t) = pretty i <> ":" <+> pretty t

prettyRank :: (Int, Maybe [Int]) -> Doc ann
prettyRank (i, Nothing) = pretty i
prettyRank (i, Just as) = pretty i <+> "∘" <+> encloseSep lbracket rbracket comma (pretty<$>as)

instance Pretty Builtin where
    pretty Plus      = "+"
    pretty Fold      = "/"
    pretty FoldS     = "/ₒ"
    pretty FoldA     = "/*"
    pretty Times     = "*"
    pretty FRange    = "frange"
    pretty IRange    = "⍳"
    pretty Floor     = "⌊"
    pretty Minus     = "-"
    pretty Max       = "⋉"
    pretty Min       = "⋊"
    pretty Map       = "'"
    pretty Zip       = "`"
    pretty Div       = "%"
    pretty IntExp    = "^"
    pretty Exp       = "**"
    pretty ItoF      = "ℝ"
    pretty Neg       = "_"
    pretty Sqrt      = "√"
    pretty Log       = "_."
    pretty Re        = "re:"
    pretty Size      = ":"
    pretty (Rank as) = "`" <> encloseSep lbrace rbrace comma (prettyRank<$>as)
    pretty IDiv      = "/."
    pretty Scan      = "Λ"
    pretty ScanS     = "Λₒ"
    pretty (DI i)    = "\\`" <> pretty i
    pretty (Conv ns) = "⨳" <+> encloseSep lbrace rbrace comma (pretty<$>ns)
    pretty (TAt i)   = parens ("->" <> pretty i)
    pretty Gen       = "gen."
    pretty Last      = "}."
    pretty LastM     = "}.?"
    pretty Head      = "{."
    pretty HeadM     = "{.?"
    pretty Tail      = "{:"
    pretty Init      = "}:"
    pretty ConsE     = "⊲"
    pretty Snoc      = "⊳"
    pretty Mul       = "%."
    pretty VMul      = "%:"
    pretty Iter      = "^:"
    pretty Succ      = "\\~"
    pretty T         = "|:"
    pretty Fib       = "𝓕"
    pretty Dim       = "𝓉"
    pretty Sin       = "sin."
    pretty Cos       = "cos."
    pretty Tan       = "tan."
    pretty Gte       = "≥"
    pretty Gt        = ">"
    pretty Lt        = "<"
    pretty Eq        = "="
    pretty Neq       = "≠"
    pretty Lte       = "≤"
    pretty CatE      = "⧺"
    pretty R         = "𝔯"
    pretty Rot       = "⊖"
    pretty Cyc       = "⊙"
    pretty A1        = "˙"
    pretty Even      = "even."
    pretty Odd       = "odd."
    pretty Mod       = "mod"
    pretty IOf       = "@."
    pretty Abs       = "abs."
    pretty Di        = "di."
    pretty RevE      = "~"
    pretty Flat      = "♭"

data Builtin = Plus | Minus | Times | Div | IntExp | Exp | Log
             | Eq | Neq | Gt | Lt | Gte | Lte | CatE | IDiv | Mod
             | Max | Min | Neg | Sqrt | T | Di | Flat
             | IRange | FRange
             | Map | FoldA | Zip
             | Rank [(Int, Maybe [Int])]
             | Fold | FoldS | Foldl | Floor | ItoF | Iter
             | Scan | ScanS | Size | Dim | Re | Gen | Fib | Succ
             | DI !Int -- dyadic infix
             | Conv [Int] | TAt !Int | Last | LastM | ConsE | Snoc
             | Mul | VMul | Outer | R | Head | HeadM | Tail | Init | RevE
             | Sin | Cos | Rot | Tan | Cyc | A1 | Even | Odd | IOf | Abs
             deriving (Generic)
             -- TODO: window (feuilleter, stagger, ...) functions, reshape...?

ptName :: Nm (T a) -> Doc ann
ptName n@(Nm _ _ t) = parens (pretty n <+> ":" <+> pretty t)

prettyC :: (T (), [(Nm a, C)]) -> Doc ann
prettyC (t, []) = pretty t
prettyC (t, cs) = tupled (pc<$>cs) <+> ":=>" <+> pretty t
    where pc (n, c) = pretty c <+> pretty n

-- TODO: constraints
prettyTyped :: E (T a) -> Doc ann
prettyTyped (Var t n)                                             = parens (pretty n <+> ":" <+> pretty t)
prettyTyped (Builtin t b)                                         = parens (pretty b <+> ":" <+> pretty t)
prettyTyped (ILit t n)                                            = parens (pretty n <+> ":" <+> pretty t)
prettyTyped (FLit t x)                                            = parens (pretty x <+> ":" <+> pretty t)
prettyTyped (BLit t True)                                         = parens ("#t" <+> colon <+> pretty t)
prettyTyped (BLit t False)                                        = parens ("#f" <+> colon <+> pretty t)
prettyTyped (Cond t p e0 e1)                                      = parens ("?" <+> prettyTyped p <+> ",." <+> prettyTyped e0 <+> prettyTyped e1) <+> colon <+> pretty t
prettyTyped (Lam _ n@(Nm _ _ xt) e)                               = parens ("λ" <> parens (pretty n <+> ":" <+> pretty xt) <> "." <+> prettyTyped e)
prettyTyped (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) e0) e1) e2) = parens (prettyTyped e0 <> "/" <+> prettyTyped e1 <+> prettyTyped e2)
prettyTyped (EApp _ (EApp _ (EApp _ (Builtin _ FoldA) e0) e1) e2) = parens (prettyTyped e0 <> "/*" <+> prettyTyped e1 <+> prettyTyped e2)
prettyTyped (EApp _ (EApp _ (EApp _ (Builtin _ Foldl) e0) e1) e2) = parens (prettyTyped e0 <> "/l" <+> prettyTyped e1 <+> prettyTyped e2)
prettyTyped (EApp t (EApp _ (EApp _ (Builtin _ Outer) e0) e1) e2) = parens (prettyTyped e1 <+> pretty e0 <> "⊗" <+> prettyTyped e2 <+> ":" <+> pretty t)
prettyTyped (EApp _ e0@(Builtin _ op) e1) | isBinOp op            = parens (prettyTyped e1 <+> prettyTyped e0)
prettyTyped (EApp _ e0 e1)                                        = parens (prettyTyped e0 <+> prettyTyped e1)
prettyTyped (Let t (n, e) e')                                     = parens (braces (ptName n <+> "←" <+> prettyTyped e <> ";" <+> prettyTyped e') <+> pretty t)
prettyTyped (LLet t (n, e) e')                                    = parens (braces (ptName n <+> "⟜" <+> prettyTyped e <> ";" <+> prettyTyped e') <+> pretty t)
prettyTyped (Def t (n, e) e')                                     = parens (braces (ptName n <+> "⇐" <+> prettyTyped e <> ";" <+> prettyTyped e') <+> pretty t)
prettyTyped (Tup _ es)                                            = tupled (prettyTyped <$> es)
prettyTyped e@(ALit t _)                                          = parens (pretty e <+> ":" <+> pretty t)

mPrec :: Builtin -> Maybe Int
mPrec Plus   = Just 6
mPrec Minus  = Just 6
mPrec Times  = Just 7
mPrec Div    = Just 7
mPrec IDiv   = Just 7
mPrec Exp    = Just 8
mPrec IntExp = Just 8
mPrec Mod    = Just 7
mPrec Succ   = Just 9
mPrec Fold   = Just 9
mPrec Map    = Just 5
mPrec ConsE  = Just 4
mPrec Snoc   = Just 4
mPrec CatE   = Just 5
mPrec _      = Nothing

isBinOp :: Builtin -> Bool
isBinOp Plus   = True
isBinOp Minus  = True
isBinOp Times  = True
isBinOp Div    = True
isBinOp IDiv   = True
isBinOp Exp    = True
isBinOp IntExp = True
isBinOp DI{}   = True
isBinOp Conv{} = True
isBinOp Mul    = True
isBinOp VMul   = True
isBinOp Rot    = True
isBinOp ConsE  = True
isBinOp Snoc   = True
isBinOp Scan   = True
isBinOp Fold   = True
isBinOp Map    = True
isBinOp Cyc    = True
isBinOp A1     = True
isBinOp Mod    = True
isBinOp IOf    = True
isBinOp _      = False

instance Pretty (E a) where pretty=ps 0

instance PS (E a) where
    ps d (Lam _ n e)                                              = parensp (d>1) ("λ" <> pretty n <> "." <+> ps 2 e)
    ps _ (Var _ n)                                                = pretty n
    ps _ (Builtin _ op) | isBinOp op                              = parens (pretty op)
    ps _ (Builtin _ b)                                            = pretty b
    ps d (EApp _ (Builtin _ (TAt i)) e)                           = parensp (d>9) (ps 10 e <> "->" <> pretty i)
    ps _ (EApp _ (Builtin _ op) e0) | isBinOp op                  = parens (pretty e0 <+> pretty op)
    ps d (EApp _ (EApp _ (Builtin _ op) e0) e1) | Just d' <- mPrec op = parensp (d>d') (ps (d'+1) e0 <+> pretty op <+> ps (d'+1) e1)
    ps _ (EApp _ (EApp _ (Builtin _ op) e0) e1) | isBinOp op      = parens (pretty e0 <+> pretty op <+> pretty e1)
    ps _ (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) e0) e1) e2)    = parens (pretty e0 <> "/" <+> pretty e1 <+> pretty e2)
    ps _ (EApp _ (EApp _ (EApp _ (Builtin _ Foldl) e0) e1) e2)    = parens (pretty e0 <> "/l" <+> pretty e1 <+> pretty e2)
    ps _ (EApp _ (EApp _ (EApp _ (Builtin _ FoldA) e0) e1) e2)    = parens (pretty e0 <> "/*" <+> pretty e1 <+> pretty e2)
    ps _ (EApp _ (EApp _ (EApp _ (Builtin _ ScanS) e0) e1) e2)    = parens (pretty e0 <+> "Λₒ" <+> pretty e1 <+> pretty e2)
    ps _ (EApp _ (EApp _ (EApp _ (Builtin _ Zip) e0) e1) e2)      = parens (pretty e0 <+> "`" <+> pretty e1 <+> pretty e2)
    ps _ (EApp _ (EApp _ (EApp _ (Builtin _ Outer) e0) e1) e2)    = parens (pretty e1 <+> pretty e0 <+> "⊗" <+> pretty e1 <+> pretty e2)
    ps _ (EApp _ (EApp _ (Builtin _ op@Rank{}) e0) e1)            = parens (pretty e0 <+> pretty op <+> pretty e1)
    ps _ (EApp _ (EApp _ (Builtin _ op@Conv{}) e0) e1)            = parens (pretty e0 <+> pretty op <+> pretty e1)
    ps _ (EApp _ (EApp _ (Builtin _ (DI i)) e0) e1)               = parens (pretty e0 <+> "\\`" <> pretty i <+> pretty e1)
    ps _ (EApp _ (EApp _ (Builtin _ Succ) e0) e1)                 = parens (pretty e0 <+> "\\~" <+> pretty e1)
    ps d (EApp _ e0 e1)                                           = parensp (d>10) (ps 10 e0 <+> ps 11 e1)
    ps _ (FLit _ x)                                               = pretty x
    ps _ (ILit _ n)                                               = pretty n
    ps _ (BLit _ True)                                            = "#t"
    ps _ (BLit _ False)                                           = "#f"
    ps _ (Dfn _ e)                                                = brackets (pretty e)
    ps _ (ResVar _ x)                                             = pretty x
    ps _ (Parens _ e)                                             = parens (pretty e)
    ps _ (Let _ (n, e) e')                                        = braces (pretty n <+> "←" <+> pretty e <> ";" <+> pretty e')
    ps _ (Def _ (n, e) e')                                        = braces (pretty n <+> "⇐" <+> pretty e <> ";" <+> pretty e')
    ps _ (LLet _ (n, e) e')                                       = braces (pretty n <+> "⟜" <+> pretty e <> ";" <+> pretty e')
    ps _ (Id _ idm)                                               = pretty idm
    ps _ (Tup _ es)                                               = tupled (pretty <$> es)
    ps _ (ALit _ es)                                              = tupledArr (pretty <$> es)
    ps d (Ann _ e t)                                              = parensp (d>1) (ps 2 e <+> "::" <+> ps 1 t)
    ps d (Cond _ p e₀ e₁)                                         = "?" <> pretty p <> ",." <+> ps d e₀ <+> ",." <+> ps d e₁

instance Show (E a) where show=show.pretty

data ResVar = X | Y deriving (Generic)

instance Pretty ResVar where
    pretty X = "x"; pretty Y = "y"

data Idiom = FoldSOfZip { seedI :: E (T ()), opI :: E (T ()), esI :: [E (T ())] }
           | FoldOfZip { zopI :: E (T ()), opI :: E (T ()), esI :: [E (T ())] }
           | AShLit { litSh :: [Int], esLit :: [E (T ())] }
           deriving (Generic)

instance Pretty Idiom where
    pretty (FoldSOfZip seed op es) = parens ("foldS-of-zip" <+> pretty seed <+> pretty op <+> pretty es)
    pretty (FoldOfZip zop op es)   = parens ("fold-of-zip" <+> pretty zop <+> pretty op <+> pretty es)
    pretty (AShLit re es)          = parens ("re" <+> hsep (pretty <$> re) <+> "|" <+> pretty es)

data E a = ALit { eAnn :: a, arrLit :: [E a] } -- TODO: include shape?
         -- TODO: bool array
         | Var { eAnn :: a, eVar :: Nm a }
         | Builtin { eAnn :: a, eBuiltin :: !Builtin }
         | EApp { eAnn :: a, eF :: E a, eArg :: E a }
         | Lam { eAnn :: a, eVar :: Nm a, eIn :: E a }
         | ILit { eAnn :: a, eILit :: !Integer }
         | FLit { eAnn :: a, eFLit :: !Double }
         | BLit { eAnn :: a, eBLit :: !Bool }
         | Cond { eAnn :: a, prop :: E a, ifBranch :: E a, elseBranch :: E a }
         | Let { eAnn :: a, eBnd :: (Nm a, E a), eIn :: E a }
         | Def { eAnn :: a, eBnd :: (Nm a, E a), eIn :: E a }
         | LLet { eAnn :: a, eBnd :: (Nm a, E a), eIn :: E a }
         | Dfn { eAnn :: a, eIn :: E a }
         | ResVar { eAnn :: a, eXY :: ResVar }
         | Parens { eAnn :: a, eExp :: E a }
         | Ann { eAnn :: a, eEe :: E a, eTy :: T () }
         | Tup { eAnn :: a, eEs :: [E a] }
         | Id { eAnn :: a, eIdiom :: Idiom }
         deriving (Functor, Generic)

instance NFData Builtin where
instance NFData ResVar where
instance NFData Idiom where
instance NFData a => NFData (E a) where
instance NFData a => NFData (I a) where
instance NFData a => NFData (Sh a) where
instance NFData a => NFData (T a) where
