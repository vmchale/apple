{-# LANGUAGE DeriveGeneric #-}

module A ( T (..)
         , (~>)
         , C (..)
         , E (..)
         , Idiom (..)
         , Builtin (..)
         , ResVar (..)
         , prettyTyped
         , fi, isBinOp
         , rLi
         ) where

import           Control.DeepSeq   (NFData (rnf))
import           Data.Bifunctor    (first)
import           Data.Foldable     (toList)
import qualified Data.IntMap       as IM
import qualified Data.Set          as S
import qualified Data.Text         as T
import           GHC.Generics      (Generic)
import           Nm
import           Prettyprinter     (Doc, Pretty (..), align, braces, brackets, colon, comma, concatWith, encloseSep, fillSep, flatAlt, group, hardline, hsep, lbrace, lbracket,
                                    line, parens, pipe, punctuate, rbrace, rbracket, tupled, vsep, (<+>))
import           Prettyprinter.Ext
import           Sh

data C = IsOrd | IsEq | HasBits | IsZ deriving (Eq, Ord)

instance NFData C where rnf x=seq x ()

instance Pretty C where pretty IsOrd = "IsOrd"; pretty IsEq = "IsEq"; pretty HasBits = "HasBits"; pretty IsZ = "IsNum"

instance Show C where show=show.pretty

tupledArr = group.align.encloseSep (flatAlt "⟨ " "⟨") (flatAlt " ⟩" "⟩") ", "

infixr 0 ~>
(~>) = Arrow

data T a = Arr (Sh a) (T a)
         | F -- | double
         | I -- | int
         | B -- | bool
         | Li (I a)
         | TV !(Nm a) (S.Set C)
         | FV !(Nm a) (S.Set C)
         | IZ (I a) !(Nm a)
         | Arrow (T a) (T a)
         | P [T a]
         | Ρ !(Nm a) (IM.IntMap (T a))
         deriving (Functor, Generic)

instance Show (T a) where show=show.pretty

tl = LC (\(CT x _ _ _) -> T.singleton x) (\(CT v i e s) -> CT (succ v) i e s)

instance PT (T a) where
    pp F             = pure F
    pp I             = pure I
    pp B             = pure B
    pp t@Li{}        = pure t
    pp (TV n c)      = TV<$>fr tl n<*>pure c
    pp (FV n c)      = FV<$>fr tl n<*>pure c
    pp (IZ i n)      = IZ i<$>fr tl n
    pp (Arrow t₀ t₁) = Arrow<$>pp t₀<*>pp t₁
    pp (Arr sh t)    = Arr <$>pp sh<*>pp t
    pp (P ts)        = P<$>traverse pp ts
    pp (Ρ n ts)      = Ρ<$>fr tl n<*>traverse pp ts

instance Pretty (T a) where pretty=ps 0.ppt

instance PS (T a) where
    ps d (Arr (i `Cons` Nil) t) = group (parensp (d>appPrec) ("Vec" <+> ps (appPrec+1) i <+> ps (appPrec+1) t))
    ps d (Arr i t)              = group (parensp (d>appPrec) ("Arr" <+> ps (appPrec+1) i <+> ps (appPrec+1) t))
    ps _ F                      = "float"
    ps _ I                      = "int"
    ps _ (Li i)                 = "int" <> parens (pretty i)
    ps _ (IZ i _)               = "num" <> parens (pretty i)
    ps _ B                      = "bool"
    ps _ (TV n c) | S.null c    = pretty n
                  | otherwise   = braces(pretty n<>"|"<>concatWith (\x y -> x<>","<>y) (pretty<$>S.toList c))
    ps d (FV n c)               = "♯" <> ps d (TV n c)
    ps d (Arrow t0 t1)          = parensp (d>0) (ps 1 t0 <+> "→" <+> ps 0 t1)
    ps _ (P ts)                 = tupledBy " * " (pretty <$> ts)
    ps _ (Ρ n fs)               = braces (pretty n <+> pipe <+> prettyFields (IM.toList fs))

rLi :: T a -> T a
rLi Li{}          = I
rLi (IZ _ n)      = TV n (S.singleton IsZ)
rLi (Arrow t0 t1) = Arrow (rLi t0) (rLi t1)
rLi (Arr sh t)    = Arr sh (rLi t)
rLi (Ρ n ts)      = Ρ n (rLi <$> ts)
rLi (P ts)        = P (rLi <$> ts)
rLi t             = t

prettyFields :: [(Int, T a)] -> Doc ann
prettyFields = mconcat . punctuate "," . fmap g where g (i, t) = pretty i <> ":" <+> pretty t

prettyRank :: (Int, Maybe [Int]) -> Doc ann
prettyRank (i, Nothing) = pretty i
prettyRank (i, Just as) = pretty i <> "∘" <> encloseSep lbracket rbracket comma (pretty<$>as)

pStride :: (Int, Maybe Int) -> Doc ann
pStride (i, Nothing) = pretty i
pStride (i, Just d)  = pretty i <> "∘" <> pretty d

nOp = encloseSep lbrace rbrace comma

instance Pretty Builtin where
    pretty Plus       = "+"
    pretty Fold       = "/"
    pretty FoldS      = "/ₒ"
    pretty Foldl      = "/l"
    pretty FoldA      = "/*"
    pretty Times      = "*"
    pretty FRange     = "𝒻"
    pretty Io         = "⍳"
    pretty Range      = ".."
    pretty Floor      = "⌊"
    pretty Ceil       = "⌈"
    pretty Minus      = "-"
    pretty Max        = "⋉"
    pretty Min        = "⋊"
    pretty Map        = "'"
    pretty Zip        = "`"
    pretty Div        = "%"
    pretty IntExp     = "^"
    pretty Exp        = "**"
    pretty ItoF       = "ℝ"
    pretty Neg        = "_"
    pretty Sqrt       = "√"
    pretty Log        = "_."
    pretty Re         = "〃"
    pretty Size       = ":"
    pretty (Rank as)  = "`" <> nOp (prettyRank<$>as)
    pretty IDiv       = "/."
    pretty Scan       = "Λ"
    pretty ScanS      = "Λₒ"
    pretty (DI i)     = "\\`" <> pretty i
    pretty (Conv ns)  = "⨳" <+> nOp (pStride<$>ns)
    pretty (Focus ns) = "⦠" <> nOp (pretty<$>ns)
    pretty (TAt i)    = parens ("->" <> pretty i)
    pretty Gen        = "gen."
    pretty Last       = "}."
    pretty LastM      = "}.?"
    pretty Head       = "{."
    pretty HeadM      = "{.?"
    pretty Tail       = "{:"
    pretty TailM      = "{:?"
    pretty Init       = "}:"
    pretty InitM      = "}:?"
    pretty ConsE      = "⊲"
    pretty Snoc       = "⊳"
    pretty Mul        = "%."
    pretty VMul       = "%:"
    pretty Succ       = "\\~"
    pretty T          = "|:"
    pretty Fib        = "𝓕"
    pretty Dim        = "𝓉"
    pretty Sin        = "sin."
    pretty Cos        = "cos."
    pretty Tan        = "tan."
    pretty ASin       = "asin."
    pretty Gte        = "≥"
    pretty Gt         = ">"
    pretty Lt         = "<"
    pretty Eq         = "="
    pretty Neq        = "≠"
    pretty Lte        = "≤"
    pretty CatE       = "⧺"
    pretty R          = "𝔯"
    pretty Rot        = "⊖"
    pretty Cyc        = "⊙"
    pretty A1         = "˙"
    pretty I1         = "⊂"
    pretty Even       = "even."
    pretty Odd        = "odd."
    pretty Mod        = "|"
    pretty IOf        = "@."
    pretty Filt       = "§"
    pretty Part       = "℘"
    pretty Abs        = "abs."
    pretty Di         = "di."
    pretty RevE       = "~"
    pretty Flat       = "♭"
    pretty AddDim     = "♯"
    pretty Xor        = "⊻"
    pretty And        = "∧"
    pretty Or         = "∨"
    pretty N          = "¬"
    pretty Ices       = "⩪"
    pretty Sr         = ">>"
    pretty Sl         = "<<"
    pretty C          = "∴"
    pretty Dot        = "⋅"
    pretty Outer      = "⊗"
    pretty Take       = "take#"
    pretty Drop       = "drop#"
    pretty Del        = "\\-"
    pretty DelM       = "\\\\"
    pretty Ug         = "ug."
    pretty S'         = "⑂"
    pretty S          = "𝐒"
    pretty K          = "𝐊"
    pretty Ix'd       = "ᶥ"
    pretty Sort       = "⍋"
    pretty CS         = "cpysgn."
    pretty Refl       = "refl."

data Builtin = Plus | Minus | Times | Div | IntExp | Exp | Log
             | Eq | Neq | Gt | Lt | Gte | Lte | CatE | IDiv | Mod
             | Max | Min | Neg | Sqrt | T | Di
             | Flat | AddDim | Ices | Filt | Part
             | Io | Range | Ix'd | FRange
             | Map | FoldA | Zip
             | Rank [(Int, Maybe [Int])]
             | Fold | FoldS | Foldl
             | Floor | ItoF | Ceil
             | Scan | ScanS | Size | Dim | Re | Gen | Fib | Succ
             | DI !Int -- infix
             | Conv [(Int, Maybe Int)] | Focus [Int]
             | TAt !Int | Last | LastM | ConsE | Snoc
             | Mul | VMul | Outer | RevE | R
             | Head | HeadM | Tail | Init | TailM | InitM
             | Sin | Cos | Tan | ASin | Abs | Even | Odd
             | Rot | Cyc | A1 | I1 | IOf | Del | DelM
             | And | Or | Xor | N | Sr | Sl | C | Dot
             | Take | Drop | Ug | Sort
             | CS | S' | S | K | Refl
             deriving (Generic)
             -- TODO: (feuilleter, stagger, ...) reshape...?

(<::>) :: Doc ann -> T b -> Doc ann
x<::>y = parens (x <+> ":" <+> pretty y)

ptn :: Nm (T a) -> Doc ann
ptn n@(Nm _ _ t) = pretty n<::>t

gt :: E (T a) -> ([([Nm (T a)], T a)], E (T a))
gt (Lam (Arrow tC _) n e)   = first (([n], tC):) $ gt e
gt (LamΠ (Arrow tC _) ns e) = first ((ns, tC):) $ gt e
gt e                        = ([], e)

prettyTyped :: E (T a) -> Doc ann
prettyTyped = pt where
    pt (Var t n)                                              = pretty n<::>t
    pt (Builtin t b)                                          = pretty b<::>t
    pt (ILit t n)                                             = pretty n<::>t
    pt (FLit t x)                                             = pretty x<::>t
    pt (BLit t True)                                          = "#t"<::>t
    pt (BLit t False)                                         = "#f"<::>t
    pt (Cond t p e0 e1)                                       = parens ("?" <+> pt p <+> ",." <+> pt e0 <+> pt e1) <+> colon <+> pretty t
    pt e | (ns@(_:_),e') <- gt e                              = group (fl (\case ([n],_) -> ptn n; (n,tC) -> tupled (pretty<$>n) <::>tC) ns <!> pt e')
    pt (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) e0) e1) e2)  = parens (pt e0 <> "/ₒ" <+> pt e1 <+> pt e2)
    pt (EApp _ (EApp _ (EApp _ (Builtin _ FoldA) e0) e1) e2)  = parens (pt e0 <> "/*" <+> pt e1 <+> pt e2)
    pt (EApp _ (EApp _ (EApp _ (Builtin _ Foldl) e0) e1) e2)  = parens (pt e0 <> "/l" <+> pt e1 <+> pt e2)
    pt (EApp t (EApp _ (EApp _ (Builtin _ Outer) e0) e1) e2)  = parens (pt e1 <+> parens (pt e0) <> "⊗" <+> pt e2 <+> ":" <+> pretty t)
    pt (EApp _ (EApp _ (EApp _ (Builtin _ ScanS) e0) e1) e2)  = parens (pt e0 <> "Λₒ" <+> pt e1 <+> pt e2)
    pt (EApp _ e0@(Builtin _ op) e1) | isBinOp op             = parens (pt e1 <+> pt e0)
    pt e@EApp{} | es <- spine e                               = parens (group (align (fillSep (pt <$> toList es))))
    pt e@Let{}                                                = pBt e
    pt e@Def{}                                                = pBt e
    pt e@LLet{}                                               = pBt e
    pt (Tup _ es)                                             = tupled (pt <$> es)
    pt e@(ALit t _)                                           = pretty e<::>t

spine :: E a -> [E a]
spine x = spine' x []
  where
    spine' (EApp _ e0 e1) es = spine' e0 (e1:es)
    spine' e es              = e:es

fi :: Builtin -> Maybe Int
fi Plus = Just 6;   fi Minus = Just 6; fi Times = Just 7
fi Div = Just 7;    fi IDiv = Just 7;  fi Exp = Just 8
fi IntExp = Just 8; fi IOf = Just 8;   fi Mod = Just 7
fi Range = Just 10; fi Succ = Just 9;  fi Fold = Just 9
fi Del = Just 9;    fi DelM = Just 9;  fi C = Just 9
fi Ices = Just 6;   fi Filt = Just 6;  fi Part = Just 6
fi Dot = Just 8;    fi Mul = Just 7;   fi VMul = Just 7
fi Re = Just 6;     fi A1 = Just 9;    fi I1 = Just 8
fi Map = Just 5;    fi ConsE = Just 4; fi Snoc = Just 4
fi CatE = Just 5;   fi Sr = Just 8;    fi Sl = Just 8
fi Xor = Just 6;    fi And = Just 3;   fi Or = Just 2
fi Eq = Just 4;     fi Neq = Just 4;   fi Gt = Just 4
fi Lt = Just 4;     fi Gte = Just 4;   fi Lte = Just 4
fi Max = Just 6;    fi Min = Just 6;   fi Scan = Just 9;
fi Conv{} = Just 5; fi _ = Nothing

isBinOp :: Builtin -> Bool
isBinOp DI{} = True; isBinOp S'  = True
isBinOp IOf = True;  isBinOp Focus{} = True
isBinOp Rot = True;  isBinOp Cyc = True
isBinOp b | Just{} <- fi b = True
          | otherwise = False

data B = L | D | Λ

unbind :: E a -> ([(B, Nm a, E a)], E a)
unbind (Let _ (n,e) e')  = first ((L,n,e):) $ unbind e'
unbind (LLet _ (n,e) e') = first ((Λ,n,e):) $ unbind e'
unbind (Def _ (n,e) e')  = first ((D,n,e):) $ unbind e'
unbind e                 = ([], e)

pArr L="←"; pArr D="⟜"; pArr Λ="⟜"

pBs [] e            = pretty e
pBs ((b,n,e):bs) e' = ssep (pretty n <+> pArr b <+> pretty e) (pBs bs e')

pBts :: [(B, Nm (T a), E (T a))] -> E (T a) -> Doc ann
pBts [] e            = prettyTyped e
pBts ((b,n,e):bs) e' = ssep (ptn n <+> pArr b <+> prettyTyped e) (pBts bs e')

ssep x y = flatAlt (x <> hardline <> ";" <+> y) (x <> ";" <> y)

bc x = flatAlt ("{" <+> x <> line <> "}") ("{" <> x <> "}")

pB=group.align.bc.uncurry pBs.unbind
pBt=align.bc.uncurry pBts.unbind

data E a = ALit { eAnn :: a, arrLit :: [E a] }
         | Var { eAnn :: a, eVar :: Nm a }
         | Builtin { eAnn :: a, eBuiltin :: !Builtin }
         | EApp { eAnn :: a, eF, eArg :: E a }
         | Lam { eAnn :: a, eVar :: Nm a, eIn :: E a }
         | LamΠ { eAnn :: a, eVars :: [Nm a], eIn :: E a }
         | ILit { eAnn :: a, eILit :: !Integer }
         | FLit { eAnn :: a, eFLit :: !Double }
         | BLit { eAnn :: a, eBLit :: !Bool }
         | Cond { eAnn :: a, prop, ifBranch, elseBranch :: E a }
         | Tup { eAnn :: a, eEs :: [E a] }
         | Let { eAnn :: a, eBnd :: (Nm a, E a), eIn :: E a }
         | Def { eAnn :: a, eBnd :: (Nm a, E a), eIn :: E a }
         | LLet { eAnn :: a, eBnd :: (Nm a, E a), eIn :: E a }
         | Dfn { eAnn :: a, eIn :: E a }
         | ResVar { eAnn :: a, eXY :: !ResVar }
         | Parens { eAnn :: a, eExp :: E a }
         | Ann { eAnn :: a, eEe :: E a, eTy :: T a }
         | Id { eAnn :: a, eIdiom :: Idiom a }
         deriving (Functor, Generic)

instance Pretty (E a) where pretty=ps 0

gg :: E a -> ([[Nm a]], E a)
gg (Lam _ n e)   = first ([n]:) $ gg e
gg (LamΠ _ ns e) = first (ns:) $ gg e
gg e             = ([], e)

fl p = foldMap (\na -> "λ" <> p na <> ".")

instance PS (E a) where
    ps d e | (ns@(_:_),e') <- gg e                                = group (parensp (d>1) (fl (\case [n] -> pretty n; n -> tupled (pretty<$>n)) ns <!> ps 2 e'))
    ps _ (Var _ n)                                                = pretty n
    ps _ (Builtin _ op) | isBinOp op                              = parens (pretty op)
    ps _ (Builtin _ b)                                            = pretty b
    ps d (EApp _ (Builtin _ (TAt i)) e)                           = parensp (d>9) (ps 10 e <> "->" <> pretty i)
    ps _ (EApp _ (Builtin _ op) e0) | isBinOp op                  = parens (ps 10 e0 <> pretty op)
    ps d (EApp _ (EApp _ (Builtin _ op) e0) e1) | Just d' <- fi op = parensp (d>d') (ps (d'+1) e0 <> pretty op <> ps (d'+1) e1)
    ps _ (EApp _ (EApp _ (Builtin _ op) e0) e1) | isBinOp op      = parens (ps 10 e0 <> pretty op <> ps 10 e1)
    ps _ (EApp _ (EApp _ (EApp _ (Builtin _ FoldS) e0) e1) e2)    = parens (pretty e0 <> "/ₒ" <+> pretty e1 <+> pretty e2)
    ps _ (EApp _ (EApp _ (EApp _ (Builtin _ Foldl) e0) e1) e2)    = parens (pretty e0 <> "/l" <+> pretty e1 <+> pretty e2)
    ps _ (EApp _ (EApp _ (EApp _ (Builtin _ FoldA) e0) e1) e2)    = parens (pretty e0 <> "/*" <+> pretty e1 <+> pretty e2)
    ps _ (EApp _ (EApp _ (EApp _ (Builtin _ ScanS) e0) e1) e2)    = parens (pretty e0 <+> "Λₒ" <+> pretty e1 <+> pretty e2)
    ps _ (EApp _ (EApp _ (EApp _ (Builtin _ Zip) e0) e1) e2)      = parens (pretty e0 <> "`" <> pretty e1 <+> pretty e2)
    ps _ (EApp _ (EApp _ (EApp _ (Builtin _ Outer) e0) e1) e2)    = parens (pretty e1 <+> ps 10 e0 <> "⊗" <+> pretty e2)
    ps _ (EApp _ (Builtin _ Outer) e0)                            = parens (pretty e0 <> "⊗")
    ps _ (EApp _ (EApp _ (Builtin _ op@Rank{}) e0) e1)            = parens (ps 10 e0 <+> pretty op <+> ps 10 e1)
    ps _ (EApp _ (EApp _ (Builtin _ op@Conv{}) e0) e1)            = parens (pretty e0 <+> pretty op <+> pretty e1)
    ps _ (EApp _ (EApp _ (Builtin _ op@Focus{}) e0) e1)           = parens (pretty e0 <+> pretty op <> pretty e1)
    ps _ (EApp _ (EApp _ (Builtin _ (DI i)) e0) e1)               = parens (pretty e0 <+> "\\`" <> pretty i <+> pretty e1)
    ps _ (EApp _ (EApp _ (Builtin _ Succ) e0) e1)                 = parens (pretty e0 <+> "\\~" <+> pretty e1)
    ps _ (EApp _ (Builtin _ Ix'd) e)                              = ps 10 e <> "ᶥ"
    ps d (EApp _ e0@Builtin{} e1@Var{})                           = parensp (d>10) (ps 10 e0 <> ps 11 e1)
    ps d (EApp _ e0 e1)                                           = parensp (d>10) (ps 10 e0 <+> ps 11 e1)
    ps _ (FLit _ x)                                               = pretty x
    ps _ (ILit _ n)                                               = pretty n
    ps _ (BLit _ True)                                            = "#t"
    ps _ (BLit _ False)                                           = "#f"
    ps _ (Dfn _ e)                                                = brackets (pretty e)
    ps _ (ResVar _ x)                                             = pretty x
    ps _ (Parens _ e)                                             = parens (pretty e)
    ps _ e@Let{}                                                  = pB e
    ps _ e@Def{}                                                  = pB e
    ps _ e@LLet{}                                                 = pB e
    ps _ (Id _ idm)                                               = pretty idm
    ps _ (Tup _ es)                                               = tupled (pretty <$> es)
    ps _ (ALit _ es)                                              = tupledArr (pretty <$> es)
    ps d (Ann _ e t)                                              = parensp (d>0) (ps 1 e <+> "::" <+> ps 0 t)
    ps d (Cond _ p e₀ e₁)                                         = "?" <> pretty p <> ",." <+> ps d e₀ <+> ",." <+> ps d e₁

instance Show (E a) where show=show.pretty

data ResVar = X | Y

instance Pretty ResVar where
    pretty X = "x"; pretty Y = "y"

-- TODO: 𝓕 𝔸10 [{n⟜ 𝓉 x;n*}.x+(_1)^n}]
-- step access in course-of-value recursion?
data Idiom a = FoldSOfZip { seedI, opI :: E a, esI :: [E a] }
             | FoldOfZip { zopI, opI :: E a, esI :: [E a] }
             | FoldGen { seedG, ufG, fG, nG :: E a }
             | U2 { seedGs, ufs :: [E a], seedC, fG, nG :: E a }
             | AShLit { litSh :: [Int], esLit :: [E a] }
             | Aɴ { idArr :: E a, idIxes :: [E a] }
             | Iter { ugI, seedI, nG :: E a }
             | Slice { idArr :: E a, begin, fin :: E a }
             deriving (Generic, Functor)

instance Pretty (Idiom a) where
    pretty (FoldSOfZip seed op es) = parens ("foldS-of-zip" <+> vsep [pretty seed, parens (pretty op), pretty es])
    pretty (FoldOfZip zop op es)   = parens ("fold-of-zip" <+> vsep [pretty zop, parens (pretty op), pretty es])
    pretty (FoldGen seed g f n)    = parens ("fold-gen" <+> brackets (pretty seed) <+> parens (pretty g) <+> parens (pretty f) <+> parens (pretty n))
    pretty (U2 seed gs u f n)      = parens ("fold-2-ix" <+> pretty seed <+> pretty gs <+> parens (pretty u <> "," <+> pretty f) <+> parens (pretty n))
    pretty (AShLit re es)          = parens ("re" <+> hsep (pretty <$> re) <+> "|" <+> pretty es)
    pretty (Aɴ a iix)              = parens ("at" <+> pretty a <+> pretty iix)
    pretty (Iter g seed n)         = parens (pretty g <+> "^:" <> pretty n <+> pretty seed)

instance Show (Idiom a) where show=show.pretty

instance NFData Builtin where
instance NFData ResVar where rnf x=seq x ()
instance NFData a => NFData (Idiom a) where
instance NFData a => NFData (E a) where
instance NFData a => NFData (T a) where
