{
    {-# LANGUAGE DeriveGeneric #-}
    module Parser ( parseWithMaxCtx
                  , ParseE (..)
                  ) where

import Control.Composition (thread)
import Control.Exception (Exception)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Control.DeepSeq (NFData)
import Data.Bifunctor (first, second)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as ASCII
import Data.Functor (void)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Nm
import Nm hiding (loc)
import A
import L
import Prettyprinter (Pretty (pretty), (<+>), concatWith, squotes)
import Sh

}

%name parseE E
%tokentype { Tok }
%error { parseErr }
%error.expected
%monad { Parse } { (>>=) } { pure }
%lexer { lift alexMonadScan >>= } { EOF _ }

%token

    lbrace { TokSym $$ LBrace }
    rbrace { TokSym $$ RBrace }
    lsqbracket { TokSym $$ LSqBracket }
    rsqbracket { TokSym $$ RSqBracket }
    lparen { TokSym $$ LParen }
    rparen { TokSym $$ RParen }
    dot { TokSym $$ L.Dot }
    dp { TokSym $$ Dp }
    bind { TokSym $$ Bind }
    lbind { TokSym $$ LBind }
    polybind { TokSym $$ PolyBind }
    semicolon { TokSym $$ Semicolon }
    comma { TokSym $$ Comma }
    mmap { TokSym $$ MMap }
    therefore { TokSym $$ Therefore }
    underscore { TokSym $$ Underscore }
    question { TokSym $$ QuestionMark }
    condSplit { TokSym $$ CondSplit }
    coronis { TokSym $$ Cor }
    larr { TokSym $$ ArrL } rarr { TokSym $$ ArrR }
    colon { TokSym $$ Colon }
    lrank { TokSym $$ LRank }
    compose { TokSym $$ Compose }
    sig { TokSym $$ Sig }
    tsig { TokSym $$ TSig }
    arrow { TokSym $$ L.Arrow }
    di { TokSym $$ DIS }
    succ { TokSym $$ L.Succ }
    lconv { TokSym $$ LConv }
    focus { TokSym $$ L.Focus }
    last { TokSym $$ L.Last } lastM { TokSym $$ L.LastM }
    head { TokSym $$ L.Head } headM { TokSym $$ L.HeadM }
    tail { TokSym $$ L.Tail } tailM { TokSym $$ L.TailM }
    init { TokSym $$ L.Init } initM { TokSym $$ L.InitM }
    tensor { TokSym $$ Tensor }
    geq { TokSym $$ Geq }
    gt { TokSym $$ L.Gt }
    leq { TokSym $$ Leq }
    lt { TokSym $$ L.Lt }
    eq { TokSym $$ L.Eq }
    neq { TokSym $$ L.Neq }
    and { TokSym $$ L.And }
    or { TokSym $$ L.Or }
    xor { TokSym $$ L.Xor }
    not { TokSym $$ Not }
    tilde { TokSym $$ Tilde }
    pp { TokSym $$ PlusPlus }
    rot { TokSym $$ Rotate }
    sr { TokSym $$ L.Sr }
    sl { TokSym $$ L.Sl }
    inv { TokSym $$ Inv }

    plus { TokSym $$ L.Plus }
    minus { TokSym $$ L.Minus }
    times { TokSym $$ L.Times }
    percent { TokSym $$ Percent }
    idiv { TokSym $$ L.IDiv }
    caret { TokSym $$ Caret }
    max { TokSym $$ MaxS }
    min { TokSym $$ MinS }
    pow { TokSym $$ Pow }
    at { $$@(TokSym _ Access{}) }
    consS { TokSym $$ L.Cons }
    snoc { TokSym $$ L.Snoc }
    trans { TokSym $$ Transp }
    fork { TokSym $$ Fork }
    bcyc { TokSym $$ L.Cyc }
    ditto { TokSym $$ L.Ditto }
    iat { TokSym $$ L.A1 }
    sub { TokSym $$ Sub }
    mod { TokSym $$ L.Mod }
    atDot { TokSym $$ AtDot }
    -- weier { TokSym $$ Weier }
    ice { TokSym $$ Ice }
    para { TokSym $$ Para }

    folds { TokSym $$ L.FoldS }
    fold { TokSym $$ L.Fold }
    foldl { TokSym $$ L.Foldl }
    foldA { TokSym $$ L.FoldA }
    quot { TokSym $$ Quot }
    zip { TokSym $$ L.Zip }
    flat { TokSym $$ L.B }
    addd { TokSym $$ Sharp }

    lam { TokSym $$ L.Lam }

    name { TokName _ $$ }

    intLit { $$@(TokInt _ _) }
    floatLit { $$@(TokFloat _ _) }
    six { $$@(TokIx _ _) }
    il { $$@(TokAdLit _ _) }

    x { TokResVar $$ VarX }
    y { TokResVar $$ VarY }

    frange { TokB $$ BuiltinFRange }
    iota { TokB $$ BuiltinIota }
    ix { TokB $$ BuiltinIi }
    floor { TokB $$ BuiltinFloor }
    ceil { TokB $$ BuiltinCeil }
    e { TokB $$ BuiltinE }
    i { TokB $$ BuiltinI }
    b { TokB $$ BuiltinBb }
    f { TokB $$ BuiltinF }
    t { TokB $$ BuiltinT }
    tt { TokB $$ BuiltinTrue }
    ff { TokB $$ BuiltinFalse }
    sqrt { TokB $$ BuiltinSqrt }
    pi { TokB $$ BuiltinPi }
    gen { TokB $$ BuiltinGen }
    ug { TokB $$ BuiltinUg }
    log { TokSym $$ SymLog }
    diag { TokB $$ BuiltinD }
    nil { TokB $$ BuiltinNil }
    cons { TokB $$ BuiltinCons }
    arr { TokB $$ BuiltinArr }
    ixTimes { TokSym $$ IxTimes }
    vec { TokB $$ BuiltinVec }
    matrix { TokB $$ BuiltinM }
    vector { TokB $$ BuiltinV }
    int { TokB $$ BuiltinInt }
    bool { TokB $$ BuiltinBool }
    float { TokB $$ BuiltinFloat }
    scanS { TokB $$ BuiltinScanS }
    scan { TokB $$ BuiltinScan }
    mul { TokB $$ BuiltinMMul }
    vmul { TokB $$ BuiltinVMul }
    r { TokB $$ BuiltinR }
    sin { TokB $$ BuiltinSin }
    cos { TokB $$ BuiltinCos }
    tan { TokB $$ BuiltinTan }
    cyc { TokB $$ BuiltinCyc }
    even { TokB $$ BuiltinEven }
    odd { TokB $$ BuiltinOdd }
    abs { TokB $$ BuiltinAbs }
    sks { TokB $$ BuiltinS }
    skk { TokB $$ BuiltinK }

%right semicolon mmap
%nonassoc leq geq gt lt neq eq

%%

many(p)
    : many(p) p { $2 : $1 }
    | { [] }

sepBy(p,q)
    : sepBy(p,q) q p { $3 : $1 }
    | p { [$1] }

tupBody(p,q)
    : tupBody(p,q) q p { $3 : $1 }
    | p q p { $3 : [$1] }

tupBy(p,q)
    : lparen tupBody(p,q) rparen { ($1, $2) }

tupled(p)
    : tupBy(p,comma) { $1 }

braces(p)
    : lbrace p rbrace { $2 }

brackets(p)
    : lsqbracket p rsqbracket { $2 }

parens(p)
    : lparen p rparen { $2 }

flipSeq(p,q)
    : p q { $1 }

I :: { I AlexPosn }
  : intLit { Ix (loc $1) (fromInteger $ int $1) }
  | name { IVar (Nm.loc $1) $1 }
  | I plus I { StaPlus $2 $1 $3 }
  | I times I { StaMul $2 $1 $3 }

Sh :: { Sh AlexPosn }
   : nil { Nil }
   | I cons Sh { Sh.Cons $1 $3 }
   | name { SVar $1 }
   | parens(Sh) { $1 }
   | tupBy(I,ixTimes) { foldl (flip Sh.Cons) Nil (snd $1) }

T :: { T AlexPosn }
  : arr Sh T { Arr $2 $3 }
  | vec I T { Arr ($2 `Sh.Cons` Nil) $3 }
  | matrix six comma six T { Arr ((Ix (loc $2) (six $2)) `Sh.Cons` (Ix (loc $4) (six $4)) `Sh.Cons` Nil) $5 }
  | matrix T {% do {i <- lift $ freshName "i"; j <- lift $ freshName "j"; pure $ Arr (IVar $1 i `Sh.Cons` IVar $1 j `Sh.Cons` Nil) $2 } }
  | vector T {% do {i <- lift $ freshName "n"; pure (Arr (IVar $1 i `Sh.Cons` Nil) $2) } }
  | int { I } | bool { A.B } | float { F }
  | parens(T) { $1 }
  | T arrow T { A.Arrow $1 $3 }
  | tupled(T) { P (reverse (snd $1)) }
  | name { TVar $1 }

R :: { (Int, Maybe [Int]) }
  : intLit compose lsqbracket sepBy(intLit,comma) rsqbracket { (fromInteger $ int $1, Just (reverse (fmap (fromInteger.int) $4))) }
  | intLit { (fromInteger $ int $1, Nothing) }

S :: { (Int, Maybe Int) }
  : intLit compose intLit { (fromInteger (int $1), Just (fromInteger (int $3))) }
  | intLit { (fromInteger (int $1), Nothing) }

-- binary operator
BBin :: { E AlexPosn }
     : plus { Builtin $1 A.Plus } | minus { Builtin $1 A.Minus }
     | times { Builtin $1 A.Times } | percent { Builtin $1 Div }
     | idiv { Builtin $1 A.IDiv }
     | caret { Builtin $1 IntExp }
     | max { Builtin $1 Max } | min { Builtin $1 Min }
     | scan { Builtin $1 Scan }
     | quot { Builtin $1 Map }
     | di intLit { Builtin $1 (DI (fromInteger $ int $2)) }
     | lconv sepBy(S,comma) rbrace { Builtin $1 (Conv (reverse $2)) }
     | focus braces(sepBy(intLit,comma)) { Builtin $1 (A.Focus (reverse (map (fromInteger.int) $2)))  }
     -- FIXME: not necessarily binary operator!!
     | lrank sepBy(R,comma) rbrace { Builtin $1 (Rank (reverse $2)) }
     | succ { Builtin $1 A.Succ }
     | pow { Builtin $1 Exp }
     | consS { Builtin $1 ConsE }
     | snoc { Builtin $1 A.Snoc }
     | mul { Builtin $1 Mul }
     | vmul { Builtin $1 VMul }
     | geq { Builtin $1 Gte } | gt { Builtin $1 A.Gt }
     | leq { Builtin $1 Lte } | lt { Builtin $1 A.Lt }
     | eq { Builtin $1 A.Eq } | neq { Builtin $1 A.Neq }
     | pp { Builtin $1 CatE }
     | rot { Builtin $1 Rot }
     | fold { Builtin $1 A.Fold }
     | bcyc { Builtin $1 A.Cyc }
     | iat { Builtin $1 A.A1 }
     | sub { Builtin $1 I1 }
     | mod { Builtin $1 A.Mod }
     | atDot { Builtin $1 IOf }
     | ditto { Builtin $1 Re }
     | and { Builtin $1 A.And } | or { Builtin $1 A.Or }
     | xor { Builtin $1 A.Xor }
     | ice { Builtin $1 Ices }
     | para { Builtin $1 Filt }
     | sr { Builtin $1 A.Sr } | sl { Builtin $1 A.Sl }
     | therefore { Builtin $1 C } | fork { Builtin $1 S' }
     | dp { Builtin $1 A.Dot }

B :: { (Bnd, (Nm AlexPosn, E AlexPosn)) }
  : name bind E { (L, ($1, $3)) }
  | name lbind E { (LL, ($1, $3)) }
  | name polybind E { (D, ($1, $3)) }

Lam :: { [(AlexPosn, [Nm AlexPosn])] }
    : lam tupled(name) dot { [$2] }
    | lam tupled(name) dot Lam { $2 : $4 }

E :: { E AlexPosn }
  : name { Var (Nm.loc $1) $1 }
  | E ix { EApp (eAnn $1) (Builtin $2 Ix'd) $1 }
  | intLit { ILit (loc $1) (int $1) }
  | floatLit { FLit (loc $1) (float $1) }
  | pi { FLit $1 pi }
  | tt { BLit $1 True }
  | ff { BLit $1 False }
  | inv E { EApp $1 (EApp $1 (Builtin $1 Div) (FLit $1 1)) $2 }
  | parens(inv) { EApp $1 (Builtin $1 Div) (FLit $1 1) }
  | parens(BBin) { Parens (eAnn $1) $1 }
  | lparen E BBin rparen { Parens $1 (EApp $1 $3 $2) }
  | lparen BBin E rparen {% do { n <- lift $ freshName "x"; pure (A.Lam $1 n (EApp $1 (EApp $1 $2 (Var (Nm.loc n) n)) $3)) } }
  | E BBin E { EApp (eAnn $1) (EApp (eAnn $3) $2 $1) $3 }
  | parens(E) { Parens (eAnn $1) $1 }
  | larr sepBy(E,comma) rarr { ALit $1 (reverse $2) }
  | il { let l=loc $1 in ALit l (map (ILit l.fromInteger) (ints $1)) }
  | lam name dot E { A.Lam $1 $2 $4 }
  | name mmap E { A.Lam $2 $1 $3 }
  | Lam E {% bindΠ (reverse $1) $2 }
  | tupled(E) { Tup (fst $1) (reverse (snd $1)) }
  | lbrace many(flipSeq(B,semicolon)) E rbrace { mkLet $1 (reverse $2) $3 }
  | coronis many(flipSeq(B,semicolon)) E { mkLet $1 (reverse $2) $3 }
  | lsqbracket E rsqbracket { Dfn $1 $2 }
  | frange { Builtin $1 FRange } | iota { Builtin $1 IRange }
  | floor { Builtin $1 Floor } | ceil { Builtin $1 Ceil }
  | sqrt { Builtin $1 Sqrt } | log { Builtin $1 Log }
  | underscore { Builtin $1 Neg }
  | gen { Builtin $1 Gen } | ug { Builtin $1 Ug }
  | colon { Builtin $1 Size }
  | i { Builtin $1 ItoF } | b { Builtin $1 Bit }
  | t { Builtin $1 Dim }
  | E folds E E { EApp (eAnn $1) (EApp (eAnn $1) (EApp $2 (Builtin $2 A.FoldS) $1) $3) $4 }
  | E foldl E E { EApp (eAnn $1) (EApp (eAnn $1) (EApp $2 (Builtin $2 A.Foldl) $1) $3) $4 }
  | E foldA E E { EApp (eAnn $1) (EApp (eAnn $1) (EApp $2 (Builtin $2 A.FoldA) $1) $3) $4 }
  | E scanS E E { EApp (eAnn $1) (EApp (eAnn $1) (EApp $2 (Builtin $2 ScanS) $1) $3) $4 }
  | E zip E E { EApp (eAnn $1) (EApp (eAnn $1) (EApp $2 (Builtin $2 A.Zip) $1) $3) $4 }
  | E E { EApp (eAnn $1) $1 $2 }
  | x { ResVar $1 X } | y { ResVar $1 Y }
  | f { Builtin $1 Fib }
  | last { Builtin $1 A.Last } | lastM { Builtin $1 A.LastM }
  | head { Builtin $1 A.Head } | headM { Builtin $1 A.HeadM }
  | tail { Builtin $1 A.Tail } | tailM { Builtin $1 A.TailM }
  | init { Builtin $1 A.Init } | initM { Builtin $1 A.InitM }
  | diag { Builtin $1 Di }
  | question E condSplit E condSplit E { Cond $1 $2 $4 $6 }
  | E sig T { Ann $2 $1 $3 }
  | E tsig parens(Sh) {% do{a <- lift$freshName "a"; pure$Ann $2 $1 (Arr $3 (TVar a))} }
  | e { EApp $1 (Builtin $1 Exp) (FLit $1 (exp 1)) }
  | E at { EApp (eAnn $1) (Builtin (loc $2) (TAt (iat $ sym $2))) $1 }
  | parens(at) { Builtin (loc $1) (TAt (iat $ sym $1)) }
  | E E tensor E { EApp (eAnn $1) (EApp (eAnn $4) (EApp (eAnn $2) (Builtin $3 Outer) $2) $1) $4 }
  | trans { Builtin $1 T }
  | r { Builtin $1 R }
  | sin { Builtin $1 Sin }
  | cos { Builtin $1 Cos }
  | tan { Builtin $1 Tan }
  | cyc { Builtin $1 A.Cyc }
  | tilde { Builtin $1 RevE }
  | odd { Builtin $1 Odd } | even { Builtin $1 Even }
  | abs { Builtin $1 Abs }
  | flat { Builtin $1 Flat }
  | addd { Builtin $1 AddDim }
  | not { Builtin $1 N }
  | sks { Builtin $1 S } | skk { Builtin $1 K }

{

parseErr :: Tok -> [String] -> Parse a
parseErr tok = throwError . Unexpected tok

data Bnd = L | LL | D

bindΠ :: [(AlexPosn, [Nm AlexPosn])] -> E AlexPosn -> Parse (E AlexPosn)
bindΠ vs e = do
    (lams, bΡ) <- unzip <$> traverse (uncurry b) vs
    pure $ thread lams $ thread bΡ e
  where
    b l ns = do
        ρ <- lift $ freshName "ρ"
        let bΡs = thread (zipWith (\n i -> let lϵϵ=Nm.loc n in (LLet lϵϵ (n, EApp lϵϵ (Builtin lϵϵ (TAt i)) (Var lϵϵ ρ)))) (reverse ns) [1..])
        pure (A.Lam l ρ, bΡs)

mkLet :: a -> [(Bnd, (Nm a, E a))] -> E a -> E a
mkLet _ [] e            = e
mkLet l ((L, b):bs) e   = Let l b (mkLet l bs e)
mkLet l ((LL, b):bs) e  = LLet l b (mkLet l bs e)
mkLet l ((D, b):bs) e   = Def l b (mkLet l bs e)

data ParseE = Unexpected Tok [String] | LexErr String deriving (Generic)

instance Pretty ParseE where
    pretty (Unexpected tok valid) = pretty (loc tok) <+> "Unexpected" <+> pretty tok <> "." <+> "Expected one of" <+> concatWith (\x y -> x <> "," <+> y) (squotes.pretty<$>valid)
    pretty (LexErr str)           = pretty (T.pack str)

instance Show ParseE where
    show = show . pretty

instance Exception ParseE

instance NFData ParseE where

type Parse = ExceptT ParseE Alex

parseAll :: AlexUserState -> BSL.ByteString -> Either ParseE (AlexUserState, E AlexPosn)
parseAll = runParseSt parseE

parseWithMaxCtx :: AlexUserState -> BSL.ByteString -> Either ParseE (Int, E AlexPosn)
parseWithMaxCtx st b = fmap (first fst3) (parseAll st b) where fst3 (x, _, _) = x

runParseSt :: Parse a -> AlexUserState -> BSL.ByteString -> Either ParseE (AlexUserState, a)
runParseSt parser u bs = liftErr $ withAlexSt bs u (runExceptT parser)

liftErr :: Either String (b, Either ParseE c) -> Either ParseE (b, c)
liftErr (Left err)            = Left (LexErr err)
liftErr (Right (_, Left err)) = Left err
liftErr (Right (i, Right x))  = Right (i, x)

}
