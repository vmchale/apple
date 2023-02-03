{
    {-# LANGUAGE DeriveGeneric #-}
    {-# LANGUAGE OverloadedStrings #-}
    module Parser ( parseAll
                  , parseWithMax
                  , parseWithMaxCtx
                  , ParseE (..)
                  ) where

import Control.Exception (Exception)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.Trans.Class (lift)
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as ASCII
import Data.Functor (void)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import qualified Name
import Name hiding (loc)
import A
import L
import Prettyprinter (Pretty (pretty), (<+>))

}

%name parseE E
%name parseBind B
%tokentype { Token AlexPosn }
%error { parseError }
%monad { Parse } { (>>=) } { pure }
%lexer { lift alexMonadScan >>= } { EOF _ }

%token

    lbrace { TokSym $$ LBrace }
    rbrace { TokSym $$ RBrace }
    lsqbracket { TokSym $$ LSqBracket }
    rsqbracket { TokSym $$ RSqBracket }
    lparen { TokSym $$ LParen }
    rparen { TokSym $$ RParen }
    dot { TokSym $$ Dot }
    bind { TokSym $$ Bind }
    lbind { TokSym $$ LBind }
    polybind { TokSym $$ PolyBind }
    semicolon { TokSym $$ Semicolon }
    comma { TokSym $$ Comma }
    underscore { TokSym $$ Underscore }
    question { TokSym $$ QuestionMark }
    condSplit { TokSym $$ CondSplit }
    larr { TokSym $$ ArrL }
    rarr { TokSym $$ ArrR }
    colon { TokSym $$ Colon }
    lrank { TokSym $$ LRank }
    compose { TokSym $$ Compose }
    sig { TokSym $$ Sig }
    tsig { TokSym $$ TSig }
    arrow { TokSym $$ L.Arrow }
    di { TokSym $$ DIS }
    succ { TokSym $$ L.Succ }
    conv { TokSym $$ L.Conv }
    last { TokSym $$ L.Last }
    lastM { TokSym $$ L.LastM }
    head { TokSym $$ L.Head }
    headM { TokSym $$ L.HeadM }
    tail { TokSym $$ L.Tail }
    init { TokSym $$ L.Init }
    do { TokSym $$ Do }
    tensor { TokSym $$ Tensor }
    geq { TokSym $$ Geq }
    gt { TokSym $$ L.Gt }
    leq { TokSym $$ Leq }
    lt { TokSym $$ L.Lt }
    eq { TokSym $$ L.Eq }
    neq { TokSym $$ L.Neq }
    tilde { TokSym $$ Tilde }
    pp { TokSym $$ PlusPlus }
    rot { TokSym $$ Rotate }

    plus { TokSym $$ L.Plus }
    minus { TokSym $$ L.Minus }
    times { TokSym $$ L.Times }
    percent { TokSym $$ Percent }
    caret { TokSym $$ Caret }
    max { TokSym $$ MaxS }
    min { TokSym $$ MinS }
    pow { TokSym $$ Pow }
    at { $$@(TokSym _ Access{}) }
    consS { TokSym $$ L.Cons }
    snoc { TokSym $$ L.Snoc }
    trans { TokSym $$ Transp }

    folds { TokSym $$ L.FoldS }
    fold { TokSym $$ L.Fold }
    foldl { TokSym $$ L.Foldl }
    foldA { TokSym $$ L.FoldA }
    quot { TokSym $$ Quot }
    zip { TokSym $$ L.Zip }

    lam { TokSym $$ L.Lam }

    name { TokName _ $$ }

    intLit { $$@(TokInt _ _) }
    floatLit { $$@(TokFloat _ _) }

    x { TokResVar $$ VarX }
    y { TokResVar $$ VarY }

    frange { TokB $$ BuiltinFRange }
    iota { TokB $$ BuiltinIota }
    floor { TokB $$ BuiltinFloor }
    e { TokB $$ BuiltinE }
    i { TokB $$ BuiltinI }
    f { TokB $$ BuiltinF }
    t { TokB $$ BuiltinT }
    tt { TokB $$ BuiltinTrue }
    ff { TokB $$ BuiltinFalse }
    sqrt { TokB $$ BuiltinSqrt }
    pi { TokB $$ BuiltinPi }
    gen { TokB $$ BuiltinGen }
    log { TokSym $$ SymLog }
    re { TokB $$ BuiltinRep }
    nil { TokB $$ BuiltinNil }
    cons { TokB $$ BuiltinCons }
    arr { TokB $$ BuiltinArr }
    int { TokB $$ BuiltinInt }
    float { TokB $$ BuiltinFloat }
    scanS { TokB $$ BuiltinScanS }
    scan { TokB $$ BuiltinScan }
    mul { TokB $$ BuiltinMMul }
    vmul { TokB $$ BuiltinVMul }
    r { TokB $$ BuiltinR }
    sin { TokB $$ BuiltinSin }
    cos { TokB $$ BuiltinCos }
    tan { TokB $$ BuiltinTan }

%left paren
%nonassoc leq geq gt lt neq eq

%%

many(p)
    : many(p) p { $2 : $1 }
    | { [] }

sepBy(p,q)
    : sepBy(p,q) q p { $3 : $1 }
    | p { [$1] }

tupled(p,q)
    : tupled(p,q) q p { $3 : $1 }
    | p q p { $3 : [$1] }

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
  | name { IVar (Name.loc $1) $1 }
  | I plus I { StaPlus $2 $1 $3 }

Sh :: { Sh AlexPosn }
   : nil { Nil }
   | I cons Sh { A.Cons $1 $3 }
   | name { SVar $1 }
   | parens(Sh) { $1 }

T :: { T AlexPosn }
  : arr Sh T { Arr $2 $3 }
  | int { I }
  | float { F }
  | parens(T) { $1 }
  | T arrow T { A.Arrow $1 $3 }

R :: { (Int, Maybe [Int]) }
  : intLit compose lsqbracket sepBy(intLit,comma) rsqbracket { (fromInteger $ int $1, Just (reverse (fmap (fromInteger.int) $4))) }
  | intLit { (fromInteger $ int $1, Nothing) }

-- binary operator
BBin :: { E AlexPosn }
     : plus { Builtin $1 A.Plus }
     | minus { Builtin $1 A.Minus }
     | times { Builtin $1 A.Times }
     | percent { Builtin $1 Div }
     | caret { Builtin $1 IntExp }
     | max { Builtin $1 Max }
     | min { Builtin $1 Min }
     | scan { Builtin $1 Scan }
     | quot { Builtin $1 Map }
     | di intLit { Builtin $1 (DI (fromInteger $ int $2)) }
     | conv braces(sepBy(intLit,comma)) { Builtin $1 (A.Conv (reverse (fmap (fromInteger.int) $2))) }
     -- FIXME: not necessarily binary operator!!
     | lrank sepBy(R,comma) rbrace { Builtin $1 (Rank (reverse $2)) }
     | succ { Builtin $1 A.Succ }
     | pow { Builtin $1 Exp }
     | consS { Builtin $1 ConsE }
     | snoc { Builtin $1 A.Snoc }
     | mul { Builtin $1 Mul }
     | vmul { Builtin $1 VMul }
     | do { Builtin $1 Iter }
     | geq { Builtin $1 Gte }
     | gt { Builtin $1 A.Gt }
     | leq { Builtin $1 Lte }
     | lt { Builtin $1 A.Lt }
     | eq { Builtin $1 A.Eq }
     | neq { Builtin $1 A.Neq }
     | pp { Builtin $1 CatE }
     | rot { Builtin $1 Rot }
     | fold { Builtin $1 A.Fold }

B :: { (Bnd, (Name AlexPosn, E AlexPosn)) }
  : name bind E { (L, ($1, $3)) }
  | name lbind E { (LL, ($1, $3)) }
  | name polybind E { (D, ($1, $3)) }

E :: { E AlexPosn }
  : name { Var (Name.loc $1) $1 }
  | intLit { ILit (loc $1) (int $1) }
  | floatLit { FLit (loc $1) (float $1) }
  | pi { FLit $1 pi }
  | tt { BLit $1 True }
  | ff { BLit $1 False }
  | parens(BBin) { $1 }
  | lparen E BBin rparen { EApp $1 $3 $2 }
  | lparen BBin E rparen {% do { n <- lift $ freshName "x" ; pure (A.Lam $1 n (EApp $1 (EApp $1 $2 (Var (Name.loc n) n)) $3)) } }
  | E BBin E { EApp (eAnn $1) (EApp (eAnn $3) $2 $1) $3 }
  | parens(E) { Parens (eAnn $1) $1 }
  | larr sepBy(E,comma) rarr { ALit $1 (reverse $2) }
  | lparen tupled(E,comma) rparen { Tup $1 (reverse $2) }
  | lam name dot E { A.Lam $1 $2 $4 }
  | lbrace many(flipSeq(B,semicolon)) E rbrace { mkLet $1 (reverse $2) $3 }
  | lsqbracket E rsqbracket { Dfn $1 $2 }
  | frange { Builtin $1 FRange } | iota { Builtin $1 IRange }
  | floor { Builtin $1 Floor } | sqrt { Builtin $1 Sqrt } | log { Builtin $1 Log }
  | underscore { Builtin $1 Neg }
  | gen { Builtin $1 Gen }
  | colon { Builtin $1 Size }
  | i { Builtin $1 ItoF }
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
  | tail { Builtin $1 A.Tail }
  | init { Builtin $1 A.Init }
  | re { Builtin $1 Re }
  | question E condSplit E condSplit E { Cond $1 $2 $4 $6 }
  | E sig T { Ann $2 $1 (void $3) }
  | E tsig parens(Sh) {% do{a <- lift$freshName "a"; pure$Ann $2 $1 (void$Arr $3 (TVar a))} }
  | e { EApp $1 (Builtin $1 Exp) (FLit $1 (exp 1)) }
  | E at { EApp (eAnn $1) (Builtin (loc $2) (TAt (iat $ sym $2))) $1 }
  | parens(at) { Builtin (loc $1) (TAt (iat $ sym $1)) }
  | E E tensor E { EApp (eAnn $1) (EApp (eAnn $4) (EApp (eAnn $2) (Builtin $3 Outer) $2) $1) $4 }
  | trans { Builtin $1 T }
  | r { Builtin $1 R }
  | sin { Builtin $1 Sin }
  | cos { Builtin $1 Cos }
  | tan { Builtin $1 Tan }
  | tilde { Builtin $1 Flip }

{

parseError :: Token AlexPosn -> Parse a
parseError = throwError . Unexpected

data Bnd = L | LL | D

mkLet :: a -> [(Bnd, (Name a, E a))] -> E a -> E a
mkLet _ [] e            = e
mkLet l ((L, b):bs) e   = Let l b (mkLet l bs e)
mkLet l ((LL, b):bs) e  = LLet l b (mkLet l bs e)
mkLet l ((D, b):bs) e   = Def l b (mkLet l bs e)

data ParseE a = Unexpected (Token a)
              | LexErr String
              deriving (Generic)

instance Pretty a => Pretty (ParseE a) where
    pretty (Unexpected tok)  = pretty (loc tok) <+> "Unexpected" <+> pretty tok
    pretty (LexErr str)      = pretty (T.pack str)

instance Pretty a => Show (ParseE a) where
    show = show . pretty

instance (Pretty a, Typeable a) => Exception (ParseE a)

instance NFData a => NFData (ParseE a) where

type Parse = ExceptT (ParseE AlexPosn) Alex

parseWithMax :: BSL.ByteString -> Either (ParseE AlexPosn) (Int, E AlexPosn)
parseWithMax = parseWithMaxCtx alexInitUserState

parseAll :: AlexUserState -> BSL.ByteString -> Either (ParseE AlexPosn) (AlexUserState, E AlexPosn)
parseAll = runParseSt parseE

parseWithMaxCtx :: AlexUserState -> BSL.ByteString -> Either (ParseE AlexPosn) (Int, E AlexPosn)
parseWithMaxCtx st b = fmap (first fst3) (parseAll st b) where fst3 (x, _, _) = x

runParseSt :: Parse a -> AlexUserState -> BSL.ByteString -> Either (ParseE AlexPosn) (AlexUserState, a)
runParseSt parser u bs = liftErr $ withAlexSt bs u (runExceptT parser)

liftErr :: Either String (b, Either (ParseE a) c) -> Either (ParseE a) (b, c)
liftErr (Left err)            = Left (LexErr err)
liftErr (Right (_, Left err)) = Left err
liftErr (Right (i, Right x))  = Right (i, x)

}
