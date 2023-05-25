{
    {-# LANGUAGE DeriveGeneric #-}
    {-# LANGUAGE DeriveAnyClass #-}
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE StandaloneDeriving #-}
    module L ( alexMonadScan
             , alexInitUserState
             , runAlex
             , runAlexSt
             , withAlexSt
             , freshName
             , newIdent
             , AlexPosn (..)
             , Alex (..)
             , Token (..)
             , Sym (..)
             , Builtin (..)
             , Var (..)
             , AlexUserState
             ) where

import Control.Arrow ((&&&))
import Control.DeepSeq (NFData)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as ASCII
import Data.Functor (($>))
import qualified Data.IntMap as IM
import qualified Data.Map as M
import Data.Semigroup ((<>))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (pretty), (<+>), colon, squotes)
import Nm
import U

}

%wrapper "monadUserState-bytestring"

$digit = [0-9]

$latin = [a-zA-Z]

$subscript = [ₐ-ₜ]
$digitsubscript = [₀-₉]

$greek = [α-ωΑ-Ω]

$mathgreek = [𝛢-𝛺𝛼-𝜛]
$mathlatin = [𝐴-𝑍𝑎-𝑧]

$letter = [$latin $greek]
$sub = [$subscript $digitsubscript]

@follow_char = [$letter $digit \_]

@name = ($letter#[Λλ] @follow_char* $sub* | $mathgreek $sub* | $mathlatin $sub* | ∫ | 𝛻 | ∇)

@exp = e\-?$digit+
@float = $digit+\.$digit+@exp?

tokens :-

    <0> "["                      { mkSym LSqBracket `andBegin` dfn } -- FIXME: this doesn't allow nested
    <0> `$white*"{"              { mkSym LRank `andBegin` braces }

    <dfn> {
        x                        { mkRes VarX }
        y                        { mkRes VarY }
        `$white*"{"              { mkSym LRank `andBegin` dbraces }
    }

    <braces,dbraces> {
        "["                      { mkSym LSqBracket }
        "]"                      { mkSym RSqBracket }
        ∘                        { mkSym Compose }
        o                        { mkSym Compose }
    }

    <braces>  "}"                { mkSym RBrace `andBegin` 0 }
    <dbraces> "}"                { mkSym RBrace `andBegin` dfn }

    <0,dfn,braces,dbraces> {
        $white+                  ;

        "--".*                   ;

        ","                      { mkSym Comma }

        $digit+                  { tok (\p s -> alex $ TokInt p (read $ ASCII.unpack s)) }
    }

    <0,dfn> {
        "{"                      { mkSym LBrace }
        "}"                      { mkSym RBrace }

        -- symbols/operators
        "%"                      { mkSym Percent }
        "*"                      { mkSym Times }
        "**"                     { mkSym Pow }
        "+"                      { mkSym Plus }
        "-"                      { mkSym Minus }
        "^"                      { mkSym Caret }

        "/"                      { mkSym Fold }
        "/ₒ"                     { mkSym FoldS }
        "/o"                     { mkSym FoldS }
        "/l"                     { mkSym Foldl }
        "/*"                     { mkSym FoldA }
        '                        { mkSym Quot }
        `                        { mkSym Zip }

        "("                      { mkSym LParen }
        ")"                      { mkSym RParen }
        λ                        { mkSym Lam }
        \\                       { mkSym Lam }
        "\`"                     { mkSym DIS }
        "\~"                     { mkSym Succ }
        "."                      { mkSym Dot }
        ";"                      { mkSym Semicolon }
        :                        { mkSym Colon }
        "←"                      { mkSym Bind }
        "<-"                     { mkSym Bind }
        _                        { mkSym Underscore }
        "?"                      { mkSym QuestionMark }
        ",."                     { mkSym CondSplit }
        ⟨                        { mkSym ArrL }
        ⟩                        { mkSym ArrR }
        "_."                     { mkSym SymLog }
        ⟜                        { mkSym LBind }
        ⇐                        { mkSym PolyBind }
        →                        { mkSym Arrow }
        "->"                     { mkSym Arrow }
        "->"$digit+              { tok (\p s -> alex $ TokSym p (Access (read $ ASCII.unpack $ BSL.drop 2 s))) }
        ::                       { mkSym Sig }
        ":~"                     { mkSym TSig }
        ⋉                        { mkSym MaxS }
        ">."                     { mkSym MaxS }
        ⋊                        { mkSym MinS }
        "<."                     { mkSym MinS }
        ⨳                        { mkSym Conv }
        "{."                     { mkSym Head }
        "{.?"                    { mkSym HeadM }
        "}."                     { mkSym Last }
        "}.?"                    { mkSym LastM }
        "{:"                     { mkSym Tail }
        "}:"                     { mkSym Init }
        ⊲                        { mkSym Cons }
        "<|"                     { mkSym Cons }
        ⊳                        { mkSym Snoc }
        "|>"                     { mkSym Snoc }
        "^:"                     { mkSym Do }
        ⊗                        { mkSym Tensor }
        "|:"                     { mkSym Transp }
        ⍉                        { mkSym Transp }
        ≥                        { mkSym Geq }
        ">="                     { mkSym Geq }
        ">"                      { mkSym Gt }
        =                        { mkSym Eq }
        ≠                        { mkSym Neq }
        "!="                     { mkSym Neq }
        "<"                      { mkSym Lt }
        "<="                     { mkSym Leq }
        ≤                        { mkSym Leq }
        "~"                      { mkSym Tilde }
        ⧺                        { mkSym PlusPlus }
        "++"                     { mkSym PlusPlus }
        ⊖                        { mkSym Rotate }
        ⊙                        { mkSym Cyc }
        ˙                        { mkSym A1 }
        "|"                      { mkSym Mod }
        "@."                     { mkSym AtDot }
        👁️                        { mkSym Eye }

        "]"                      { mkSym RSqBracket `andBegin` 0 }

        frange                   { mkBuiltin BuiltinFRange }
        𝒻                        { mkBuiltin BuiltinFRange }
        irange                   { mkBuiltin BuiltinIota }
        ⍳                        { mkBuiltin BuiltinIota }
        ⌊                        { mkBuiltin BuiltinFloor }
        "|."                     { mkBuiltin BuiltinFloor }
        ℯ                        { mkBuiltin BuiltinE }
        "e:"                     { mkBuiltin BuiltinE }
        itof                     { mkBuiltin BuiltinI }
        ℝ                        { mkBuiltin BuiltinI }
        𝓕                        { mkBuiltin BuiltinF }
        𝓉                        { mkBuiltin BuiltinT }
        "#t"                     { mkBuiltin BuiltinTrue }
        "#f"                     { mkBuiltin BuiltinFalse }
        √                        { mkBuiltin BuiltinSqrt }
        𝜋                        { mkBuiltin BuiltinPi }
        "gen."                   { mkBuiltin BuiltinGen }
        "cyc."                   { mkBuiltin BuiltinCyc }
        "re:"                    { mkBuiltin BuiltinRep }
        "di."                    { mkBuiltin BuiltinD }
        Λ                        { mkBuiltin BuiltinScan }
        Λₒ                       { mkBuiltin BuiltinScanS }
        "/\"                     { mkBuiltin BuiltinScan }
        "/\o"                    { mkBuiltin BuiltinScanS }
        "`Cons`"                 { mkBuiltin BuiltinCons }
        Nil                      { mkBuiltin BuiltinNil }
        "%."                     { mkBuiltin BuiltinMMul }
        "%:"                     { mkBuiltin BuiltinVMul }
        Arr                      { mkBuiltin BuiltinArr }
        float                    { mkBuiltin BuiltinFloat }
        int                      { mkBuiltin BuiltinInt }
        𝔯                        { mkBuiltin BuiltinR }
        "rand."                  { mkBuiltin BuiltinR }
        "sin."                   { mkBuiltin BuiltinSin }
        "cos."                   { mkBuiltin BuiltinCos }
        "tan."                   { mkBuiltin BuiltinTan }
        "odd."                   { mkBuiltin BuiltinOdd }
        "even."                  { mkBuiltin BuiltinEven }
        "abs."                   { mkBuiltin BuiltinAbs }

        _$digit+                 { tok (\p s -> alex $ TokInt p (negate $ read $ ASCII.unpack $ BSL.tail s)) }

        @float                   { tok (\p s -> alex $ TokFloat p (read $ ASCII.unpack s)) }
        _@float                  { tok (\p s -> alex $ TokFloat p (negate $ read $ ASCII.unpack $ BSL.tail s)) }

        @name                    { tok (\p s -> TokName p <$> newIdentAlex p (mkText s)) }

    }

{

alex :: a -> Alex a
alex = pure

tok f (p,_,s,_) len = f p (BSL.take len s)

constructor c t = tok (\p _ -> alex $ c p t)

mkRes = constructor TokResVar

mkSym = constructor TokSym

mkBuiltin = constructor TokB

mkText :: BSL.ByteString -> T.Text
mkText = decodeUtf8 . BSL.toStrict

instance Pretty AlexPosn where
    pretty (AlexPn _ line col) = pretty line <> colon <> pretty col

deriving instance Generic AlexPosn

deriving instance NFData AlexPosn

-- functional bimap?
type AlexUserState = (Int, M.Map T.Text Int, IM.IntMap (Nm AlexPosn))

alexInitUserState :: AlexUserState
alexInitUserState = (0, mempty, mempty)

gets_alex :: (AlexState -> a) -> Alex a
gets_alex f = Alex (Right . (id &&& f))

get_ust :: Alex AlexUserState
get_ust = gets_alex alex_ust

get_pos :: Alex AlexPosn
get_pos = gets_alex alex_pos

set_ust :: AlexUserState -> Alex ()
set_ust st = Alex (Right . (go &&& (const ())))
    where go s = s { alex_ust = st }

alexEOF = EOF <$> get_pos

data Sym = Plus | Minus | Fold | Foldl | Percent | Times | Semicolon | Bind | Pow
         | LSqBracket | RSqBracket | LBrace | RBrace | LParen | RParen | Lam
         | Dot | Caret | Quot | Zip | Comma | Underscore | QuestionMark | Colon
         | CondSplit | ArrL | ArrR | SymLog | LBind | PolyBind | LRank | Compose
         | Arrow | Sig | MaxS | MinS | DIS | Succ | Conv | Access { iat :: !Int }
         | TSig | Cons | Snoc | Do | Tensor | Transp | PlusPlus | Rotate
         | Last | LastM | Head | HeadM | Tail | Init
         | Geq | Gt | Eq | Neq | Leq | Lt
         | FoldA | FoldS | Tilde | Cyc | A1 | Mod | AtDot | Eye
         deriving (Generic, NFData)

instance Pretty Sym where
    pretty Plus         = "+"
    pretty Minus        = "-"
    pretty Percent      = "%"
    pretty Fold         = "/"
    pretty FoldS        = "/ₒ"
    pretty Foldl        = "/l"
    pretty FoldA        = "/*"
    pretty Pow          = "**"
    pretty Times        = "*"
    pretty Semicolon    = ";"
    pretty Colon        = ":"
    pretty Bind         = "←"
    pretty LSqBracket   = "["
    pretty RSqBracket   = "]"
    pretty LBrace       = "{"
    pretty RBrace       = "}"
    pretty LParen       = "("
    pretty RParen       = ")"
    pretty Lam          = "λ"
    pretty Dot          = "."
    pretty Caret        = "^"
    pretty Quot         = "'"
    pretty Zip          = "`"
    pretty Comma        = ","
    pretty Underscore   = "_"
    pretty QuestionMark = "?"
    pretty CondSplit    = ",."
    pretty ArrL         = "⟨"
    pretty ArrR         = "⟩"
    pretty SymLog       = "_."
    pretty LBind        = "⟜"
    pretty PolyBind     = "⇐"
    pretty LRank        = "`{"
    pretty Compose      = "∘"
    pretty Arrow        = "→"
    pretty Sig          = "::"
    pretty TSig         = ":~"
    pretty MaxS         = "⋉"
    pretty MinS         = "⋊"
    pretty DIS          = "\\`"
    pretty Succ         = "\\~"
    pretty Conv         = "⨳"
    pretty (Access i)   = "->" <> pretty i
    pretty Last         = "}."
    pretty LastM        = "}.?"
    pretty Head         = "{."
    pretty HeadM        = "{.?"
    pretty Cons         = "⊲"
    pretty Snoc         = "⊳"
    pretty Do           = "^:"
    pretty Tensor       = "⊗"
    pretty Transp       = ":|"
    pretty Geq          = "≥"
    pretty Gt           = ">"
    pretty Eq           = "="
    pretty Leq          = "≤"
    pretty Neq          = "≠"
    pretty Lt           = "<"
    pretty Tilde        = "~"
    pretty PlusPlus     = "⧺"
    pretty Tail         = "{:"
    pretty Init         = "}:"
    pretty Rotate       = "⊖"
    pretty Cyc          = "⊙"
    pretty A1           = "˙"
    pretty Mod          = "|"
    pretty AtDot        = "@."
    pretty Eye          = "👁️"

-- | Reserved/special variables
data Var = VarX | VarY deriving (Generic, NFData)

instance Pretty Var where
    pretty VarX     = "x"
    pretty VarY     = "y"

data Builtin = BuiltinFRange | BuiltinIota | BuiltinFloor | BuiltinE | BuiltinI
             | BuiltinF | BuiltinTrue | BuiltinFalse | BuiltinSqrt | BuiltinPi
             | BuiltinGen | BuiltinRep | BuiltinScan | BuiltinCons | BuiltinNil
             | BuiltinMMul | BuiltinArr | BuiltinInt | BuiltinFloat | BuiltinT
             | BuiltinR | BuiltinSin | BuiltinCos | BuiltinScanS | BuiltinTan
             | BuiltinVMul | BuiltinCyc | BuiltinOdd | BuiltinEven | BuiltinAbs
             | BuiltinD
             deriving (Generic, NFData)

instance Pretty Builtin where
    pretty BuiltinFRange = "frange"
    pretty BuiltinIota   = "⍳"
    pretty BuiltinFloor  = "⌊"
    pretty BuiltinE      = "ℯ"
    pretty BuiltinI      = "ℝ"
    pretty BuiltinF      = "𝓕"
    pretty BuiltinTrue   = "#t"
    pretty BuiltinFalse  = "#f"
    pretty BuiltinSqrt   = "√"
    pretty BuiltinPi     = "𝜋"
    pretty BuiltinGen    = "gen."
    pretty BuiltinRep    = "re:"
    pretty BuiltinScan   = "Λ"
    pretty BuiltinScanS  = "Λₒ"
    pretty BuiltinCons   = "`Cons`"
    pretty BuiltinNil    = "Nil"
    pretty BuiltinMMul   = "%."
    pretty BuiltinVMul   = "%:"
    pretty BuiltinArr    = "Arr"
    pretty BuiltinInt    = "int"
    pretty BuiltinFloat  = "float"
    pretty BuiltinT      = "𝓉"
    pretty BuiltinR      = "𝔯"
    pretty BuiltinSin    = "sin."
    pretty BuiltinCos    = "cos."
    pretty BuiltinTan    = "tan."
    pretty BuiltinCyc    = "cyc."
    pretty BuiltinOdd    = "odd."
    pretty BuiltinEven   = "even."
    pretty BuiltinAbs    = "abs."
    pretty BuiltinD      = "di."

data Token a = EOF { loc :: a }
             | TokSym { loc :: a, sym :: Sym }
             | TokName { loc :: a, _name :: Nm a }
             | TokB { loc :: a, _builtin :: Builtin }
             | TokResVar { loc :: a, _var :: Var }
             | TokInt { loc :: a, int :: Integer }
             | TokFloat { loc :: a, float :: Double }
             deriving (Generic, NFData)

instance Pretty (Token a) where
    pretty EOF{}           = "(eof)"
    pretty (TokSym _ s)    = "symbol" <+> squotes (pretty s)
    pretty (TokName _ n)   = "identifier" <+> squotes (pretty n)
    pretty (TokB _ b)      = "builtin" <+> squotes (pretty b)
    pretty (TokInt _ i)    = pretty i
    pretty (TokResVar _ v) = "reserved variable" <+> squotes (pretty v)
    pretty (TokFloat _ f)  = pretty f

freshName :: T.Text -> Alex (Nm AlexPosn)
freshName t = do
    pos <- get_pos
    newIdentAlex pos t

newIdentAlex :: AlexPosn -> T.Text -> Alex (Nm AlexPosn)
newIdentAlex pos t = do
    st <- get_ust
    let (st', n) = newIdent pos t st
    set_ust st' $> (n $> pos)

newIdent :: AlexPosn -> T.Text -> AlexUserState -> (AlexUserState, Nm AlexPosn)
newIdent pos t pre@(max', names, uniqs) =
    case M.lookup t names of
        Just i -> (pre, Nm t (U i) pos)
        Nothing -> let i = max' + 1
            in let newName = Nm t (U i) pos
                in ((i, M.insert t i names, IM.insert i newName uniqs), newName)

runAlexSt :: BSL.ByteString -> Alex a -> Either String (AlexUserState, a)
runAlexSt inp = withAlexSt inp alexInitUserState

withAlexSt :: BSL.ByteString -> AlexUserState -> Alex a -> Either String (AlexUserState, a)
withAlexSt inp ust (Alex f) = first alex_ust <$> f
    (AlexState { alex_bpos = 0
               , alex_pos = alexStartPos
               , alex_inp = inp
               , alex_chr = '\n'
               , alex_ust = ust
               , alex_scd = 0
               })

}
