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
import Name
import U

}

%wrapper "monadUserState-bytestring"

$digit = [0-9]

$latin = [a-zA-Z]

$greek = [α-ωΑ-Ω]

$mathgreek = [𝛼-𝜛]

$letter = [$latin $greek]

@follow_char = [$letter $digit \_]

@name = ($letter#[Λλ] @follow_char* | $mathgreek)

@exp = e\-?$digit+
@float = $digit+\.$digit+@exp?

tokens :-

    <0> "["                      { mkSym LSqBracket `andBegin` dfn } -- FIXME: this doesn't allow nested

    <dfn> {
        x                        { mkRes VarX }
        y                        { mkRes VarY }
    }

    <braces> {
        "["                      { mkSym LSqBracket }
        "]"                      { mkSym RSqBracket }
        -- FIXME: what if this is in a dfn?
        "}"                      { mkSym RBrace `andBegin` 0 }
        ∘                        { mkSym Compose }
        o                        { mkSym Compose }
    }

    <0,dfn,braces> {
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
        "/l"                     { mkSym Foldl }
        '                        { mkSym Quot }
        `$white*"{"              { mkSym LRank `andBegin` braces }
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
        "}."                     { mkSym Last }
        "}.?"                    { mkSym LastM }
        ⊲                        { mkSym Cons }
        "<|"                     { mkSym Cons }
        ⊳                        { mkSym Snoc }
        "|>"                     { mkSym Snoc }
        "^:"                     { mkSym Do }
        ⊗                        { mkSym Tensor }
        "|:"                     { mkSym Transp }

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
        𝑖                        { mkBuiltin BuiltinI } -- TODO: better as ℝ?
        𝓕                        { mkBuiltin BuiltinF }
        𝓉                        { mkBuiltin BuiltinT }
        "#t"                     { mkBuiltin BuiltinTrue }
        "#f"                     { mkBuiltin BuiltinFalse }
        √                        { mkBuiltin BuiltinSqrt }
        𝜋                        { mkBuiltin BuiltinPi }
        "gen."                   { mkBuiltin BuiltinGen }
        "r:"                     { mkBuiltin BuiltinRep }
        Λ                        { mkBuiltin BuiltinScan }
        "/\"                     { mkBuiltin BuiltinScan }
        "`Cons`"                 { mkBuiltin BuiltinCons }
        Nil                      { mkBuiltin BuiltinNil }
        "%."                     { mkBuiltin BuiltinMMul }
        Arr                      { mkBuiltin BuiltinArr }
        float                    { mkBuiltin BuiltinFloat }
        int                      { mkBuiltin BuiltinInt }
        𝔯                        { mkBuiltin BuiltinR }

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

deriving instance Ord AlexPosn

deriving instance Generic AlexPosn

deriving instance NFData AlexPosn

-- functional bimap?
type AlexUserState = (Int, M.Map T.Text Int, IM.IntMap (Name AlexPosn))

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
         | Last | LastM | TSig | Cons | Snoc | Do | Tensor | Transp
         deriving (Generic, NFData)

instance Pretty Sym where
    pretty Plus         = "+"
    pretty Minus        = "-"
    pretty Percent      = "%"
    pretty Fold         = "/"
    pretty Foldl        = "/l"
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
    pretty Cons         = "⊲"
    pretty Snoc         = "⊳"
    pretty Do           = "^:"
    pretty Tensor       = "⊗"
    pretty Transp       = ":|"

-- | Reserved/special variables
data Var = VarX | VarY deriving (Generic, NFData)

instance Pretty Var where
    pretty VarX     = "x"
    pretty VarY     = "y"

data Builtin = BuiltinFRange | BuiltinIota | BuiltinFloor | BuiltinE | BuiltinI
             | BuiltinF | BuiltinTrue | BuiltinFalse | BuiltinSqrt | BuiltinPi
             | BuiltinGen | BuiltinRep | BuiltinScan | BuiltinCons | BuiltinNil
             | BuiltinMMul | BuiltinArr | BuiltinInt | BuiltinFloat | BuiltinT
             | BuiltinR
             deriving (Generic, NFData)

instance Pretty Builtin where
    pretty BuiltinFRange = "frange"
    pretty BuiltinIota   = "⍳"
    pretty BuiltinFloor  = "⌊"
    pretty BuiltinE      = "ℯ"
    pretty BuiltinI      = "𝑖"
    pretty BuiltinF      = "𝓕"
    pretty BuiltinTrue   = "#t"
    pretty BuiltinFalse  = "#f"
    pretty BuiltinSqrt   = "√"
    pretty BuiltinPi     = "𝜋"
    pretty BuiltinGen    = "gen."
    pretty BuiltinRep    = "r:"
    pretty BuiltinScan   = "Λ"
    pretty BuiltinCons   = "`Cons`"
    pretty BuiltinNil    = "Nil"
    pretty BuiltinMMul   = "%."
    pretty BuiltinArr    = "Arr"
    pretty BuiltinInt    = "int"
    pretty BuiltinFloat  = "float"
    pretty BuiltinT      = "𝓉"
    pretty BuiltinR      = "𝔯"

data Token a = EOF { loc :: a }
             | TokSym { loc :: a, sym :: Sym }
             | TokName { loc :: a, _name :: Name a }
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

freshName :: T.Text -> Alex (Name AlexPosn)
freshName t = do
    pos <- get_pos
    newIdentAlex pos t

newIdentAlex :: AlexPosn -> T.Text -> Alex (Name AlexPosn)
newIdentAlex pos t = do
    st <- get_ust
    let (st', n) = newIdent pos t st
    set_ust st' $> (n $> pos)

newIdent :: AlexPosn -> T.Text -> AlexUserState -> (AlexUserState, Name AlexPosn)
newIdent pos t pre@(max', names, uniqs) =
    case M.lookup t names of
        Just i -> (pre, Name t (U i) pos)
        Nothing -> let i = max' + 1
            in let newName = Name t (U i) pos
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
