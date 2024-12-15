{
    {-# LANGUAGE DeriveGeneric #-}
    {-# LANGUAGE DeriveAnyClass #-}
    {-# LANGUAGE StandaloneDeriving #-}
    module L ( alexMonadScan
             , alexInitUserState
             , withAlexSt
             , freshName
             , newIdent
             , AlexPosn (..)
             , Alex (..)
             , Tok (..)
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
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)
import GHC.Generics (Generic)
import Prettyprinter (Pretty (pretty), (<+>), colon, squotes)
import Nm
import U

}

%wrapper "monadUserState-bytestring"

$digit = [0-9]
$hexit = [0-9a-f]

$latin = [a-zA-Z]

$subscript = [ₐ-ₜ]
$digitsubscript = [₀-₉]

$greek = [α-ωΑ-Ω]

$mathgreek = [𝛢-𝛺𝛼-𝜛]
$mathlatin = [𝐴-𝑍𝑎-𝑧]

$letter = [$latin $greek]
$sub = [$subscript $digitsubscript]

@follow_char = [$letter $digit \_]

-- TODO: M₂,₂ without the space
@name = ($letter#[Λλ] @follow_char* $sub* | $mathgreek $sub* | $mathlatin $sub* | ∫ | 𝛻 | ∇) [′″‴⁗]?

@exp = e\-?$digit+
@float = ($digit+\.$digit+@exp? | $digit+@exp)

tokens :-

    <0> "["                      { mkSym LSqBracket `andBegin` dfn } -- FIXME: this doesn't allow nested

    <0> {
        `$white*"{"              { mkSym LRank `andBegin` braces }
        ⨳$white*"{"              { mkSym LConv `andBegin` braces }
    }

    <dfn> {
        x                        { mkRes VarX }
        y                        { mkRes VarY }
        `$white*"{"              { mkSym LRank `andBegin` dbraces }
        ⨳$white*"{"              { mkSym LConv `andBegin` dbraces }
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
        "/."                     { mkSym IDiv }

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
        ⑄                        { mkSym DIS }
        "\~"                     { mkSym Succ }
        "."                      { mkSym Dot }
        ↦                        { mkSym MMap }
        ";"                      { mkSym Semicolon }
        :                        { mkSym Colon }
        "←"                      { mkSym Bind }
        "<-"                     { mkSym Bind }
        _                        { mkSym Underscore }
        "?"                      { mkSym QuestionMark }
        ",."                     { mkSym CondSplit }
        ⸎                        { mkSym Cor }
        ×                        { mkSym IxTimes }
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
        ⦠                        { mkSym Focus }
        ">@"                     { mkSym Focus }
        🎱                       { mkSym Focus }
        "{."                     { mkSym Head }
        "{.?"                    { mkSym HeadM }
        "}."                     { mkSym Last }
        "}.?"                    { mkSym LastM }
        "{:"                     { mkSym Tail }
        "{:?"                    { mkSym TailM }
        "}:"                     { mkSym Init }
        "}:?"                    { mkSym InitM }
        ⊲                        { mkSym Cons }
        "<|"                     { mkSym Cons }
        ⊳                        { mkSym Snoc }
        "|>"                     { mkSym Snoc }
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
        ">>"                     { mkSym Sr }
        "<<"                     { mkSym Sl }
        ∴                        { mkSym Therefore }
        ⑂                        { mkSym Fork }
        ⋅                        { mkSym Dp }
        ⊖                        { mkSym Rotate }
        ⊙                        { mkSym Cyc }
        ˙                        { mkSym A1 }
        "|"                      { mkSym Mod }
        "@."                     { mkSym AtDot }
        ℘                        { mkSym Weier }
        ⩪                        { mkSym Ice }
        §                        { mkSym Para }
        "#."                     { mkSym Para }
        👁️                        { mkSym Eye }
        "eye."                   { mkSym Eye }
        ♭                        { mkSym B }
        ♯                        { mkSym Sharp }
        ⊻                        { mkSym Xor }
        ∧                        { mkSym And }
        ∨                        { mkSym Or }
        ¬                        { mkSym Not }
        ⅟                        { mkSym Inv }
        ⊂                        { mkSym Sub }
        〃                       { mkSym Ditto }

        "]"                      { mkSym RSqBracket `andBegin` 0 }

        frange                   { mkB BuiltinFRange }
        𝒻                        { mkB BuiltinFRange }
        irange                   { mkB BuiltinIota }
        ⍳                        { mkB BuiltinIota }
        ⌊                        { mkB BuiltinFloor }
        "|."                     { mkB BuiltinFloor }
        ℯ                        { mkB BuiltinE }
        "e:"                     { mkB BuiltinE }
        itof                     { mkB BuiltinI }
        ℝ                        { mkB BuiltinI }
        𝓕                        { mkB BuiltinF }
        𝓉                        { mkB BuiltinT }
        "#t"                     { mkB BuiltinTrue }
        "#f"                     { mkB BuiltinFalse }
        √                        { mkB BuiltinSqrt }
        𝜋                        { mkB BuiltinPi }
        "gen."                   { mkB BuiltinGen }
        "ug."                    { mkB BuiltinUg }
        "cyc."                   { mkB BuiltinCyc }
        "di."                    { mkB BuiltinD }
        Λ                        { mkB BuiltinScan }
        Λₒ                       { mkB BuiltinScanS }
        "/\"                     { mkB BuiltinScan }
        "/\o"                    { mkB BuiltinScanS }
        "`Cons`"                 { mkB BuiltinCons }
        Nil                      { mkB BuiltinNil }
        "%."                     { mkB BuiltinMMul }
        "%:"                     { mkB BuiltinVMul }
        Arr                      { mkB BuiltinArr }
        Vec                      { mkB BuiltinVec }
        M                        { mkB BuiltinM }
        𝟙                        { mkB BuiltinV }
        𝟚                        { mkB BuiltinM }
        𝟘                        { mkB BuiltinInt }
        𝞈                        { mkB BuiltinFloat }
        float                    { mkB BuiltinFloat }
        int                      { mkB BuiltinInt }
        bool                     { mkB BuiltinBool }
        𝔯                        { mkB BuiltinR }
        "rand."                  { mkB BuiltinR }
        "sin."                   { mkB BuiltinSin }
        "cos."                   { mkB BuiltinCos }
        "tan."                   { mkB BuiltinTan }
        "odd."                   { mkB BuiltinOdd }
        "even."                  { mkB BuiltinEven }
        "abs."                   { mkB BuiltinAbs }
        𝐒                        { mkB BuiltinS }
        𝐊                        { mkB BuiltinK }

        _$digit+                 { tok (\p s -> alex $ TokInt p (negate $ read $ ASCII.unpack $ BSL.tail s)) }
        "0x"$hexit+              { tok (\p s -> alex $ TokInt p (hexP $ BSL.drop 2 s)) }
        _"0x"$hexit+             { tok (\p s -> alex $ TokInt p (negate $ hexP $ BSL.drop 3 s)) }
        $digitsubscript+         { tok (\p s -> alex $ TokIx p (parseSubscript $ mkText s)) }

        ¼                        { mkFloat 0.25 }
        ½                        { mkFloat 0.5 }
        ¾                        { mkFloat 0.75 }

        ⅓                        { mkFloat (1/3) }
        ⅕                        { mkFloat 0.2 }
        ⅙                        { mkFloat (1/6) }
        ⅐                        { mkFloat (1/7) }
        ⅛                        { mkFloat 0.125 }
        ⅑                        { mkFloat (1/9) }
        ⅒                        { mkFloat (1/10) }
        ⅔                        { mkFloat (2/3) }
        ⅖                        { mkFloat 0.4 }
        ⅗                        { mkFloat 0.6 }
        ⅘                        { mkFloat 0.8 }
        ⅚                        { mkFloat (5/6) }
        ⅜                        { mkFloat 0.375 }
        ⅝                        { mkFloat 0.625 }
        ⅞                        { mkFloat 0.875 }

        𝔸$digit+                 { tok (\p s -> alex $ TokAdLit p (ad $ BSL.drop 4 s)) }

        @float                   { tok (\p s -> alex $ TokFloat p (read $ ASCII.unpack s)) }
        _@float                  { tok (\p s -> alex $ TokFloat p (negate $ read $ ASCII.unpack $ BSL.tail s)) }

        @name                    { tok (\p s -> TokName p <$> newIdentAlex p (mkText s)) }

    }

{

ad :: BSL.ByteString -> [Integer]
ad = map (fromIntegral.subtract 48) . BSL.unpack

alex :: a -> Alex a
alex = pure

tok f (p,_,s,_) len = f p (BSL.take len s)

constructor c t = tok (\p _ -> alex $ c p t)

mkRes = constructor TokResVar

mkSym = constructor TokSym

mkB = constructor TokB

mkFloat = constructor TokFloat

mkText :: BSL.ByteString -> T.Text
mkText = decodeUtf8 . BSL.toStrict

parseSubscript :: T.Text -> Int
parseSubscript = T.foldl' (\seed c -> 10 * seed + f c) 0
    where f = (subtract 8320).fromEnum

hexP :: BSL.ByteString -> Integer
hexP = ASCII.foldl' (\seed x -> 10 * seed + f x) 0
    where f '0' = 0; f '1' = 1; f '2' = 2; f '3' = 3;
          f '4' = 4; f '5' = 5; f '6' = 6; f '7' = 7;
          f '8' = 8; f '9' = 9; f 'a' = 10; f 'b' = 11
          f 'c' = 12; f 'd' = 13; f 'e'= 14; f 'f'=15
          f c   = error (c:" is not a valid hexit!")

instance Pretty AlexPosn where
    pretty (AlexPn _ line col) = pretty line <> colon <> pretty col

deriving instance Generic AlexPosn

deriving instance NFData AlexPosn

type AlexUserState = (Int, M.Map T.Text Int, IM.IntMap (Nm AlexPosn))

alexInitUserState :: AlexUserState
alexInitUserState = (0, mempty, mempty)

gets_alex :: (AlexState -> a) -> Alex a
gets_alex f = Alex (Right . (id &&& f))

get_pos :: Alex AlexPosn
get_pos = gets_alex alex_pos

alexEOF = EOF <$> get_pos

data Sym = Plus | Minus | Fold | Foldl | Percent | Times | Semicolon | Bind | Pow
         | LSqBracket | RSqBracket | LBrace | RBrace | IxTimes | LParen | RParen | Lam
         | MMap | Dot | Caret | Quot | Zip | Comma | Underscore | QuestionMark | Colon
         | CondSplit | Cor | ArrL | ArrR | SymLog | LBind | PolyBind | LRank | Compose
         | Arrow | Sig | MaxS | MinS | DIS | Succ
         | LConv | Focus
         | Access { iat :: !Int }
         | TSig | Cons | Snoc | Tensor | Transp | PlusPlus | Rotate
         | Last | LastM | Head | HeadM | Tail | TailM | Init | InitM
         | Geq | Gt | Eq | Neq | Leq | Lt
         | FoldA | FoldS | Tilde | Cyc | Ditto | A1 | Sub
         | AtDot | Eye | Para | Weier | Ice | B | Sharp
         | And | Or | Xor | Not | Sr | Sl | IDiv | Inv | Mod
         | Therefore | Fork | Dp
         deriving (Generic, NFData)

instance Pretty Sym where
    pretty Plus         = "+"
    pretty Minus        = "-"
    pretty Percent      = "%"
    pretty IDiv         = "/."
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
    pretty MMap         = "↦"
    pretty Dot          = "."
    pretty Dp           = "⋅"
    pretty Caret        = "^"
    pretty Quot         = "'"
    pretty Zip          = "`"
    pretty Comma        = ","
    pretty Underscore   = "_"
    pretty QuestionMark = "?"
    pretty CondSplit    = ",."
    pretty Cor          = "⸎"
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
    pretty LConv        = "⨳ {"
    pretty Focus        = "⦠"
    pretty (Access i)   = "->" <> pretty i
    pretty Last         = "}."
    pretty LastM        = "}.?"
    pretty Head         = "{."
    pretty HeadM        = "{.?"
    pretty Cons         = "⊲"
    pretty Snoc         = "⊳"
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
    pretty TailM        = "{:?"
    pretty Init         = "}:"
    pretty InitM        = "}:?"
    pretty Rotate       = "⊖"
    pretty Cyc          = "⊙"
    pretty Ditto        = "〃"
    pretty A1           = "˙"
    pretty Mod          = "|"
    pretty AtDot        = "@."
    pretty Eye          = "👁️"
    pretty B            = "♭"
    pretty Sharp        = "♯"
    pretty Xor          = "⊻"
    pretty And          = "∧"
    pretty Or           = "∨"
    pretty Not          = "¬"
    pretty Weier        = "℘"
    pretty Ice          = "⩪"
    pretty Para         = "§"
    pretty IxTimes      = "×"
    pretty Sr           = ">>"
    pretty Sl           = "<<"
    pretty Therefore    = "∴"
    pretty Fork         = "⑂"
    pretty Sub          = "⊂"
    pretty Inv          = "⅟"

-- | Reserved/special variables
data Var = VarX | VarY deriving (Generic, NFData)

instance Pretty Var where
    pretty VarX     = "x"
    pretty VarY     = "y"

data Builtin = BuiltinFRange | BuiltinIota | BuiltinFloor | BuiltinE | BuiltinI
             | BuiltinF | BuiltinTrue | BuiltinFalse | BuiltinSqrt | BuiltinPi
             | BuiltinGen | BuiltinUg | BuiltinScan | BuiltinCons | BuiltinNil
             | BuiltinMMul | BuiltinArr | BuiltinV | BuiltinInt | BuiltinFloat | BuiltinT
             | BuiltinR | BuiltinSin | BuiltinCos | BuiltinScanS | BuiltinTan
             | BuiltinVMul | BuiltinCyc | BuiltinOdd | BuiltinEven | BuiltinAbs
             | BuiltinD | BuiltinVec | BuiltinM | BuiltinBool
             | BuiltinS | BuiltinK
             deriving (Generic, NFData)

instance Pretty Builtin where
    pretty BuiltinFRange = "frange"
    pretty BuiltinIota   = "⍳"
    pretty BuiltinFloor  = "⌊"
    pretty BuiltinE      = "e:"
    pretty BuiltinI      = "ℝ"
    pretty BuiltinF      = "𝓕"
    pretty BuiltinTrue   = "#t"
    pretty BuiltinFalse  = "#f"
    pretty BuiltinSqrt   = "√"
    pretty BuiltinPi     = "𝜋"
    pretty BuiltinGen    = "gen."
    pretty BuiltinUg     = "ug."
    pretty BuiltinScan   = "Λ"
    pretty BuiltinScanS  = "Λₒ"
    pretty BuiltinCons   = "`Cons`"
    pretty BuiltinNil    = "Nil"
    pretty BuiltinMMul   = "%."
    pretty BuiltinVMul   = "%:"
    pretty BuiltinArr    = "Arr"
    pretty BuiltinVec    = "Vec"
    pretty BuiltinM      = "M"
    pretty BuiltinV      = "𝟙"
    pretty BuiltinInt    = "int"
    pretty BuiltinFloat  = "float"
    pretty BuiltinBool   = "bool"
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
    pretty BuiltinS      = "𝐒"
    pretty BuiltinK      = "𝐊"

data Tok = EOF { loc :: AlexPosn }
         | TokSym { loc :: AlexPosn, sym :: !Sym }
         | TokName { loc :: AlexPosn, _name :: !(Nm AlexPosn) }
         | TokIx { loc :: AlexPosn, six :: !Int }
         | TokB { loc :: AlexPosn, _builtin :: !Builtin }
         | TokResVar { loc :: AlexPosn, _var :: !Var }
         | TokInt { loc :: AlexPosn, int :: !Integer }
         | TokFloat { loc :: AlexPosn, float :: !Double }
         | TokAdLit { loc :: AlexPosn, ints :: [Integer] }
         deriving (Generic, NFData)

instance Pretty Tok where
    pretty EOF{}           = "(eof)"
    pretty (TokSym _ s)    = "symbol" <+> squotes (pretty s)
    pretty (TokName _ n)   = "identifier" <+> squotes (pretty n)
    pretty (TokB _ b)      = "builtin" <+> squotes (pretty b)
    pretty (TokInt _ i)    = pretty i
    pretty (TokResVar _ v) = "reserved variable" <+> squotes (pretty v)
    pretty (TokFloat _ f)  = pretty f
    pretty (TokIx _ i)     = pretty (pSubscript i)
    pretty (TokAdLit _ is) = "𝔸" <> foldMap pretty is

pSubscript :: Int -> T.Text
pSubscript i =
    case i `quotRem` 10 of
        (0, d) -> pChar d
        (b, d) -> pSubscript b <> pChar d
  where
    pChar iϵ = T.singleton (toEnum (iϵ+8320))

freshName :: T.Text -> Alex (Nm AlexPosn)
freshName t = do
    pos <- get_pos
    (i, ns, us) <- alexGetUserState
    let (j, n) = freshIdent pos t i
    alexSetUserState (j, ns, us) $> (n$>pos)

newIdentAlex :: AlexPosn -> T.Text -> Alex (Nm AlexPosn)
newIdentAlex pos t = do
    st <- alexGetUserState
    let (st', n) = newIdent pos t st
    alexSetUserState st' $> n

freshIdent :: AlexPosn -> T.Text -> Int -> (Int, Nm AlexPosn)
freshIdent pos t max' =
    let i=max'+1; nm=Nm t (U i) pos
        in (i, nm)

newIdent :: AlexPosn -> T.Text -> AlexUserState -> (AlexUserState, Nm AlexPosn)
newIdent pos t pre@(max', ns, us) =
    case M.lookup t ns of
        Just i  -> (pre, Nm t (U i) pos)
        Nothing -> let i = max'+1; nNm = Nm t (U i) pos
                   in ((i, M.insert t i ns, IM.insert i nNm us), nNm)

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
