{-# LANGUAGE DeriveGeneric #-}

-- pipeline
module P ( Err (..), FErr (..)
         , tyParse
         , tyParseCtx
         , tc
         , tyExpr
         , tyC
         , getTy
         , rwP
         , opt
         , ir
         , cmm
         , refcard
         , padstr
         , eDumpC
         , eDumpIR
         , aarch64
         , as, x86G
         , eDumpX86, eDumpAarch64
         , ex86G, eAarch64
         ) where

import           A
import           A.Eta
import           A.Opt
import           Asm.Aarch64
import qualified Asm.Aarch64.Opt                  as Aarch64
import qualified Asm.Aarch64.P                    as Aarch64
import           Asm.Aarch64.T
import           Asm.M
import           Asm.Pr
import           Asm.X86
import           Asm.X86.Opt
import qualified Asm.X86.P                        as X86
import           Asm.X86.Trans
import           C
import           C.Alloc
import           C.Trans                          as C
import           CF                               (Liveness)
import           Control.DeepSeq                  (NFData)
import           Control.Exception                (Exception, throw, throwIO)
import           Control.Monad                    ((<=<))
import           Control.Monad.Trans.State.Strict (evalState, state)
import           Data.Bifunctor                   (first, second)
import qualified Data.ByteString.Lazy             as BSL
import qualified Data.Text                        as T
import qualified Data.Text.Lazy.Builder           as B
import           Data.Typeable                    (Typeable)
import           GHC.Generics                     (Generic)
import           I
import           IR
import           IR.C
import           IR.Hoist
import           IR.Opt
import           L
import           Parser
import           Parser.Rw
import           Prettyprinter                    (Doc, Pretty (..))
import           R.R
import           Ty
import           Ty.M

data FErr a = FE !FilePath (Err a)

instance Pretty a => Pretty (FErr a) where
    pretty (FE fp e) = pretty fp <> ":" <> pretty e

instance Pretty a => Show (FErr a) where show=show.pretty

data Err a = PErr ParseE | TyErr (TyE a) | RErr RE deriving (Generic)

instance Pretty a => Show (Err a) where show = show.pretty

instance (Pretty a, Typeable a) => Exception (Err a) where
instance (Pretty a, Typeable a) => Exception (FErr a) where

instance NFData a => NFData (Err a) where

instance Pretty a => Pretty (Err a) where
    pretty (PErr err)  = pretty err; pretty (TyErr err) = pretty err
    pretty (RErr err)  = pretty err

rwP st = fmap (uncurry rG.second rewrite) . parseWithMaxCtx st

tyC :: Int -> E a -> Either (Err a) (E (T ()), Int)
tyC u = (\(e,uϵ) -> (,uϵ)<$>checkM e) <=< first TyErr . tyClosed u

tyExpr :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
tyExpr = fmap (pretty.eAnn.fst).tyParse

getTy :: BSL.ByteString -> Either (Err AlexPosn) (T ())
getTy = fmap (eAnn.fst) . checkCtx <=< tyParse

as :: T.Text -> BSL.ByteString -> B.Builder
as f = prolegomena.either throw (second aso).aarch64
    where prolegomena (d,i) = ".p2align 2\n\n.data\n\n" <> pAD d <#> ".text\n\n.global " <> pSym (B.fromText f) <#> pSym (B.fromText f) <> ":" <#> pAsm i

-- TODO: Call internal
aso (MovRCf () r0 f:Blr () r1:asms) | r0 == r1 = Bl () f:aso asms
aso (asm:asms) = asm:aso asms; aso [] = []

aarch64 :: BSL.ByteString -> Either (Err AlexPosn) (IR.AsmData, [AArch64 AReg FAReg ()])
aarch64 = fmap (second (Aarch64.opt . Aarch64.opt . uncurry Aarch64.gallocFrame).(\(x,aa,st) -> (aa,irToAarch64 st x))) . ir

x86G :: BSL.ByteString -> Either (Err AlexPosn) (IR.AsmData, [X86 X86Reg FX86Reg ()])
x86G = walloc (uncurry X86.gallocFrame)

eAarch64 :: Int -> E a -> Either (Err a) (IR.AsmData, [AArch64 AReg FAReg ()])
eAarch64 i = fmap (second (Aarch64.opt . uncurry Aarch64.gallocFrame).(\(x,aa,st) -> (aa,irToAarch64 st x))) . eir i

ex86G :: Int -> E a -> Either (Err a) (IR.AsmData, [X86 X86Reg FX86Reg ()])
ex86G i = wallocE i (uncurry X86.gallocFrame)

eDumpX86, eDumpAarch64 :: Int -> E a -> Either (Err a) (Doc ann)
eDumpX86 i = fmap prettyAsm . ex86G i
eDumpAarch64 i = fmap prettyAsm . eAarch64 i

walloc f = fmap (second (optX86.optX86.f) . (\(x,aa,st) -> (aa,irToX86 st x))) . ir
wallocE i f = fmap (second (optX86.optX86.f) . (\(x,aa,st) -> (aa,irToX86 st x))) . eir i

cmm :: BSL.ByteString -> Either (Err AlexPosn) ([CS Liveness], C.AsmData)
cmm = fmap (f.C.writeC).opt where f (cs,_,aa,t)=(frees t cs,aa)

ec :: Int -> E a -> Either (Err a) ([CS Liveness], LSt, C.AsmData)
ec i = fmap ((\(cs,u,aa,t) -> (frees t cs,u,aa)) . C.writeC) . optE i

cir (cs,u,aa,t) = let (s, WSt u' m)=cToIR u (frees t cs); (s',n)=hoist u' (optIR s) in (s',aa,WSt n m)

ir :: BSL.ByteString -> Either (Err AlexPosn) ([Stmt], IR.AsmData, WSt)
ir = fmap (cir.C.writeC).opt

eir :: Int -> E a -> Either (Err a) ([Stmt], IR.AsmData, WSt)
eir i = fmap (cir.C.writeC).optE i

eDumpC :: Int -> E a -> Either (Err a) (Doc ann)
eDumpC i = fmap (prettyCS.𝜋).ec i where 𝜋 (a,_,c)=(c,a)

eDumpIR :: Int -> E a -> Either (Err a) (Doc ann)
eDumpIR i = fmap (prettyIR.𝜋) . eir i where 𝜋 (a,b,_)=(b,a)

optE :: Int -> E a -> Either (Err a) (E (T ()))
optE i e = uncurry βηast <$> eInline i e

opt :: BSL.ByteString -> Either (Err AlexPosn) (E (T ()))
opt bsl = uncurry βηast <$> parseInline bsl

βηast e = evalState (β'=<<optA'=<<β'=<<η=<<optA' e) where
  β' eϵ = state (`β` eϵ)
  optA' eϵ = state (\k -> runM k (optA eϵ))

eInline :: Int -> E a -> Either (Err a) (E (T ()), Int)
eInline m e = (\(eϵ, i) -> inline i eϵ) <$> (checkCtx =<< liftErr (tyClosed m e)) where liftErr = first TyErr

checkM :: E (T ()) -> Either (Err a) (E (T ()))
checkM e = maybe (Right e) (Left . RErr) $ check e

checkCtx :: (E (T ()), b) -> Either (Err a) (E (T ()), b)
checkCtx (e, u) = (,u)<$>checkM e

parseInline :: BSL.ByteString -> Either (Err AlexPosn) (E (T ()), Int)
parseInline bsl =
    (\(e, i) -> inline i e) <$> (checkCtx =<< tyParse bsl)

tyParseCtx :: AlexUserState -> BSL.ByteString -> Either (Err AlexPosn) (E (T ()), Int)
tyParseCtx st bsl =
    case rwP st bsl of
        Left err       -> Left $ PErr err
        Right (ast, m) -> first TyErr $ tyClosed m ast

tc :: FilePath -> IO ()
tc fp = do
    s <- BSL.readFile fp
    either (throwIO.FE fp) (\_ -> pure ()) $ tyParse s

tyParse :: BSL.ByteString -> Either (Err AlexPosn) (E (T ()), Int)
tyParse = tyParseCtx alexInitUserState

refcard :: String
refcard = concat
    [ lOption "Λ" "scan" "√" "sqrt"
    , lOption "⋉"  "max" "⋊"  "min"
    , lOption "⍳" "iota" "⌊, ⌈" "floor, ceiling"
    , lOption "e:" "exp" "⨳ {m,n}" "convolve"
    , lOption "\\~" "successive application" "\\`n" "infix"
    , lOption "_." "log" "'" "map"
    , lOption "`" "zip" "`{i,j∘[k,l]}" "rank"
    , lOption "𝒻" "range (real)" "𝜋" "pi"
    , lOption "_" "negate" ":" "size"
    , lOption "𝓉" "dimension" "{x⟜y;z}" "no inline"
    , lOption "->n" "select" "**" "power"
    , lOption "⊂" "scatter" "}." "last"
    , lOption "⊲" "cons" "⊳" "snoc"
    , lOption "^:" "iterate" "%." "matmul"
    , lOption "⊗" "outer product" "⍉, |:" "transpose"
    , lOption "{." "head" "}:" "typesafe init"
    , lOption "⟨z,w⟩" "array literal" "?p,.e1,.e2" "conditional"
    , lOption "/*" "fold all" "ℝ" "i->f conversion"
    , lOption "⧺" "cat" "{:" "typesafe tail"
    , lOption "⊖" "rotate" "sin." "sine"
    , lOption "𝔯" "rand" ".." "(integer) range"
    , lOption "/ₒ" "fold with seed" "Λₒ" "scan with seed"
    , lOption "{x←y;z}" "let...in" "⊙" "cycle"
    , lOption "˙" "at" "|" "rem"
    , lOption "@." "index of" "/." "idiv"
    , lOption "%:" "vector mul" "odd." "parity"
    , lOption "⍋" "sort" "𝓕" "course-of-value recursion"
    , lOption "~" "reverse" "¬,⊻,∧,∨" "logical"
    , lOption "♭" "flatten" "♯" "add dimension"
    , lOption "⩪" "indices of" "§, #." "filter"
    , lOption "ug." "unfold" "(i × j)" "dimensions"
    , lOption "gen." "generate" "}:?" "init"
    , lOption "{:?" "tail" "∴" "compose"
    , lOption "𝔸" "digit literal" "ᶥ" "vector indices"
    , lOption "〃" "ditto" "𝐒,𝐊" "combinators"
    , lOption "⑂" "fork" "𝞈,𝟘,𝟙,𝟚" "fancy types"
    , lOption "℘" "partition" "refl." "efficient odd function"
    ]
  where
    lOption op0 desc0 op1 desc1 =
        padstr 14 op0 ++ padstr 25 desc0 ++ padstr 14 op1 ++ desc1 ++ "\n"

padstr :: Int -> String -> String
padstr n str = take n $ str ++ repeat ' '
