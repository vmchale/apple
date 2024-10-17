{-# LANGUAGE DeriveGeneric #-}

-- pipeline
module P ( Err (..), FErr (..)
         , CCtx
         , tyParse
         , tyParseCtx
         , tc
         , tyExpr
         , tyOf
         , tyC
         , getTy
         , parseInline
         , parseRename
         , rwP
         , opt
         , ir
         , cmm
         , eDumpC
         , eDumpIR
         , aarch64
         , as, x86G
         , eDumpX86, eDumpAarch64
         , ex86G, eAarch64
         , funP, aFunP
         , eFunP, eAFunP
         , ctxFunP, actxFunP
         ) where

import           A
import           A.Eta
import           A.Opt
import           Asm.Aarch64
import qualified Asm.Aarch64.Byte                 as Aarch64
import qualified Asm.Aarch64.Opt                  as Aarch64
import qualified Asm.Aarch64.P                    as Aarch64
import           Asm.Aarch64.T
import           Asm.M
import           Asm.X86
import           Asm.X86.Byte
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
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Lazy             as BSL
import qualified Data.Text                        as T
import           Data.Tuple.Extra                 (first3)
import           Data.Typeable                    (Typeable)
import           Data.Word                        (Word64)
import           Foreign.Ptr                      (FunPtr, Ptr)
import           GHC.Generics                     (Generic)
import           I
import           IR
import           IR.C
import           IR.Hoist
import           IR.Opt
import           L
import           Nm
import           Parser
import           Parser.Rw
import           Prettyprinter                    (Doc, Pretty (..))
import           Prettyprinter.Ext
import           R.Dfn
import           R.R
import           Sys.DL
import           Ty
import           Ty.M

data FErr a = FE !FilePath (Err a)

instance Pretty a => Pretty (FErr a) where
    pretty (FE fp e) = pretty fp <> ":" <> pretty e

instance Pretty a => Show (FErr a) where show=show.pretty

data Err a = PErr ParseE | TyErr (TyE a) | RErr RE deriving (Generic)

instance Pretty a => Show (Err a) where
    show = show . pretty

instance (Pretty a, Typeable a) => Exception (Err a) where
instance (Pretty a, Typeable a) => Exception (FErr a) where

instance NFData a => NFData (Err a) where

instance Pretty a => Pretty (Err a) where
    pretty (PErr err)  = pretty err
    pretty (TyErr err) = pretty err
    pretty (RErr err)  = pretty err

rwP st = fmap (uncurry renameECtx.second rewrite) . parseWithMaxCtx st

parseRenameCtx :: AlexUserState -> BSL.ByteString -> Either ParseE (E AlexPosn, Int)
parseRenameCtx st = fmap (uncurry renameECtx.second rewrite) . parseWithMaxCtx st

renameECtx :: Int -> E a -> (E a, Int)
renameECtx i ast = rG i (dedfn ast)

parseRename :: BSL.ByteString -> Either ParseE (E AlexPosn, Int)
parseRename = parseRenameCtx alexInitUserState

tyC :: Int -> E a -> Either (Err a) (E (T ()), [(Nm a, C)], Int)
tyC u = (\(e,cs,uϵ) -> (,cs,uϵ)<$>checkM e) <=< first TyErr . tyClosed u

tyExpr :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
tyExpr = fmap prettyC.tyOf

tyOf :: BSL.ByteString -> Either (Err AlexPosn) (T (), [(Nm AlexPosn, C)])
tyOf = fmap (first eAnn) . annTy

getTy :: BSL.ByteString -> Either (Err AlexPosn) (T (), [(Nm AlexPosn, C)])
getTy = fmap (first eAnn) . checkCtx <=< annTy

annTy :: BSL.ByteString -> Either (Err AlexPosn) (E (T ()), [(Nm AlexPosn, C)])
annTy = fmap discard . tyConstrCtx alexInitUserState where discard (x, y, _) = (x, y)

eFunP :: (Pretty a, Typeable a) => Int -> CCtx -> E a -> IO (Int, FunPtr b, Maybe (Ptr Word64))
eFunP = eFunPG assembleCtx ex86G

eAFunP :: (Pretty a, Typeable a) => Int -> (CCtx, MCtx) -> E a -> IO (Int, FunPtr b, Maybe (Ptr Word64))
eAFunP = eFunPG Aarch64.assembleCtx eAarch64

eFunPG jit asm m ctx = fmap (first3 BS.length) . (jit ctx <=< either throwIO pure . asm m)

ctxFunP :: CCtx -> BSL.ByteString -> IO (Int, FunPtr a, Maybe (Ptr Word64))
ctxFunP = ctxFunPG assembleCtx x86G

actxFunP :: (CCtx, MCtx) -> BSL.ByteString -> IO (Int, FunPtr a, Maybe (Ptr Word64))
actxFunP = ctxFunPG Aarch64.assembleCtx aarch64

ctxFunPG jit asm ctx = fmap (first3 BS.length) . (jit ctx <=< either throwIO pure . asm)

funP :: BSL.ByteString -> IO (FunPtr a, Maybe (Ptr Word64))
funP = fmap π.allFp <=< either throwIO pure . x86G

π :: (a, b, c) -> (b, c)
π (_,y,z) = (y,z)

aFunP :: BSL.ByteString -> IO (FunPtr a, Maybe (Ptr Word64))
aFunP = fmap π.Aarch64.allFp <=< either throwIO pure . aarch64

as :: T.Text -> BSL.ByteString -> Doc ann
as f = prolegomena.either throw (second aso).aarch64
    where prolegomena (d,i) = ".p2align 2\n\n.data\n\n" <> pAD d <#> ".text\n\n.global " <> pSym f <#> pSym f <> ":" <#> pAsm i

-- TODO: Call internal
aso (MovRCf () r0 f:Blr () r1:asms) | r0 == r1 = Bl () f:aso asms
aso (asm:asms) = asm:aso asms; aso [] = []

aarch64 :: BSL.ByteString -> Either (Err AlexPosn) (IR.AsmData, [AArch64 AReg FAReg ()])
aarch64 = fmap (second (Aarch64.opt . Aarch64.opt . uncurry Aarch64.gallocFrame).(\(x,aa,st) -> (aa,irToAarch64 st x))) . ir

x86G :: BSL.ByteString -> Either (Err AlexPosn) (IR.AsmData, [X86 X86Reg FX86Reg ()])
x86G = walloc (uncurry X86.gallocFrame)

eAarch64 :: Int -> E a -> Either (Err a) (IR.AsmData, [AArch64 AReg FAReg ()])
eAarch64 i = fmap (second (Aarch64.opt . Aarch64.opt . uncurry Aarch64.gallocFrame).(\(x,aa,st) -> (aa,irToAarch64 st x))) . eir i

ex86G :: Int -> E a -> Either (Err a) (IR.AsmData, [X86 X86Reg FX86Reg ()])
ex86G i = wallocE i (uncurry X86.gallocFrame)

eDumpX86 :: Int -> E a -> Either (Err a) (Doc ann)
eDumpX86 i = fmap prettyAsm . ex86G i

eDumpAarch64 :: Int -> E a -> Either (Err a) (Doc ann)
eDumpAarch64 i = fmap prettyAsm . eAarch64 i

walloc f = fmap (second (optX86.optX86.f) . (\(x,aa,st) -> (aa,irToX86 st x))) . ir
wallocE i f = fmap (second (optX86.optX86.f) . (\(x,aa,st) -> (aa,irToX86 st x))) . eir i

cmm :: BSL.ByteString -> Either (Err AlexPosn) ([CS Liveness], C.AsmData)
cmm = fmap (f.C.writeC).opt where f (cs,_,aa,t)=(frees t cs,aa)

ec :: Int -> E a -> Either (Err a) ([CS Liveness], LSt, C.AsmData)
ec i = fmap ((\(cs,u,aa,t) -> (frees t cs,u,aa)) . C.writeC) . optE i

cir (cs,u,aa,t) = let (s, WSt u' m)=cToIR u (frees t cs); (n,s')=pall u' (optIR s) in (s',aa,WSt n m)

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
eInline m e = (\(eϵ, i) -> inline i eϵ) <$> (checkCtx =<< liftErr (fmap sel (tyClosed m e))) where sel ~(x, _, z) = (x, z); liftErr = first TyErr

checkM :: E (T ()) -> Either (Err a) (E (T ()))
checkM e = maybe (Right e) (Left . RErr) $ check e

checkCtx :: (E (T ()), b) -> Either (Err a) (E (T ()), b)
checkCtx (e, u) = (,u)<$>checkM e

parseInline :: BSL.ByteString -> Either (Err AlexPosn) (E (T ()), Int)
parseInline bsl =
    (\(e, i) -> inline i e) <$> (checkCtx =<< tyParse bsl)

tyConstrCtx :: AlexUserState -> BSL.ByteString -> Either (Err AlexPosn) (E (T ()), [(Nm AlexPosn, C)], Int)
tyConstrCtx st bsl =
    case parseRenameCtx st bsl of
        Left err       -> Left $ PErr err
        Right (ast, m) -> first TyErr $ tyClosed m ast

tyParseCtx :: AlexUserState -> BSL.ByteString -> Either (Err AlexPosn) (E (T ()), Int)
tyParseCtx st = fmap sel . tyConstrCtx st where sel ~(x, _, z) = (x, z)

tc :: FilePath -> IO ()
tc fp = do
    s <- BSL.readFile fp
    either (throwIO.FE fp) (\_ -> pure ()) $ tyParse s

tyParse :: BSL.ByteString -> Either (Err AlexPosn) (E (T ()), Int)
tyParse = tyParseCtx alexInitUserState
