{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

-- pipeline
module P ( Err (..)
         , tyParse
         , tyParseCtx
         , tyExpr
         , tyOf
         , getTy
         , parseInline
         , parseRename
         , rwP
         , opt
         , ir
         , eDumpC
         , eDumpIR
         , aarch64
         , as, x86G
         , eDumpX86, eDumpAarch64
         , ex86G, eAarch64
         , bytes
         , funP, aFunP
         , eFunP, eAFunP
         , ctxFunP, actxFunP
         ) where

import           A
import           A.Eta
import           A.Opt
import           Asm.Aarch64
import qualified Asm.Aarch64.Byte           as Aarch64
import qualified Asm.Aarch64.Opt            as Aarch64
import qualified Asm.Aarch64.P              as Aarch64
import           Asm.Aarch64.T
import           Asm.M
import           Asm.X86
import           Asm.X86.Byte
import           Asm.X86.Opt
import qualified Asm.X86.P                  as X86
import           Asm.X86.Trans
import           C
import           C.Trans                    as C
import           Control.DeepSeq            (NFData)
import           Control.Exception          (Exception, throw, throwIO)
import           Control.Monad              ((<=<))
import           Control.Monad.State.Strict (evalState, state)
import           Data.Bifunctor             (first, second)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.IntMap                as IM
import qualified Data.Text                  as T
import           Data.Tuple.Extra           (first3)
import           Data.Typeable              (Typeable)
import           Data.Word                  (Word64)
import           Foreign.Ptr                (FunPtr, Ptr)
import           GHC.Generics               (Generic)
import           I
import           IR
import           IR.Alloc
import           IR.C
import           IR.Opt
import           L
import           Nm
import           Parser
import           Parser.Rw
import           Prettyprinter              (Doc, Pretty (..))
import           Prettyprinter.Ext
import           R.Dfn
import           R.R
import           Ty
import           Ty.M

data Err a = PErr (ParseE a) | TyErr (TyE a) | RErr RE deriving (Generic)

instance Pretty a => Show (Err a) where
    show = show . pretty

instance (Pretty a, Typeable a) => Exception (Err a) where

instance NFData a => NFData (Err a) where

instance Pretty a => Pretty (Err a) where
    pretty (PErr err)  = pretty err
    pretty (TyErr err) = pretty err
    pretty (RErr err)  = pretty err

rwP st = fmap (uncurry renameECtx.second rewrite) . parseWithMaxCtx st

parseRenameCtx :: AlexUserState -> BSL.ByteString -> Either (ParseE AlexPosn) (E AlexPosn, Int)
parseRenameCtx st = fmap (uncurry renameECtx.second rewrite) . parseWithMaxCtx st

renameECtx :: Int -> E a -> (E a, Int)
renameECtx i ast = let (e, m) = dedfn i ast in rG m e

parseRename :: BSL.ByteString -> Either (ParseE AlexPosn) (E AlexPosn, Int)
parseRename = parseRenameCtx alexInitUserState

tyExpr :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
tyExpr = fmap prettyC.tyOf

tyOf :: BSL.ByteString -> Either (Err AlexPosn) (T (), [(Nm AlexPosn, C)])
tyOf = fmap (first eAnn) . annTy

getTy :: BSL.ByteString -> Either (Err AlexPosn) (T (), [(Nm AlexPosn, C)])
getTy = fmap (first eAnn) . eCheck <=< annTy

annTy :: BSL.ByteString -> Either (Err AlexPosn) (E (T ()), [(Nm AlexPosn, C)])
annTy = fmap discard . tyConstrCtx alexInitUserState where discard (x, y, _) = (x, y)

eFunP :: (Pretty a, Typeable a) => Int -> (Int, Int) -> E a -> IO (Int, FunPtr b, Maybe (Ptr Word64))
eFunP = eFunPG assembleCtx ex86G

eAFunP :: (Pretty a, Typeable a) => Int -> (Int, Int) -> E a -> IO (Int, FunPtr b, Maybe (Ptr Word64))
eAFunP = eFunPG Aarch64.assembleCtx eAarch64

eFunPG jit asm m ctx = fmap (first3 BS.length) . (jit ctx <=< either throwIO pure . asm m)

ctxFunP :: (Int, Int) -> BSL.ByteString -> IO (Int, FunPtr a, Maybe (Ptr Word64))
ctxFunP = ctxFunPG assembleCtx x86G

actxFunP :: (Int, Int) -> BSL.ByteString -> IO (Int, FunPtr a, Maybe (Ptr Word64))
actxFunP = ctxFunPG Aarch64.assembleCtx aarch64

ctxFunPG jit asm ctx = fmap (first3 BS.length) . (jit ctx <=< either throwIO pure . asm)

funP :: BSL.ByteString -> IO (FunPtr a, Maybe (Ptr Word64))
funP = fmap π.allFp <=< either throwIO pure . x86G

π :: (a, b, c) -> (b, c)
π (_,y,z) = (y,z)

aFunP :: BSL.ByteString -> IO (FunPtr a, Maybe (Ptr Word64))
aFunP = fmap π.Aarch64.allFp <=< either throwIO pure . aarch64

bytes :: BSL.ByteString -> Either (Err AlexPosn) BS.ByteString
bytes = fmap assemble . x86G

as :: T.Text -> BSL.ByteString -> Doc ann
as f = (prolegomena <#>) . prettyAsm . either throw id . aarch64
    where prolegomena = ".p2align 2\n\n.text\n\n.global _" <> pretty f <#> "_" <> pretty f <> ":"

aarch64 :: BSL.ByteString -> Either (Err AlexPosn) (IR.AsmData, [AArch64 AReg FAReg ()])
aarch64 = fmap (second (Aarch64.opt . uncurry Aarch64.gallocFrame).(\(x,aa,st) -> (aa,irToAarch64 st x))) . ir

x86G :: BSL.ByteString -> Either (Err AlexPosn) (IR.AsmData, [X86 X86Reg FX86Reg ()])
x86G = walloc (uncurry X86.gallocFrame)

eAarch64 :: Int -> E a -> Either (Err a) (IR.AsmData, [AArch64 AReg FAReg ()])
eAarch64 i = fmap (second (Aarch64.opt . uncurry Aarch64.gallocFrame).(\(x,aa,st) -> (aa,irToAarch64 st x))) . eir i

ex86G :: Int -> E a -> Either (Err a) (IR.AsmData, [X86 X86Reg FX86Reg ()])
ex86G i = wallocE i (uncurry X86.gallocFrame)

eDumpX86 :: Int -> E a -> Either (Err a) (Doc ann)
eDumpX86 i = fmap prettyAsm . ex86G i

eDumpAarch64 :: Int -> E a -> Either (Err a) (Doc ann)
eDumpAarch64 i = fmap prettyAsm . eAarch64 i

walloc f = fmap (second (optX86.optX86.f) . (\(x,aa,st) -> (aa,irToX86 st x))) . ir
wallocE i f = fmap (second (optX86.optX86.f) . (\(x,aa,st) -> (aa,irToX86 st x))) . eir i

ec :: Int -> E a -> Either (Err a) ([CS], LSt, C.AsmData, IM.IntMap C.Temp)
ec i = fmap C.writeC . optE i

ir :: BSL.ByteString -> Either (Err AlexPosn) ([Stmt], IR.AsmData, WSt)
ir = fmap (f.C.writeC).opt where f (cs,u,aa,t) = let (s,u')=cToIR u cs in (frees (ctemp<$>t) (optIR s),aa,u')

eir :: Int -> E a -> Either (Err a) ([Stmt], IR.AsmData, WSt)
eir i = fmap (f.C.writeC).optE i where f (cs,u,aa,t) = let (s,u')=cToIR u cs in (frees (ctemp<$>t) (optIR s),aa,u')

eDumpC :: Int -> E a -> Either (Err a) (Doc ann)
eDumpC i = fmap (prettyCS.𝜋).ec i where 𝜋 (a,_,c,_)=(c,a)

eDumpIR :: Int -> E a -> Either (Err a) (Doc ann)
eDumpIR i = fmap (prettyIR.𝜋) . eir i where 𝜋 (a,b,_)=(b,a)

optE :: Int -> E a -> Either (Err a) (E (T ()))
optE i e =
  uncurry go <$> eInline i e where
  go eϵ = evalState (β'=<<optA'=<<β'=<<η=<<optA' eϵ)
  β' eϵ = state (`β` eϵ)
  optA' eϵ = state (\k -> runM k (optA eϵ))

opt :: BSL.ByteString -> Either (Err AlexPosn) (E (T ()))
opt bsl =
    uncurry go <$> parseInline bsl where
    go e = evalState (β'=<<optA'=<<β'=<<η=<<optA' e)
    β' e = state (`β` e)
    optA' e = state (\k -> runM k (optA e))

eInline :: Int -> E a -> Either (Err a) (E (T ()), Int)
eInline m e = (\(eϵ, i) -> inline i eϵ) <$> (eCheck =<< liftErr (fmap sel (tyClosed m e))) where sel ~(x, _, z) = (x, z); liftErr = first TyErr

eCheck :: (E (T ()), b) -> Either (Err a) (E (T ()), b)
eCheck e = maybe (Right e) (Left . RErr) $ check (fst e)

parseInline :: BSL.ByteString -> Either (Err AlexPosn) (E (T ()), Int)
parseInline bsl =
    (\(e, i) -> inline i e) <$> (eCheck =<< tyParse bsl)

tyConstrCtx :: AlexUserState -> BSL.ByteString -> Either (Err AlexPosn) (E (T ()), [(Nm AlexPosn, C)], Int)
tyConstrCtx st bsl =
    case parseRenameCtx st bsl of
        Left err       -> Left $ PErr err
        Right (ast, m) -> first TyErr $ tyClosed m ast

tyParseCtx :: AlexUserState -> BSL.ByteString -> Either (Err AlexPosn) (E (T ()), Int)
tyParseCtx st = fmap sel . tyConstrCtx st where sel ~(x, _, z) = (x, z)

tyParse :: BSL.ByteString -> Either (Err AlexPosn) (E (T ()), Int)
tyParse = tyParseCtx alexInitUserState
