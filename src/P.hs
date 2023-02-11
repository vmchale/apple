{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}

-- pipeline
module P ( Err (..)
         , tyParse
         , tyParseCtx
         , tyExpr
         , tyOf
         , parseInline
         , parseRename
         , rwP
         , opt
         , ir
         , irCtx
         , eDumpIR
         , x86G
         , x86GDef
         , x86L
         , eDumpX86
         , bytes
         , funP
         , eFunP
         , ctxFunDef
         ) where

import           A
import           A.Eta
import           A.Opt
import           Asm.X86
import qualified Asm.X86.Alloc              as X86
import           Asm.X86.Byte
import qualified Asm.X86.LI                 as X86
import           Asm.X86.Opt
import qualified Asm.X86.P                  as X86
import           Asm.X86.Trans
import           Control.DeepSeq            (NFData)
import           Control.Exception          (Exception, throwIO)
import           Control.Monad              ((<=<))
import           Control.Monad.State.Strict (evalState, state)
import           Data.Bifunctor             (first, second)
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import           Data.Typeable              (Typeable)
import           Foreign.Ptr                (FunPtr)
import           GHC.Generics               (Generic)
import           I
import           IR
import           IR.Alloc
import           IR.Opt
import           IR.Trans
import           L
import           Name
import           Parser
import           Parser.Rw
import           Prettyprinter              (Doc, Pretty (..))
import           R.Dfn
import           R.R
import           Ty

data Err a = PErr (ParseE a) | TyErr (TyE a) deriving (Generic)

instance Pretty a => Show (Err a) where
    show = show . pretty

instance (Pretty a, Typeable a) => Exception (Err a) where

instance NFData a => NFData (Err a) where

instance Pretty a => Pretty (Err a) where
    pretty (PErr err)  = pretty err
    pretty (TyErr err) = pretty err

rwP st = fmap (uncurry renameECtx.second rewrite) . parseWithMaxCtx st

parseRenameCtx :: AlexUserState -> BSL.ByteString -> Either (ParseE AlexPosn) (E AlexPosn, Int)
parseRenameCtx st = fmap (uncurry renameECtx.second rewrite) . parseWithMaxCtx st

renameECtx :: Int -> E a -> (E a, Int)
renameECtx i ast = let (e, m) = dedfn i ast in rG m e

parseRename :: BSL.ByteString -> Either (ParseE AlexPosn) (E AlexPosn, Int)
parseRename = parseRenameCtx alexInitUserState

tyExpr :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
tyExpr = fmap prettyC.tyOf

tyOf :: BSL.ByteString -> Either (Err AlexPosn) (T (), [(Name AlexPosn, C)])
tyOf = fmap (first eAnn.discard) . tyConstrCtx alexInitUserState where discard (x, y, _) = (x, y)

ctxFunDef :: (Int, Int) -> BSL.ByteString -> IO (Int, FunPtr a)
ctxFunDef = ctxFunP alexInitUserState

eFunP :: (Pretty a, Typeable a) => Int -> (Int, Int) -> E a -> IO (Int, FunPtr a)
eFunP m ctx = fmap (first BS.length) . (assembleCtx ctx <=< either throwIO pure . ex86G m)

ctxFunP :: AlexUserState -> (Int, Int) -> BSL.ByteString -> IO (Int, FunPtr a)
ctxFunP st ctx = fmap (first BS.length) . (assembleCtx ctx <=< either throwIO pure . x86G st)

funP :: BSL.ByteString -> IO (Int, FunPtr a)
funP = aFp <=< either throwIO pure . x86G alexInitUserState

bytes :: BSL.ByteString -> Either (Err AlexPosn) BS.ByteString
bytes = fmap assemble . x86G alexInitUserState

x86GDef :: BSL.ByteString -> Either (Err AlexPosn) [X86 X86Reg FX86Reg ()]
x86GDef = x86G alexInitUserState

x86L, x86G :: AlexUserState -> BSL.ByteString -> Either (Err AlexPosn) [X86 X86Reg FX86Reg ()]
x86G st = walloc st (uncurry X86.gallocFrame)
x86L st = walloc st (X86.allocFrame . X86.mkIntervals . snd)

ex86G :: Int -> E a -> Either (TyE a) [X86 X86Reg FX86Reg ()]
ex86G i = wallocE i (uncurry X86.gallocFrame)

eDumpX86 :: Int -> E a -> Either (TyE a) (Doc ann)
eDumpX86 i = fmap prettyX86 . ex86G i

walloc lSt f = fmap (optX86 . f . (\(x, st) -> irToX86 st x)) . irCtx lSt
wallocE i f = fmap (optX86 . f . (\(x, st) -> irToX86 st x)) . eir i

ir :: BSL.ByteString -> Either (Err AlexPosn) ([Stmt], WSt)
ir = irCtx alexInitUserState

irCtx :: AlexUserState -> BSL.ByteString -> Either (Err AlexPosn) ([Stmt], WSt)
irCtx st = fmap (f.writeC) . opt st where f (s,r,t) = (frees t (optIR s),r)

eir :: Int -> E a -> Either (TyE a) ([Stmt], WSt)
eir i = fmap (f.writeC) . optE i where f (s,r,t) = (frees t (optIR s),r)

eDumpIR :: Int -> E a -> Either (TyE a) (Doc ann)
eDumpIR i = fmap (prettyIR.fst) . eir i

optE :: Int -> E a -> Either (TyE a) (E (T ()))
optE i e =
  uncurry go <$> eInline i e where
  go eϵ = evalState (β'=<<optA'=<<β'=<<eta=<<optA' eϵ)
  β' eϵ = state (`β` eϵ)
  optA' eϵ = state (\k -> runM k (optA eϵ))

opt :: AlexUserState -> BSL.ByteString -> Either (Err AlexPosn) (E (T ()))
opt st bsl =
    uncurry go <$> parseInline st bsl where
    go e = evalState (β'=<<optA'=<<β'=<<eta=<<optA' e)
    β' e = state (`β` e)
    optA' e = state (\k -> runM k (optA e))

eInline :: Int -> E a -> Either (TyE a) (E (T ()), Int)
eInline m e = (\(eϵ, i) -> inline i eϵ) . sel <$> tyClosed m e where sel ~(x, _, z) = (x, z)

parseInline :: AlexUserState -> BSL.ByteString -> Either (Err AlexPosn) (E (T ()), Int)
parseInline st bsl =
    (\(e, i) -> inline i e) <$> tyParseCtx st bsl

tyConstrCtx :: AlexUserState -> BSL.ByteString -> Either (Err AlexPosn) (E (T ()), [(Name AlexPosn, C)], Int)
tyConstrCtx st bsl =
    case parseRenameCtx st bsl of
        Left err       -> Left $ PErr err
        Right (ast, m) -> first TyErr $ tyClosed m ast

tyParseCtx :: AlexUserState -> BSL.ByteString -> Either (Err AlexPosn) (E (T ()), Int)
tyParseCtx st = fmap sel . tyConstrCtx st where sel ~(x, _, z) = (x, z)

tyParse :: BSL.ByteString -> Either (Err AlexPosn) (E (T ()), Int)
tyParse = tyParseCtx alexInitUserState
