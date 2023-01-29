{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE FlexibleContexts #-}

-- pipeline
module P ( Err (..)
         , tyParse
         , tyParseCtx
         , tyExpr
         , tyExprCtx
         , tyOf
         , parseInline
         , parseRename
         , opt
         , ir
         , irCtx
         , x86G
         , x86GDef
         , x86L
         , bytes
         , funP
         , ctxFunP
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

parseRenameCtx :: AlexUserState -> BSL.ByteString -> Either (ParseE AlexPosn) (E AlexPosn, Int)
parseRenameCtx st = fmap (go.second rewrite) . parseWithMaxCtx st where
    go (i, ast) = let (e, m) = dedfn i ast in rG m e

parseRename :: BSL.ByteString -> Either (ParseE AlexPosn) (E AlexPosn, Int)
parseRename = parseRenameCtx alexInitUserState

tyExprCtx :: AlexUserState -> BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
tyExprCtx st = fmap prettyC.tyOfCtx st

tyExpr :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
tyExpr = tyExprCtx alexInitUserState

tyOfCtx :: AlexUserState -> BSL.ByteString -> Either (Err AlexPosn) (T (), [(Name AlexPosn, C)])
tyOfCtx st = fmap (first eAnn.discard) . tyConstrCtx st where discard (x, y, _) = (x, y)

tyOf :: BSL.ByteString -> Either (Err AlexPosn) (T (), [(Name AlexPosn, C)])
tyOf = tyOfCtx alexInitUserState

ctxFunDef :: (Int, Int) -> BSL.ByteString -> IO (Int, FunPtr a)
ctxFunDef = ctxFunP alexInitUserState

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

walloc lSt f = fmap (optX86 . f . (\(x, st) -> irToX86 st x)) . irCtx lSt

ir :: BSL.ByteString -> Either (Err AlexPosn) ([Stmt], WSt)
ir = irCtx alexInitUserState

irCtx :: AlexUserState -> BSL.ByteString -> Either (Err AlexPosn) ([Stmt], WSt)
irCtx st = fmap (f.writeC) . opt st where f (s,r,t) = (frees t (optIR s),r)

opt :: AlexUserState -> BSL.ByteString -> Either (Err AlexPosn) (E (T ()))
opt st bsl =
    uncurry go <$> parseInline st bsl where
    go e = evalState (β'=<<optA'=<<β'=<<eta=<<optA' e)
    β' e = state (`β` e)
    optA' e = state (\k -> runM k (optA e))

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
