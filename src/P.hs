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
         , x86G
         , x86L
         , bytes
         , funP
         , ctxFunP
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
import           IR.Trans
import           L
import           Name
import           Parser
import           Parser.Rw
import           Prettyprinter              (Doc, Pretty (..))
import           R
import           R.Dfn
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

ctxFunP :: (Int, Int) -> BSL.ByteString -> IO (Int, FunPtr a)
ctxFunP ctx = fmap (first BS.length) . (assembleCtx ctx <=< either throwIO pure . x86G)

funP :: BSL.ByteString -> IO (Int, FunPtr a)
funP = aFp <=< either throwIO pure . x86G

bytes :: BSL.ByteString -> Either (Err AlexPosn) BS.ByteString
bytes = fmap assemble . x86G

x86L, x86G :: BSL.ByteString -> Either (Err AlexPosn) [X86 X86Reg FX86Reg ()]
x86G = walloc X86.gallocFrame
x86L = walloc (X86.allocFrame . X86.mkIntervals)

walloc f = fmap (optX86 . f . (\(x, st) -> irToX86 st x)) . ir

ir :: BSL.ByteString -> Either (Err AlexPosn) ([Stmt], WSt)
ir = fmap (f.writeC) . opt where f (s,r,t) = (frees t s,r)

opt :: BSL.ByteString -> Either (Err AlexPosn) (E (T ()))
opt bsl =
    uncurry go <$> parseInline bsl where
    go e = evalState (β'=<<optA=<<β'=<<eta=<<optA e)
    β' e = state (`β` e)

parseInline :: BSL.ByteString -> Either (Err AlexPosn) (E (T ()), Int)
parseInline bsl =
    (\(e, i) -> inline i e) <$> tyParse bsl

tyConstrCtx :: AlexUserState -> BSL.ByteString -> Either (Err AlexPosn) (E (T ()), [(Name AlexPosn, C)], Int)
tyConstrCtx st bsl =
    case parseRenameCtx st bsl of
        Left err       -> Left $ PErr err
        Right (ast, m) -> first TyErr $ tyClosed m ast

tyParseCtx :: AlexUserState -> BSL.ByteString -> Either (Err AlexPosn) (E (T ()), Int)
tyParseCtx st = fmap sel . tyConstrCtx st where sel ~(x, _, z) = (x, z)

tyParse :: BSL.ByteString -> Either (Err AlexPosn) (E (T ()), Int)
tyParse = tyParseCtx alexInitUserState
