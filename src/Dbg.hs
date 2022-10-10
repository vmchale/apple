{-# LANGUAGE OverloadedStrings #-}

module Dbg ( dumpX86G
           , dumpX86L
           , dumpX86Abs
           , dumpX86Liveness
           , dumpIR
           , dumpIRI
           , dumpX86Intervals
           , printParsed
           , printTypes
           , topt
           , nasm
           , pB
           , pBIO
           , module P
           ) where

import           A
import           Asm.X86
import           Asm.X86.Byte
import qualified Asm.X86.CF           as X86
import           Asm.X86.Trans
import           CF
import           Control.Exception    (throw)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import           Data.Semigroup       ((<>))
import qualified Data.Text            as T
import           IR
import           IR.Alloc
import           L
import           LI
import           LR
import           Numeric              (showHex)
import           P
import           Prettyprinter        (Doc, pretty, prettyList)
import           Prettyprinter.Ext
import           Ty

pBIO :: BSL.ByteString -> IO (Either (Err AlexPosn) (Doc ann))
pBIO = fmap (fmap pHex) . comm . fmap dbgFp . x86L
    where comm :: Either a (IO b) -> IO (Either a b)
          comm (Left err) = pure(Left err)
          comm (Right x)  = Right <$> x

pB :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
pB = fmap pHex . bytes

pHex :: BS.ByteString -> Doc ann
pHex = prettyList . fmap (($"").showHex) . BS.unpack

nasm :: T.Text -> BSL.ByteString -> Doc ann
nasm f = (prolegomena <#>) . prettyX86 . either throw id . x86G
    where prolegomena = "section .text\n\nextern malloc\n\nextern free\n\nglobal " <> pretty f <#> pretty f <> ":"

dumpX86G, dumpX86L :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86G = fmap prettyX86 . x86G
dumpX86L = fmap prettyX86 . x86L

dumpX86Abs :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86Abs = fmap (prettyX86 . (\(x, st) -> irToX86 st x)) . ir

dumpIR :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpIR = fmap (prettyIR.fst) . ir

dumpIRI :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpIRI = fmap (prettyIRI.live.fst).ir

dumpX86Intervals :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86Intervals = fmap prettyDebugX86 . x86Iv

dumpX86Liveness :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86Liveness = fmap (prettyDebugX86 . fmap (fmap liveness) . reconstruct . X86.mkControlFlow . (\(x, st) -> irToX86 st x)) . ir

x86Iv :: BSL.ByteString -> Either (Err AlexPosn) [X86 AbsReg FAbsReg Interval]
x86Iv = fmap (intervals . reconstruct . X86.mkControlFlow . (\(x, st) -> irToX86 st x)) . ir

printParsed :: BSL.ByteString -> Doc ann
printParsed = pretty . fst . either throw id . parseRename

-- throws exception
printTypes :: BSL.ByteString -> Doc ann
printTypes bsl =
    case parseRename bsl of
        Left err       -> throw err
        Right (ast, m) -> either throw (prettyTyped.fst3) $ tyClosed m ast
    where fst3 ~(x, _, _) = x

topt :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
topt = fmap prettyTyped . opt
