{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Dbg ( dumpAAbs
           , dumpX86G
           , dumpX86Abs
           , dumpX86Liveness
           , dumpIR
           , dumpIRI
           , dumpX86Intervals
           , dumpX86Ass
           , printParsed
           , printTypes
           , topt
           , nasm
           , pBIO, dtxt
           , module P
           ) where

import           A
import           Asm.Aarch64.T
import           Asm.M
import           Asm.X86
import           Asm.X86.Byte
import qualified Asm.X86.CF           as X86
import           Asm.X86.P
import           Asm.X86.Trans
import           CF
import           Control.Exception    (throw, throwIO)
import           Control.Monad        ((<=<))
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap          as IM
import           Data.Semigroup       ((<>))
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           IR
import           IR.Alloc
import           L
import           LI
import           LR
import           Numeric              (showHex)
import           P
import           Prettyprinter        (Doc, Pretty (..))
import           Prettyprinter.Ext
import           Ty

pBIO :: BSL.ByteString -> IO ()
pBIO = either throwIO TIO.putStr <=< dtxt

dtxt :: BSL.ByteString -> IO (Either (Err AlexPosn) T.Text)
dtxt = fmap (fmap (T.unlines.fmap present.uncurry zipS)) . comm . fmap (wIdM dbgFp) . x86G
    where comm :: Either a (IO b) -> IO (Either a b)
          comm (Left err) = pure(Left err)
          comm (Right x)  = Right <$> x
          wIdM :: Functor m => (a -> m b) -> a -> m (a, b)
          wIdM f x = (x,)<$>f x
          zipS [] []             = []
          zipS (x@Label{}:xs) ys = (x,BS.empty):zipS xs ys
          zipS (x:xs) (y:ys)     = (x,y):zipS xs ys

rightPad :: Int -> T.Text -> T.Text
rightPad n str = T.take n (str <> T.replicate n " ")

present :: Pretty a => (a, BS.ByteString) -> T.Text
present (x, b) = rightPad 40 (ptxt x) <> he b
    where he = T.unwords.fmap (pad.T.pack.($"").showHex).BS.unpack
          pad s | T.length s == 1 = T.cons '0' s | otherwise = s

nasm :: T.Text -> BSL.ByteString -> Doc ann
nasm f = (prolegomena <#>) . prettyAsm . either throw id . x86G
    where prolegomena = "section .text\n\nextern malloc\n\nextern free\n\nglobal " <> pretty f <#> pretty f <> ":"

dumpX86Ass :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86Ass = fmap ((\(regs, fregs, _) -> pR regs <#> pR fregs).uncurry gallocOn.(\(x, st) -> irToX86 st x)) . ir
    where pR :: Pretty b => IM.IntMap b -> Doc ann; pR = prettyDumpBinds . IM.mapKeys (subtract 16)

dumpX86G, dumpX86L :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86G = fmap prettyAsm . x86G
dumpX86L = fmap prettyAsm . x86L

dumpX86Abs :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86Abs = fmap (prettyAsm . (\(x, st) -> snd (irToX86 st x))) . ir

dumpAAbs :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpAAbs = fmap (prettyAsm . (\(x, st) -> snd (irToAarch64 st x))) . ir

dumpIR :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpIR = fmap (prettyIR.fst) . ir

dumpIRI :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpIRI = fmap (prettyIRI.live.fst).ir

dumpX86Intervals :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86Intervals = fmap prettyDebugX86 . x86Iv

dumpX86Liveness :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86Liveness = fmap (prettyDebugX86 . fmap (fmap liveness) . reconstruct . X86.mkControlFlow . (\(x, st) -> snd (irToX86 st x))) . ir

x86Iv :: BSL.ByteString -> Either (Err AlexPosn) [X86 AbsReg FAbsReg Interval]
x86Iv = fmap (intervals . reconstruct . X86.mkControlFlow . (\(x, st) -> snd (irToX86 st x))) . ir

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
