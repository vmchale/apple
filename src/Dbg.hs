{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Dbg ( dumpAAbs
           , dumpAarch64
           , dumpAAss
           , dumpX86G
           , dumpX86Abs
           , dumpX86Liveness
           , dumpC
           , dumpIR
           , dumpIRI
           , dumpSCC
           , dumpX86Intervals
           , dumpALiveness
           , dumpAIntervals
           , dumpX86Ass
           , printParsed
           , printTypes
           , topt
           , nasm
           , pBIO, dtxt, dAtxt
           , edAtxt, eDtxt
           , module P
           ) where

import           A
import qualified Asm.Aarch64          as Aarch64
import qualified Asm.Aarch64.Byte     as Aarch64
import qualified Asm.Aarch64.P        as Aarch64
import           Asm.Aarch64.T
import           Asm.L
import           Asm.LI
import           Asm.M
import qualified Asm.X86              as X86
import           Asm.X86.Byte
import           Asm.X86.P
import           Asm.X86.Trans
import           C
import           CF
import           Control.Exception    (throw, throwIO)
import           Control.Monad        ((<=<))
import           Data.Bifunctor       (second)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.IntMap          as IM
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import           Data.Tree            (drawTree)
import           Data.Tuple           (swap)
import           Data.Tuple.Extra     (fst3)
import           Data.Word            (Word64)
import           IR
import           IR.Alloc
import           IR.Hoist
import           L
import           Numeric              (showHex)
import           P
import           Prettyprinter        (Doc, Pretty (..), comma, concatWith, punctuate, (<+>))
import           Prettyprinter.Ext
import           Ty

pBIO :: BSL.ByteString -> IO ()
pBIO = either throwIO TIO.putStr <=< dtxt

comm :: Either a (IO b) -> IO (Either a b)
comm (Left err) = pure(Left err)
comm (Right x)  = Right <$> x

wIdM :: Functor m => ((c, a) -> m b) -> (c, a) -> m (a, b)
wIdM f (d, x) = (x,)<$>f (d, x)

dtxt :: BSL.ByteString -> IO (Either (Err AlexPosn) T.Text)
dtxt = asmTxt x86G

eDtxt :: Int -> E a -> IO (Either (Err a) T.Text)
eDtxt k = asmTxt (ex86G k)

asmTxt f = fmap (fmap (T.unlines.fmap present.uncurry zipS)) . comm . fmap (wIdM dbgFp) . f
    where zipS [] []                 = []
          zipS (x@X86.Label{}:xs) ys = (x,BS.empty):zipS xs ys
          zipS (x:xs) (y:ys)         = (x,y):zipS xs ys

edAtxt :: Int -> E a -> IO (Either (Err a) T.Text)
edAtxt k = aAsmTxt (eAarch64 k)

dAtxt :: BSL.ByteString -> IO (Either (Err AlexPosn) T.Text)
dAtxt = aAsmTxt aarch64

aAsmTxt f = fmap (fmap (T.unlines.fmap present.uncurry zipS)) . comm . fmap (wIdM Aarch64.dbgFp) . f
    where zipS [] []                                    = []
          zipS (x@Aarch64.C{}:xs) (y0:y1:y2:y3:y4:ys)   = (x,y0):(x,y1):(x,y2):(x,y3):(x,y4):zipS xs ys
          zipS (x@Aarch64.MovRCf{}:xs) (y0:y1:y2:y3:ys) = (x,y0):(x,y1):(x,y2):(x,y3):zipS xs ys
          zipS (x@Aarch64.LdrRL{}:xs) (y0:y1:y2:y3:ys)  = (x,y0):(x,y1):(x,y2):(x,y3):zipS xs ys
          zipS (x@Aarch64.Label{}:xs) ys                = (x,BS.empty):zipS xs ys
          zipS (x:xs) (y:ys)                            = (x,y):zipS xs ys

rightPad :: Int -> T.Text -> T.Text
rightPad n str = T.take n (str <> T.replicate n " ")

present :: Pretty a => (a, BS.ByteString) -> T.Text
present (x, b) = rightPad 45 (ptxt x) <> he b
    where he = T.unwords.fmap (pad.T.pack.($"").showHex).BS.unpack
          pad s | T.length s == 1 = T.cons '0' s | otherwise = s

nasm :: T.Text -> BSL.ByteString -> Doc ann
nasm f = (\(d,i) -> "section .data\n\n" <> nasmD (IM.toList d) <#> i) . second ((prolegomena <#>).pAsm) . either throw id . x86G
    where prolegomena = "section .text\n\nextern malloc\n\nextern free\n\nglobal " <> pretty f <#> pretty f <> ":"

nasmD :: [(Int, [Word64])] -> Doc ann
nasmD = prettyLines . fmap nasmArr
    where nasmArr (i, ds) = "arr_" <> pretty i <+> "dq" <+> concatWith (<>) (punctuate comma (fmap hexn ds))
          hexn = pretty.($"").(("0x"++).).showHex

dumpX86Ass :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86Ass = fmap ((\(regs, fregs, _) -> pR regs <#> pR fregs).uncurry gallocOn.(\(x,_,st) -> irToX86 st x)) . ir
    where pR :: Pretty b => IM.IntMap b -> Doc ann; pR = prettyDumpBinds . IM.mapKeys (subtract 16)

dumpAAss :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpAAss = fmap ((\(regs, fregs, _) -> pR regs <#> pR fregs).uncurry Aarch64.gallocOn.(\(x,_,st) -> irToAarch64 st x)) . ir
    where pR :: Pretty b => IM.IntMap b -> Doc ann; pR = prettyDumpBinds . IM.mapKeys (subtract 19)

dumpX86G :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86G = fmap prettyAsm . x86G

dumpAarch64 :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpAarch64 = fmap prettyAsm . aarch64

dumpX86Abs :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86Abs = fmap (prettyAsm.(\(x,aa,st) -> (aa,snd (irToX86 st x)))) . ir

dumpAAbs :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpAAbs = fmap (prettyAsm.(\(x,aa,st) -> (aa,snd (irToAarch64 st x)))) . ir

dumpC :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpC = fmap (prettyCS.swap).cmm

-- dumpLoop :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
-- dumpLoop = fmap (pg.loop.π).ir where π (a,_,_)=a; pg (t,ss) = pS ss<#>pretty (show t); pS=prettyDumpBinds.fmap fst

-- dumpHs :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
-- dumpHs = fmap (pretty.show.hs.π).ir where π (a,_,_)=a

dumpSCC :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpSCC = fmap (pg.hoist.π).ir where π (a,_,_)=a; pg (t,asϵ,_) = pS asϵ<#>pretty (drawTree (show<$>t)); pS=prettyLines.fmap (\(s,l) -> pretty (node l) <> ":" <+> pretty s)

dumpIR :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpIR = fmap (prettyIR.π).ir where π (a,b,_)=(b,a)

dumpIRI :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpIRI = fmap (prettyIRI.live.fst3).ir

dumpX86Intervals :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86Intervals = fmap X86.prettyDebugX86 . x86Iv

dumpAIntervals :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpAIntervals = fmap Aarch64.prettyDebug . aarch64Iv

dumpX86Liveness :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86Liveness = fmap (X86.prettyDebugX86 . mkLive . (\(x,_,st) -> snd (irToX86 st x))) . ir

dumpALiveness :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpALiveness = fmap (Aarch64.prettyDebug . mkLive . (\(x,_,st) -> snd (irToAarch64 st x))) . ir

x86Iv :: BSL.ByteString -> Either (Err AlexPosn) [X86.X86 X86.AbsReg X86.FAbsReg Live]
x86Iv = fmap (mkIntervals . (\(x,_,st) -> snd (irToX86 st x))) . ir

aarch64Iv :: BSL.ByteString -> Either (Err AlexPosn) [Aarch64.AArch64 Aarch64.AbsReg Aarch64.FAbsReg Live]
aarch64Iv = fmap (mkIntervals . (\(x,_,st) -> snd (irToAarch64 st x))) . ir

printParsed :: BSL.ByteString -> Doc ann
printParsed = pretty . fst . either throw id . parseRename

-- throws exception
printTypes :: BSL.ByteString -> Doc ann
printTypes bsl =
    case parseRename bsl of
        Left err       -> throw err
        Right (ast, m) -> either throw (prettyTyped.fst3) $ tyClosed m ast

topt :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
topt = fmap prettyTyped . opt
