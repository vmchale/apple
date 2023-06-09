{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

module Dbg ( dumpAAbs
           , dumpAarch64
           , dumpAAss
           , dumpX86G
           , dumpX86Abs
           , dumpX86Liveness
           , dumpIR
           , dumpIRI
           , dumpX86Intervals
           , dumpX86BB
           , dumpX86BBL
           , dumpABB
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
import           Asm.Ar
import           Asm.BB
import           Asm.L
import           Asm.LI
import           Asm.M
import qualified Asm.X86              as X86
import           Asm.X86.Byte
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
import           Numeric              (showHex)
import           P
import           Prettyprinter        (Doc, Pretty (..), concatWith)
import           Prettyprinter.Ext
import           Ty

pBIO :: BSL.ByteString -> IO ()
pBIO = either throwIO TIO.putStr <=< dtxt

comm :: Either a (IO b) -> IO (Either a b)
comm (Left err) = pure(Left err)
comm (Right x)  = Right <$> x

wIdM :: Functor m => (a -> m b) -> a -> m (a, b)
wIdM f x = (x,)<$>f x

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
          zipS (x@Aarch64.MovRCf{}:xs) (y0:y1:y2:y3:ys) = (x,y0):(x,y1):(x,y2):(x,y3):zipS xs ys
          zipS (x@Aarch64.Label{}:xs) ys                = (x,BS.empty):zipS xs ys
          zipS (x:xs) (y:ys)                            = (x,y):zipS xs ys

rightPad :: Int -> T.Text -> T.Text
rightPad n str = T.take n (str <> T.replicate n " ")

present :: Pretty a => (a, BS.ByteString) -> T.Text
present (x, b) = rightPad 45 (ptxt x) <> he b
    where he = T.unwords.fmap (pad.T.pack.($"").showHex).BS.unpack
          pad s | T.length s == 1 = T.cons '0' s | otherwise = s

nasm :: T.Text -> BSL.ByteString -> Doc ann
nasm f = (prolegomena <#>) . prettyAsm . either throw id . x86G
    where prolegomena = "section .text\n\nextern malloc\n\nextern free\n\nglobal " <> pretty f <#> pretty f <> ":"

dumpX86Ass :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86Ass = fmap ((\(regs, fregs, _) -> pR regs <#> pR fregs).uncurry gallocOn.(\(x, st) -> irToX86 st x)) . ir
    where pR :: Pretty b => IM.IntMap b -> Doc ann; pR = prettyDumpBinds . IM.mapKeys (subtract 16)

dumpAAss :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpAAss = fmap ((\(regs, fregs, _) -> pR regs <#> pR fregs).uncurry Aarch64.gallocOn.(\(x, st) -> irToAarch64 st x)) . ir
    where pR :: Pretty b => IM.IntMap b -> Doc ann; pR = prettyDumpBinds . IM.mapKeys (subtract 19)

dumpX86G :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86G = fmap prettyAsm . x86G

dumpAarch64 :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpAarch64 = fmap prettyAsm . aarch64

dumpX86Abs :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86Abs = fmap (prettyAsm . (\(x, st) -> snd (irToX86 st x))) . ir

dumpAAbs :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpAAbs = fmap (prettyAsm . (\(x, st) -> snd (irToAarch64 st x))) . ir

dumpIR :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpIR = fmap (prettyIR.fst) . ir

dumpIRI :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpIRI = fmap (prettyIRI.live.fst).ir

dumpX86Intervals :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86Intervals = fmap X86.prettyDebugX86 . x86Iv

dumpAIntervals :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpAIntervals = fmap Aarch64.prettyDebug . aarch64Iv

dumpX86Liveness :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86Liveness = fmap (X86.prettyDebugX86 . mkLive . (\(x, st) -> snd (irToX86 st x))) . ir

dumpALiveness :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpALiveness = fmap (Aarch64.prettyDebug . mkLive . (\(x, st) -> snd (irToAarch64 st x))) . ir

dumpABB :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpABB = fmap (prettyBBLs . liveBB . (\(x, st) -> snd (irToAarch64 st x))) . ir

dumpX86BBL :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86BBL = fmap (prettyBBLs . liveBB . (\(x, st) -> snd (irToX86 st x))) . ir

dumpX86BB :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86BB = fmap (prettyBBs . bb . (\(x, st) -> snd (irToX86 st x))) . ir

prettyBBs = concatWith (\x y -> x <#> "==BB==" <#> y) . fmap (\(BB asms _) -> prettyLines (pretty <$> asms))

prettyBBLs :: Pretty (arch reg freg ()) => [BB arch reg freg () Liveness] -> Doc ann
prettyBBLs = prettyLines . fmap prettyBBL

prettyBBL :: Pretty (arch reg freg ()) => BB arch reg freg () Liveness -> Doc ann
prettyBBL (BB asms l) = pretty l <#> prettyLines (fmap pretty asms)

x86Iv :: BSL.ByteString -> Either (Err AlexPosn) [X86.X86 X86.AbsReg X86.FAbsReg Interval]
x86Iv = fmap (mkIntervals . (\(x, st) -> snd (irToX86 st x))) . ir

aarch64Iv :: BSL.ByteString -> Either (Err AlexPosn) [Aarch64.AArch64 Aarch64.AbsReg Aarch64.FAbsReg Interval]
aarch64Iv = fmap (mkIntervals . (\(x, st) -> snd (irToAarch64 st x))) . ir

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
