module Dbg ( dumpAAbs
           , dumpAarch64
           , dumpAAss
           , dumpX86G
           , dumpX86Abs
           , dumpX86Liveness
           , dumpC
           , dumpCI
           , dumpIR
           , dumpDomTree
           , dumpLoop
           , dumpX86Intervals
           , dumpALiveness
           , dumpAIntervals
           , dumpX86Ass
           , topt
           , nasm
           , module P
           ) where

import           A
import qualified Asm.Aarch64                as Aarch64
import qualified Asm.Aarch64.P              as Aarch64
import           Asm.Aarch64.T
import           Asm.L
import           Asm.LI
import           Asm.M
import qualified Asm.X86                    as X86
import           Asm.X86.P
import           Asm.X86.Trans
import           C
import           C.Alloc
import qualified C.Trans                    as C
import           CF
import           Control.Exception          (throw)
import           Data.Bifunctor             (second)
import qualified Data.ByteString.Lazy       as BSL
import qualified Data.IntMap                as IM
import qualified Data.IntSet                as IS
import qualified Data.Text                  as T
import           Data.Text.Lazy.Builder     (toLazyText)
import           Data.Text.Lazy.Builder.Int (hexadecimal)
import           Data.Tree                  (drawTree)
import           Data.Tuple                 (swap)
import           Data.Word                  (Word8)
import           IR
import           IR.Hoist
import           L
import           P
import           Prettyprinter              (Doc, Pretty (..), comma, concatWith, punctuate, space, (<+>))
import           Prettyprinter.Ext

nasm :: T.Text -> BSL.ByteString -> Doc ann
nasm f = (\(d,i) -> "section .data\n\n" <> nasmD (IM.toList d) <#> i) . second ((prolegomena <#>).pAsm) . either throw id . x86G
    where prolegomena = "section .text\n\nextern malloc\n\nextern free\n\nglobal " <> pretty f <#> pretty f <> ":"

nasmD :: [(Int, [Word8])] -> Doc ann
nasmD = prettyLines . fmap nasmArr
    where nasmArr (i, ds) = "arr_" <> pretty i <+> "db" <+> concatWith (<>) (punctuate comma (fmap hexn ds))
          hexn = pretty.toLazyText.hexadecimal

dumpX86Ass :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpX86Ass = fmap ((\(regs, fregs, _) -> pR regs <#> pR fregs).uncurry gallocOn.(\(x,_,st) -> irToX86 st x)) . ir
    where pR :: Pretty b => IM.IntMap b -> Doc ann; pR = prettyDumpBinds . IM.mapKeysMonotonic (subtract 16)

dumpAAss :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpAAss = fmap ((\(regs, fregs, _) -> pR regs <#> pR fregs).uncurry Aarch64.gallocOn.(\(x,_,st) -> irToAarch64 st x)) . ir
    where pR :: Pretty b => IM.IntMap b -> Doc ann; pR = prettyDumpBinds . IM.mapKeysMonotonic (subtract 19)

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

dumpCI :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpCI = fmap (prettyCI.live.f.C.writeC).opt where f (cs,_,_,_) = cs

prettyCI :: [CS Liveness] -> Doc ann
prettyCI = prettyLines.fmap (pL ((space<>).pretty))

dumpLoop :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpLoop = fmap (pg.loop.π).ir where π (a,_,_)=a; pg (t,ss,_) = lir ss<#>pretty (fmap (\(nϵ,ns) -> nϵ:IS.toList ns) t);

dumpDomTree :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpDomTree = fmap (pg.graphParts.π).ir where π (a,_,_)=a; pg (_,t,asϵ,_) = lir asϵ<#>pretty (drawTree (show<$>t))

lir=prettyLines.fmap (\(s,l) -> pretty (nx l) <> ":" <+> pretty s)

dumpIR :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
dumpIR = fmap (prettyIR.π).ir where π (a,b,_)=(b,a)

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

topt :: BSL.ByteString -> Either (Err AlexPosn) (Doc ann)
topt = fmap prettyTyped . opt
