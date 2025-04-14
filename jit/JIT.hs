module JIT ( CCtx
           , funP, aFunP
           , eFunP, eAFunP
           , ctxFunP, actxFunP
           , pBIO, edAtxt, eDtxt
           , module Dbg
           ) where

import           A
import qualified Asm.Aarch64          as Aarch64
import qualified Asm.Aarch64.Byte     as Aarch64
import qualified Asm.X86              as X86
import           Asm.X86.Byte
import           Control.Exception    (throwIO)
import           Control.Monad        ((<=<))
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as T
import qualified Data.Text.IO         as TIO
import qualified Data.Text.Lazy       as TL
import           Data.Tuple.Extra     (first3)
import           Data.Typeable        (Typeable)
import           Data.Word            (Word8)
import           Dbg
import           Foreign.Ptr          (FunPtr, Ptr)
import           L
import           Prettyprinter        (Pretty)
import           Prettyprinter.Ext
import           Sys.DL
import           System.Info          (arch)

eFunP :: (Pretty a, Typeable a) => Int -> CCtx -> E a -> IO (Int, FunPtr b, Maybe (Ptr Word8))
eFunP = eFunPG assembleCtx ex86G

eAFunP :: (Pretty a, Typeable a) => Int -> (CCtx, MCtx) -> E a -> IO (Int, FunPtr b, Maybe (Ptr Word8))
eAFunP = eFunPG Aarch64.assembleCtx eAarch64

eFunPG jit asm m ctx = fmap (first3 BS.length) . (jit ctx <=< either throwIO pure . asm m)

ctxFunP :: CCtx -> BSL.ByteString -> IO (Int, FunPtr a, Maybe (Ptr Word8))
ctxFunP = ctxFunPG assembleCtx x86G

actxFunP :: (CCtx, MCtx) -> BSL.ByteString -> IO (Int, FunPtr a, Maybe (Ptr Word8))
actxFunP = ctxFunPG Aarch64.assembleCtx aarch64

ctxFunPG jit asm ctx = fmap (first3 BS.length) . (jit ctx <=< either throwIO pure . asm)

funP :: BSL.ByteString -> IO (Int, FunPtr a, Maybe (Ptr Word8))
funP = fmap π.allFp <=< either throwIO pure . x86G

aFunP :: BSL.ByteString -> IO (Int, FunPtr a, Maybe (Ptr Word8))
aFunP = fmap π.Aarch64.allFp <=< either throwIO pure . aarch64

pBIO :: BSL.ByteString -> IO ()
pBIO = either throwIO TIO.putStr <=< case arch of {"x86_64" -> dtxt; "aarch64" -> dAtxt}

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
    where he = T.unwords.fmap (TL.toStrict . tlhex2).BS.unpack

π :: (a, b, c, d) -> (b, c, d)
π (_,y,z,w) = (y,z,w)
