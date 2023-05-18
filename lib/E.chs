module E () where

import CGen
import Control.Monad (zipWithM_)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Unsafe as BS
import Data.Coerce (coerce)
import Data.Functor (($>))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Dbg
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..), CSize (..), CChar)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (IntPtr(..), Ptr, castPtr, castFunPtrToPtr, nullPtr)
import Foreign.Storable (poke, pokeByteOff, sizeOf)
import Prettyprinter (Doc, Pretty)
import Prettyprinter.Ext
import System.Info (arch)

#include <string.h>
#include <sys/mman.h>
#include <apple.h>

data FnTy

{# fun memcpy as ^ { castPtr `Ptr a', castPtr `Ptr a', coerce `CSize' } -> `Ptr a' castPtr #}

-- how tf do C weenies store like... function types??
{# enum apple_t as CT {} #}

ct :: CType -> CT
ct CR = F_t
ct CI = I_t
ct Af = FA
ct Ai = IA

t32 :: CType -> CInt
t32 = fromIntegral.fromEnum.ct

tcstr :: T.Text -> IO CString
tcstr t =
    BS.unsafeUseAsCStringLen (encodeUtf8 t) $ \(bs,sz) -> do
        p <- mallocBytes (sz+1)
        _ <- memcpy p bs (fromIntegral sz)
        pokeByteOff p sz (0::CChar) $> p

harnessString :: Pretty a => (BSL.ByteString -> Either a (Doc ann)) -> CString -> Ptr CString -> IO CString
harnessString oup src errPtr = do
    bSrc <- BS.unsafePackCString src
    case oup (BSL.fromStrict bSrc) of
        Left err ->
            (poke errPtr =<< tcstr (ptxt err)) $> nullPtr
        Right d -> tcstr (aText d)

apple_dumpasm, apple_x86, apple_aarch64 :: CString -> Ptr CString -> IO CString
apple_dumpasm = case arch of {"aarch64" -> apple_aarch64; "x86_64" -> apple_x86}

apple_x86 = harnessString dumpX86G

apple_aarch64 = harnessString dumpAarch64

apple_dumpir :: CString -> Ptr CString -> IO CString
apple_dumpir = harnessString dumpIR

apple_printty :: CString -> Ptr CString -> IO CString
apple_printty = harnessString tyExpr

apple_ty :: CString -> Ptr CString -> IO (Ptr FnTy)
apple_ty src errPtr = do
    bSrc <- BS.unsafePackCString src
    let b = getTy (BSL.fromStrict bSrc)
    case b of
        Left err -> do
            poke errPtr =<< tcstr (ptxt err)
            pure nullPtr
        Right (t, []) ->
            case tCTy t of
                Left te -> do {poke errPtr =<< tcstr (ptxt te); pure nullPtr}
                Right (tis, to) -> do
                    let argc = length tis
                    sp <- mallocBytes {# sizeof FnTy #}
                    ip <- mallocBytes (argc * sizeOf (undefined::CInt))
                    {# set FnTy.argc #} sp (fromIntegral argc)
                    {# set FnTy.res #} sp (t32 to)
                    zipWithM_ (\ti n -> do
                        pokeByteOff ip (n * sizeOf (undefined::CInt)) (t32 ti)) tis [0..]
                    {# set FnTy.args #} sp ip
                    pure sp

cfp = case arch of {"aarch64" -> actxFunP; "x86_64" -> ctxFunP}

apple_compile :: IntPtr -> IntPtr -> CString -> Ptr CSize -> IO (Ptr Word8)
apple_compile (IntPtr m) (IntPtr f) src szPtr = do
    bSrc <- BS.unsafePackCString src
    (sz, fp) <- cfp (m,f) (BSL.fromStrict bSrc)
    poke szPtr (fromIntegral sz) $> castFunPtrToPtr fp

foreign export ccall apple_compile :: IntPtr -> IntPtr -> CString -> Ptr CSize -> IO (Ptr Word8)
foreign export ccall apple_printty :: CString -> Ptr CString -> IO CString
foreign export ccall apple_dumpasm :: CString -> Ptr CString -> IO CString
foreign export ccall apple_dumpir :: CString -> Ptr CString -> IO CString
foreign export ccall apple_ty :: CString -> Ptr CString -> IO (Ptr FnTy)
