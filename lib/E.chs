module E () where

import qualified A
import CGen
import Control.Monad (zipWithM_)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Unsafe as BS
import Data.Coerce (coerce)
import Data.Functor (($>))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Word (Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..), CSize (..), CChar)
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr, castPtr, castFunPtrToPtr, nullPtr)
import Foreign.Storable (poke, pokeByteOff, sizeOf)
import P
import Prettyprinter.Ext

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

apple_printty :: CString -> Ptr CString -> IO CString
apple_printty src errPtr = do
    bSrc <- BS.unsafePackCString src
    case tyExpr (BSL.fromStrict bSrc) of
        Left err ->
            (poke errPtr =<< tcstr (ptxt err)) $> nullPtr
        Right d -> tcstr (aText d)

apple_ty :: CString -> Ptr CString -> Ptr (Ptr FnTy) -> IO CInt
apple_ty src errPtr argsP = do
    bSrc <- BS.unsafePackCString src
    let b = tyOf (BSL.fromStrict bSrc)
    case b of
        Left err -> do
            poke errPtr =<< tcstr (ptxt err)
            pure (-1)
        Right (t@A.Arrow{}, []) -> do
            let mCty = tCTy t
            case mCty of
                Left te -> do {poke errPtr =<< tcstr (ptxt te); pure (-1)}
                Right (tis, to) -> do
                    let argc = length tis
                    sp <- mallocBytes {# sizeof FnTy #}
                    ip <- mallocBytes (argc * sizeOf (undefined::CInt))
                    {# set FnTy.argc #} sp (fromIntegral argc)
                    {# set FnTy.res #} sp (t32 to)
                    zipWithM_ (\ti n -> do
                        pokeByteOff ip (n * sizeOf (undefined::CInt)) (t32 ti)) tis [0..]
                    {# set FnTy.args #} sp ip
                    poke argsP sp
                    pure $ fromIntegral $ fromEnum Fn
            pure (fromIntegral $ fromEnum Fn)
        Right (t, []) -> pure $ fromIntegral $ fromEnum $ case t of
            A.I -> I_t
            A.F -> F_t
            (A.Arr _ A.I) -> IA
            (A.Arr _ A.F) -> FA

apple_compile :: CString -> IO (Ptr Word8)
apple_compile src = do
    bSrc <- BS.unsafePackCString src
    castFunPtrToPtr <$> funP (BSL.fromStrict bSrc)

foreign export ccall apple_compile :: CString -> IO (Ptr Word8)
foreign export ccall apple_printty :: CString -> Ptr CString -> IO CString
foreign export ccall apple_ty :: CString -> Ptr CString -> Ptr (Ptr FnTy) -> IO CInt
