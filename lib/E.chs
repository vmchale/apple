module E () where

import qualified A
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
import Foreign.Storable (poke, pokeByteOff)
import P
import Prettyprinter (Pretty (..), layoutCompact)
import Prettyprinter.Render.Text (renderStrict)

#include <string.h>
#include <sys/mman.h>
#include <apple.h>

{# fun memcpy as ^ { castPtr `Ptr a', castPtr `Ptr a', coerce `CSize' } -> `Ptr a' castPtr #}

-- how tf do C weenies store like... function types??
{# enum apple_t as CT {} #}
{# enum apple_err as IErr {} #}

tcstr :: T.Text -> IO CString
tcstr t =
    BS.unsafeUseAsCStringLen (encodeUtf8 t) $ \(bs,sz) -> do
        p <- mallocBytes (sz+1)
        memcpy p bs (fromIntegral sz)
        pokeByteOff p sz (0::CChar) $> p

apple_printty :: CString -> Ptr CString -> IO CString
apple_printty src errPtr = do
    bSrc <- BS.unsafePackCString src
    case tyExpr (BSL.fromStrict bSrc) of
        Left err ->
            (poke errPtr =<< tcstr (ptxt err)) $> nullPtr
        Right t -> tcstr (ptxt t)

ptxt :: Pretty a => a -> T.Text
ptxt = renderStrict . layoutCompact . pretty

apple_ty :: CString -> Ptr CString -> IO CInt
apple_ty src errPtr = do
    bSrc <- BS.unsafePackCString src
    let b = tyExpr (BSL.fromStrict bSrc)
    case b of
        Left err -> do
            poke errPtr =<< tcstr (ptxt err)
            pure (-1)
        Right t -> pure $ fromIntegral $ fromEnum $ case t of   
            A.I -> I_t
            A.F -> F_t
            (A.Arr _ A.I) -> IA
            (A.Arr _ A.F) -> FA
            A.Arrow{} -> Fn

apple_compile :: CString -> IO (Ptr Word8)
apple_compile src = do
    bSrc <- BS.unsafePackCString src
    castFunPtrToPtr <$> funP (BSL.fromStrict bSrc)

foreign export ccall apple_compile :: CString -> IO (Ptr Word8)
foreign export ccall apple_printty :: CString -> Ptr CString -> IO CString
foreign export ccall apple_ty :: CString -> Ptr CString -> IO CInt
