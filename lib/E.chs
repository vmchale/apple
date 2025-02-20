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
import Foreign.Ptr (Ptr, castPtr, castFunPtrToPtr, plusPtr, nullPtr)
import Foreign.Storable (poke, pokeByteOff)
import Prettyprinter (Doc, Pretty)
import Prettyprinter.Ext
import System.Info (arch)

#include <string.h>
#include <sys/mman.h>
#include <apple.h>

data FnTy
data JitCtx

{# fun memcpy as ^ { castPtr `Ptr a', castPtr `Ptr a', coerce `CSize' } -> `Ptr a' castPtr #}

{# enum apple_at as CA {} #}
{# enum HK as HK {} #}
{# enum NK as NK {} #}

ct :: CAt -> CA
ct CR = F_t; ct CI = I_t; ct CB = B_t

t32 :: CAt -> CInt
t32 = fromIntegral.fromEnum.ct

hk32 :: HK -> CInt
hk32 = fromIntegral.fromEnum

nk32 :: NK -> CInt
nk32 = fromIntegral.fromEnum

ppn :: T.Text -> Ptr CSize -> IO CString
ppn t szP = BS.unsafeUseAsCStringLen (encodeUtf8 t) $ \(bs, sz) -> do
    p <- mallocBytes (sz+1)
    _ <- memcpy p bs (fromIntegral sz)
    poke szP (fromIntegral sz)
    pokeByteOff p sz (0::CChar) $> p

tcstr :: T.Text -> IO CString
tcstr t =
    BS.unsafeUseAsCStringLen (encodeUtf8 t) $ \(bs,sz) -> do
        p <- mallocBytes (sz+1)
        _ <- memcpy p bs (fromIntegral sz)
        pokeByteOff p sz (0::CChar) $> p

tn :: CString -> Ptr CSize -> Ptr CString -> IO CString
tn src nPtr errPtr = do
    bSrc <- BS.unsafePackCString src
    case tyExpr (BSL.fromStrict bSrc) of
        Left err -> (poke errPtr =<< tcstr (ptxt err)) $> nullPtr
        Right d -> ppn (aText d) nPtr

apple_print_ts_sz = tn

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

apple_dumpir, apple_printty :: CString -> Ptr CString -> IO CString
apple_dumpir = harnessString dumpIR
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
                    ip <- mallocBytes (argc * {# sizeof apple_t #})
                    {# set FnTy.argc #} sp (fromIntegral argc)
                    case to of
                        SC tao -> do
                            {# set FnTy.res.f #} sp (hk32 Sc)
                            {# set FnTy.res.rr #} sp (nk32 Rc)
                            {# set FnTy.res.ty.aa #} sp (t32 tao)
                        AC tao -> do
                            {# set FnTy.res.f #} sp (hk32 Aa)
                            {# set FnTy.res.rr #} sp (nk32 Rc)
                            {# set FnTy.res.ty.aa #} sp (t32 tao)
                        ΠC ts -> do
                            let nr=length ts
                            pp <- mallocBytes (nr*{#sizeof apple_t#})
                            {# set FnTy.res.f #} sp (hk32 Sc)
                            {# set FnTy.res.rr #} sp (nk32 Pi)
                            {# set FnTy.res.ty.APi.pi_n #} sp (fromIntegral nr::CInt)
                            {# set FnTy.res.ty.APi.a_pi #} sp pp
                            zipWithM_ (\tϵ n -> do
                                let ap=pp `plusPtr` (n*{#sizeof apple_t#})
                                case tϵ of
                                    ΠA{} -> error "nested tuples not implemented."
                                    ΠC{} -> error "nested tuples not implemented."
                                    AC taϵ -> do
                                        {# set apple_t.f #} ap (hk32 Aa)
                                        {# set apple_t.ty.aa #} ap (t32 taϵ)
                                    SC taϵ -> do
                                        {# set apple_t.f #} ap (hk32 Sc)
                                        {# set apple_t.ty.aa #} ap (t32 taϵ)) ts [0..]
                    zipWithM_ (\ti n ->
                        case ti of
                            ΠC{} -> error "tuple arguments not implemented."
                            ΠA{} -> error "array-of-tuple arguments not implemented."
                            SC tai -> do
                                argn ip n {# offsetof apple_t->f #} (hk32 Sc)
                                argn ip n {# offsetof apple_t->ty.aa #} (t32 tai)
                            AC tao -> do
                                argn ip n {# offsetof apple_t->f #} (hk32 Aa)
                                argn ip n {# offsetof apple_t->ty.aa #} (t32 tao)) tis [0..]
                    {# set FnTy.args #} sp ip
                    pure sp
  where 
    argn p n = pokeByteOff (p `plusPtr` (n*{# sizeof apple_t #}))

cfp = case arch of {"aarch64" -> actxFunP; "x86_64" -> ctxFunP.fst}
jNull x p = case x of {Nothing -> poke p nullPtr; Just xϵ -> poke p xϵ}

apple_compile :: Ptr JitCtx -> CString -> Ptr CSize -> Ptr (Ptr Word8) -> IO (Ptr Word8)
apple_compile jp src szPtr sPtr = do
    m <- il <$> {# get JC->ma #} jp
    f <- il <$> {# get JC->free #} jp
    r <- il <$> {# get JC->r #} jp
    xr <- il <$> {# get JC->xr #} jp
    e <- il <$> {# get JC->e #} jp
    l <- il <$> {# get JC->log #} jp
    p <- il <$> {# get JC->pow #} jp
    bSrc <- BS.unsafePackCString src
    (sz, fp, aa) <- cfp ((m,f,xr,r), (e,l,p)) (BSL.fromStrict bSrc)
    jNull aa sPtr
    poke szPtr (fromIntegral sz) $> castFunPtrToPtr fp
  where
    il = fromIntegral

foreign export ccall apple_compile :: Ptr JitCtx -> CString -> Ptr CSize -> Ptr (Ptr Word8) -> IO (Ptr Word8)
foreign export ccall apple_printty :: CString -> Ptr CString -> IO CString
foreign export ccall apple_dumpasm :: CString -> Ptr CString -> IO CString
foreign export ccall apple_dumpir :: CString -> Ptr CString -> IO CString
foreign export ccall apple_ty :: CString -> Ptr CString -> IO (Ptr FnTy)
foreign export ccall apple_print_ts_sz :: CString -> Ptr CSize -> Ptr CString -> IO CString
