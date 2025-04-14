module Sys.DL ( CCtx, MCtx, libc, mem', math' ) where

import           Data.Functor                          (($>))
import           Foreign.C.Types                       (CSize)
import           Data.Int                              (Int32)
import           Foreign.Ptr                           (FunPtr, IntPtr (..), Ptr, castFunPtrToPtr, ptrToIntPtr)
import           System.Posix.DynamicLinker.ByteString (DL, RTLDFlags (RTLD_LAZY), dlclose, dlopen, dlsym)

#ifdef linux_HOST_OS
#include <gnu/lib-names.h>
#endif

type CCtx = (Int, Int, Int, Int); type MCtx = (Int, Int, Int)

math' :: IO MCtx
math' = do {(e,l,p) <- math; pure (ip e, ip l, ip p)}

mem' :: IO CCtx
mem' = do {(m,f,xr,r) <- mem; pure (ip m, ip f, ip xr, ip r)}

ip = (\(IntPtr i) -> i) . ptrToIntPtr . castFunPtrToPtr

mem :: IO (FunPtr (CSize -> IO (Ptr a)), FunPtr (Ptr a -> IO ()), FunPtr (IO Double), FunPtr (IO Int32))
mem = do {c <- libc; m <- dlsym c "malloc"; f <- dlsym c "free"; xr <- dlsym c "drand48"; r <- dlsym c "lrand48"; dlclose c$>(m,f,xr,r)}

math :: IO (FunPtr (Double -> Double), FunPtr (Double -> Double), FunPtr (Double -> Double -> Double))
math = do {m <- libm; e <- dlsym m "exp"; l <- dlsym m "log"; p <- dlsym m "pow"; dlclose m$>(e,l,p)}

ll p = dlopen p [RTLD_LAZY]

libc, libm :: IO DL
#ifdef linux_HOST_OS
libc = ll {# const LIBC_SO #}
libm = ll {# const LIBM_SO #}
#elif darwin_HOST_OS
libc = ll "libc.dylib"
libm = ll "libm.dylib"
#endif
