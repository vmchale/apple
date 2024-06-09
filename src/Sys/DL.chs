{-# LANGUAGE OverloadedStrings #-}

module Sys.DL ( CCtx, MCtx, libc, mem', math' ) where

import           Data.Functor                          (($>))
import           Foreign.C.Types                       (CSize)
import           Foreign.Ptr                           (FunPtr, IntPtr (..), Ptr, castFunPtrToPtr, ptrToIntPtr)
import           System.Posix.DynamicLinker.ByteString (DL, RTLDFlags (RTLD_LAZY), dlclose, dlopen, dlsym)

#include <gnu/lib-names.h>

type CCtx = (Int, Int); type MCtx = (Int, Int)

math' :: IO MCtx
math' = do {(e,l) <- math; pure (ip e, ip l)}

mem' :: IO CCtx
mem' = do {(m,f) <- mem; pure (ip m, ip f)}

ip = (\(IntPtr i) -> i) . ptrToIntPtr . castFunPtrToPtr

mem :: IO (FunPtr (CSize -> IO (Ptr a)), FunPtr (Ptr a -> IO ()))
mem = do {c <- libc; m <- dlsym c "malloc"; f <- dlsym c "free"; dlclose c$>(m,f)}

math :: IO (FunPtr (Double -> Double), FunPtr (Double -> Double))
math = do {m <- libm; e <- dlsym m "exp"; l <- dlsym m "log"; dlclose m$>(e,l)}

ll p = dlopen p [RTLD_LAZY]

libc :: IO DL
libc = ll {# const LIBC_SO #}

libm :: IO DL
libm = ll {# const LIBM_SO #}
