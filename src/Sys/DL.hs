{-# LANGUAGE OverloadedStrings #-}

module Sys.DL ( libc, mem' ) where

import           Data.Functor                          (($>))
import           Foreign.C.Types                       (CSize)
import           Foreign.Ptr                           (FunPtr, IntPtr (..), Ptr, castFunPtrToPtr, ptrToIntPtr)
import           System.Posix.DynamicLinker.ByteString (DL, RTLDFlags (RTLD_LAZY), dlclose, dlopen, dlsym)

mem' :: IO (Int, Int)
mem' = do {(m,f) <- mem; pure (g m, g f)}
    where g = (\(IntPtr i) -> i) . ptrToIntPtr . castFunPtrToPtr

mem :: IO (FunPtr (CSize -> IO (Ptr a)), FunPtr (Ptr a -> IO ()))
mem = do {c <- libc; m <- dlsym c "malloc"; f <- dlsym c "free"; dlclose c$>(m, f)}

ll p = dlopen p [RTLD_LAZY]

libc :: IO DL
libc = ll "libc.so"
