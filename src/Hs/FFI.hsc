-- https://eli.thegreenplace.net/2013/11/05/how-to-jit-an-introduction
module Hs.FFI ( pI
              , bsFp
              , allocNear
              , allocExec
              , finish
              , freeFunPtr
              ) where

import Data.Bits ((.|.))
import Data.Functor (void)
import Control.Monad (when)
import Foreign.C.Types (CInt (..), CSize (..), CChar)
import Foreign.Ptr (FunPtr, IntPtr (..), castFunPtrToPtr, castPtrToFunPtr, Ptr, intPtrToPtr, ptrToIntPtr, nullPtr)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import System.Posix.Types (COff (..))

#include <sys/mman.h>

pI :: Ptr a -> Int
pI = (\(IntPtr i) -> i) . ptrToIntPtr

allocNear :: Int -> CSize -> IO (Ptr a)
allocNear i sz =
    mmap (intPtrToPtr (IntPtr$i+6*1024*104)) sz #{const PROT_WRITE} (#{const MAP_PRIVATE} .|. #{const MAP_ANONYMOUS}) (-1) 0
    -- libc.so is 2.1MB, libm is 918kB

allocExec :: CSize -> IO (Ptr a)
allocExec sz =
    mmap nullPtr sz #{const PROT_WRITE} (#{const MAP_PRIVATE} .|. #{const MAP_ANONYMOUS}) (-1) 0

finish :: BS.ByteString -> Ptr CChar -> IO (FunPtr a)
finish bs fAt = BS.unsafeUseAsCStringLen bs $ \(b, sz) -> do
    let sz' = fromIntegral sz
    _ <- memcpy fAt b sz'
    r <- mprotect fAt sz' #{const PROT_EXEC}
    when (r == -1) $ error "call to mprotect failed."
    pure (castPtrToFunPtr fAt)

bsFp :: BS.ByteString -> IO (FunPtr a, CSize)
bsFp bs = BS.unsafeUseAsCStringLen bs $ \(bytes, sz) -> do
    let sz' = fromIntegral sz
    fAt <- {-# SCC "mmap" #-} allocExec sz'
    _ <- {-# SCC "memcpy" #-} memcpy fAt bytes sz'
    _ <- {-# SCC "mprotect" #-} mprotect fAt sz' #{const PROT_EXEC}
    pure (castPtrToFunPtr fAt, sz')

freeFunPtr :: Int -> FunPtr a -> IO ()
freeFunPtr sz fp = void $ munmap (castFunPtrToPtr fp) (fromIntegral sz)

foreign import ccall mmap :: Ptr a -> CSize -> CInt -> CInt -> CInt -> COff -> IO (Ptr a)
foreign import ccall mprotect :: Ptr a -> CSize -> CInt -> IO CInt
foreign import ccall memcpy :: Ptr a -> Ptr a -> CSize -> IO (Ptr a)
foreign import ccall munmap :: Ptr a -> CSize -> IO CInt
