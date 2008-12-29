{-# LANGUAGE ForeignFunctionInterface #-}
module Flock (exclusive) where

import System.Posix.Types
import Foreign.C
import Prelude (IO, ($))

#include <sys/file.h>

foreign import ccall unsafe "flock" c_flock :: CInt -> CInt -> IO CInt

flock :: CInt -> CInt -> IO ()
flock fd op = throwErrnoIfMinus1_ "flock" $ c_flock fd op

exclusive :: Fd -> IO ()
exclusive (Fd fd) = flock fd (#const LOCK_EX)
