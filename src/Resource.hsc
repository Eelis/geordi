-- System.Posix.Resource does not support non-POSIX resources like RLIMIT_NPROC.

module Resource (setrlimit) where

import System.Posix.Types (CRLim)
import Foreign
import Foreign.C

#include <sys/resource.h>

type RLimit = ()

foreign import ccall unsafe "setrlimit" c_setrlimit :: CInt -> Ptr RLimit -> IO CInt

setrlimit :: CInt -> CRLim -> IO ()
setrlimit r l = do
  allocaBytes (#const sizeof(struct rlimit)) $ \p -> do
  (#poke struct rlimit, rlim_cur) p l
  (#poke struct rlimit, rlim_max) p l
  throwErrnoIfMinus1 "setrlimit" $ c_setrlimit r p
  return ()
