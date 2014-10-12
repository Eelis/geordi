{-# LANGUAGE UnicodeSyntax, ForeignFunctionInterface, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, PatternGuards #-}
-- Miscellaneous additional system API wrapping.

module Sys where

import qualified System.Posix.Internals
import qualified Data.Sequence as Seq
import qualified Foreign.Ptr

import Data.Sequence (Seq, ViewL(..), (<|))
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad.Instances ()
import Control.Monad.Fix (fix)
import System.Posix.Time (epochTime)
import Network.Socket (Socket(..), setSocketOption, SocketOption(..))
import Foreign (with, sizeOf, Ptr, allocaBytes)
import System.IO.Unsafe (unsafePerformIO)
import System.Posix (Fd(Fd), ByteCount)
import Foreign.C
  (CInt(CInt), CUInt(CUInt), CLong, CString, getErrno, throwErrno, withCString, throwErrnoIfMinus1_, eWOULDBLOCK, peekCString, peekCStringLen, Errno(Errno))

import Prelude hiding ((.))

#include <syscall.h>
#include <sys/reg.h>
#include <netinet/in.h>
#include <netinet/tcp.h>

syscall_off, syscall_ret :: CLong
syscall_off = (#const ORIG_RAX) * 8
syscall_ret = (#const RAX) * 8

foreign import ccall unsafe "__hsunix_wifexited" c_WIFEXITED :: CInt → CInt
foreign import ccall unsafe "__hsunix_wexitstatus" c_WEXITSTATUS :: CInt → CInt
foreign import ccall unsafe "__hsunix_wifsignaled" c_WIFSIGNALED :: CInt → CInt
foreign import ccall unsafe "__hsunix_wtermsig" c_WTERMSIG :: CInt → CInt
foreign import ccall unsafe "__hsunix_wifstopped" c_WIFSTOPPED :: CInt → CInt
foreign import ccall unsafe "__hsunix_wstopsig" c_WSTOPSIG :: CInt → CInt

foreign import ccall unsafe "unistd.h sleep" sleep :: CUInt → IO ()

foreign import ccall unsafe "string.h strsignal" c_strsignal :: CInt → IO CString
foreign import ccall unsafe "string.h strerror" c_strerror :: Errno → IO CString

strsignal :: CInt → String
strerror :: Errno → String

strsignal = unsafePerformIO `fmap` (peekCString =<<) `fmap` c_strsignal
strerror = unsafePerformIO `fmap` (peekCString =<<) `fmap` c_strerror

fdReadNonBlocking :: Fd → ByteCount → IO String
fdReadNonBlocking (Fd fd) bc = do
  allocaBytes (fromIntegral bc) $ \buf → do
   r ← System.Posix.Internals.c_read fd (Foreign.Ptr.castPtr buf) bc
   case r of
    -1 → getErrno >>= \e → if e == eWOULDBLOCK then return [] else throwErrno "nonblocking_read"
    n → peekCStringLen (buf, fromIntegral n)
  -- Wrapping c_read ourselves is easier and more to the point than using fdRead and catching&filtering (stringized) eWOULDBLOCK errors. hGetBufNonBlocking works on a Handle, which is even worse.
  -- Note that peekCStringLen decodes Chars according to the current locale.

foreign import ccall unsafe "setsockopt"
  c_setsockopt :: CInt → CInt → CInt → Ptr CInt → CInt → IO CInt

bareSetSocketOption :: Socket → CInt → CInt → CInt → IO ()
  -- Needed because Network.Socket.SocketOption lacks several options (such as TCP_KEEPIDLE, TCP_KEEPINTVL, and KEEPCNT).
bareSetSocketOption (MkSocket fd _ _ _ _) level option value = do
   with value $ \p →
    throwErrnoIfMinus1_ "bareSetSocketOption" $
     c_setsockopt fd level option p (fromIntegral $ sizeOf value)
   return ()

setKeepAlive :: Socket → CInt → CInt → CInt → IO ()
setKeepAlive sock keepidle keepintvl keepcnt = do
  setSocketOption sock KeepAlive 1
  let sso = bareSetSocketOption sock (#const IPPROTO_TCP)
  sso (#const TCP_KEEPIDLE) keepidle
  sso (#const TCP_KEEPINTVL) keepintvl
  sso (#const TCP_KEEPCNT) keepcnt

foreign import ccall "unistd.h chroot" c_chroot :: CString → IO CInt

chroot :: FilePath → IO ()
chroot s = throwErrnoIfMinus1_ "chroot" (withCString s c_chroot)

fdOfFd :: Fd → CInt
fdOfFd (Fd fd) = fd

class Queue q e | q → e where
  qpush :: e → q → q
  qpop :: q → Maybe (e, q)

instance Queue (Seq e) e where
  qpush = (<|)
  qpop q = case (Seq.viewl q) of
    Seq.EmptyL → Nothing
    e :< q' → Just (e, q')

qPopWhile :: Queue q e => (e → Bool) → q → q
qPopWhile p q | Just (e, q') ← qpop q, p e = qPopWhile p q'
qPopWhile _ q = q

rate_limiter :: Int → Int → IO (IO ())
rate_limiter bound window = do
  r ← newIORef Seq.empty
  return $ fix $ \loop → do
    now ← epochTime
    hist ← discard_until (now - fromIntegral window) `fmap` readIORef r
    if Seq.length hist < bound
      then writeIORef r (qpush now hist)
      else writeIORef r hist >> sleep 1 >> loop
 where discard_until t = qPopWhile (< t)
  -- Given |rl ← rate_limiter b w|, |rl| actions will sleep as required to make sure no more than b actions pass during any w second time window.
