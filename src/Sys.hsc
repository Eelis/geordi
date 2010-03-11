{-# LANGUAGE ForeignFunctionInterface, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, PatternGuards #-}
-- Miscellaneous additional system API wrapping.

module Sys where

import qualified System.Posix.Internals
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Data.Sequence as Seq
import qualified Foreign.Ptr

import Data.Sequence (Seq, ViewL(..), (<|))
import Data.IORef (newIORef, readIORef, writeIORef)
import Control.Monad.Instances ()
import Control.Monad.Fix (fix)
import System.Posix.Time (epochTime)
import Network.Socket (Socket(..), setSocketOption, SocketOption(..))
import Foreign (with, sizeOf, peek, Ptr, Word8, unsafePerformIO, allocaBytes)
import Prelude hiding (catch, (.))
import System.Exit (ExitCode(..))
import System.Posix (Fd(..), CPid, ByteCount, Signal)
import Foreign.C
  (CInt, CUInt, CLong, CString, getErrno, eCHILD, throwErrno, withCString, throwErrnoIfMinus1_, eWOULDBLOCK, peekCString, peekCStringLen, Errno)

#include <syscall.h>
#include <sys/ptrace.h>
#include <sys/reg.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <locale.h>

syscall_off, syscall_ret :: CLong
#ifdef __x86_64__
syscall_off = (#const ORIG_RAX) * 8
syscall_ret = (#const RAX) * 8
#else
syscall_off = (#const ORIG_EAX) * 4
syscall_ret = (#const EAX) * 4
#endif

foreign import ccall unsafe "__hsunix_wifexited" c_WIFEXITED :: CInt -> CInt
foreign import ccall unsafe "__hsunix_wexitstatus" c_WEXITSTATUS :: CInt -> CInt
foreign import ccall unsafe "__hsunix_wifsignaled" c_WIFSIGNALED :: CInt -> CInt
foreign import ccall unsafe "__hsunix_wtermsig" c_WTERMSIG :: CInt -> CInt
foreign import ccall unsafe "__hsunix_wifstopped" c_WIFSTOPPED :: CInt -> CInt
foreign import ccall unsafe "__hsunix_wstopsig" c_WSTOPSIG :: CInt -> CInt

foreign import ccall unsafe "unistd.h sleep" sleep :: CUInt -> IO ()

foreign import ccall unsafe "string.h strsignal" c_strsignal :: CInt -> IO CString
foreign import ccall unsafe "string.h strerror" c_strerror :: Errno -> IO CString

strsignal :: CInt -> String
strerror :: Errno -> String

strsignal = UTF8.decodeString `fmap` unsafePerformIO `fmap` (peekCString =<<) `fmap` c_strsignal
strerror = UTF8.decodeString `fmap` unsafePerformIO `fmap` (peekCString =<<) `fmap` c_strerror


nonblocking_read :: Fd -> ByteCount -> IO [Word8]
nonblocking_read (Fd fd) bc = do
  allocaBytes (fromIntegral bc) $ \buf -> do
  r <- System.Posix.Internals.c_read fd (Foreign.Ptr.castPtr buf) bc
  case r of
    -1 -> getErrno >>= \e -> if e == eWOULDBLOCK then return [] else throwErrno "nonblocking_read"
    n -> (fromIntegral `fmap` fromEnum `fmap`) `fmap` peekCStringLen (buf, fromIntegral n)
  -- Wrapping c_read ourselves is easier and more to the point than using fdRead and catching&filtering (stringized) eWOULDBLOCK errors.

foreign import ccall "wait" c_wait :: Ptr CInt -> IO CPid

data WaitResult = WaitNoChild | WaitExited ExitCode | WaitSignaled Signal | WaitStopped Signal
  deriving (Show, Eq)

wait :: Ptr CInt -> IO WaitResult
  -- Sharing the CInt is required for acceptable performance when wait is called very often (like when we intercept each system call when ptracing).
wait p = do
  r <- c_wait p
  if r /= -1 then d `fmap` peek p else do
  e <- getErrno
  if e == eCHILD then return WaitNoChild else throwErrno "wait"
  where
    ec :: CInt -> ExitCode
    ec 0 = ExitSuccess
    ec n = ExitFailure $ fromIntegral n
    d :: CInt -> WaitResult
    d s | c_WIFEXITED s /= 0 = WaitExited $ ec $ c_WEXITSTATUS s
    d s | c_WIFSIGNALED s /= 0 = WaitSignaled $ c_WTERMSIG s
    d s | c_WIFSTOPPED s /= 0 = WaitStopped $ c_WSTOPSIG s
    d _ = error "unknown wait status"

foreign import ccall unsafe "setsockopt"
  c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt

bareSetSocketOption :: Socket -> CInt -> CInt -> CInt -> IO ()
  -- Needed because Network.Socket.SocketOption lacks several options (such as TCP_KEEPIDLE, TCP_KEEPINTVL, and KEEPCNT).
bareSetSocketOption (MkSocket fd _ _ _ _) level option value = do
   with value $ \p -> do
   throwErrnoIfMinus1_ "bareSetSocketOption" $
    c_setsockopt fd level option p (fromIntegral $ sizeOf value)
   return ()

setKeepAlive :: Socket -> CInt -> CInt -> CInt -> IO ()
setKeepAlive sock keepidle keepintvl keepcnt = do
  setSocketOption sock KeepAlive 1
  let sso = bareSetSocketOption sock (#const IPPROTO_TCP)
  sso (#const TCP_KEEPIDLE) keepidle
  sso (#const TCP_KEEPINTVL) keepintvl
  sso (#const TCP_KEEPCNT) keepcnt

foreign import ccall "unistd.h chroot" c_chroot :: CString -> IO CInt

chroot :: FilePath -> IO ()
chroot s = throwErrnoIfMinus1_ "chroot" (withCString s c_chroot)

fdOfFd :: Fd -> CInt
fdOfFd (Fd fd) = fd

foreign import ccall unsafe "locale.h setlocale" setlocale :: CInt -> CString -> IO CString

setlocale_ALL_env :: IO ()
setlocale_ALL_env = withCString "" $ \s -> setlocale (#const LC_ALL) s >> return ()

class Queue q e | q -> e where
  qpush :: e -> q -> q
  qpop :: q -> Maybe (e, q)

instance Queue (Seq e) e where
  qpush = (<|)
  qpop q = case (Seq.viewl q) of
    Seq.EmptyL -> Nothing
    e :< q' -> Just (e, q')

qPopWhile :: Queue q e => (e -> Bool) -> q -> q
qPopWhile p q | Just (e, q') <- qpop q, p e = qPopWhile p q'
qPopWhile _ q = q

rate_limiter :: Int -> Int -> IO (IO ())
rate_limiter bound window = do
  r <- newIORef Seq.empty
  return $ fix $ \loop -> do
    now <- epochTime
    hist <- discard_until (now - fromIntegral window) `fmap` readIORef r
    if Seq.length hist < bound
      then writeIORef r (qpush now hist)
      else writeIORef r hist >> sleep 1 >> loop
 where discard_until t = qPopWhile (< t)
  -- Given |rl <- rate_limiter b w|, |rl| actions will sleep as required to make sure no more than b actions pass during any w second time window.
