
{-- Correct ptrace-ing

Our goal is simple: we want to be in control over everything the child does after it execve's. In particular, we want to know which syscall traps are entries and exits (we need this to be able to have ignored syscalls whose entry we replace by entry to getpid, and whose exits we replace with "return 0;"). Since the ptrace interface offers no built-in way to distinguish between the two, this means that we need to keep an in_syscall flag that we toggle at every syscall trap. Correct initialization of this flag is a concern.

How does ptrace-ing commence? The ptrace manual states that once the child has done PTRACE_TRACEME, "all subsequent calls to exec() by this process will cause a SIGTRAP to be sent to it". Unfortunately, this wording seemingly allows for any of the following 6 parent-observable initial sequences of events:

  1. execve entry trap, execve exit trap
  2. execve entry trap
  3. execve exit trap
  4. execve entry trap, separate trap, execve exit trap
  5. execve entry trap, separate trap
  6. separate trap, execve exit trap

Furthermore, different sequences have been observed on different machines (see ptracetest1.c). Worse, we cannot even write a test program to determine which of the sequences happened, since that requires distinguishing between SIGTRAPs caused by entries to syscalls, exits from syscalls, and SIGTRAPs not caused by either entry to or exit from a syscall, but to be able to make that distinction PTRACE_O_TRACESYSGOOD needs to be set, but we cannot reliably set it before the parent's first wait() call (see test2.c). Consequently, execve is just not a solid "first SIGTRAP" to build on.

We therefore abandon execve as ptrace initiator, and instead have the child raise SIGSTOP before it execve's. Now, there are two important aspects here. First, we need to know that the parent will observe the child's SIGSTOP /before/ the child proceeds to do anything else (otherwise anything can happen). This behavior is guaranteed by the glibc manual, which states that:

  If kill is used by a process to send a signal to itself, and the signal is not blocked, then kill delivers at least one signal (which might be some other pending unblocked signal instead of the signal signum) to that process before it returns.

The second important aspect relates to the initialization of the in_syscall flag. We need to know conclusively whether the kill() system call that raise() makes has exited by the time the parent observes the SIGSTOP signal. If it has, the in_syscall flag needs to be initialized to false. If it hasn't, it needs to be initialized to true. So far, on all machines tested it has been the case that the signal is not delivered until after the kill() call has exited, though I can't find documentation that guarantees this. This is currently /the/ big assumption that geordi makes in its ptrace usage. If this behavior turns out to differ between machines, then we may have to resort to something like delaying the initialization of the in_syscall flag until the first execve syscall trap is observed (although that could be tricky, because it would rely on ORIG_EAX still being set to the syscall number on syscall exit traps - behavior that I'm not sure is guaranteed).

-}

module EvalCpp (evalCpp) where

import Prelude hiding (catch, (.))
import Control.Monad
import Control.Monad.Fix
import Foreign
import Foreign.C
import System.IO
import System.Exit
import System.Posix hiding (Stopped, Exited, executeFile)
import System.Posix.Error
import System.Posix.Internals
import System.FilePath
import Util
import ErrorFilters
import qualified Ptrace
import Data.List
import Data.Word
import Data.Char
import qualified Codec.Binary.UTF8.String as UTF8
import Data.Maybe
import SyscallNames

#include <syscall.h>
#include <sys/ptrace.h>
#include <sys/reg.h>

syscall_off, syscall_ret :: CInt
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

foreign import ccall unsafe "string.h strsignal" c_strsignal :: CInt -> IO CString

strsignal :: CInt -> String
strsignal s = unsafePerformIO $ c_strsignal s >>= peekCString

foreign import ccall unsafe "execve" c_execve :: CString -> Ptr CString -> Ptr CString -> IO CInt

executeFile :: FilePath -> [String] -> [String] -> IO ()
executeFile path args env = {- path is also used as first arg -}
  withCString path $ \s ->
  withMany withCString (path:args) $ \cstrs -> withArray0 nullPtr cstrs $ \arg_arr ->
  withMany withCString env $ \cenv -> withArray0 nullPtr cenv $ \env_arr ->
  throwErrnoPathIfMinus1_ "executeFile" path (c_execve s arg_arr env_arr)

{- This executeFile is heavily based on unix/System/Posix/Process.hsc' executeFile. Differences:
- no pPrPr_disableITimers (because we need our kill timer) (raison d'être);
- no search;
- environment not optional and not as key-value pairs.
-}

nonblocking_read :: Fd -> ByteCount -> IO [Word8]
nonblocking_read (Fd fd) bc = do
  allocaBytes (fromIntegral bc) $ \buf -> do
  r <- c_read fd buf bc
  case r of
    -1 -> getErrno >>= \e -> if e == eWOULDBLOCK then return [] else throwErrno "nonblocking_read"
    n -> (fromIntegral . ord .) . peekCStringLen (buf, fromIntegral n)
  -- Wrapping c_read ourselves is easier and more to the point than using fdRead and catching&filtering (stringized) eWOULDBLOCK errors.

foreign import ccall "wait" c_wait :: Ptr CInt -> IO CPid

wait :: Ptr CInt -> IO WaitResult
wait p = do
  r <- c_wait p
  if r /= -1 then d =<< peek p else do
  e <- getErrno
  if e == eCHILD then return WR_NoChild else throwErrno "wait"
  where
    d :: CInt -> IO WaitResult
    d s | c_WIFEXITED s /= 0 = return $ WR_Exited $ c_WEXITSTATUS s
    d s | c_WIFSIGNALED s /= 0 = return $ WR_Signaled $ c_WTERMSIG s
    d s | c_WIFSTOPPED s /= 0 = return $ WR_Stopped $ c_WSTOPSIG s
    d _ = fail "unexpected wait status"

data WaitResult = WR_NoChild | WR_Exited CInt | WR_Signaled CInt | WR_CoreDump | WR_Stopped CInt
  deriving Eq

data SuperviseResult = Exited ExitCode | DisallowedSyscall CInt | Signaled Signal | ChildVanished
  deriving Eq

instance Show SuperviseResult where
  show sr = case sr of
    Exited c -> "Exited: " ++ show c
    DisallowedSyscall c -> "Disallowed system call: " ++ syscallName c
    Signaled s -> if s == sigALRM then "Timeout" else strsignal s
    ChildVanished -> "Child vanished"

supervise :: ProcessID -> IO SuperviseResult
  -- supervise assumes that the first event observed is the child raising sigSTOP.
  -- wait is called so often that sharing the allocated wstatp is required for acceptable performance.
supervise pid = alloca $ \wstatp -> do
  wait wstatp >>= \s -> when (s /= WR_Stopped sigSTOP) $ fail "first ptraced event not sigSTOP"
  Ptrace.tracesysgood pid
  Ptrace.syscall pid
  ($ Nothing) $ fix $ \sv current_syscall -> do
    wstat <- wait wstatp
    case wstat of
      WR_NoChild -> return ChildVanished
      WR_Exited e -> return $ Exited $ if e == 0 then ExitSuccess else ExitFailure $ fromIntegral e
      WR_Signaled s -> return $ Signaled s
      WR_Stopped s | s == sigTRAP -> Ptrace.syscall pid >> sv current_syscall
      WR_Stopped s | s == (sigTRAP .|. 0x80) -> do
        case current_syscall of
          Just sc -> do
            when (elem sc ignored_syscalls) $ Ptrace.pokeuser pid syscall_ret 0
            Ptrace.syscall pid; sv Nothing
          Nothing -> Ptrace.peekuser pid syscall_off >>= \syscall -> case () of
            _ | elem syscall ignored_syscalls -> do
              Ptrace.pokeuser pid syscall_off #const SYS_getpid
              Ptrace.syscall pid; sv (Just syscall)
            _ | elem syscall allowed_syscalls -> Ptrace.syscall pid >> sv (Just syscall)
            _ -> do
              Ptrace.pokeuser pid syscall_off #const SYS_exit_group
              Ptrace.kill pid
                -- The documentation for PTRACE_KILL is extremely vague, but it seems that it in this scenario it actually restarts the process, after which one of two things happens: either exit_group succeeds and the next wait returns WR_Exited, or it fails and the process is half dead, twitching, and being delivered SIGKILL. Both cases are dealt with adequately by sv.
              sv $ Just #const SYS_exit_group
              return $ DisallowedSyscall syscall
      WR_Stopped sig -> Ptrace.kill pid >> sv Nothing >> return (Signaled sig)

simpleResourceLimits :: Integer -> ResourceLimits
simpleResourceLimits l = ResourceLimits (ResourceLimit l) (ResourceLimit l)

data Resources = Resources { walltime :: Int, rlimits :: [(Resource, ResourceLimits)], bufsize :: CSize }

-- capture_restricted assumes the program produces UTF-8 encoded text and returns it as a proper Unicode String.

capture_restricted :: FilePath -> [String] -> [String] -> Resources -> IO (SuperviseResult, String)
capture_restricted a argv env (Resources timeout rlims bs) =
  withResource createPipe $ \(pipe_r, pipe_w) -> do
    setFdOption pipe_r NonBlockingRead True
    res <- (=<<) supervise $ forkProcess $ do
      dupTo pipe_w stdError
      dupTo pipe_w stdOutput
      closeFd stdInput
      scheduleAlarm timeout
      mapM (uncurry setResourceLimit) rlims
      Ptrace.traceme
      raiseSignal sigSTOP
      executeFile a argv env
      exitImmediately ExitSuccess
    (,) res . UTF8.decode . (nonblocking_read pipe_r bs)

-- The actual output size is also limited by the pipe buffer.

evalCpp :: IO (String -> Bool -> IO String)
  -- Two-stage IO: first must happen before jail time, because gcc-execs and the syscall names need to be read from file, second happens inside jail and does actual evaluation.

evalCpp = do
  let
    cap :: FilePath -> [String] -> Resources -> (String -> String) -> IO String -> IO String
    cap a argv r err act = do
      (res, out) <- capture_restricted a argv [] r
      case res of
        Exited ExitSuccess -> act
        Exited (ExitFailure _) -> return $ err out
        _ -> return $ takeFileName a ++ ": " ++ show res
  (cc1plus, as, ld) <- readTypedFile "gcc-execs"
  return $ \code also_run -> do
    writeFile "t.cpp" code
    cap (head cc1plus) (tail cc1plus) cc1plus_resources process_cc1plus_errors $ do
    if not also_run then return "Compilation successful" else do
    cap (head as) (tail as) as_resources process_as_errors $ do
    cap (head ld) (tail ld) ld_resources process_ld_errors $ do
    (prog_result, prog_output) <- capture_restricted "/t" [] ["GLIBCXX_DEBUG_MESSAGE_LENGTH=0"] prog_resources
    return $ if prog_result == Exited ExitSuccess
      then prog_output
      else process_prog_errors prog_output (show prog_result)

------------- Config (or at least things that are likely more prone to per-site modification):

-- System calls

ignored_syscalls, allowed_syscalls :: [CInt]

ignored_syscalls = [(#const SYS_chmod), (#const SYS_fadvise64), (#const SYS_unlink), (#const SYS_munmap), (#const SYS_madvise), (#const SYS_umask), (#const SYS_rt_sigaction), (#const SYS_rt_sigprocmask)]
  -- These are replaced with "return 0".

allowed_syscalls =
  [ (#const SYS_open), (#const SYS_write), (#const SYS_uname), (#const SYS_brk), (#const SYS_read), (#const SYS_mmap), (#const SYS_mprotect), (#const SYS_exit_group), (#const SYS_getpid), (#const SYS_access), (#const SYS_getrusage), (#const SYS_ioctl), (#const SYS_close), (#const SYS_gettimeofday), (#const SYS_writev), (#const SYS_execve)

  #ifdef __x86_64__
    , (#const SYS_stat), (#const SYS_fstat), (#const SYS_arch_prctl), (#const SYS_getrlimit), (#const SYS_fcntl), (#const SYS_lseek), (#const SYS_lstat), (#const SYS_dup)
  #else
    , (#const SYS_fstat64), (#const SYS_lstat64), (#const SYS_stat64), (#const SYS_ugetrlimit), (#const SYS_fcntl64), (#const SYS__llseek), (#const SYS_mmap2), (#const SYS_mremap), (#const SYS_set_thread_area), (#const SYS_times), (#const SYS_time), (#const SYS_readlink), (#const SYS_getcwd)
  #endif
  ]

-- Resources:

common_resources :: Int -> Resources
common_resources t = Resources
  { walltime = t
  , rlimits =
    [ (ResourceCPUTime, simpleResourceLimits $ fromIntegral t)
    , (ResourceOpenFiles, simpleResourceLimits 25)
    , (ResourceTotalMemory, simpleResourceLimits $ 200 * mebi)
    , (ResourceFileSize, simpleResourceLimits $ 5 * mebi)
    ]
  , bufsize = 4 * kibi
  }

cc1plus_resources, as_resources, ld_resources, prog_resources :: Resources

cc1plus_resources = common_resources 10
as_resources = common_resources 5
ld_resources = common_resources 10
prog_resources = common_resources 4
