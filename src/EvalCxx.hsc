
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

-- Secure compilation

Requirements:
  First, since we expose cc1plus/as/ld to malicious data (the code), their execution needs to be ptraced.
  Second, we want to specify the names of the intermediary .s and .o files.

If we try to use a single g++ invocation to compile, assemble, and link, we do not get sufficient control over the names of the intermediary files. We /could/ in theory intercept the exec() calls with ptrace and change the file names, but that's too much trouble.

We could invoke cc1plus/as/ld directly (and in fact previous geordi versions did this), but then we need to pass obscure flags that would otherwise be passed by g++. Previous geordi versions had a separate step in the installation procedure where a script was used to strace g++ to find out these flags and write them to a file which was then read by geordi on startup. It was a kludge.

We currently invoke g++ three times. Once with -S to compile, once with -c to assemble, and once to link. This allows us to specify the intermediary files, and lets g++ add whatever obscure flags it wants. Previous versions of geordi did not use this approach because it seems to require letting g++ vfork and then ptracing the child, which makes things complicated (and we don't want complication in this security-critical code). However, by intercepting g++'s vfork and replacing it with "return 0;", we trick it into thinking it is the newly spawned child process, which causes it to exec() cc1plus/as/ld, replacing itself. This scheme works because g++ (when invoked to do only one thing (e.g. compile/assemble/link)) doesn't have anything useful to do after the exec() anyway.

-- Inherited file descriptors

A newly fork()ed child inherits its parent's file descriptors. In our case, that means that things like the network socket are exposed unless we either close them or set FD_CLOEXEC on them before calling execve().

The most obvious approach is to try and vigilantly set FD_CLOEXEC every time the bot opens an FD. However, that means we immediately have a potential security problem if we forget to do so once, or if Haskell is harboring some FDs we don't know about. We want something more rigorous and fool-proof.

The next most obvious approach is to get a list of open FDs and then close them (except for stdout/stderr) just before calling execve(). (At this point, setting FD_CLOEXEC on them would be equivalent.) Some of the BSD's apparently have closefrom() and/or F_CLOSEM commands for fcnl(), which offers this functionality out of the box. Unfortunately, Linux does not have these, so we'd have to do it manually. Unfortunately, getting a list of open FDs while in our chroot (which has no /proc filesystem) turns out to be hard if not impossible.

The next most obvious approach is to get the highest currently open FD, and then close all FDs (except for stdout/stderr) below it, regardless of whether they're open. FreeBSD apparently has a F_MAXFD fcntl() command that returns the greatest FD currently open by the process, which would be exactly what we need, but unfortunately Linux does not have anything equivalent to it.

The approach we resort to is the following:
- make an estimate N how many files the bot will ever want to have open;
- pick an M well above N;
- setrlimit RLIMIT_NOFILE to M immediately upon program startup to make sure that if our M guess was too low, it will cause the bot to shut down with a file open error rather than expose FDs to the child process;
- close FDs in the range [0, M) just before execve().

In our code, M is close_range_end.

-}

module EvalCxx (evalCxx, close_range_end) where

import Prelude hiding (catch, (.))
import Control.Monad
import Control.Monad.Fix
import Foreign
import Foreign.C
import System.IO
import System.Exit
import System.Posix hiding (Stopped, Exited)
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
import Resource

#include <syscall.h>
#include <sys/ptrace.h>
#include <sys/reg.h>
#include <sys/resource.h>

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
strsignal = unsafePerformIO . (peekCString =<<) . c_strsignal

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
  if r /= -1 then d . peek p else do
  e <- getErrno
  if e == eCHILD then return WR_NoChild else throwErrno "wait"
  where
    d :: CInt -> WaitResult
    d s | c_WIFEXITED s /= 0 = WR_Exited $ c_WEXITSTATUS s
    d s | c_WIFSIGNALED s /= 0 = WR_Signaled $ c_WTERMSIG s
    d s | c_WIFSTOPPED s /= 0 = WR_Stopped $ c_WSTOPSIG s
    d _ = error "unknown wait status"

data WaitResult = WR_NoChild | WR_Exited CInt | WR_Signaled CInt | WR_Stopped CInt
  deriving (Show, Eq)

data SuperviseResult = Exited ExitCode | DisallowedSyscall CInt | Signaled Signal | ChildVanished
  deriving Eq

instance Show SuperviseResult where
  show (Exited c) = "Exited: " ++ show c
  show (DisallowedSyscall c) = "Disallowed system call: " ++ syscallName c
  show (Signaled s) = if s == sigALRM then "Timeout" else strsignal s
  show ChildVanished = "Child vanished"

supervise :: ProcessID -> IO SuperviseResult
  -- supervise assumes that the first event observed is the child raising sigSTOP.
  -- wait is called so often that sharing the allocated wstatp is required for acceptable performance.
supervise pid = alloca $ \wstatp -> do
  wait wstatp >>= \s -> when (s /= WR_Stopped sigSTOP) $ fail $ "first ptraced event not sigSTOP, but " ++ show s
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
            when (sc `elem` ignored_syscalls) $ Ptrace.pokeuser pid syscall_ret 0
            Ptrace.syscall pid; sv Nothing
          Nothing -> Ptrace.peekuser pid syscall_off >>= \syscall -> case () of
            _ | syscall `elem` ignored_syscalls -> do
              Ptrace.pokeuser pid syscall_off #const SYS_getpid
              Ptrace.syscall pid; sv (Just syscall)
            _ | syscall `elem` allowed_syscalls -> Ptrace.syscall pid >> sv (Just syscall)
            _ -> do
              Ptrace.pokeuser pid syscall_off #const SYS_exit_group
              Ptrace.kill pid
                -- The documentation for PTRACE_KILL is extremely vague, but it seems that it in this scenario it actually restarts the process, after which one of two things happens: either exit_group succeeds and the next wait returns WR_Exited, or it fails and the process is half dead, twitching, and being delivered SIGKILL. Both cases are dealt with adequately by sv.
              sv $ Just #const SYS_exit_group
              return $ DisallowedSyscall syscall
      WR_Stopped sig -> Ptrace.kill pid >> sv Nothing >> return (Signaled sig)

data Resources = Resources { walltime :: Int, rlimits :: [(CInt, CRLim)], bufsize :: CSize }

close_range_end :: CInt
close_range_end = 25

-- capture_restricted assumes the program produces UTF-8 encoded text and returns it as a proper Unicode String.

capture_restricted :: FilePath -> [String] -> [(String,String)] -> Resources -> IO (SuperviseResult, String)
capture_restricted a argv env (Resources timeout rlims bs) =
  withResource createPipe $ \(pipe_r, pipe_w) -> do
    setFdOption pipe_r NonBlockingRead True
    res <- (=<<) supervise $ forkProcess $ do
      dupTo pipe_w stdError
      dupTo pipe_w stdOutput
      forM_ ([0..close_range_end] \\ [fdOfFd stdOutput, fdOfFd stdError]) c_close
      scheduleAlarm timeout
      mapM (uncurry setrlimit) rlims
      Ptrace.traceme
      raiseSignal sigSTOP
      executeFile a False argv (Just env)
        -- The Haskell implementation of executeFile calls pPrPr_disableITimers, which calls setitimer to disable all interval timers, including ours set a few lines above. However, since by this time we're being ptraced, the setitimer calls are ignored.
      exitImmediately ExitSuccess
    (,) res . UTF8.decode . nonblocking_read pipe_r bs

-- The actual output size is also limited by the pipe buffer.

evalCxx :: FilePath -> [String] -> String -> Bool -> IO String
evalCxx gxx flags code also_run = do
  let
    cap :: [String] -> Resources -> (String -> String) -> IO String -> IO String
    cap argv r err act = do
      (res, out) <- capture_restricted gxx argv [] r
      case res of
        Exited ExitSuccess -> act
        Exited (ExitFailure _) -> return $ err out
        _ -> return $ "g++: " ++ show res
  writeFile "t.cpp" code
  cap (words "-S t.cpp" ++ flags) cc1plus_resources process_cc1plus_errors $ do
  if not also_run then return "Compilation successful" else do
  cap (words "-c t.s" ++ flags) as_resources process_as_errors $ do
  cap (words "t.o -o t" ++ flags) ld_resources process_ld_errors $ do
  (prog_result, prog_output) <- capture_restricted "/t" [] [("GLIBCXX_DEBUG_MESSAGE_LENGTH","0")] prog_resources
  return $ if prog_result == Exited ExitSuccess
    then prog_output
    else process_prog_errors prog_output (show prog_result)

------------- Config (or at least things that are likely more prone to per-site modification):

-- System calls:

ignored_syscalls, allowed_syscalls :: [CInt]

ignored_syscalls = -- These are effectively replaced with "return 0;".
  [(#const SYS_chmod), (#const SYS_fadvise64), (#const SYS_unlink), (#const SYS_munmap), (#const SYS_madvise), (#const SYS_umask), (#const SYS_rt_sigaction), (#const SYS_rt_sigprocmask), (#const SYS_ioctl), (#const SYS_setitimer), (#const SYS_vfork) {- see "Secure compilation" -}]

allowed_syscalls =
  [ (#const SYS_open), (#const SYS_write), (#const SYS_uname), (#const SYS_brk), (#const SYS_read), (#const SYS_mmap), (#const SYS_exit_group), (#const SYS_getpid), (#const SYS_access), (#const SYS_getrusage), (#const SYS_close), (#const SYS_gettimeofday), (#const SYS_writev), (#const SYS_execve), (#const SYS_mprotect), (#const SYS_getcwd)

  #ifdef __x86_64__
    , (#const SYS_stat), (#const SYS_fstat), (#const SYS_arch_prctl), (#const SYS_getrlimit), (#const SYS_fcntl), (#const SYS_lseek), (#const SYS_lstat), (#const SYS_dup)
  #else
    , (#const SYS_fstat64), (#const SYS_lstat64), (#const SYS_stat64), (#const SYS_ugetrlimit), (#const SYS_fcntl64), (#const SYS__llseek), (#const SYS_mmap2), (#const SYS_mremap), (#const SYS_set_thread_area), (#const SYS_times), (#const SYS_time), (#const SYS_readlink)
  #endif
  ]

-- Resources:

common_resources :: Int -> Resources
common_resources t = Resources
  { walltime = t
  , rlimits =
    [ ((#const RLIMIT_CPU), fromIntegral t)
    , ((#const RLIMIT_AS), 200 * mebi)
    , ((#const RLIMIT_FSIZE), 5 * mebi)
    , ((#const RLIMIT_NPROC), 0) -- Strictly redundant since SYS_clone is not allowed.
    , ((#const RLIMIT_CORE), 0)
    , ((#const RLIMIT_MSGQUEUE), 0)
    ]
  , bufsize = 4 * kibi
  }

cc1plus_resources, as_resources, ld_resources, prog_resources :: Resources

cc1plus_resources = common_resources 10
as_resources = common_resources 5
ld_resources = common_resources 10
prog_resources = common_resources 4

-- Note: We don't add RLIMIT_NOFILE here, because it is already set as part of the fd closing scheme described in the "Inherited file descriptors" section at the top of this file, and that "global" limit is sufficient.
