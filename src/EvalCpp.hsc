
module EvalCpp (evalCpp) where

import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (catch, (.))
import Control.Monad
import Control.Monad.Fix
import Foreign
import Foreign.C
import System.IO
import System.Exit
import System.Posix hiding (Stopped, Exited)
import System.Posix.Error
import System.Posix.Internals
import Text.Regex
import Util
import qualified Ptrace
import Data.Word
import Data.Char
import qualified Codec.Binary.UTF8.String as UTF8
import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token

#include <syscall.h>
#include <sys/ptrace.h>
#include <sys/reg.h>

syscall_off :: CInt
#ifdef __x86_64__
syscall_off = (#const ORIG_RAX) * 8
#else
syscall_off = (#const ORIG_EAX) * 4
#endif

foreign import ccall unsafe "__hsunix_wifexited" c_WIFEXITED :: CInt -> CInt
foreign import ccall unsafe "__hsunix_wexitstatus" c_WEXITSTATUS :: CInt -> CInt
foreign import ccall unsafe "__hsunix_wifsignaled" c_WIFSIGNALED :: CInt -> CInt
foreign import ccall unsafe "__hsunix_wtermsig" c_WTERMSIG :: CInt -> CInt
foreign import ccall unsafe "__hsunix_wifstopped" c_WIFSTOPPED :: CInt -> CInt
foreign import ccall unsafe "__hsunix_wstopsig" c_WSTOPSIG :: CInt -> CInt

-- OS calls:

foreign import ccall unsafe "string.h strsignal" c_strsignal :: CInt -> IO CString

strsignal :: CInt -> IO String
strsignal s = c_strsignal s >>= peekCString

foreign import ccall unsafe "execve" c_execve :: CString -> Ptr CString -> Ptr CString -> IO CInt

myExecuteFile :: FilePath -> [String] -> [String] -> IO ()
myExecuteFile path args env = {- path is also used as first arg -}
  withCString path $ \s ->
  withMany withCString (path:args) $ \cstrs -> withArray0 nullPtr cstrs $ \arg_arr ->
  withMany withCString env $ \cenv -> withArray0 nullPtr cenv $ \env_arr ->
  throwErrnoPathIfMinus1_ "executeFile" path (c_execve s arg_arr env_arr)

{- myExecuteFile is heavily based on unix/System/Posix/Process.hsc' executeFile. Differences:
- no pPrPr_disableITimers (because we need our kill timer) (this is the primary difference);
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

data SuperviseResult = Exited ExitCode | DisallowedSyscall CInt | Signaled Signal deriving Eq

show_SuperviseResult :: Map CInt String -> SuperviseResult -> IO String
show_SuperviseResult sn sr = case sr of
  Exited c -> return $ "Exited: " ++ show c
  DisallowedSyscall c -> return $ "Disallowed system call: " ++ maybe (show c) id (Map.lookup c sn)
  Signaled s -> if s == sigALRM then return "Timeout" else strsignal s

foreign import ccall "wait" c_wait :: Ptr CInt -> IO CPid

supervise :: ProcessID -> IO SuperviseResult
supervise pid = alloca $ \wstatp -> fix $ \sv -> do
  throwErrnoIfMinus1_ "wait" $ c_wait wstatp
  wstat <- peek wstatp
  case () of
    _ | c_WIFEXITED wstat /= 0 -> let e = c_WEXITSTATUS wstat in
      return $ Exited $ if e == 0 then ExitSuccess else ExitFailure $ fromIntegral e
    _ | c_WIFSIGNALED wstat /= 0 -> return $ Signaled $ c_WTERMSIG wstat
    _ | c_WIFSTOPPED wstat /= 0 -> let stopsig = c_WSTOPSIG wstat in
      if stopsig /= sigTRAP then Ptrace.kill pid >> sv >> return (Signaled stopsig) else do
        syscall <- Ptrace.peekuser pid syscall_off
        if elem syscall allowed_syscalls
          then Ptrace.syscall pid >> sv
          else do
            Ptrace.pokeuser pid syscall_off #const SYS_exit_group
            Ptrace.cont pid Nothing; sv
            return $ DisallowedSyscall syscall
    _ -> fail "wait() returned unexpectedly"

simpleResourceLimits :: Integer -> ResourceLimits
simpleResourceLimits l = ResourceLimits (ResourceLimit l) (ResourceLimit l)

data Resources = Resources { walltime :: Integer, rlimits :: [(Resource, ResourceLimits)], bufsize :: CSize }

-- capture_restricted assumes the program produces UTF-8 encoded text and returns it as a proper Unicode String.

capture_restricted :: FilePath -> [String] -> [String] -> Resources -> IO (SuperviseResult, String)
capture_restricted a argv env (Resources timeout rlims bs) =
  withResource createPipe $ \(pipe_r, pipe_w) -> do
    setFdOption pipe_r NonBlockingRead True
    res <- (=<<) supervise $ forkProcess $ do
      dupTo pipe_w stdError
      dupTo pipe_w stdOutput
      closeFd stdInput
      Ptrace.traceme
      scheduleAlarm $ fromIntegral timeout
      mapM (uncurry setResourceLimit) rlims
      myExecuteFile a argv env
      exitImmediately ExitSuccess
    output <- nonblocking_read pipe_r bs
    return (res, UTF8.decode output)

-- The actual output size is also limited by the pipe buffer.

unistd_file :: FilePath
#ifdef __x86_64__
unistd_file = "/usr/include/asm-x86_64/unistd.h"
#else
unistd_file = "/usr/include/asm/unistd.h"
#endif

syscall_names :: IO (Map CInt String)
syscall_names = Map.fromList . mapMaybe (either (const Nothing) Just . parse par "") . lines . readFile unistd_file
  where
    par = do
      spaces; char '#'; spaces; string "define"; spaces; string "__NR_"
      name <- many $ satisfy $ \c -> isAlphaNum c || c == '_'
      number <- fromIntegral . (spaces >> natural haskell)
      spaces; eof
      return (number, name)

evalCpp :: IO (String -> Bool -> IO String)
  -- Two-stage IO: first must happen before jail time, because gcc-execs and the syscall names need to be read from file, second happens inside jail and does actual evaluation.

evalCpp = do
  show_sr <- show_SuperviseResult . syscall_names
  (cc1plus, as, ld) <- read . readFile "gcc-execs"
  let
    cap :: FilePath -> [String] -> Resources -> (String -> String) -> IO String -> IO String
    cap a argv r err act = do
      (res, out) <- capture_restricted a argv [] r
      case res of
        Exited ExitSuccess -> act
        Exited (ExitFailure _) -> return $ err out
        _ -> ((leaf a ++ ": ") ++) . show_sr res
  return $ \code also_run -> do
    writeFile "t.cpp" code
    cap (head cc1plus) (tail cc1plus) cc1plus_resources process_cc1plus_errors $ do
    if not also_run then return "Compilation successful" else do
    cap (head as) (tail as) as_resources process_as_errors $ do
    cap (head ld) (tail ld) ld_resources process_ld_errors $ do
    (prog_result, prog_output) <- capture_restricted "/t" [] ["GLIBCXX_DEBUG_MESSAGE_LENGTH=0"] prog_resources
    if prog_result == Exited ExitSuccess then return prog_output else return . process_prog_errors prog_output =<< show_sr prog_result

------------- Config (or at least things that are likely more prone to per-site modification):

-- System calls

allowed_syscalls :: [CInt]
allowed_syscalls =
  [ (#const SYS_execve) -- Only needed for return of initial execve call.
  , (#const SYS_open), (#const SYS_write), (#const SYS_uname), (#const SYS_brk), (#const SYS_read)
  , (#const SYS_mmap), (#const SYS_close), (#const SYS_mprotect), (#const SYS_munmap)
  , (#const SYS_exit_group), (#const SYS_getpid), (#const SYS_access), (#const SYS_getrusage)
  , (#const SYS_umask), (#const SYS_chmod), (#const SYS_fadvise64), (#const SYS_ioctl)
  , (#const SYS_gettimeofday), (#const SYS_writev)

  , (#const SYS_unlink) -- as insists on attempting to unlink t.o just before it starts writing to it. We could choose to have it create a new file, but then we need to allow creation of new files, which is less desireable. unlink will fail because nobody does not have write access to rt, and as will ignore that failure.

  , (#const SYS_rt_sigaction) -- This cannot be used to circumvent the alarm, because even if a signal handler (or SIG_IGN) is installed, ptrace still intercepts the signal.

  #ifdef __x86_64__
    , (#const SYS_stat), (#const SYS_fstat), (#const SYS_arch_prctl), (#const SYS_getrlimit), (#const SYS_fcntl), (#const SYS_lseek), (#const SYS_lstat), (#const SYS_dup)
  #else
    , (#const SYS_fstat64), (#const SYS_lstat64), (#const SYS_stat64), (#const SYS_ugetrlimit), (#const SYS_fcntl64), (#const SYS__llseek), (#const SYS_mmap2), (#const SYS_madvise), (#const SYS_mremap), (#const SYS_set_thread_area), (#const SYS_times), (#const SYS_time), (#const SYS_readlink), (#const SYS_getcwd)
  #endif
  ]

-- Error string processing:

process_as_errors, process_ld_errors, process_cc1plus_errors :: String -> String

process_as_errors e = maybe e (!!1) $ matchRegex (mkRegex "t.s: Assembler messages:\nt.s:[0-9]+: (Error|Warning): ([^\n]*)") e

process_ld_errors e = maybe e head $ matchRegex (mkRegex "\\b(undefined reference to [^\n]*)") e

process_cc1plus_errors e = maybe e' (!!1) $ matchRegex (mkRegex "\\b(error|warning): ([^\n]*)") e'
  where
    e' = foldl (\u (regex, repl) -> subRegex (mkRegex regex) u repl) e $
      [ ("\\bstd::", "")
      , ("\\b(vector|list|deque)<([\\w\\s]+), allocator<(\\2)> >", "\\1<\\2>")
      , ("\\bbasic_(string|[io]?(f|string)?stream)<(\\w+), char_traits<(\\3)>(, allocator<(\\3)>)? ?>", "basic_\\1<\\3>")
      , ("\\bbasic_(string|[io]?(f|string)?stream)<char>", "\\1")
      , ("\\b__gnu_(cxx|debug(_def)?)::", "")
      ]

process_prog_errors :: String -> String -> String
process_prog_errors output result =
  maybe (output ++ result) head $ matchRegex (mkRegex ":error: ([^\n]*)") output

-- Resources:

common_resources :: Integer -> Resources
common_resources t = Resources
  { walltime = t
  , rlimits =
    [ (ResourceCPUTime, simpleResourceLimits t)
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
