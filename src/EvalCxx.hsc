{-# LANGUAGE UnicodeSyntax, PatternGuards, RecordWildCards, LambdaCase, ViewPatterns, TupleSections #-}

{-- Secure compilation

Requirements:
  First, since we expose cc1plus/as/ld to malicious data (the code), their execution needs to be locked down.
  Second, we want to specify the names of the intermediary .s and .o files.

If we try to use a single g++ invocation to compile, assemble, and link, we do not get sufficient control over the names of the intermediary files.

We could invoke cc1plus/as/ld directly (and in fact previous geordi versions did this), but then we need to pass obscure flags that would otherwise be passed by g++. Previous geordi versions had a separate step in the installation procedure where a script was used to strace g++ to find out these flags and write them to a file which was then read by geordi on startup. It was a kludge.

We currently invoke g++ three times. Once with -S to compile, once with -c to assemble, and once to link. This allows us to specify the intermediary files, and lets g++ add whatever obscure flags it wants. Previous versions of geordi did not use this approach because it seems to require letting g++ vfork and then ptracing the child, which makes things complicated (and we don't want complication in this security-critical code). However, by replacing (with seccomp) g++'s calls to vfork with "return 0;", we trick it into thinking it is the newly spawned child process, which causes it to exec() cc1plus/as/ld, replacing itself. This scheme works because g++ (when invoked to do only one thing (e.g. compile/assemble/link)) doesn't have anything useful to do after the exec() anyway.

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

module EvalCxx (evaluator, WithEvaluation, withEvaluation, noEvaluation, EvaluationResult(..), Request(..), CompileConfig(..)) where

import qualified Flock
import qualified ErrorFilters
import qualified System.Directory
import qualified System.Posix.Process (getProcessID)
import qualified Data.Char as Char

import Paths_geordi (getDataFileName)

import Data.Pointed (Pointed(..))
import Sys (strsignal, chroot, strerror)
import Control.Monad (when, liftM2, forM_)
import System.Environment (getEnvironment)
import System.IO (withFile, IOMode(..), hSetEncoding, utf8, hPutStrLn, hGetContents)
import GHC.IO.Encoding.UTF8 (mkUTF8)
import GHC.IO.Encoding.Failure (CodingFailureMode(TransliterateCodingFailure))
import Foreign.C (CInt, eOK)
import System.Exit (ExitCode(..))
import Data.List ((\\), isPrefixOf)
import System.Posix.User
  (getGroupEntryForName, getUserEntryForName, setGroupID, setUserID, groupID, userID)
import System.Process (createProcess, CmdSpec(..), CreateProcess(..), StdStream(..), waitForProcess)
import System.Posix
  (Signal, sigSEGV, sigILL, openFd, defaultFileFlags, OpenMode(..), Resource(..), ResourceLimit(..), ResourceLimits(..), setResourceLimit)
import Gcc (Stage(..), isMainMissingDiagnostic)
import CompileConfig

#ifndef __x86_64__
#error only x86_64 is supported
#endif

import Prelude hiding ((.))
import Util

#include <sys/reg.h>

data SuperviseResult = Exited ExitCode | Signaled Signal
  deriving Eq

recognizeSignaled :: ExitCode -> SuperviseResult
recognizeSignaled e
  | ExitFailure i <- e, i < 0 = Signaled (fromIntegral $ -i)
  | otherwise = Exited e

instance Show SuperviseResult where
  show (Exited c) = "Exited: " ++ show c
  show (Signaled s) = strsignal s

close_range_end :: CInt
close_range_end = 25

cap_fds :: IO ()
  -- See section "Inherited file descriptors." in EvalCxx.hsc.
cap_fds = do
  let cre = close_range_end
  setResourceLimit ResourceOpenFiles $
    ResourceLimits (ResourceLimit $ fromIntegral cre) (ResourceLimit $ fromIntegral cre)
  high_fds ← filter (>= cre) . (read .) . (\\ [".", ".."]) . (System.Directory.getDirectoryContents =<< (\s → "/proc/" ++ s ++ "/fd") . show . System.Posix.Process.getProcessID)
  when (high_fds /= []) $ fail $ "fd(s) open >= " ++ show cre ++ ": " ++ show high_fds

data CaptureResult = CaptureResult { supervise_result :: SuperviseResult, output :: String } deriving Eq

capture_restricted :: FilePath → [String] → [(String,String)] → IO CaptureResult
  -- We assume the program produces UTF-8 encoded text and return it as a proper Unicode String.
capture_restricted a argv envi = do
  (Nothing, Just stdout_hdl, Nothing, p) <- createProcess CreateProcess{
    cmdspec = RawCommand "/lockdown" (a : argv),
    cwd = Just "/",
    env = Just envi,
    std_in = Inherit,
    std_out = CreatePipe,
    std_err = Inherit,
    close_fds = True,
    create_group = False,
    delegate_ctlc = False}
  hSetEncoding stdout_hdl $ mkUTF8 TransliterateCodingFailure
  liftM2 CaptureResult
    (recognizeSignaled . waitForProcess p)
    (hGetContents stdout_hdl)

subst_parseps :: String → String
subst_parseps = f
  where
    f [] = []
    f (c:s) | c == parsep = f s
    f (c:d:s) | Char.isSpace c, d == parsep = c : f s
    f (c:d:s) | d == parsep = c : case f s of
      [] → []
      s'@(',' : _) → s'
      s' → ' ' : s'
    f (c:s) = c : f s

data EvaluationResult = EvaluationResult Stage CaptureResult
  -- The capture result of the last stage attempted.

instance Show EvaluationResult where
  show (EvaluationResult stage (CaptureResult r o)) = subst_parseps $ ErrorFilters.cleanup_output stage o ++
    if stage == Run
      then case r of
        Exited ExitSuccess → ""
        Signaled s | s ∈ [sigSEGV, sigILL] → parsep : "Undefined behavior detected."
        _ → (parsep : show r)
      else case r of
        Exited ExitSuccess → if null o then strerror eOK else ""
        Exited (ExitFailure _) | not (null o) → ""
        _ → parsep : show stage ++ ": " ++ show r

compile_env :: [(String, String)]
compile_env =
  [("LD_PRELOAD", "/libdiagnose_sigsys.so")]

prog_env :: [(String, String)]
prog_env =
  [ ("GLIBCXX_DEBUG_MESSAGE_LENGTH", "0")
  , ("LD_PRELOAD", "/libtpreload.so /libdiagnose_sigsys.so")
  ]

data JailConfig = JailConfig { user, group :: String } deriving Read

jail :: IO ()
jail = do
  cfg ← getDataFileName "jail-config" >>= readTypedFile
  gid ← groupID . getGroupEntryForName (group cfg)
  uid ← userID . getUserEntryForName (user cfg)
  getDataFileName "rt" >>= chroot
  System.Directory.setCurrentDirectory "/"
  setGroupID gid
  setUserID uid

data Request = Request { units :: [String], stageOfInterest :: Stage, no_warn :: Bool }

pass_env :: String → Bool
pass_env s = ("LC_" `isPrefixOf` s) || (s `elem` ["PATH", "LD_LIBRARY_PATH", "LD_PRELOAD"])

evaluate :: CompileConfig → Request → IO EvaluationResult
evaluate cfg Request{..} = do
  withResource (openFd "lock" ReadOnly Nothing defaultFileFlags) $ \lock_fd → do
  Flock.exclusive lock_fd

  let
    namedUnits :: [(String, String)]
    namedUnits = zip [show i | i <- [0..9::Int]] units

  forM_ namedUnits $ \(unit, code) ->
    withFile unit WriteMode $ \h → hSetEncoding h utf8 >> hPutStrLn h code

  baseEnv ← filter (pass_env . fst) . getEnvironment
  let
    runStages :: [(String, Stage)] → IO EvaluationResult
    runStages [] = error "assert failed ;)"
    runStages ((unit, stage) : more) = do
      capture_restricted (path stage) (argv unit stage) (envi stage) >>= \case
        CaptureResult (Exited (ExitFailure _)) (isMainMissingDiagnostic -> True) | stage == Link
          → return $ EvaluationResult Compile (CaptureResult (Exited ExitSuccess) "")
        CaptureResult (Exited ExitSuccess) "" | not (null more) → runStages more
        cr → return $ EvaluationResult stage cr

    path :: Stage → String
    path Run = "/t"
    path _ = gxxPath cfg

    argv :: String -> Stage → [String]
    argv unit stage = case stage of
        Run → ["second", "third", "fourth"]
        Preprocess → ["-fpch-preprocess", "-E", unit] ++ cf
        Compile → ["-S", "-x", "c++", unit] ++ cf
        Assemble → ["-c", unit ++ ".s"] ++ cf
        Link → ((++ ".o") . fst . namedUnits) ++ ["-o", "t"] ++ cf ++ linkFlags cfg
      where cf = if no_warn then "-w" : compileFlags cfg else compileFlags cfg

    envi :: Stage → [(String, String)]
    envi Run = baseEnv ++ prog_env
    envi _ = baseEnv ++ compile_env

    stages_per_unit =
      if stageOfInterest == Preprocess
        then [Preprocess]
        else [Compile .. min stageOfInterest Assemble]
    final_stages = [Link .. stageOfInterest]

  runStages $ [(unit, s) | (unit, _) <- namedUnits, s <- stages_per_unit] ++ (([],) . final_stages)

data WithEvaluation a
  = WithoutEvaluation a
  | WithEvaluation Request (EvaluationResult → a)

instance Functor WithEvaluation where
  fmap f (WithoutEvaluation x) = WithoutEvaluation (f x)
  fmap f (WithEvaluation r g) = WithEvaluation r (f . g)

instance Pointed WithEvaluation where
  point = WithoutEvaluation

-- WithEvaluation is not a monad because it only supports a single evaluation.

withEvaluation :: Request → WithEvaluation EvaluationResult
withEvaluation r = WithEvaluation r id

noEvaluation :: a → WithEvaluation a
noEvaluation = point

evaluator :: IO (WithEvaluation a → IO a, CompileConfig)
evaluator = do
  cap_fds
  cfg ← readCompileConfig
  jail
  return (\we → case we of
      WithoutEvaluation x → return x
      WithEvaluation r g → g . evaluate cfg r
    , cfg)
