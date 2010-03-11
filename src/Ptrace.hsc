{-# LANGUAGE ForeignFunctionInterface #-}
module Ptrace (traceme, syscall, peektext, peekuser, pokeuser, cont, kill, tracesysgood) where

import Foreign.C
import System.Posix
import Control.Monad (when)
import Prelude

#include <sys/ptrace.h>
#include <linux/ptrace.h>
#include <syscall.h>

foreign import ccall "sys/ptrace.h ptrace" c_ptrace :: CInt -> CPid -> CLong -> CLong -> IO CLong

ignored_param :: CLong
ignored_param = 0

ptrace :: CInt -> ProcessID -> CLong -> CLong -> IO CLong
ptrace act procid addr datum = do
  resetErrno
  r <- c_ptrace act procid addr datum
  errno <- getErrno
  if errno /= eOK then throwErrno "ptrace" else return r

traceme :: IO ()
traceme = ptrace (#const PTRACE_TRACEME) 0 ignored_param ignored_param >> return ()

syscall :: ProcessID -> IO ()
syscall p = ptrace (#const PTRACE_SYSCALL) p ignored_param ignored_param >> return ()

peekuser :: ProcessID -> CLong -> IO CLong
peekuser p a = ptrace (#const PTRACE_PEEKUSER) p a ignored_param

peektext :: ProcessID -> CLong -> IO CLong
peektext p a = ptrace (#const PTRACE_PEEKTEXT) p a ignored_param

pokeuser :: ProcessID -> CLong -> CLong -> IO ()
pokeuser p a d = ptrace (#const PTRACE_POKEUSER) p a d >> return ()

cont :: ProcessID -> Maybe CLong -> IO ()
cont p s = ptrace (#const PTRACE_CONT) p ignored_param (maybe 0 id s) >> return ()

tracesysgood :: ProcessID -> IO ()
tracesysgood p = ptrace (#const PTRACE_SETOPTIONS) p
  ignored_param (#const PTRACE_O_TRACESYSGOOD) >> return ()

kill :: ProcessID -> IO ()
kill pid = do
  r <- c_ptrace (#const PTRACE_KILL) pid ignored_param ignored_param
  e <- getErrno
  when (r == -1 && e /= eSRCH) $ throwErrno "ptrace KILL"
