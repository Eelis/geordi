{-# LANGUAGE CPP #-}

#include "Util.h"

import qualified System.Environment
import qualified RequestEval
import qualified System.Console.Readline as RL
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Sys
import qualified Cxx.Show

import Request (Response(..), HistoryModification(..), Context(..), modify_history)
import Control.Monad (forM_, when)
import Control.Monad.Fix (fix)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import Data.IORef (newIORef, readIORef, writeIORef)

import Prelude hiding ((.), readFile)
import Util

data Opt = Help deriving Eq

optsDesc :: [OptDescr Opt]
optsDesc = [Option "h" ["help"] (NoArg Help) "Display this help and exit."]

help :: String
help = usageInfo "Usage: sudo geordi-local [option]... [request]...\nOptions:" optsDesc ++ "\nSee README.xhtml for more information."

getArgs :: IO ([Opt], [String])
getArgs = do
  args ← System.Environment.getArgs
  case getOpt RequireOrder optsDesc args of
    (_, _, err:_) → fail $ init err
    (opts, rest, []) → return (opts, rest)

make_history_adder :: IO (String → IO ())
make_history_adder = do
  r ← newIORef Nothing
  return $ \s → do
    prev ← readIORef r
    when (Just s ≠ prev) $ do
      RL.addHistory s
      writeIORef r (Just s)

data Memory = Memory { context :: Context, last_output :: Maybe String }

blankMemory :: Memory
blankMemory = Memory (Context []) Nothing

main :: IO ()
main = do
  Sys.setlocale_ALL_env
  RL.initialize -- Reads stuff from files not present in the chroot.
  (opts, rest) ← getArgs
  if Help ∈ opts then putStrLn help else do
  eval ← RequestEval.evaluator Cxx.Show.noHighlighting
  forM_ rest $ \l → do Request.Response _ output ← eval l (Context []); putStrLn output
  addHistory ← make_history_adder
  when (rest == []) $ flip fix blankMemory $ \loop mem → (UTF8.decodeString .) . RL.readline "geordi: " >>= case_of
    Nothing → putNewLn
    Just "" → loop mem
    Just l → do
      Response history_modification output ← eval l $ context mem
      case history_modification of
        Just (AddLast e) → addHistory $ UTF8.encodeString $ show e
        Just (ReplaceLast e) → addHistory $ UTF8.encodeString $ show e
        _ → return ()
      putStrLn $ describe_new_output (last_output mem) output
      loop Memory
        { context = maybe id modify_history history_modification $ context mem
        , last_output = Just output }
