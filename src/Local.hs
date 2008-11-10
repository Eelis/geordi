#include "Util.h"

import qualified System.Environment
import qualified Request
import qualified RequestEval
import qualified System.Console.Readline as RL
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Sys

import Control.Monad (forM_, when)
import Control.Monad.Fix (fix)
import System.IO.UTF8 (putStrLn)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import Data.IORef (newIORef, readIORef, writeIORef)

import Prelude hiding ((.), readFile, putStrLn)
import Util

data Opt = Help deriving Eq

optsDesc :: [OptDescr Opt]
optsDesc = [Option "h" ["help"] (NoArg Help) "Display this help and exit."]

help :: String
help = usageInfo "Usage: sudo ./Local [option]... [request]...\nOptions:" optsDesc ++ "\nSee README.xhtml for more information."

getArgs :: IO ([Opt], [String])
getArgs = do
  args <- System.Environment.getArgs
  case getOpt RequireOrder optsDesc args of
    (_, _, err:_) -> fail $ init err
    (opts, rest, []) -> return (opts, rest)

make_history_adder :: IO (String -> IO ())
make_history_adder = do
  r <- newIORef Nothing
  return $ \s -> do
    prev <- readIORef r
    when (Just s /= prev) $ do
      RL.addHistory s
      writeIORef r (Just s)

data Memory = Memory { editable_requests :: [Request.EditableRequest], last_output :: Maybe String }

blankMemory :: Memory
blankMemory = Memory [] Nothing

main :: IO ()
main = do
  Sys.setlocale_ALL_env
  RL.initialize -- Reads stuff from files not present in the chroot.
  (opts, rest) <- getArgs
  if Help `elem` opts then putStrLn help else do
  eval <- RequestEval.evaluator
  forM_ rest $ \l -> do Request.Response _ output <- eval l (Request.Context []); putStrLn output
  addHistory <- make_history_adder
  when (rest == []) $ flip fix blankMemory $ \loop mem -> (UTF8.decodeString .) . RL.readline "geordi: " >>= case_of
    Nothing -> putNewLn
    Just "" -> loop mem
    Just l -> do
      Request.Response history_addition output <- eval l $ Request.Context $ editable_requests mem
      maybeM history_addition $ addHistory . UTF8.encodeString . show
      putStrLn $ describe_new_output (last_output mem) output
      loop $ Memory
        { editable_requests = (maybe id (:) history_addition) (editable_requests mem)
        , last_output = Just output }
