#include "Util.h"

import qualified System.Environment
import qualified Request
import qualified System.Console.Readline as RL
import qualified EditCmds
import qualified Sys
import qualified Data.List

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

main :: IO ()
main = do
  Sys.setlocale_ALL_env
  RL.initialize -- Reads stuff from files not present in the chroot.
  (opts, rest) <- getArgs
  if Help `elem` opts then putStrLn help else do
  evalRequest <- Request.evaluator
  let eval = either return evalRequest . Request.parse
  forM_ rest $ (>>= putStrLn) . eval
  addHistory <- make_history_adder
  when (rest == []) $ flip fix "" $ \loop prev -> RL.readline "geordi: " >>= case_of
    Nothing -> putNewLn
    Just "" -> loop prev
    Just l -> if any (`Data.List.isPrefixOf` l) EditCmds.commands
      then case EditCmds.exec l prev of
        Left e -> do putStrLn e; addHistory l; loop prev
        Right x -> do eval x >>= putStrLn; addHistory x; loop x
      else addHistory l >> case Request.parse l of
        Left e -> putStrLn e >> loop prev
        Right r -> do
          evalRequest r >>= putStrLn
          loop $ if Request.is_editable r then l else prev
