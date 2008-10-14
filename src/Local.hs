#include "Util.h"

import qualified System.Environment
import qualified Request
import qualified System.Console.Readline as RL
import qualified EditCmds
import qualified Sys

import Control.Monad (forM_, when)
import Control.Monad.Fix (fix)
import System.IO.UTF8 (putStrLn)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (listToMaybe)

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

data Memory = Memory { editable_requests :: [String], last_output :: Maybe String }

blankMemory :: Memory
blankMemory = Memory [] Nothing

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
  when (rest == []) $ flip fix blankMemory $ \loop mem -> RL.readline "geordi: " >>= case_of
    Nothing -> putNewLn
    Just "" -> loop mem
    Just "diff" -> do
      case editable_requests mem of
        y : x : _ -> putStrLn $ either id show $ EditCmds.diff x y
        _ -> putStrLn "Need at least two editable requests to compare."
      loop mem
    Just l -> case EditCmds.new_or_edited (listToMaybe $ editable_requests mem) l of
      Left e -> addHistory l >> putStrLn e >> loop (mem { last_output = Nothing })
      Right l' -> addHistory l' >> case Request.parse l' of
        Left e -> putStrLn e >> loop (mem { last_output = Nothing })
        Right r -> do
          o <- evalRequest r
          putStrLn $ describe_new_output (last_output mem) o
          loop $ Memory
            { editable_requests = (if Request.is_editable r then (l':) else id) (editable_requests mem)
            , last_output = Just o }
