import qualified System.Environment
import qualified Request
import qualified System.Console.Readline as RL
import qualified Sys

import Control.Monad (forM_, when)
import Control.Monad.Fix (fix)
import System.IO.UTF8 (putStrLn)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)

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

main :: IO ()
main = do
  Sys.setlocale_ALL_env
  RL.initialize -- Reads stuff from files not present in the chroot.
  (opts, rest) <- getArgs
  if Help `elem` opts then putStrLn help else do
  evalRequest <- Request.evaluator
  forM_ rest $ (>>= putStrLn) . evalRequest
  when (rest == []) $ fix $ \loop -> do
    ml <- RL.readline "geordi: "
    case ml of
      Nothing -> putNewLn
      Just l -> do
        when (l /= "") $ do
          evalRequest l >>= putStrLn
          RL.addHistory l
        loop
