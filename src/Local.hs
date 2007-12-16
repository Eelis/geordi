import qualified System.Environment
import qualified System.Posix.Terminal
import qualified Request

import System.IO (hFlush, stdout)
import System.Posix.IO (stdInput)
import Control.Monad (forM_, when)
import System.IO.UTF8 (putStr, putStrLn)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)

import Prelude hiding (catch, (.), readFile, putStrLn, putStr, print)
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
  (opts, rest) <- getArgs
  if Help `elem` opts then putStrLn help else do
  echo <- not . System.Posix.Terminal.queryTerminal stdInput
  evalRequest <- Request.prepare_evaluator
  forM_ rest $ (>>= putStrLn) . evalRequest
  when (rest == []) $ forever $ do
    putStr "\ngeordi: "
    hFlush stdout
    l <- getLine
    when echo $ putStrLn l
    evalRequest l >>= putStrLn
