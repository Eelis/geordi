import qualified Network.Socket as Net
import qualified Network.IRC as IRC
import qualified System.Environment
import qualified System.Posix.Env
import qualified Request

import Network.BSD (getProtocolNumber, hostAddress, getHostByName)
import Control.Exception (bracketOnError)
import System.IO (hGetLine, hFlush, Handle, IOMode(..))
import Control.Monad (when)
import Control.Monad.Error ()
import Control.Monad.State (execStateT, lift)
import System.IO.UTF8 (putStr, putStrLn, hPutStrLn, print)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import Sys (setKeepAlive)

import Prelude hiding (catch, (.), readFile, putStrLn, putStr, print)
import Util

data BotConfig = BotConfig
  { server :: String, port :: Net.PortNumber, max_msg_length :: Int
  , chans :: [String], nick :: String, nick_pass :: Maybe String, alternate_nick :: String
  , blacklist :: [String]
  , no_output_msg :: String
  , join_trigger :: Maybe IRC.Message
      -- Defaults to RPL_WELCOME. Can be set to NickServ/cloak confirmations and such.
  } deriving Read

instance Read Net.PortNumber where
  readsPrec i s = (\(x, s') -> (fromIntegral (x :: Int), s')) . readsPrec i s

data Opt = Config String | Help deriving Eq

optsDesc :: [OptDescr Opt]
optsDesc =
  [ Option "c" ["config"] (ReqArg Config "<file>") "Load configuration from <file> instead of \"bot-config\"."
  , Option "h" ["help"] (NoArg Help) "Display this help and exit."
  ]

help :: String
help = usageInfo "Usage: sudo ./Bot [option]...\nOptions:" optsDesc ++ "\nSee README.xhtml for more information."

getArgs :: IO [Opt]
getArgs = do
  args <- System.Environment.getArgs
  case getOpt RequireOrder optsDesc args of
    (_, _, err:_) -> fail $ init err
    (_, w:_, []) -> fail $ "superfluous command line argument: " ++ w
    (opts, [], []) -> return opts

msg :: IRC.Command -> [IRC.Parameter] -> IRC.Message
msg = IRC.Message Nothing

main :: IO ()
main = do
  opts <- getArgs
  if Help `elem` opts then putStrLn help else do
  cfg <- readTypedFile $ maybe "bot-config" id $ findMaybe (\o -> case o of Config cf -> Just cf; _ -> Nothing) opts
  putStrLn $ "Connecting to " ++ server cfg ++ ":" ++ show (port cfg)
  withResource (connect (server cfg) (fromIntegral $ port cfg)) $ \h -> do
  putStrLn "Connected"
  System.Posix.Env.setEnv "LC_ALL" "C" True
    -- Otherwise compiler diagnostics may use non-ASCII characters (e.g. for quotes).
  evalRequest <- Request.prepare_evaluator
  let send m = hPutStrLn h (IRC.render m) >> hFlush h
  send $ msg "NICK" [nick cfg]
  send $ msg "USER" [nick cfg, "0", "*", nick cfg]
  forever $ do
    l <- hGetLine h
    case IRC.parseMessage (l ++ "\n") of
      Nothing -> putStr "Malformed IRC message: " >> putStrLn l
      Just m -> do
        print m
        r <- on_msg evalRequest cfg m
        mapM_ print r
        mapM_ send r

on_msg :: (Functor m, Monad m) => (String -> m String) -> BotConfig -> IRC.Message -> m [IRC.Message]
on_msg eval cfg m = flip execStateT [] $ do
  when (join_trigger cfg == Just m) join_chans
  case m of
    IRC.Message (Just (IRC.NickName who _ _)) "QUIT" _ | who == nick cfg ->
      send $ msg "NICK" [nick cfg]
    IRC.Message (Just (IRC.NickName from _ _)) "PRIVMSG" [_, "\1VERSION\1"] ->
      send $ msg "NOTICE" [from, "\1VERSION Geordi C++ bot - http://www.eelis.net/geordi/\1"]
    IRC.Message _ "433" {- ERR_NICKNAMEINUSE -} _ -> send $ msg "NICK" [alternate_nick cfg]
    IRC.Message _ "PING" a -> msapp [msg "PONG" a]
    IRC.Message (Just (IRC.NickName fromnick _ _)) "PRIVMSG" [c, txt] ->
      when (c `elem` chans cfg && not (fromnick `elem` blacklist cfg)) $ do
      maybeM (Request.is_request (nick cfg) (alternate_nick cfg) txt) $ \r -> do
      o <- lift $ take (max_msg_length cfg) . takeWhile (/= '\n') . eval r
      send $ msg "PRIVMSG" [c, if null o then no_output_msg cfg else o]
    IRC.Message _ "001" {- RPL_WELCOME -} _ -> do
      maybeM (nick_pass cfg) $ \np -> send $ msg "PRIVMSG" ["NickServ", "identify " ++ np]
      when (join_trigger cfg == Nothing) join_chans
    _ -> return ()
  where
    send = msapp . (:[])
    join_chans = msapp $ msg "JOIN" . (:[]) . chans cfg

connect :: String -> Net.PortNumber -> IO Handle
  -- Mostly copied from Network.connectTo. We can't use that one because we want to set SO_KEEPALIVE (and related) options on the socket, which can't be done on a Handle.
connect host portn = do
  proto <- getProtocolNumber "tcp"
  bracketOnError (Net.socket Net.AF_INET Net.Stream proto) Net.sClose $ \sock -> do
  setKeepAlive sock 30 10 5
  Net.connect sock =<< Net.SockAddrInet portn . hostAddress . getHostByName host
  Net.socketToHandle sock ReadWriteMode
