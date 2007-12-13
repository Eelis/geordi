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

import Prelude hiding (catch, (.), readFile, putStrLn, putStr, print)
import Util

data BotConfig = BotConfig
  { server :: Net.HostName, port :: Net.PortNumber, max_msg_length :: Int
  , chans :: [String], nick :: String, nick_pass :: Maybe String, alternate_nick :: String
  , blacklist :: [String]
  , no_output_msg :: String
  , join_trigger :: Maybe IRC.Message
      -- Defaults to RPL_WELCOME. Can be set to NickServ/cloak confirmations and such.
  } deriving Read

instance Read Net.PortNumber where
  readsPrec i s = (\(x, s') -> (fromIntegral (x :: Int), s')) . readsPrec i s

local_prompt :: String
local_prompt = "\n> "

data Opt = Config String | Help deriving Eq

optsDesc :: [OptDescr Opt]
optsDesc =
  [ Option "c" ["config"] (ReqArg Config "<file>") "Load configuration from <file> instead of \"bot-config\"."
  , Option "h" ["help"] (NoArg Help) "Display this help and exit."
  ]

help :: String
help = usageInfo "Usage: sudo ./Bot [option]...\nOptions:" optsDesc ++ "\nSee README.xhtml for more information."

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case getOpt RequireOrder optsDesc args of
    (_, _, err:_) -> putStr err
    (opts, _, []) | Help `elem` opts -> putStrLn help
    (_, w:_, []) -> putStr "Redundant argument: " >> putStrLn w
    (opts, [], []) -> do
      cfg <- readTypedFile $ maybe "bot-config" id $ findMaybe (\o -> case o of Config cf -> Just cf; _ -> Nothing) opts
      putStrLn $ "connecting to " ++ show (server cfg) ++ " on port " ++ show (Net.PortNum $ fromIntegral $ port cfg)
      withResource (connect (server cfg) (fromIntegral $ port cfg)) $ \h -> do
      print "connected"
      evalRequest <- Request.prepare_evaluator
      System.Posix.Env.setEnv "LC_ALL" "C" True
        -- Otherwise compiler warnings may use non-ASCII characters (e.g. for quotes).
      jail
      let
        send = (>> hFlush h) . mapM_ (hPutStrLn h . IRC.render)
        join_chans = msapp $ msg "JOIN" . (:[]) . chans cfg
        msg = IRC.Message Nothing
        on_msg :: IRC.Message -> IO [IRC.Message]
        on_msg m = flip execStateT [] $ do
          when (join_trigger cfg == Just m) join_chans
          case m of
            IRC.Message (Just (IRC.NickName who _ _)) "QUIT" _ | who == nick cfg ->
              msapp [msg "NICK" [nick cfg]]
            IRC.Message (Just (IRC.NickName from _ _)) "PRIVMSG" [_, "\1VERSION\1"] ->
              msapp [msg "NOTICE" [from, "\1VERSION Geordi C++ bot - http://www.eelis.net/geordi/\1"]]
            IRC.Message _ "433" {- Nick in use. -} _ -> msapp $ [msg "NICK" [alternate_nick cfg]]
            IRC.Message _ "PING" a -> msapp [msg "PONG" a]
            IRC.Message (Just (IRC.NickName fromnick _ _)) "PRIVMSG" [c, txt] ->
              when (c `elem` chans cfg && not (fromnick `elem` blacklist cfg)) $ do
              maybeM (Request.is_request (nick cfg) (alternate_nick cfg) txt) $ \r -> do
              o <- lift $ take (max_msg_length cfg) . takeWhile (/= '\n') . evalRequest r
              msapp [msg "PRIVMSG" [c, if null o then no_output_msg cfg else o]]
            IRC.Message _ "001" {- RPL_WELCOME -} _ -> do
              maybeM (nick_pass cfg) $ \np -> msapp [msg "PRIVMSG" ["NickServ", "identify " ++ np]]
              when (join_trigger cfg == Nothing) join_chans
            _ -> return ()
      send [msg "NICK" [nick cfg], msg "USER" [nick cfg, "0", "*", nick cfg]]
      forever $ do
        Just m <- IRC.parseMessage . (++ "\n") . hGetLine h
        print m
        r <- on_msg m
        mapM_ print r
        send r

connect :: Net.HostName -> Net.PortNumber -> IO Handle
  -- Mostly copied from Network.connectTo. We can't use that one because we want to set SO_KEEPALIVE (and related) options on the socket, which can't be done on a Handle.
connect host portn = do
  proto <- getProtocolNumber "tcp"
  bracketOnError (Net.socket Net.AF_INET Net.Stream proto) Net.sClose $ \sock -> do
  setKeepAlive sock 30 10 5
  Net.connect sock =<< Net.SockAddrInet portn . hostAddress . getHostByName host
  Net.socketToHandle sock ReadWriteMode
