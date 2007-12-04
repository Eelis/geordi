import qualified Network.Socket as Net
import qualified Network.IRC as IRC
import qualified System.Environment
import qualified System.Posix.Terminal
import qualified System.Posix.Env
import qualified Request

import Network.BSD (getProtocolNumber, hostAddress, getHostByName)
import Control.Exception (bracketOnError)
import System.IO (hGetLine, hFlush, stdout, Handle, IOMode(..))
import System.Posix.IO (stdInput)
import Control.Monad (forM_, when)
import Control.Monad.Error ()
import Control.Monad.State (execStateT, lift)
import System.IO.UTF8 (putStr, putStrLn, hPutStrLn)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)

import Prelude hiding (catch, (.), readFile, putStrLn, putStr, print)
import Util

data BotConfig = BotConfig
  { server :: String, port :: Integer, max_msg_length :: Int
  , chans :: [String], nick :: String, nick_pass :: Maybe String, alternate_nick :: String
  , blacklist :: [String]
  , no_output_msg :: String
  , join_trigger :: Maybe IRC.Message
      -- Defaults to ENDOFMOTD. Can be set to NickServ/cloak confirmations and such.
  } deriving Read

local_prompt :: String
local_prompt = "\n> "

data LocalOpt = LO_interactive | LO_config String | LO_help deriving Eq

localOptsDesc :: [OptDescr LocalOpt]
localOptsDesc =
  [ Option "c" ["config"] (ReqArg LO_config "<file>") "Load configuration from <file> instead of \"config\"."
  , Option "i" ["interactive"] (NoArg LO_interactive) "Go into Read-Eval-Print-Loop."
  , Option "h" ["help"] (NoArg LO_help) "Display this help and exit."
  ]

help :: String
help = usageInfo "Usage: sudo ./Bot [option]... [request]...\nOptions:" localOptsDesc ++ "\nSee README.xhtml for more information."

main :: IO ()
main = do
  args <- System.Environment.getArgs
  case getOpt RequireOrder localOptsDesc args of
    (_, _, err:_) -> putStr err
    (opts, rest, []) -> do
      cfg <- readTypedFile $ maybe "config" id $ findMaybe (\o -> case o of LO_config cf -> Just cf; _ -> Nothing) opts
      evalRequest <- Request.prepare_evaluator
      case () of
        ()| LO_help `elem` opts -> putStrLn help
        ()| rest == [] && not (LO_interactive `elem` opts) -> bot cfg evalRequest
        ()| otherwise -> do
          echo <- not . System.Posix.Terminal.queryTerminal stdInput
          jail
          forM_ rest $ (>>= putStrLn) . evalRequest
          when (LO_interactive `elem` opts) $ forever $ do
          putStr local_prompt
          hFlush stdout
          l <- getLine
          when echo $ putStrLn l
          evalRequest l >>= putStrLn

connect :: BotConfig -> IO Handle
  -- Mostly copied from Network.connectTo. We can't use that one because we want to set SO_KEEPALIVE (and related) options on the socket, which can't be done on a Handle.
connect cfg = do
  proto <- getProtocolNumber "tcp"
  bracketOnError (Net.socket Net.AF_INET Net.Stream proto) Net.sClose $ \sock -> do
  setKeepAlive sock 30 10 5
  Net.connect sock =<< Net.SockAddrInet (fromIntegral $ port cfg) . hostAddress . getHostByName (server cfg)
  Net.socketToHandle sock ReadWriteMode

bot :: BotConfig -> (String -> IO String) -> IO ()
bot cfg eval = withResource (connect cfg) $ \h -> do
  System.Posix.Env.setEnv "LC_ALL" "C" True
    -- Otherwise compiler warnings may use non-ASCII characters (e.g. for quotes).
  jail
  let send = (>> hFlush h) . mapM_ (hPutStrLn h . IRC.render)
  send [msg "NICK" [nick cfg], msg "USER" [nick cfg, "0", "*", nick cfg]]
  forever $ (send =<<) . maybe (return []) on_msg =<< IRC.parseMessage . (++ "\n") . hGetLine h
 where
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
        o <- lift $ take (max_msg_length cfg) . takeWhile (/= '\n') . eval r
        msapp [msg "PRIVMSG" [c, if null o then no_output_msg cfg else o]]
      IRC.Message _ "376" {- End of motd. -} _ -> do
        maybeM (nick_pass cfg) $ \np -> msapp [msg "PRIVMSG" ["NickServ", "identify " ++ np]]
        when (join_trigger cfg == Nothing) join_chans
      _ -> return ()
