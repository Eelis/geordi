import qualified Network.Socket as Net
import qualified Network.IRC as IRC
import qualified System.Environment
import qualified System.Posix.Env
import qualified Request
import qualified Codec.Binary.UTF8.String as UTF8

import Network.BSD (getProtocolNumber, hostAddress, getHostByName)
import Control.Exception (bracketOnError)
import System.IO (hGetLine, hPutStrLn, hFlush, Handle, IOMode(..))
import Control.Monad (when)
import Control.Monad.Error ()
import Control.Monad.State (execStateT, lift)
import System.IO.UTF8 (putStr, putStrLn, print)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import Sys (setKeepAlive)
import Text.Regex (Regex, subRegex, mkRegex)

import Prelude hiding (catch, (.), readFile, putStrLn, putStr, print)
import Util

data IrcBotConfig = IrcBotConfig
  { server :: String, port :: Net.PortNumber, max_msg_length :: Int
  , chans :: [String], nick :: String, nick_pass :: Maybe String, alternate_nick :: String
  , blacklist :: [String]
  , no_output_msg :: String
  , join_trigger :: Maybe IRC.Message
      -- Defaults to RPL_WELCOME. Can be set to NickServ/cloak confirmations and such.
  , censor :: [Regex]
  , rate_limit_messages, rate_limit_window :: Int
  , serve_private_requests :: Bool
  } deriving Read

instance Read Regex where
  readsPrec i s = (\(a, r) -> (mkRegex a, r)) . readsPrec i s

instance Read Net.PortNumber where
  readsPrec i s = (\(x, s') -> (fromIntegral (x :: Int), s')) . readsPrec i s

data Opt = Config String | Help deriving Eq

optsDesc :: [OptDescr Opt]
optsDesc =
  [ Option "c" ["config"] (ReqArg Config "<file>") "Load configuration from <file> instead of \"irc-config\"."
  , Option "h" ["help"] (NoArg Help) "Display this help and exit."
  ]

help :: String
help = usageInfo "Usage: sudo ./geordi-irc [option]...\nOptions:" optsDesc ++ "\nSee README.xhtml for more information."

getArgs :: IO [Opt]
getArgs = do
  args <- System.Environment.getArgs
  case getOpt RequireOrder optsDesc args of
    (_, _, err:_) -> fail $ init err
    (_, w:_, []) -> fail $ "superfluous command line argument: " ++ w
    (opts, [], []) -> return opts

msg :: IRC.Command -> [IRC.Parameter] -> IRC.Message
msg = IRC.Message Nothing

do_censor :: IrcBotConfig -> String -> String
do_censor cfg s = foldr (\r t -> subRegex r t "<censored>") s (censor cfg)

utf8_encode_upto :: Int -> String -> String
utf8_encode_upto n (c:s) | c' <- UTF8.encodeString [c], n' <- n - length c', n' >= 0 =
  c' ++ utf8_encode_upto n' s
utf8_encode_upto _ _ = ""
  -- Only encodes as much characters as can fit in the allotted number of bytes.

send_irc_msg :: Handle -> IRC.Message -> IO ()
send_irc_msg h m = hPutStrLn h (utf8_encode_upto 450 $ IRC.render m) >> hFlush h
  -- If we use (System.IO.UTF8.hPutStrLn $ IRC.render m), we risk sending a message longer than allowed by the IRC spec. If we use (System.IO.hPutStrLn $ take 510 $ UTF8.encodeString $ IRC.render m) to fix this, take might cut the last UTF-8 encoded character in half if its encoding consists of multiple bytes. We want to avoid this because it causes some IRC clients (like irssi) to conclude that the encoding must be something other than UTF-8. If we use (System.IO.hPutStrLn $ utf8_encode_upto 510 $ IRC.render m) to fix this, we still risk the aforementioned character cutting because even if the message fit when we sent it to the server, it might not fit in the message sent to the client (because that message might include a longer prefix). Hence our limit of 450 bytes.

main :: IO ()
main = do
  opts <- getArgs
  if Help `elem` opts then putStrLn help else do
  cfg <- readTypedFile $ findMaybe (\o -> case o of Config cf -> Just cf; _ -> Nothing) opts `orElse` "irc-config"
  full_evaluate $ do_censor cfg "abc" -- So that any mkRegex failures occur before we start connecting.
  putStrLn $ "Connecting to " ++ server cfg ++ ":" ++ show (port cfg)
  withResource (connect (server cfg) (fromIntegral $ port cfg)) $ \h -> do
  putStrLn "Connected"
  System.Posix.Env.setEnv "LC_ALL" "C" True
    -- Otherwise compiler diagnostics may use non-ASCII characters (e.g. for quotes).
  evalRequest <- Request.evaluator
  limit_rate <- rate_limiter (rate_limit_messages cfg) (rate_limit_window cfg)
  let send m = limit_rate >> send_irc_msg h m
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

on_msg :: (Functor m, Monad m) => (String -> m String) -> IrcBotConfig -> IRC.Message -> m [IRC.Message]
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
      when (not (fromnick `elem` blacklist cfg)) $ do
      let
        private = elemBy caselessStringEq c [nick cfg, alternate_nick cfg]
        reply s = send $ msg "PRIVMSG" [if private then fromnick else c, s]
      if private && not (serve_private_requests cfg)
       then reply "This bot does not serve private requests."
       else case Request.is_request [nick cfg, alternate_nick cfg] txt of
        Just r -> do
          o <- lift $ take (max_msg_length cfg) . takeWhile (/= '\n') . eval r
          reply $ if null o then no_output_msg cfg else do_censor cfg o
        Nothing | private -> reply "Not a valid request. See http://www.eelis.net/geordi/ for usage syntax."
        Nothing -> return ()
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
