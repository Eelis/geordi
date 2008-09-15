import qualified Network.Socket as Net
import qualified Network.IRC as IRC
import qualified System.Environment
import qualified Request
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Sys
import qualified Data.Map as Map
import qualified EditCmds
import qualified Network.BSD

import Control.Exception (bracketOnError)
import System.IO (hGetLine, hPutStrLn, hFlush, Handle, IOMode(..))
import Control.Monad (forever, when)
import Control.Monad.Error ()
import Control.Monad.State (execStateT, lift, StateT)
import System.IO.UTF8 (putStr, putStrLn, print)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import Text.Regex (Regex, subRegex, mkRegex)
import Data.Char (toUpper, toLower, isSpace)
import Data.Map (Map)
import Data.List (isPrefixOf)

import Prelude hiding (catch, (.), readFile, putStrLn, putStr, print)
import Util

data IrcBotConfig = IrcBotConfig
  { server :: Net.HostName, port :: Net.PortNumber, max_msg_length :: Int
  , chans :: [String], nick :: String, nick_pass :: Maybe String, alternate_nick :: String
  , also_respond_to :: [String]
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
  Sys.setlocale_ALL_env
  opts <- getArgs
  if Help `elem` opts then putStrLn help else do
  cfg <- readTypedFile $ findMaybe (\o -> case o of Config cf -> Just cf; _ -> Nothing) opts `orElse` "irc-config"
  full_evaluate $ do_censor cfg "abc" -- So that any mkRegex failures occur before we start connecting.
  putStrLn $ "Connecting to " ++ server cfg ++ ":" ++ show (port cfg)
  withResource (connect (server cfg) (fromIntegral $ port cfg)) $ \h -> do
  putStrLn "Connected"
  evalRequest <- Request.evaluator
  limit_rate <- rate_limiter (rate_limit_messages cfg) (rate_limit_window cfg)
  let send m = limit_rate >> send_irc_msg h m
  send $ msg "NICK" [nick cfg]
  send $ msg "USER" [nick cfg, "0", "*", nick cfg]
  flip execStateT Map.empty $ forever $ do
    l <- lift $ hGetLine h
    case IRC.parseMessage (l ++ "\n") of
      Nothing -> lift $ putStr "Malformed IRC message: " >> putStrLn l
      Just m -> do
        lift $ print m
        r <- on_msg evalRequest cfg (length l == 511) m
        lift $ mapM_ print r >> mapM_ send r
  return ()

discarded_lines_description :: Int -> String
discarded_lines_description s =
  " [+ " ++ show s ++ " discarded line" ++ (if s == 1 then "" else "s") ++ "]"

data ChannelMemory = ChannelMemory { last_request, last_output :: String }
type ChannelMemoryMap = Map String ChannelMemory

is_request :: Bool -> [String] -> String -> Maybe String
is_request _ botnicks s | Just (n, r) <- Request.is_request s, any (\(h:t) -> n == toLower h : t || n == toUpper h : t) botnicks = Just r
is_request True _ s = Just s
is_request _ _ _ = Nothing

type Reason = String
data Permission = Allow | Deny (Maybe Reason)
data Where = Private | InChannel String

request_allowed :: IrcBotConfig -> String -> Maybe IRC.UserName -> Maybe IRC.ServerName -> Where -> Permission
request_allowed cfg _ _ _ Private | not (serve_private_requests cfg) =
  Deny $ Just "This bot does not serve private requests."
request_allowed cfg nickname _ _ _ | nickname `elem` blacklist cfg = Deny Nothing
request_allowed _ _ _ _ _ = Allow

on_msg :: (Functor m, Monad m) =>
  (String -> m String) -> IrcBotConfig -> Bool -> IRC.Message -> StateT ChannelMemoryMap m [IRC.Message]
on_msg eval cfg full_size m = flip execStateT [] $ do
  when (join_trigger cfg == Just m) join_chans
  case m of
    IRC.Message (Just (IRC.NickName who _ _)) "QUIT" _ | who == nick cfg ->
      send $ msg "NICK" [nick cfg]
    IRC.Message (Just (IRC.NickName from _ _)) "PRIVMSG" [_, "\1VERSION\1"] ->
      send $ msg "NOTICE" [from, "\1VERSION Geordi C++ bot - http://www.eelis.net/geordi/\1"]
    IRC.Message _ "433" {- ERR_NICKNAMEINUSE -} _ -> send $ msg "NICK" [alternate_nick cfg]
    IRC.Message _ "PING" a -> msapp [msg "PONG" a]
    IRC.Message _ "PRIVMSG" [_, '\1':_] -> return ()
    IRC.Message (Just (IRC.NickName who muser mserver)) "PRIVMSG" [to, txt] ->
      when (not (who `elem` blacklist cfg)) $ do
      let private = elemBy caselessStringEq to [nick cfg, alternate_nick cfg]
      let wher = if private then who else to
      let reply s = send $ msg "PRIVMSG" [wher, if null s then no_output_msg cfg else do_censor cfg s]
      maybeM (dropWhile isSpace . is_request private (nick cfg : alternate_nick cfg : also_respond_to cfg) txt) $ \r -> do
      case request_allowed cfg who muser mserver (if private then Private else InChannel to) of
        Deny reason -> maybeM reason reply
        Allow -> do
          if full_size && maybe True (not . (`elem` "};")) (maybeLast r) then reply $ "Request likely truncated after " ++ show (reverse $ take 15 $ reverse r) ++ "." else do
            -- The `elem` "};" condition gains a reduction in false positives at the cost of an increase in false negatives.
          u <- lift $ readState
          let mmem = Map.lookup wher u
          if r == "show" then reply (maybe "<none>" last_request mmem) else do
          mr <- if any (`isPrefixOf` r) EditCmds.commands
            then case mmem of
              Nothing -> reply "There is no previous request to modify." >> return Nothing
              Just mem -> case EditCmds.exec r (last_request mem) of
                Left e -> reply e >> return Nothing
                Right r' | length r' > max_msg_length cfg -> reply "Request would become too large." >> return Nothing
                Right r' -> return $ Just $ r'
            else return $ Just r
          maybeM mr $ \r' -> do
            l <- lift $ lift $ lines . eval r'
            let
             output = take (max_msg_length cfg) $ case l of
              [] -> ""; [x] -> x
              (x:xs) -> x ++ discarded_lines_description (length xs)
            lift $ mapState' $ Map.insert wher $ ChannelMemory { last_request = r', last_output = output }
            reply $ case mmem of
              Just mem | last_output mem == output -> case () of
                ()| "error:" `isPrefixOf` output -> "Same error."
                ()| "warning:" `isPrefixOf` output -> "Same warning."
                ()| length output > 20 -> "No change in output."
                () -> output
              _ -> output
    IRC.Message _ "001" {- RPL_WELCOME -} _ -> do
      maybeM (nick_pass cfg) $ \np -> send $ msg "PRIVMSG" ["NickServ", "identify " ++ np]
      when (join_trigger cfg == Nothing) join_chans
    _ -> return ()
  where
    send = msapp . (:[])
    join_chans = msapp $ msg "JOIN" . (:[]) . chans cfg

connect :: Net.HostName -> Net.PortNumber -> IO Handle
  -- Mostly copied from Network.connectTo. We can't use that one because we want to set SO_KEEPALIVE (and related) options on the socket, which can't be done on a Handle.
connect host portn = do
  proto <- Network.BSD.getProtocolNumber "tcp"
  let hints = Net.defaultHints { Net.addrSocketType = Net.Stream, Net.addrProtocol = proto }
  target <- head . Net.getAddrInfo (Just hints) (Just host) (Just $ show portn)
  bracketOnError (Net.socket (Net.addrFamily target) Net.Stream proto) Net.sClose $ \sock -> do
  Sys.setKeepAlive sock 30 10 5
  Net.connect sock (Net.addrAddress target)
  Net.socketToHandle sock ReadWriteMode
