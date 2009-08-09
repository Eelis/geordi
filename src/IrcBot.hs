{-# LANGUAGE PatternGuards #-}
import qualified Network.Socket as Net
import qualified Network.IRC as IRC
import qualified System.Environment
import qualified Request
import qualified RequestEval
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Sys
import qualified Data.Map as Map
import qualified Network.BSD
import qualified Data.List as List
import qualified Cxx.Show

import Control.Exception (bracketOnError)
import System.IO (hGetLine, hPutStrLn, hFlush, Handle, IOMode(..))
import Control.Monad (forever, when)
import Control.Monad.Error ()
import Control.Monad.State (execStateT, lift, StateT)
import System.IO.UTF8 (putStr, putStrLn, print)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import Text.Regex (Regex, subRegex, mkRegex) -- Todo: Text.Regex truncates Char's >256. Get rid of it.
import Data.Char (toUpper, toLower, isSpace, isPrint, isDigit)
import Data.Map (Map)
import Util ((.), elemBy, caselessStringEq, maybeLast, readState, msapp, maybeM, describe_new_output, orElse, findMaybe, readTypedFile, full_evaluate, withResource, mapState')
import Sys (rate_limiter)

import Prelude hiding (catch, (.), readFile, putStrLn, putStr, print)

data IrcBotConfig = IrcBotConfig
  { server :: Net.HostName, port :: Net.PortNumber, max_response_length :: Int
  , chans :: [String], key_chans :: [(String, String)]
  , nick :: String, nick_pass :: Maybe String, alternate_nick :: String
  , also_respond_to :: [String]
  , allow_short_request_syntax_in :: [String]
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
help = usageInfo "Usage: sudo geordi-irc [option]...\nOptions:" optsDesc ++ "\nSee README.xhtml for more information."

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

send_irc_msg :: Handle -> IRC.Message -> IO ()
send_irc_msg h m = hPutStrLn h (take 510 $ enc 450 $ IRC.render m) >> hFlush h
  where
    enc :: Int -> String -> String
    enc n (c:s) | c' <- UTF8.encodeString [c], n' <- n - length c', n' >= 0 || length c' == 1 = c' ++ enc n' s
    enc _ _ = ""
  -- If we simply used (System.IO.hPutStrLn $ take 510 $ UTF8.encodeString $ IRC.render m), the last UTF-8 encoded character might get cut in half if its encoding consists of multiple bytes. We want to avoid this because it causes some IRC clients (like irssi) to conclude that the encoding must be something other than UTF-8. As a further complication, while we can be sure that the server will receive messages up to 512 bytes long, it may drop anything after 450 bytes or so as it prepends our prefix when relaying messages to other clients. Hence, we can only reliably encode UTF-8 characters until a message is 450 bytes long. We don't need to immediately truncate after 450 bytes, though; after that limit, we don't truncate until an actual multi-byte character is encountered.

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
  evalRequest <- RequestEval.evaluator Cxx.Show.noHighlighting
  limit_rate <- rate_limiter (rate_limit_messages cfg) (rate_limit_window cfg)
  let send m = limit_rate >> send_irc_msg h m
  send $ msg "NICK" [nick cfg]
  send $ msg "USER" [nick cfg, "0", "*", nick cfg]
  flip execStateT Map.empty $ forever $ do
    raw <- lift $ hGetLine h
    let l = UTF8.decodeString raw
    case IRC.parseMessage (l ++ "\n") of
      Nothing -> lift $ putStr "Malformed IRC message: " >> putStrLn l
      Just m -> do
        lift $ print m
        r <- on_msg evalRequest cfg (length raw == 511) m
        lift $ mapM_ print r >> mapM_ send r
  return ()

discarded_lines_description :: Int -> String
discarded_lines_description s =
  " [+ " ++ show s ++ " discarded line" ++ (if s == 1 then "" else "s") ++ "]"

describe_lines :: [String] -> String
describe_lines [] = ""
describe_lines [x] = x
describe_lines (x:xs) = x ++ discarded_lines_description (length xs)

data ChannelMemory = ChannelMemory { context :: Request.Context, last_output :: String }
type ChannelMemoryMap = Map String ChannelMemory

is_request :: IrcBotConfig -> Where -> String -> Maybe String
is_request cfg _ s | Just (n, r) <- Request.is_addressed_request s, any (\(h:t) -> n == toLower h : t || n == toUpper h : t) (nick cfg : alternate_nick cfg : also_respond_to cfg) = Just r
is_request cfg (InChannel c) s | elemBy caselessStringEq c (allow_short_request_syntax_in cfg), Just r <- Request.is_short_request s = Just r
is_request _ Private s = Just s
is_request _ _ _ = Nothing

type Reason = String
data Permission = Allow | Deny (Maybe Reason)
data Where = Private | InChannel String

request_allowed :: IrcBotConfig -> String -> Maybe IRC.UserName -> Maybe IRC.ServerName -> Where -> Permission
request_allowed cfg _ _ _ Private | not (serve_private_requests cfg) =
  Deny $ Just "This bot does not serve private requests."
request_allowed cfg nickname _ _ _ | nickname `elem` blacklist cfg = Deny Nothing
request_allowed _ _ _ _ _ = Allow

strip_utf8_bom :: String -> String
strip_utf8_bom ('\239':'\187':'\191':s) = s
strip_utf8_bom s = s

type Eraser = String -> Maybe String

digits :: Eraser
digits (x : y : s) | isDigit x, isDigit y = Just s
digits (x : s) | isDigit x = Just s
digits _ = Nothing

color_code :: Eraser
color_code ('\x3' : ',' : s) = digits s
color_code ('\x3' : s) = case digits s of
  Just (',' : s') -> digits s'
  Just s' -> Just s'
  Nothing -> Just s
color_code _ = Nothing

apply_eraser :: Eraser -> (String -> String)
apply_eraser _ [] = []
apply_eraser p s@(h:t) = p s `orElse` (h : apply_eraser p t)

strip_color_codes :: String -> String
strip_color_codes = apply_eraser color_code
  {- Todo: The above is *much* more naturally expressed as:
        subRegex r s "" where r = mkRegex "\x3(,[[:digit:]]{1,2}|[[:digit:]]{1,2}(,[[:digit:]]{1,2})?)?"
  Unfortunately, Text.Regex is broken: it truncates Char's, resulting in spurious matches. -}

join_msg :: IrcBotConfig -> IRC.Message
join_msg cfg = msg "JOIN" $ map (concat . List.intersperse ",") $
  if null (key_chans cfg) then [chans cfg]
  else [fst . key_chans cfg ++ chans cfg, snd . key_chans cfg]

on_msg :: (Functor m, Monad m) =>
  (String -> Request.Context -> m Request.Response) -> IrcBotConfig -> Bool -> IRC.Message -> StateT ChannelMemoryMap m [IRC.Message]
on_msg eval cfg full_size m = flip execStateT [] $ do
  when (join_trigger cfg == Just m) $ send $ join_msg cfg
  case m of
    IRC.Message (Just (IRC.NickName who _ _)) "QUIT" _ | who == nick cfg ->
      send $ msg "NICK" [nick cfg]
    IRC.Message (Just (IRC.NickName from _ _)) "PRIVMSG" [_, "\1VERSION\1"] ->
      send $ msg "NOTICE" [from, "\1VERSION Geordi C++ bot - http://www.eelis.net/geordi/\1"]
    IRC.Message _ "433" {- ERR_NICKNAMEINUSE -} _ -> send $ msg "NICK" [alternate_nick cfg]
    IRC.Message _ "PING" a -> send $ msg "PONG" a
    IRC.Message _ "PRIVMSG" [_, '\1':_] -> return ()
    IRC.Message (Just (IRC.NickName who muser mserver)) "PRIVMSG" [to, txt'] -> do
      let txt = filter isPrint $ strip_color_codes $ strip_utf8_bom txt'
      let private = elemBy caselessStringEq to [nick cfg, alternate_nick cfg]
      let w = if private then Private else InChannel to
      maybeM (dropWhile isSpace . is_request cfg w txt) $ \r -> do
      let wher = if private then who else to
      let reply s = send $ msg "PRIVMSG" [wher, take (max_response_length cfg) $ if null s then no_output_msg cfg else do_censor cfg s]
      case request_allowed cfg who muser mserver w of
        Deny reason -> maybeM reason reply
        Allow -> do
          if full_size && maybe True (not . (`elem` "};")) (maybeLast r) then reply $ "Request likely truncated after `" ++ reverse (take 15 $ reverse r) ++ "`." else do
            -- The `elem` "};" condition gains a reduction in false positives at the cost of an increase in false negatives.
          mmem <- Map.lookup wher . lift readState
          let con = context . mmem `orElse` Request.Context []
          Request.Response history_modification output <- lift $ lift $ eval r con
          let output' = describe_lines $ lines output
          lift $ mapState' $ Map.insert wher $ ChannelMemory
            { context = maybe id Request.modify_history history_modification con
            , last_output = output' }
          reply $ describe_new_output (last_output . mmem) output'
    IRC.Message _ "001" {- RPL_WELCOME -} _ -> do
      maybeM (nick_pass cfg) $ \np -> send $ msg "PRIVMSG" ["NickServ", "identify " ++ np]
      when (join_trigger cfg == Nothing) $ send $ join_msg cfg
    _ -> return ()
  where
    send = msapp . (:[])

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
