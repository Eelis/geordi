{-# LANGUAGE PatternGuards #-}
import qualified Network.Socket as Net
import qualified System.Environment
import qualified Request
import qualified RequestEval
import qualified Codec.Binary.UTF8.String as UTF8
import qualified Sys
import qualified Data.Map as Map
import qualified Network.BSD
import qualified Cxx.Show
import qualified IRC

import IRC (Command(..))
import Network.IRC (Prefix(..))
import Control.Exception (bracketOnError)
import System.IO (hGetLine, hSetBinaryMode, Handle, IOMode(..))
import Control.Monad (forever, when)
import Control.Arrow (first)
import Control.Monad.Error ()
import Control.Monad.State (execStateT, lift, StateT)
import Control.Monad.Writer (execWriterT, tell)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import Text.Regex (Regex, subRegex, mkRegexWithOpts) -- Todo: Text.Regex truncates Char's >256. Get rid of it.
import Data.Char (toUpper, toLower, isSpace, isPrint, isDigit)
import Data.List (isSuffixOf)
import Data.Map (Map)
import Data.SetOps
import Util ((.), elemBy, caselessStringEq, readState, maybeM, describe_new_output,
  orElse, findMaybe, readTypedFile, full_evaluate, withResource, mapState',
  strip_utf8_bom, none, takeBack, replaceInfix)
import Sys (rate_limiter)

import Prelude hiding (catch, (.), readFile)
import Prelude.Unicode hiding ((∈))

data IrcBotConfig = IrcBotConfig
  { server :: Net.HostName, port :: Net.PortNumber
  , password :: Maybe String
  , max_response_length :: Int
  , chans :: [String], key_chans :: [(String, String)]
  , nick :: String, nick_pass :: Maybe String, alternate_nick :: String
  , also_respond_to :: [String]
  , allow_short_request_syntax_in :: [String]
  , blacklist :: [String]
  , no_output_msg :: String
  , channel_response_prefix :: String
      -- A first occurrence of the string "nick" is replaced with the nick of the requester.
  , join_trigger :: Maybe IRC.Message
      -- Defaults to RPL_WELCOME. Can be set to NickServ/cloak confirmations and such.
  , censor :: [Regex]
  , rate_limit_messages, rate_limit_window :: Int
  , serve_private_requests :: Bool
  } deriving Read

instance Read Regex where
  readsPrec i s = first (\r → mkRegexWithOpts r True False) . readsPrec i s

instance Read Net.PortNumber where
  readsPrec i s = (\(x, s') → (fromIntegral (x :: Int), s')) . readsPrec i s

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
  args ← System.Environment.getArgs
  case getOpt RequireOrder optsDesc args of
    (_, _, err:_) → fail $ init err
    (_, w:_, []) → fail $ "superfluous command line argument: " ++ w
    (opts, [], []) → return opts

do_censor :: IrcBotConfig → String → String
do_censor cfg s = foldr (\r t → subRegex r t "<censored>") s (censor cfg)

main :: IO ()
main = do
  Sys.setlocale_ALL_env
  opts ← getArgs
  if Help ∈ opts then putStrLn help else do
  cfg ← readTypedFile $ findMaybe (\o → case o of Config cf → Just cf; _ → Nothing) opts `orElse` "irc-config"
  full_evaluate $ do_censor cfg "abc" -- So that any mkRegex failures occur before we start connecting.
  putStrLn $ "Connecting to " ++ server cfg ++ ":" ++ show (port cfg)
  withResource (connect (server cfg) (fromIntegral $ port cfg)) $ \h → do
  putStrLn "Connected"
  evalRequest ← RequestEval.evaluator Cxx.Show.noHighlighting
  limit_rate ← rate_limiter (rate_limit_messages cfg) (rate_limit_window cfg)
  let send m = limit_rate >> IRC.send h (IRC.Message Nothing m)
  maybeM (password cfg) $ send . Pass
  send $ Nick $ nick cfg
  send $ User (nick cfg) 0 (nick cfg)
  flip execStateT (∅) $ forever $ do
    raw ← lift $ hGetLine h
    let l = UTF8.decodeString raw
    case IRC.decode (l ++ "\n") of
      Nothing → lift $ putStr "Malformed IRC message: " >> putStrLn l
      Just m → do
        lift $ print m
        r ← on_msg evalRequest cfg (length raw == 511) m
        lift $ mapM_ print r >> mapM_ send r
  return ()

discarded_lines_description :: Int → String
discarded_lines_description s =
  " [+ " ++ show s ++ " discarded line" ++ (if s == 1 then "" else "s") ++ "]"

describe_lines :: [String] → String
describe_lines [] = ""
describe_lines [x] = x
describe_lines (x:xs) = x ++ discarded_lines_description (length xs)

data ChannelMemory = ChannelMemory { context :: Request.Context, last_output :: String }
type ChannelMemoryMap = Map String ChannelMemory

is_request :: IrcBotConfig → Where → String → Maybe String
is_request cfg _ s | Just (n, r) ← Request.is_addressed_request s, any (\(h:t) → n ∈ [toLower h : t, toUpper h : t]) (nick cfg : alternate_nick cfg : also_respond_to cfg) = Just r
is_request cfg (InChannel c) s | elemBy caselessStringEq c (allow_short_request_syntax_in cfg), Just r ← Request.is_short_request s = Just r
is_request _ Private s = Just s
is_request _ _ _ = Nothing

type Reason = String
data Permission = Allow | Deny (Maybe Reason)
data Where = Private | InChannel String

request_allowed :: IrcBotConfig → String → Maybe IRC.UserName → Maybe IRC.ServerName → Where → Permission
request_allowed cfg _ _ _ Private | not (serve_private_requests cfg) =
  Deny $ Just "This bot does not serve private requests."
request_allowed cfg nickname _ _ _ | nickname ∈ blacklist cfg = Deny Nothing
request_allowed _ _ _ _ _ = Allow

type Eraser = String → Maybe String

digits :: Eraser
digits (x : y : s) | isDigit x, isDigit y = Just s
digits (x : s) | isDigit x = Just s
digits _ = Nothing

color_code :: Eraser
color_code ('\x3' : ',' : s) = digits s
color_code ('\x3' : s) = case digits s of
  Just (',' : s') → digits s'
  Just s' → Just s'
  Nothing → Just s
color_code _ = Nothing

apply_eraser :: Eraser → String → String
apply_eraser _ [] = []
apply_eraser p s@(h:t) = p s `orElse` (h : apply_eraser p t)

strip_color_codes :: String → String
strip_color_codes = apply_eraser color_code
  {- Todo: The above is *much* more naturally expressed as:
        subRegex r s "" where r = mkRegex "\x3(,[[:digit:]]{1,2}|[[:digit:]]{1,2}(,[[:digit:]]{1,2})?)?"
  Unfortunately, Text.Regex is broken: it truncates Char's, resulting in spurious matches. -}

version_response :: String
version_response = "Geordi C++ bot - http://www.eelis.net/geordi/"

on_msg :: (Functor m, Monad m) ⇒
  (String → Request.Context → m Request.Response) → IrcBotConfig → Bool → IRC.Message → StateT ChannelMemoryMap m [IRC.Command]
on_msg eval cfg full_size m@(IRC.Message prefix c) = execWriterT $ do
  when (join_trigger cfg == Just m) join
  case c of
    Quit _ | Just (NickName n _ _) ← prefix, n == nick cfg → send $ Nick $ nick cfg
    PrivMsg _ "\1VERSION\1" | Just (NickName n _ _) ← prefix →
      send $ Notice n $ "\1VERSION " ++ version_response ++ "\1"
    NickNameInUse → send $ Nick $ alternate_nick cfg
    Ping x → send $ Pong x
    PrivMsg _ ('\1':_) → return ()
    PrivMsg to txt' | Just (NickName who muser mserver) ← prefix → do
      let
        txt = filter isPrint $ strip_color_codes $ strip_utf8_bom txt'
        private = elemBy caselessStringEq to [nick cfg, alternate_nick cfg]
        w = if private then Private else InChannel to
        wher = if private then who else to
        reply s = send $ PrivMsg wher $ take (max_response_length cfg) $
            (if private then id else (replaceInfix "nick" who (channel_response_prefix cfg) ++)) $
            if null s then no_output_msg cfg else do_censor cfg s
      maybeM (dropWhile isSpace . is_request cfg w txt) $ \r → do
      case request_allowed cfg who muser mserver w of
        Deny reason → maybeM reason reply
        Allow →
          if full_size ∧ none (`isSuffixOf` r) ["}", ";"] then reply $ "Request likely truncated after `" ++ takeBack 15 r ++ "`." else do
            -- The "}"/";" test gains a reduction in false positives at the cost of an increase in false negatives.
          mmem ← Map.lookup wher . lift readState
          let con = (context . mmem) `orElse` Request.Context []
          Request.Response history_modification output ← lift $ lift $ eval r con
          let output' = describe_lines $ dropWhile null $ lines output
          lift $ mapState' $ insert (wher, ChannelMemory
            { context = maybe id Request.modify_history history_modification con
            , last_output = output' })
          reply $ describe_new_output (last_output . mmem) output'
    Welcome → do
      maybeM (nick_pass cfg) $ send . PrivMsg "NickServ" . ("identify " ++)
      when (join_trigger cfg == Nothing) join
    Invite _ _ → join
    _ → return ()
  where
    send = tell . (:[])
    join = send $ Join (chans cfg) (key_chans cfg)

connect :: Net.HostName → Net.PortNumber → IO Handle
  -- Mostly copied from Network.connectTo. We can't use that one because we want to set SO_KEEPALIVE (and related) options on the socket, which can't be done on a Handle.
connect host portn = do
  proto ← Network.BSD.getProtocolNumber "tcp"
  let hints = Net.defaultHints { Net.addrSocketType = Net.Stream, Net.addrProtocol = proto }
  target ← head . Net.getAddrInfo (Just hints) (Just host) (Just $ show portn)
  bracketOnError (Net.socket (Net.addrFamily target) Net.Stream proto) Net.sClose $ \sock → do
  Sys.setKeepAlive sock 30 10 5
  Net.connect sock (Net.addrAddress target)
  h ← Net.socketToHandle sock ReadWriteMode
  hSetBinaryMode h True
  return h
