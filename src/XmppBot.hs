{-# LANGUAGE PatternGuards #-}

import qualified System.Environment
import qualified XMPP
import qualified MUC
import qualified Sys
import qualified Cxx.Show
import qualified Request
import qualified RequestEval

import Data.Char (ord, toLower, toUpper)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import System.IO (putStrLn)
import Control.Monad (guard, when, forM_, forever)

import Prelude hiding (catch, (.), readFile, putStrLn, putStr, print)
import Util

type JID = String
type Nick = String

data RoomConfig = RoomConfig { room_nick :: Nick, allow_nickless_requests :: Bool } deriving Read

data BotConfig = BotConfig
  { server, user, pass :: String
  , max_msg_length :: Int
  , no_output_msg :: String
  , blacklist :: [JID] -- These may include a resource part, so that room users may be specified.
  , rate_limit_messages, rate_limit_window :: Int
  , rooms :: [(JID, RoomConfig)]
  } deriving Read

data Opt = Config String | Help deriving Eq

optsDesc :: [OptDescr Opt]
optsDesc =
  [ Option "c" ["config"] (ReqArg Config "<file>") "Load configuration from <file> instead of \"xmpp-config\"."
  , Option "h" ["help"] (NoArg Help) "Display this help and exit."
  ]

help :: String
help = usageInfo "Usage: sudo geordi-xmpp [option]...\nOptions:" optsDesc ++ "\nSee README.xhtml for more information."

getArgs :: IO [Opt]
getArgs = do
  args ← System.Environment.getArgs
  case getOpt RequireOrder optsDesc args of
    (_, _, err:_) → fail $ init err
    (_, w:_, []) → fail $ "superfluous command line argument: " ++ w
    (opts, [], []) → return opts

nicks_match :: Nick → Nick → Bool
nicks_match n (h:t) = n == toLower h : t ∨ n == toUpper h : t
nicks_match _ "" = error "empty nick"

is_request :: RoomConfig → String → Maybe String
is_request (RoomConfig mynick allow_nickless) s
  | Just (n, r) ← Request.is_addressed_request s, nicks_match n mynick = Just r
  | allow_nickless, Just r ← Request.is_short_request s = Just r
  | otherwise = Nothing

output_body :: BotConfig → Request.Response → String
output_body cfg r = xmlEntities $ take (max_msg_length cfg) $ takeWhile (/= '\n') $
  case Request.response_output r of "" → no_output_msg cfg; s → s

main :: IO ()
main = do
  Sys.setlocale_ALL_env
  opts ← getArgs
  if Help `elem` opts then putStrLn help else do
  cfg ← readTypedFile $ findMaybe (\o → case o of Config cf → Just cf; _ → Nothing) opts `orElse` "xmpp-config"
  conn ← XMPP.openStream $ server cfg
  XMPP.getStreamStart conn
  evalRequest ← RequestEval.evaluator Cxx.Show.noHighlighting
  limit_rate ← Sys.rate_limiter (rate_limit_messages cfg) (rate_limit_window cfg)
  XMPP.runXMPP conn $ do
  XMPP.startAuth (user cfg) (server cfg) (pass cfg)
  XMPP.sendPresence
  XMPP.handleVersion "Geordi C++ bot - http://www.eelis.net/geordi/" "-" "-"
  forM_ (rooms cfg) $ \(jid, RoomConfig nick _) → MUC.joinGroupchat nick jid
  forever $ do
  msg ← XMPP.waitForStanza (XMPP.isMessage .&&. XMPP.hasBody .&&. (not . isDelay))
  maybeM (XMPP.getAttr "from" msg) $ \from → do
  when (not $ from `elem` blacklist cfg) $ do
  maybeM (XMPP.getMessageBody msg) $ \body → do
  let
    eval r = XMPP.liftIO $ limit_rate >> output_body cfg . evalRequest r (Request.Context [])
  case XMPP.getAttr "type" msg of
    Just "groupchat"
      | (room, '/' : from_nick) ← span (/= '/') from, Just request ← do
          room_cfg ← lookup room $ rooms cfg
          guard $ room_nick room_cfg /= from_nick
          is_request room_cfg body
        → eval request >>= MUC.sendGroupchatMessage room
    Just "chat" → eval body >>= XMPP.sendMessage from
    Nothing → eval body >>= XMPP.sendMessage from
    _ → return ()

xmlEntities :: String → String
xmlEntities = concatMap (\c → "&#" ++ show (ord c) ++ ";")

isDelay :: XMPP.StanzaPredicate
isDelay = maybe False ((== Just "jabber:x:delay") . XMPP.getAttr "xmlns") . XMPP.xmlPath ["x"]
