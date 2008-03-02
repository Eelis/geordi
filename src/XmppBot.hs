import qualified System.Environment
import qualified XMPP
import qualified MUC

import Data.Char (ord)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import System.IO (putStrLn)
import Control.Monad (when, forM_)
import Prelude hiding (catch, (.), readFile, putStrLn, putStr, print)
import qualified List
import Util
import Request

data XmppBotConfig = XmppBotConfig
  { server :: String, max_msg_length :: Int
  , rooms :: [(String, String)] -- (room, nick) pairs.
  , jid :: String, pass :: String
  , no_output_msg :: String
  , rate_limit_messages, rate_limit_window :: Int
} deriving Read

data Opt = Config String | Help deriving Eq

optsDesc :: [OptDescr Opt]
optsDesc =
  [ Option "c" ["config"] (ReqArg Config "<file>") "Load configuration from <file> instead of \"xmpp-config\"."
  , Option "h" ["help"] (NoArg Help) "Display this help and exit."
  ]

help :: String
help = usageInfo "Usage: sudo ./geordi-xmpp [option]...\nOptions:" optsDesc ++ "\nSee README.xhtml for more information."

getArgs :: IO [Opt]
getArgs = do
  args <- System.Environment.getArgs
  case getOpt RequireOrder optsDesc args of
    (_, _, err:_) -> fail $ init err
    (_, w:_, []) -> fail $ "superfluous command line argument: " ++ w
    (opts, [], []) -> return opts

main :: IO ()
main = do
  opts <- getArgs
  if Help `elem` opts then putStrLn help else do
  cfg <- readTypedFile $ findMaybe (\o -> case o of Config cf -> Just cf; _ -> Nothing) opts `orElse` "xmpp-config"
  conn <- XMPP.openStream $ server cfg
  XMPP.getStreamStart conn
  evalRequest <- Request.evaluator
  limit_rate <- rate_limiter (rate_limit_messages cfg) (rate_limit_window cfg)
  XMPP.runXMPP conn $ do
    XMPP.startAuth (jid cfg) (server cfg) (pass cfg)
    XMPP.sendPresence
    XMPP.handleVersion "Geordi C++ bot - http://www.eelis.net/geordi/" "-" "-"
    forM_ (rooms cfg) $ uncurry $ flip MUC.joinGroupchat
    forever $ do
      msg <- XMPP.waitForStanza (MUC.isGroupchatMessage .&&. XMPP.hasBody .&&. (not . isDelay))
      maybeM (span (/='/') . XMPP.getAttr "from" msg) $ \(room, '/':fromnick) -> do
      maybeM (List.lookup room (rooms cfg)) $ \mynick -> do
      when (mynick /= fromnick) $ do
      maybeM (XMPP.getMessageBody msg >>= Request.is_request [mynick]) $ \r -> do
      o <- XMPP.liftIO $ xmlEntities . take (max_msg_length cfg) . takeWhile (/= '\n') . evalRequest r
      XMPP.liftIO limit_rate
      MUC.sendGroupchatMessage room o

xmlEntities :: String -> String
xmlEntities = concatMap (\c -> "&#" ++ show (ord c) ++ ";")

isDelay :: XMPP.StanzaPredicate
isDelay = maybe False ((== Just "jabber:x:delay") . XMPP.getAttr "xmlns") . XMPP.xmlPath ["x"]
