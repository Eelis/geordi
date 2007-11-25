import qualified Network.Socket as Net
import qualified System.Directory
import qualified EvalCxx
import qualified Network.IRC as IRC
import qualified System.Environment
import qualified System.Posix.Terminal
import qualified System.Posix.Env

import Network.BSD (getProtocolNumber, hostAddress, getHostByName)
import Control.Exception (bracketOnError)
import System.IO (hGetLine, hFlush, stdout, Handle, IOMode(..))
import System.Posix.User
  (getGroupEntryForName, getUserEntryForName, setGroupID, setUserID, groupID, userID)
import System.Posix.IO (stdInput)
import Control.Monad (forM_, when)
import Data.Char (isLetter, isPrint)
import Control.Monad.Error ()
import Control.Monad.State (execStateT, lift)
import Text.ParserCombinators.Parsec (parse, getInput, spaces, satisfy, (<|>), oneOf, try, string)
import System.IO.UTF8 (putStr, readFile, putStrLn, hPutStrLn)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt, usageInfo)
import Control.Applicative ((<*>))

import Prelude hiding (catch, (.), readFile, putStrLn, putStr, print)
import Util

data BotConfig = BotConfig
  { server :: String, port :: Integer, max_msg_length :: Int
  , chans :: [String], nick :: String, nick_pass :: Maybe String, alternate_nick :: String
  , blacklist :: [String]
  , user, group :: String
  , no_output_msg :: String
  , join_trigger :: Maybe IRC.Message
      -- Defaults to ENDOFMOTD. Can be set to NickServ/cloak confirmations and such.
  } deriving Read

jail :: BotConfig -> IO ()
jail cfg = do
  gid <- groupID . getGroupEntryForName (group cfg)
  uid <- userID . getUserEntryForName (user cfg)
  chroot "rt"
  System.Directory.setCurrentDirectory "/"
  setGroupID gid
  setUserID uid

data RequestOpt = RO_compileOnly | RO_terse | RO_help | RO_version deriving Eq

requestOptsDesc :: [OptDescr RequestOpt]
requestOptsDesc =
  [ Option "c" ["compile-only"] (NoArg RO_compileOnly) undefined
  , Option "t" ["terse"] (NoArg RO_terse) undefined
  , Option "h" ["help"] (NoArg RO_help) undefined
  , Option "v" ["version"] (NoArg RO_version) undefined
  ]

wrapPrePost :: String -> String -> String
wrapPrePost t c = "GEORDI_" ++ t ++ "_PRE " ++ c ++ "\nGEORDI_" ++ t ++ "_POST"

wrapPrint, wrapStmts :: String -> String
wrapPrint = wrapPrePost "PRINT"
wrapStmts = wrapPrePost "STATEMENTS"

is_request :: String -> String -> String -> Maybe String
is_request botnick botaltnick txt = either (const Nothing) Just (parse p "" txt)
  where
   p = do
    foldr1 (<|>) $ try . string . sortByProperty length
      [botnick, capitalize botnick, botaltnick, capitalize botaltnick]
    (oneOf ":," >> getInput) <|> ((:) . (spaces >> satisfy (not . (isLetter .||. (`elem` "(\'/*")))) <*> getInput)

parse_request :: Monad m => String -> m (String {- code -}, Bool {- also run -})
parse_request req = do
  (opts, rest) <- case getOpt RequireOrder requestOptsDesc (words req) of
    (_, _, (err:_)) -> fail err
    (opts, non_opts, []) -> return (opts, concat $ takeBack (length non_opts) $ wordsWithWhite req)
      -- We can't use non_opts' contents, because whitespace between tokens has been lost.
  let
    opt = (`elem` opts)
    code = unlines $
      ["#include \"prelude.h\""] ++
      (if opt RO_terse then ["#include \"terse.hpp\""] else []) ++
      case () of
        _ | opt RO_help -> [wrapPrint "help"]
        _ | opt RO_version -> [wrapPrint $ "\"g++ (GCC) \" << __VERSION__"]
        _ | '{':_ <- rest -> [wrapStmts rest]
        _ | '<':'<':x <- rest -> [wrapPrint x]
        _ -> [rest]
    also_run = opt RO_help || opt RO_version || not (opt RO_compileOnly)
  return (code, also_run)

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
  EvalCxx.cap_fds
  args <- System.Environment.getArgs
  case getOpt RequireOrder localOptsDesc args of
    (_, _, err:_) -> putStr err
    (opts, rest, []) -> do
      cfg <- readTypedFile $ maybe "config" id $ findMaybe (\o -> case o of LO_config cf -> Just cf; _ -> Nothing) opts
      gxx : flags <- words . (full_evaluate =<< readFile "compile-config")
        -- readFile would fail after the chroot, hence full_evaluate.
      let
        evalRequest :: String -> IO String
        evalRequest = either return ((filter (isPrint .||. (== '\n')) . show .) . uncurry (EvalCxx.evaluate gxx (["prelude.a", "-lmcheck"] ++ flags))) . parse_request
          -- filtering using isPrint works properly because (1) EvalCxx.evaluate returns a proper Unicode String, not a load of bytes; and (2) to print filtered strings we will use System.IO.UTF8's hPutStrLn which properly UTF-8 encodes the filtered String.
          -- Possible problem: terminals which have not been (properly) UTF-8 configured might interpret bytes that are part of UTF-8 encoded characters as control characters.
      case () of
        ()| LO_help `elem` opts -> putStrLn help
        ()| rest == [] && not (LO_interactive `elem` opts) -> bot cfg evalRequest
        ()| otherwise -> do
          echo <- not . System.Posix.Terminal.queryTerminal stdInput
          jail cfg
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
  jail cfg
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
        maybeM (is_request (nick cfg) (alternate_nick cfg) txt) $ \r -> do
        o <- lift $ take (max_msg_length cfg) . takeWhile (/= '\n') . eval r
        msapp [msg "PRIVMSG" [c, if null o then no_output_msg cfg else o]]
      IRC.Message _ "376" {- End of motd. -} _ -> do
        maybeM (nick_pass cfg) $ \np -> msapp [msg "PRIVMSG" ["NickServ", "identify " ++ np]]
        when (join_trigger cfg == Nothing) join_chans
      _ -> return ()
