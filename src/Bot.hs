
import qualified Network.Socket as Net
import Network.BSD (getProtocolNumber, hostAddress, getHostByName)
import Control.Exception (bracketOnError)
import System.IO (hGetLine, hFlush, stdout, Handle, IOMode(..))
import System.Environment (getArgs)
import System.Directory (setCurrentDirectory, getDirectoryContents)
import System.Posix.Process (getProcessID)
import System.Posix.Env (setEnv)
import System.Posix.User
import System.Posix.Resource
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdInput)
import Control.Monad.Error ()
import Control.Monad.State
import Control.Monad
import Text.ParserCombinators.Parsec
import Prelude hiding (catch, (.), readFile, putStrLn, putStr, print)
import Data.Char
import Data.List ((\\), sortBy)
import Data.Maybe
import EvalCxx (evalCxx)
import qualified EvalCxx
import Util
import System.IO.UTF8 hiding (hGetLine, getLine)
import System.Console.GetOpt
import Control.Applicative hiding ((<|>))
import qualified Network.IRC as IRC

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
  setCurrentDirectory "/"
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
    foldr1 (<|>) $ try . string . sortBy (\x y -> compare (length y) (length x))
      [botnick, capitalize botnick, botaltnick, capitalize botaltnick]
    (oneOf ":," >> getInput) <|> ((:) . (spaces >> satisfy (not . (isLetter .||. (`elem` "\'/*")))) <*> getInput)

parse_request :: Monad m => String -> m (String {- code -}, Bool {- also run -})
parse_request s = do
  (opts, nonopcount) <- case getOpt RequireOrder requestOptsDesc (words s) of
    (_, _, (e:_)) -> fail e
    (f, o, []) -> return (f, length o)
  let
    u = concat $ takeBack nonopcount $ wordsWithWhite s
    also_run = RO_help `elem` opts || RO_version `elem` opts || not (RO_compileOnly `elem` opts)
    code
      | RO_help `elem` opts = wrapPrint "help"
      | RO_version `elem` opts = wrapPrint $ "\"g++ (GCC) \" << __VERSION__"
      | otherwise = maybe_if (stripPrefix "<<" u) wrapPrint $ maybe_if (stripPrefix "{" u) (wrapStmts . ('{':)) u
  return (unlines $ ["#include \"prelude.h\""] ++ (if RO_terse `elem` opts then ["#include \"terse.hpp\""] else []) ++ [code], also_run)

local_prompt :: String
local_prompt = "\n> "

data LocalOpt = LO_interactive | LO_config String | LO_help deriving Eq

localOptsDesc :: [OptDescr LocalOpt]
localOptsDesc =
  [ Option "c" ["config"] (ReqArg LO_config "<file>") "Load configuration from <file> instead of \"config\"."
  , Option "i" ["interactive"] (NoArg LO_interactive) "Go into Read-Eval-Print-Loop."
  , Option "h" ["help"] (NoArg LO_help) "Display this help and exit."
  ]

help :: IO ()
help = putStrLn $ usageInfo "Usage: sudo ./Bot [option]... [request]...\nOptions:" localOptsDesc ++ "\nSee INSTALL.xhtml for more information."

main :: IO ()
main = do

  do -- See section "Inherited file descriptors." in EvalCxx.hsc.
    let cre = EvalCxx.close_range_end
    setResourceLimit ResourceOpenFiles $ simpleResourceLimits $ fromIntegral cre
    high_fds <- filter (>= cre) . (read .) . (\\ [".", ".."]) . (getDirectoryContents =<< (\s -> "/proc/" ++ s ++ "/fd") . show . getProcessID)
    when (high_fds /= []) $ fail $ "fd(s) open >= " ++ show cre ++ ": " ++ show high_fds

  args <- getArgs
  case getOpt RequireOrder localOptsDesc args of
    (opts, rest, []) ->
      if LO_help `elem` opts then help else do
      cfg <- readTypedFile $ maybe "config" id $ findMaybe (\o -> case o of LO_config cf -> Just cf; _ -> Nothing) opts
      gxx : flags <- words . (full_evaluate =<< readFile "compile-config")
      let
        evalRequest :: String -> IO String
        evalRequest = either return ((filter (isPrint .||. (== '\n')) .) . uncurry (evalCxx gxx (["prelude.a"] ++ flags))) . parse_request
      let interactive = LO_interactive `elem` opts
      if rest == [] && not interactive then bot cfg evalRequest else do
      echo <- not . queryTerminal stdInput
      jail cfg
      forM_ rest $ (>>= putStrLn) . evalRequest
      when interactive $ forever $ do
      putStr local_prompt
      hFlush stdout
      l <- getLine
      when echo $ putStrLn l
      evalRequest l >>= putStrLn
    (_, _, e) -> putStr $ concat e

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
  setEnv "LC_ALL" "C" True -- Otherwise compiler warnings may use non-ASCII characters (e.g. for quotes).
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

-- (filter isPrint) works properly because (1) eval returns a proper Unicode String, not a load of bytes; and (2) we use System.IO.UTF8's hPutStrLn which properly UTF-8 encodes the filtered String.
-- Possible problem: terminals which have not been (properly) UTF-8 configured might interpret bytes that are part of UTF-8 encoded characters as control characters.
