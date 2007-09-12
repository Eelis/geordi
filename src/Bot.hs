
import Network (PortID(..), connectTo)
import System.IO (hSetBuffering, BufferMode(..), hGetLine, hFlush, stdout)
import System.Environment (getArgs)
import System.Directory (setCurrentDirectory, getDirectoryContents)
import System.Posix.Process (getProcessID)
import System.Posix.Env (setEnv)
import System.Posix.User
import System.Posix.Resource
import System.Posix.Terminal (queryTerminal)
import System.Posix.IO (stdInput)
import Control.Monad.Error
import Control.Monad
import Text.ParserCombinators.Parsec
import Prelude hiding (catch, (.), readFile, putStrLn, putStr, print)
import Data.Char
import Data.List ((\\))
import Data.Maybe
import EvalCpp (evalCpp)
import qualified EvalCpp
import Util
import System.IO.UTF8 hiding (hGetLine, getLine)
import System.Console.GetOpt
import Control.Applicative hiding ((<|>))

data BotConfig = BotConfig
  { server :: String, port :: Integer, max_msg_length :: Int
  , chans :: [String], nick :: String, nick_pass :: Maybe String
  , blacklist :: [String]
  , user, group :: String
  , no_output_msg :: String
  } deriving Read

data IRCid = IRCid { ircid_nick, ircid_user, ircid_host :: String }
data IRCmsg = IRCmsg { ircmsg_id :: Maybe IRCid, ircmsg_cmd :: String, ircmsg_args :: [String] }

parse_irc_msg :: String -> IRCmsg
parse_irc_msg s = IRCmsg idd c args
  where
    (idd, rest) =
      if head s == ':' then
        let
          (prefix, s') = splitOnce (tail s) " "
          (x, host) = splitOnce prefix "@"
          (nik, usr) = splitOnce x "!"
        in (Just $ IRCid nik usr host, s')
      else (Nothing, s)
    (np, tp) = splitOnce rest " :"
    (c : args) = words np ++ (if null tp then [] else [tp])
  -- Todo: This is nasty.

jail :: BotConfig -> IO ()
jail cfg = do
  gid <- groupID . getGroupEntryForName (group cfg)
  uid <- userID . getUserEntryForName (user cfg)
  chroot "rt"
  setCurrentDirectory "/"
  setGroupID gid
  setUserID uid

data RequestOpt = RO_compileOnly | RO_terse | RO_help deriving Eq

requestOptsDesc :: [OptDescr RequestOpt]
requestOptsDesc =
  [ Option "c" ["compile-only"] (NoArg RO_compileOnly) undefined
  , Option "t" ["terse"] (NoArg RO_terse) undefined
  , Option "h" ["help"] (NoArg RO_help) undefined
  ]

wrapPrePost :: String -> String -> String
wrapPrePost t c = "GEORDI_" ++ t ++ "_PRE " ++ c ++ "\nGEORDI_" ++ t ++ "_POST"

wrapPrint, wrapStmts :: String -> String
wrapPrint = wrapPrePost "PRINT"
wrapStmts = wrapPrePost "STATEMENTS"

is_request :: String -> String -> Maybe String
is_request botnick txt = either (const Nothing) Just (parse p "" txt)
  where
   p = do
    string botnick <|> string (capitalize botnick)
    notFollowedBy $ char '\''
    (oneOf ":," >> getInput) <|> ((:) . (spaces >> satisfy (not . isLetter)) <*> getInput)

parse_request :: Monad m => String -> m (Bool {- also run -}, String {- code -})
parse_request s = do
  (opts, nonopcount) <- case getOpt RequireOrder requestOptsDesc (words s) of
    (_, _, (e:_)) -> fail e
    (f, o, []) -> return (f, length o)
  code <- if RO_help `elem` opts then return $ wrapPrint "help" else do
    let u = concat $ takeBack nonopcount $ wordsWithWhite s
    case stripPrefix "<<" u of
      Just r -> return $ wrapPrint r
      Nothing -> maybe (return u) (return . wrapStmts . ('{':)) (stripPrefix "{" u)
  return (RO_help `elem` opts || not (RO_compileOnly `elem` opts),
    unlines $ ["#include \"prelude.h\""] ++ (if RO_terse `elem` opts then ["#include \"terse.hpp\""] else []) ++ [code])

prompt :: String
prompt = "\n> "

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

  do -- See section "Inherited file descriptors." in EvalCpp.hsc.
    setResourceLimit ResourceOpenFiles $ simpleResourceLimits $ fromIntegral EvalCpp.close_range_end
    pid <- getProcessID
    open_fds <- (read .) . (\\ [".", ".."]) . getDirectoryContents ("/proc/" ++ show pid ++ "/fd")
    when (maximum open_fds >= EvalCpp.close_range_end) $ do
      fail $ "fd(s) open >= " ++ show EvalCpp.close_range_end ++ ": " ++ show (filter (>= EvalCpp.close_range_end) open_fds)

  eval <- evalCpp
  let
    evalRequest :: String -> IO String
    evalRequest = either return (\(also_run, code) -> filter (\c -> isPrint c || c == '\n') . eval code also_run) . parse_request

  args <- getArgs
  case getOpt RequireOrder localOptsDesc args of
    (opts, rest, []) ->
      if LO_help `elem` opts then help else do
      let cfgFile = maybe "config" id $ findMaybe (\o -> case o of LO_config cf -> Just cf; _ -> Nothing) opts
      cfg <- readTypedFile cfgFile
      let interactive = LO_interactive `elem` opts
      if rest == [] && not interactive then bot cfg evalRequest else do
      echo <- not . queryTerminal stdInput
      jail cfg
      forM_ rest $ \c -> evalRequest c >>= putStrLn
      when interactive $ forever $ do
      putStr prompt
      hFlush stdout
      l <- getLine
      when echo $ putStrLn l
      evalRequest l >>= putStrLn
    (_, _, e) -> putStr $ concat e

bot :: BotConfig -> (String -> IO String) -> IO ()
bot cfg eval = withResource (connectTo (server cfg) (PortNumber (fromIntegral $ port cfg))) $ \h -> do
  setEnv "LC_ALL" "C" True -- Otherwise compiler warnings may use non-ASCII characters (e.g. for quotes).
  jail cfg
  let cmd c a = hPutStrLn h $ c ++ concatMap (' ':) (init a) ++ " :" ++ last a
  hSetBuffering h NoBuffering
  cmd "NICK" [nick cfg]
  cmd "USER" [nick cfg, "0", "*", nick cfg]
  forever $ parse_irc_msg . init . liftIO (hGetLine h) >>= \m -> case m of
    IRCmsg _ "PING" a -> cmd "PONG" a
    IRCmsg (Just (IRCid fromnick _ _)) "PRIVMSG" [c, txt] ->
      when (elem c (chans cfg) && not (elem fromnick $ blacklist cfg)) $
        maybeM (is_request (nick cfg) txt) $ \r -> do
          o <- take (max_msg_length cfg) . takeWhile (/= '\n') . eval r
          cmd "PRIVMSG" [c, if null o then no_output_msg cfg else o]
    IRCmsg _ "376" {- End of motd. -} _ -> do
      forM_ (chans cfg) $ cmd "JOIN" . (:[])
      maybeM (nick_pass cfg) $ \np -> cmd "PRIVMSG" ["NickServ",  "identify " ++ np]
    _ -> return ()

-- (filter isPrint) works properly because (1) eval returns a proper Unicode String, not a load of bytes; and (2) we use System.IO.UTF8's hPutStrLn which properly UTF-8 encodes the filtered String.
-- Possible problem: terminals which have not been (properly) UTF-8 configured might interpret bytes that are part of UTF-8 encoded characters as control characters.
