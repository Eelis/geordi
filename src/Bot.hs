
import Network (PortID(..), connectTo)
import System.IO (hSetBuffering, BufferMode(..), hGetLine)
import System.Environment (getArgs)
import System.Directory (setCurrentDirectory)
import System.Posix.Env (setEnv)
import System.Posix.User
import Control.Monad.Reader
import Control.Monad
import Text.ParserCombinators.Parsec
import Prelude hiding (catch, (.), readFile, putStrLn, print)
import Data.Char
import Data.Maybe
import EvalCpp (evalCpp)
import Util
import System.IO.UTF8 hiding (hGetLine, getLine)

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

cmd_parser :: String -> CharParser st (Bool, String)
cmd_parser (botnick_h:botnick_t) = do
  spaces
  oneOf [toLower botnick_h, toUpper botnick_h]
    -- There is currently no proper case-insensitive char/string comparison function in Data.Char (see GHC ticket #1506).
  string botnick_t
  spaces
  also_run <- (string "-c" >> spaces >> return False) <|> return True
  delimited <- (oneOf ":," >> spaces >> return True) <|> return False
  let
    wrapProgram c = "#include \"prelude.h\"\n" ++ c ++ "\n"
    wrapPrePost t c = wrapProgram $ "GEORDI_" ++ t ++ "_PRE " ++ c ++ "\nGEORDI_" ++ t ++ "_POST"
  code <- foldr1 (<|>) $
    [ wrapPrePost "PRINT" . (string "<<" >> getInput),
      wrapPrePost "STATEMENTS" . ('{':) . (char '{' >> getInput) ] ++
    if delimited then [wrapProgram . getInput] else []
  return (also_run, code)

main :: IO ()
main = do
  cfg <- readTypedFile "config"
  eval <- evalCpp
  args <- getArgs
  let
    localEval l =
      case parse (cmd_parser $ nick cfg) "" l of
        Left e -> putStrLn $ "parse error: " ++ show e
        Right (also_run, code) -> putStrLn =<< filter isPrint . eval code also_run
  case args of
    ["-i"] -> do
      jail cfg
      forever $ localEval =<< getLine
    [] -> bot cfg eval
    [c] -> jail cfg >> localEval c

bot :: BotConfig -> (String -> Bool -> IO String) -> IO ()
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
      when (elem c (chans cfg) && not (elem fromnick $ blacklist cfg)) $ do
        case parse (cmd_parser $ nick cfg) "" txt of
          Left _ -> return () -- Parse error
          Right (also_run, code) -> do
            o <- take (max_msg_length cfg) . filter isPrint . takeWhile (/= '\n') . eval code also_run
            cmd "PRIVMSG" [c, if null o then no_output_msg cfg else o]
    IRCmsg _ "376" {- End of motd. -} _ -> do
      forM_ (chans cfg) $ cmd "JOIN" . (:[])
      maybeM (nick_pass cfg) $ \np -> cmd "PRIVMSG" ["NickServ",  "identify " ++ np]
    _ -> return ()

-- (filter isPrint) works properly because (1) evalCpp returns a proper Unicode String, not a load of bytes; and (2) we use System.IO.UTF8's hPutStrLn which properly UTF-8 encodes the filtered String.
-- Possible problem: terminals which have not been (properly) UTF-8 configured might interpret bytes that are part of UTF-8 encoded characters as control characters.
