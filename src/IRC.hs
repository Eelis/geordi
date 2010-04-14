{-# LANGUAGE PatternGuards #-}
module IRC (Message(..), Command(..), encode, decode, UserName, ServerName, Prefix, send) where

import qualified Network.IRC as Base
import qualified Data.ByteString as ByteString
import qualified Codec.Binary.UTF8.String as UTF8

import System.IO (hFlush, Handle)
import Data.Maybe (listToMaybe)
import Data.List (intersperse)
import Prelude hiding ((.))
import Prelude.Unicode
import Util ((.))

data Message = Message { msg_prefix :: Maybe Prefix, msg_command :: Command }
  deriving (Show, Read, Eq)

data Command
  = PrivMsg { privMsg_target, privMsg_text :: String }
  | Notice { notice_target, notice_text :: String }
  | Quit { quit_msg :: Maybe String }
  | Welcome
  | Invite { invite_nick, invite_channel :: String }
  | Nick String
  | Pass String
  | Ping { ping_servers :: [String] }
  | Pong { pong_servers :: [String] }
  | NickNameInUse
  | User { user :: String, mode :: Integer, realname :: String }
  | Join { join_chans :: [String], join_keychans :: [(String, String)] }
  | OtherCommand String [Base.Parameter]
  deriving (Show, Read, Eq)
    -- Clearly woefully incomplete, but all geordi needs.

fromBase :: Base.Message → Message
fromBase (Base.Message prefix cmd params) =
  Message prefix $ case (cmd, params) of
    ("PING", x) → Ping x
    ("PRIVMSG", [x, y]) → PrivMsg x y
    ("QUIT", x) → Quit $ listToMaybe x
    ("INVITE", [x, y]) → Invite x y
    ("001", _) → Welcome
    ("433", _) → NickNameInUse
    _ → OtherCommand cmd params

toBase :: Message → Base.Message
toBase (Message prefix command) = uncurry (Base.Message prefix) $ case command of
  Pong x → ("PONG", x)
  Pass x → ("PASS", [x])
  Nick x → ("NICK", [x])
  User u m n → ("USER", [u, show m, "*", n])
  PrivMsg x y → ("PRIVMSG", [x, y])
  Notice x y → ("NOTICE", [x, y])
  Join c kc → ("JOIN", map (concat . intersperse ",") $
    if null kc then [c] else [fst . kc ++ c, snd . kc])
  _ → error "sorry, not implemented"

encode :: Message → String
encode = Base.encode . toBase

decode :: String → Maybe Message
decode = (fromBase .) . Base.decode

type UserName = Base.UserName
type ServerName = Base.ServerName
type Prefix = Base.Prefix

send :: Handle → Message → IO ()
send h m = ByteString.hPutStrLn h (ByteString.pack $ take 510 $ enc 450 $ encode m) >> hFlush h
  where
    enc n (c:s) | c' ← UTF8.encode [c], n' ← n - length c', n' ≥ 0 ∨ length c' == 1 = c' ++ enc n' s
    enc _ _ = []
  -- If we simply used (System.IO.hPutStrLn $ take 510 $ UTF8.encodeString $ IRC.render m), the last UTF-8 encoded character might get cut in half if its encoding consists of multiple bytes. We want to avoid this because it causes some IRC clients (like irssi) to conclude that the encoding must be something other than UTF-8. As a further complication, while we can be sure that the server will receive messages up to 512 bytes long, it may drop anything after 450 bytes or so as it prepends our prefix when relaying messages to other clients. Hence, we can only reliably encode UTF-8 characters until a message is 450 bytes long. We don't need to immediately truncate after 450 bytes, though; after that limit, we don't truncate until an actual multi-byte character is encountered.
