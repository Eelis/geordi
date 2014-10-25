{-# LANGUAGE UnicodeSyntax, PatternGuards #-}
module IRC (Message(..), Command(..), encode, decode, UserName, ServerName, Prefix(..), send) where

import qualified Network.IRC as Base
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8
import qualified Data.ByteString.UTF8
import qualified Codec.Binary.UTF8.String as UTF8

import System.IO (hFlush, Handle)
import Data.List (intersperse)
import Control.Arrow (first)
import Prelude hiding ((.))
import Prelude.Unicode
import Util ((.))

data Prefix = Server ServerName | NickName String (Maybe UserName) (Maybe ServerName)
  deriving (Show, Read, Eq)

data Message = Message { msg_prefix :: Maybe Prefix, msg_command :: Command }
 deriving (Show, Read, Eq)

data Command
  = PrivMsg { privMsg_target, privMsg_text :: String }
  | Notice { notice_target, notice_text :: String }
  | Quit
  | Welcome
  | Invite { invite_nick, invite_channel :: String }
  | Nick String
  | Pass String
  | Ping { ping_servers :: [ByteString.ByteString] }
  | Pong { pong_servers :: [ByteString.ByteString] }
  | NickNameInUse
  | User { user :: String, mode :: Integer, realname :: String }
  | Join { join_chans :: [String], join_keychans :: [(String, String)] }
  | OtherCommand
  deriving (Show, Read, Eq)
    -- Clearly woefully incomplete, but all geordi needs.

fromBase :: Base.Message → Message
fromBase (Base.Message prefix cmd params) =
  Message (fromPrefix . prefix) $ case (Data.ByteString.Char8.unpack cmd, params) of
    ("PING", x) → Ping x
    ("PRIVMSG", [x, y]) → PrivMsg (utf8dec x) (utf8dec y)
    ("QUIT", _) → Quit
    ("INVITE", [x, y]) → Invite (utf8dec x) (utf8dec y)
    ("001", _) → Welcome
    ("433", _) → NickNameInUse
    _ → OtherCommand
  where
    utf8dec = Data.ByteString.UTF8.toString
    fromPrefix :: Base.Prefix -> Prefix
    fromPrefix (Base.Server n) = Server n
    fromPrefix (Base.NickName n x y) = NickName (utf8dec n) x y

toBase :: Message → Base.Message
toBase (Message prefix command) =
  uncurry (Base.Message $ toPrefix . prefix) $ first Data.ByteString.Char8.pack $ case command of
    Pong x → ("PONG", x)
    Pass x → ("PASS", [utf8enc x])
    Nick x → ("NICK", [utf8enc x])
    User u m n → ("USER", utf8enc . [u, show m, "*", n])
    PrivMsg x y → ("PRIVMSG", [utf8enc x, utf8enc y])
    Notice x y → ("NOTICE", [utf8enc x, utf8enc y])
    Join c kc → ("JOIN", map (utf8enc . concat . intersperse ",") $
      if null kc then [c] else [fst . kc ++ c, snd . kc])
    _ → error "sorry, not implemented"
  where
    utf8enc = Data.ByteString.UTF8.fromString
    toPrefix :: Prefix -> Base.Prefix
    toPrefix (Server n) = Base.Server n
    toPrefix (NickName n x y) = Base.NickName (utf8enc n) x y

encode :: Message → String
encode = Data.ByteString.UTF8.toString . Base.encode . toBase

decode :: ByteString.ByteString → Maybe Message
decode = (fromBase .) . Base.decode

type UserName = Base.UserName
type ServerName = Base.ServerName

send :: Handle → Message → IO ()
send h m = Data.ByteString.Char8.hPutStrLn h (ByteString.pack $ take 510 $ enc 450 $ takeWhile (not . (`elem` "\r\n")) $ encode m) >> hFlush h
  where
    enc n (c:s) | c' ← UTF8.encode [c], n' ← n - length c', n' ≥ 0 ∨ length c' == 1 = c' ++ enc n' s
    enc _ _ = []
  -- Without enc, the last UTF-8 encoded character might get cut in half if its encoding consists of multiple bytes. We want to avoid this because it causes some IRC clients (like irssi) to conclude that the encoding must be something other than UTF-8. As a further complication, while we can be sure that the server will receive messages up to 512 bytes long, it may drop anything after 450 bytes or so as it prepends our prefix when relaying messages to other clients. Hence, we can only reliably encode UTF-8 characters until a message is 450 bytes long. We don't need to immediately truncate after 450 bytes, though; after that limit, we don't truncate until an actual multi-byte character is encountered.
