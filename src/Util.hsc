module Util where

import qualified System.Posix.IO
import qualified GHC.Read
import qualified Data.Monoid

import Data.List (sortBy, isPrefixOf)
import Data.Char (toUpper, isSpace)
import Control.Exception (catch, bracket, evaluate)
import Control.Monad.State (MonadState, ap, modify)
import Control.Applicative (Applicative(..))
import Control.Monad.Instances ()
import System.Posix.Types (Fd(..))
import System.Posix.Resource (ResourceLimits(..), ResourceLimit(..))
import System.IO (Handle, hClose)
import Control.Parallel.Strategies (NFData, rnf)
import Network.Socket (Socket(..), setSocketOption, SocketOption(..))
import Foreign (Ptr, with, sizeOf)

import Foreign.C
import Prelude hiding (catch, (.))

#include <netinet/in.h>
#include <netinet/tcp.h>

full_evaluate :: NFData a => a -> IO a
full_evaluate x = do () <- evaluate (rnf x); return x
  -- evaluate only evaluates up to WHNF.

(.) :: Functor f => (a -> b) -> f a -> f b
(.) = fmap

instance (Monad m, Functor m) => Applicative m where
  pure = return
  (<*>) = ap

forever :: Monad m => m a -> m b
forever x = x >> forever x

noThrow :: IO () -> IO ()
noThrow x = x `catch` const (return ())

class IOResource a where dealloc :: a -> IO ()

withResource :: IOResource a => IO a -> (a -> IO b) -> IO b
withResource x = bracket x $ noThrow . dealloc

instance IOResource Handle where dealloc = hClose
instance IOResource Fd where dealloc = System.Posix.IO.closeFd

instance (IOResource x, IOResource y) => IOResource (x, y) where
  dealloc (x, y) = noThrow (dealloc x) >> dealloc y

splitOnce :: String -> String -> (String, String)
splitOnce "" _ = ("", "")
splitOnce s@(sh:st) d =
  if d `isPrefixOf` s then ("", drop (length d) s) else let (a, b) = splitOnce st d in (sh : a, b)

kibi, mebi :: Integral a => a
kibi = 1024
mebi = kibi * kibi

foreign import ccall "unistd.h chroot" c_chroot :: CString -> IO CInt

chroot :: FilePath -> IO ()
chroot s = throwErrnoIfMinus1_ "chroot" (withCString s c_chroot)

readTypedFile :: Read a => FilePath -> IO a
readTypedFile f = either (const $ fail $ "parsing \"" ++ f ++ "\"") return =<< GHC.Read.readEither . readFile f

simpleResourceLimits :: Integer -> ResourceLimits
simpleResourceLimits l = ResourceLimits (ResourceLimit l) (ResourceLimit l)

stripPrefix :: String -> String -> Maybe String
stripPrefix [] ys = Just ys
stripPrefix (x:xs) (y:ys) | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing

stripSuffix :: String -> String -> Maybe String
stripSuffix x y = reverse . stripPrefix (reverse x) (reverse y)

capitalize :: String -> String
capitalize "" = ""
capitalize (x:xs) = toUpper x : xs

takeBack :: Int -> [a] -> [a]
takeBack n = reverse . take n . reverse

wordsWithWhite :: String -> [String]
wordsWithWhite "" = []
wordsWithWhite s = (a ++ w) : wordsWithWhite s''
  where (a,s') = break isSpace s; (w,s'') = span isSpace s'

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe _ [] = Nothing
findMaybe f (h:_) | Just x <- f h = Just x
findMaybe f (_:t) = findMaybe f t

fdOfFd :: Fd -> CInt
fdOfFd (Fd fd) = fd

dropTailWhile :: (a -> Bool) -> [a] -> [a]
dropTailWhile p = reverse . dropWhile p . reverse

maybeM :: Monad m => Maybe a -> (a -> m ()) -> m ()
maybeM m a = maybe (return ()) a m

msapp :: (Data.Monoid.Monoid a, MonadState a m) => a -> m ()
msapp = modify . flip Data.Monoid.mappend

maybe_if :: Maybe a -> (a -> b) -> b -> b
maybe_if x f y = maybe y f x
  -- Like ordinary if, but with a Maybe condition instead of a Bool condition.

(.||.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f .||. g = \x -> f x || g x

foreign import ccall unsafe "setsockopt"
  c_setsockopt :: CInt -> CInt -> CInt -> Ptr CInt -> CInt -> IO CInt

bareSetSocketOption :: Socket -> CInt -> CInt -> CInt -> IO ()
  -- Needed because Network.Socket.SocketOption lacks several options (such as TCP_KEEPIDLE, TCP_KEEPINTVL, and KEEPCNT).
bareSetSocketOption socket level option value = do
   with value $ \p -> do
   throwErrnoIfMinus1_ "bareSetSocketOption" $
    c_setsockopt (socketFd socket) level option p (fromIntegral $ sizeOf value)
   return ()

socketFd :: Socket -> CInt
socketFd (MkSocket fd _ _ _ _) = fd

setKeepAlive :: Socket -> CInt -> CInt -> CInt -> IO ()
setKeepAlive sock keepidle keepintvl keepcnt = do
  setSocketOption sock KeepAlive 1
  let sso = bareSetSocketOption sock (#const IPPROTO_TCP)
  sso (#const TCP_KEEPIDLE) keepidle
  sso (#const TCP_KEEPINTVL) keepintvl
  sso (#const TCP_KEEPCNT) keepcnt

sortByProperty :: Ord b => (a -> b) -> [a] -> [a]
sortByProperty f = sortBy $ \x y -> compare (f x) (f y)

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse
