
module Util where

import Foreign.C
import Prelude hiding (catch, (.))
import Data.List
import Control.Exception
import System.Posix.IO
import Control.Monad.Instances
import System.Posix.Types
import System.IO

(.) :: Functor f => (a -> b) -> f a -> f b
(.) = fmap

forever :: Monad m => m a -> m b
forever x = x >> forever x

noThrow :: IO () -> IO ()
noThrow x = x `catch` const (return ())

class IOResource a where dealloc :: a -> IO ()

withResource :: IOResource a => IO a -> (a -> IO b) -> IO b
withResource x = bracket x $ noThrow . dealloc

withResource' :: IOResource a => a -> IO b -> IO b
withResource' x y = bracket (return x) (noThrow . dealloc) $ const y

instance IOResource Handle where dealloc = hClose
instance IOResource Fd where dealloc = closeFd

instance (IOResource x, IOResource y) => IOResource (x, y) where
  dealloc (x, y) = dealloc x >> dealloc y

splitOnce :: String -> String -> (String, String)
splitOnce "" _ = ("", "")
splitOnce s@(sh:st) d =
  if d `isPrefixOf` s then ("", drop (length d) s) else let (a, b) = splitOnce st d in (sh : a, b)

maybeM :: Monad m => Maybe a -> (a -> m ()) -> m ()
maybeM Nothing _ = return ()
maybeM (Just x) a = a x

kibi, mebi :: Integral a => a
kibi = 1024
mebi = kibi * kibi

foreign import ccall "unistd.h chroot" c_chroot :: CString -> IO CInt

chroot :: FilePath -> IO ()
chroot s = throwErrnoIfMinus1_ "chroot" (withCString s c_chroot)

leaf :: FilePath -> String
leaf = reverse . takeWhile (/= '/') . reverse
