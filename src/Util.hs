
module Util where

import Foreign.C
import Prelude hiding (catch, (.))
import Data.List
import Data.Char
import Control.Exception
import Control.Monad
import Control.Applicative
import Control.Monad.Instances
import System.Posix.Types
import System.Posix.Resource
import System.Posix.IO
import System.IO
import GHC.Read

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
instance IOResource Fd where dealloc = closeFd

instance (IOResource x, IOResource y) => IOResource (x, y) where
  dealloc (x, y) = noThrow (dealloc x) >> dealloc y

splitOnce :: String -> String -> (String, String)
splitOnce "" _ = ("", "")
splitOnce s@(sh:st) d =
  if d `isPrefixOf` s then ("", drop (length d) s) else let (a, b) = splitOnce st d in (sh : a, b)

maybeM :: Monad m => Maybe a -> (a -> m ()) -> m ()
maybeM m a = maybe (return ()) a m

kibi, mebi :: Integral a => a
kibi = 1024
mebi = kibi * kibi

foreign import ccall "unistd.h chroot" c_chroot :: CString -> IO CInt

chroot :: FilePath -> IO ()
chroot s = throwErrnoIfMinus1_ "chroot" (withCString s c_chroot)

readTypedFile :: Read a => FilePath -> IO a
readTypedFile f = either (const $ fail $ "parsing \"" ++ f ++ "\"") return =<< readEither . readFile f

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
