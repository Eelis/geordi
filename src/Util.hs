module Util where

import qualified System.Posix.IO
import qualified GHC.Read
import qualified Data.Monoid

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Monoid (Monoid(..))
import Data.List (sortBy)
import Data.Char (isSpace)
import Control.Exception (catch, bracket, evaluate)
import Control.Monad (liftM2)
import Control.Monad.State (MonadState, modify)
import Control.Monad.Instances ()
import Control.Parallel.Strategies (NFData, rnf)
import System.Posix.Types (Fd(..))
import System.IO (Handle, hClose)

import Prelude hiding (catch, (.))

full_evaluate :: NFData a => a -> IO a
full_evaluate x = do () <- evaluate (rnf x); return x
  -- evaluate only evaluates up to WHNF.

readFileNow :: FilePath -> IO String
readFileNow f = readFile f >>= full_evaluate
  -- Useful when, for example, one needs to read from a file before performing a chroot after which the file is no longer accessible.

(.) :: Functor f => (a -> b) -> f a -> f b
(.) = fmap

forever :: Monad m => m a -> m b
forever x = x >> forever x
  -- ExtDep: GHC 6.8.2 has this in Control.Monad.

noThrow :: IO () -> IO ()
noThrow x = x `catch` const (return ())

class IOResource a where dealloc :: a -> IO ()

withResource :: IOResource a => IO a -> (a -> IO b) -> IO b
withResource x = bracket x $ noThrow . dealloc

instance IOResource Handle where dealloc = hClose
instance IOResource Fd where dealloc = System.Posix.IO.closeFd

instance (IOResource x, IOResource y) => IOResource (x, y) where
  dealloc (x, y) = noThrow (dealloc x) >> dealloc y

kibi, mebi :: Integral a => a
kibi = 1024
mebi = kibi * kibi

readTypedFile :: Read a => FilePath -> IO a
readTypedFile f = either (const $ fail $ "parsing \"" ++ f ++ "\"") return =<< GHC.Read.readEither . readFile f

stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix [] ys = Just ys
stripPrefix (x:xs) (y:ys) | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing
  -- ExtDep: GHC 6.8.2 has this in Data.List.

stripSuffix :: String -> String -> Maybe String
stripSuffix x y = reverse . stripPrefix (reverse x) (reverse y)

takeBack :: Int -> [a] -> [a]
takeBack n = reverse . take n . reverse

wordsWithWhite :: String -> [String]
wordsWithWhite "" = []
wordsWithWhite s = (a ++ w) : wordsWithWhite s''
  where (a,s') = break isSpace s; (w,s'') = span isSpace s'

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f l = listToMaybe $ mapMaybe f l

maybeM :: Monad m => Maybe a -> (a -> m ()) -> m ()
maybeM m a = maybe (return ()) a m

msapp :: (Data.Monoid.Monoid a, MonadState a m) => a -> m ()
msapp = modify . flip Data.Monoid.mappend

(.||.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f .||. g = \x -> f x || g x

sortByProperty :: Ord b => (a -> b) -> [a] -> [a]
sortByProperty f = sortBy $ \x y -> compare (f x) (f y)

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

orElse :: Maybe a -> a -> a
orElse (Just x) _ = x
orElse Nothing x = x

putNewLn :: IO ()
putNewLn = putStrLn ""

(<<) :: Monad m => m a -> m b -> m a
x << y = x >>= \z -> y >> return z

(>+>) :: (Monad m, Monoid n) => m n -> m n -> m n
(>+>) = liftM2 mappend