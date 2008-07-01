module Util where

import qualified System.Posix.IO
import qualified GHC.Read
import qualified Data.Monoid
import qualified Data.Sequence as Seq
import qualified Text.ParserCombinators.Parsec as PS
import qualified Text.ParserCombinators.Parsec.Error as PSE

import Data.Maybe (listToMaybe, mapMaybe)
import Data.Monoid (Monoid(..))
import Data.List (sortBy)
import Data.Char (isSpace, isAlphaNum, toLower)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Sequence (Seq, ViewL(..), (<|))
import Control.Exception (catch, bracket, evaluate)
import Control.Monad (liftM2)
import Control.Monad.Fix (fix)
import Control.Monad.State (MonadState, modify, StateT(..))
import Control.Monad.Instances ()
import Control.Parallel.Strategies (NFData, rnf)
import Control.Arrow (first)
import System.Posix.Types (Fd(..))
import System.Posix.Time (epochTime)
import System.IO (Handle, hClose)
import Sys (sleep)

import Prelude hiding (catch, (.))

full_evaluate :: NFData a => a -> IO a
full_evaluate x = do () <- evaluate (rnf x); return x
  -- evaluate only evaluates up to WHNF.

readFileNow :: FilePath -> IO String
readFileNow f = readFile f >>= full_evaluate
  -- Useful when, for example, one needs to read from a file before performing a chroot after which the file is no longer accessible.

(.) :: Functor f => (a -> b) -> f a -> f b
(.) = fmap

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

(.||.), (.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
f .||. g = \x -> f x || g x
f .&&. g = \x -> f x && g x
  -- In applicative notation, these could be written: f .||. g = [[ f || g ]].

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

caselessStringEq :: String -> String -> Bool
caselessStringEq a b = (toLower . a) == (toLower . b)

elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy f x = or . (f x .)

class Queue q e | q -> e where
  qpush :: e -> q -> q
  qpop :: q -> Maybe (e, q)

instance Queue (Seq e) e where
  qpush = (<|)
  qpop q = case (Seq.viewl q) of
    Seq.EmptyL -> Nothing
    e :< q' -> Just (e, q')

qPopWhile :: Queue q e => (e -> Bool) -> q -> q
qPopWhile p q | Just (e, q') <- qpop q, p e = qPopWhile p q'
qPopWhile _ q = q

rate_limiter :: Int -> Int -> IO (IO ())
rate_limiter bound window = do
  r <- newIORef Seq.empty
  return $ fix $ \loop -> do
    now <- epochTime
    hist <- discard_until (now - fromIntegral window) . readIORef r
    if Seq.length hist < bound
      then writeIORef r (qpush now hist)
      else writeIORef r hist >> sleep 1 >> loop
 where discard_until t = qPopWhile (< t)
  -- Given |rl <- rate_limiter b w|, |rl| actions will sleep as required to make sure no more than b actions pass during any w second time window.

parsep :: Char
parsep = '\x2029' -- U+2029 PARAGRAPH SEPARATOR

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast [x] = Just x
maybeLast (_:t) = maybeLast t

replaceInfixM :: Eq a => [a] -> [a] -> [a] -> Maybe [a]
replaceInfixM what with l = (\(pre, post) -> pre ++ with ++ post) . stripInfix what l

replaceInfixesM :: Eq a => [a] -> [a] -> [a] -> Maybe [a]
replaceInfixesM what with l = (\(pre, post) -> pre ++ with ++ replaceInfixes what with post) . stripInfix what l

replaceInfix :: Eq a => [a] -> [a] -> [a] -> [a]
replaceInfix what with l = maybe l id (replaceInfixM what with l)

replaceInfixes :: Eq a => [a] -> [a] -> [a] -> [a]
replaceInfixes what with l = maybe l id (replaceInfixesM what with l)

stripInfix :: Eq a => [a] -> [a] -> Maybe ([a], [a])
stripInfix p s | Just r <- stripPrefix p s = Just ([], r)
stripInfix p (h:t) = first (h:) . stripInfix p t
stripInfix _ _  = Nothing

readState :: Monad y => StateT x y x
readState = StateT $ \x -> return (x, x)

mapState' :: Monad y => (x -> x) -> StateT x y ()
mapState' f = StateT $ \s -> return ((), f s)

parseOrFail :: Monad m => PS.GenParser tok () a -> [tok] -> m a
parseOrFail p t = either (fail . showParseError) return $ PS.parse p "" t
 where
  showParseError e =
    "column " ++ show (PS.sourceColumn $ PS.errorPos e) ++ ": " ++
    concatMap (++ ". ") (tail $ lines $ PSE.showErrorMessages "or" undefined undefined "unexpected" "end of request" $ filter isUnexpMsg $ PSE.errorMessages e)
  isUnexpMsg (PSE.SysUnExpect _) = True
  isUnexpMsg (PSE.UnExpect _) = True
  isUnexpMsg _ = False
