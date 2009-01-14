{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, PatternGuards, DeriveDataTypeable #-}

module Util where

import qualified System.Posix.IO
import qualified Data.Monoid
import qualified Prelude
import qualified Data.List as List
import Data.Maybe (listToMaybe, mapMaybe, fromMaybe)
import Data.Monoid (Monoid(..))
import Data.List (sortBy, minimumBy, isPrefixOf, tails, all, stripPrefix)
import Data.Char (isSpace, isAlphaNum, toLower, toUpper)
import Data.Function (on)
import Data.Generics (Typeable)
import Control.Exception (bracket, evaluate)
import Control.Arrow (Arrow, (>>>), arr, first, second, (&&&))
import Control.Monad (liftM2, when, MonadPlus(..))
import Control.Monad.Error ()
import Control.Monad.State (MonadState, modify, StateT(..))
import Control.Monad.Instances ()
import Control.Parallel.Strategies (NFData, rnf)
import System.Posix.Types (Fd(..))
import System.IO (Handle, hClose)
import Control.Applicative (Applicative(..))
import Prelude hiding ((.), (!!))

-- IO resources

class IOResource a where dealloc :: a -> IO ()

withResource :: IOResource a => IO a -> (a -> IO b) -> IO b
withResource x = bracket x $ noThrow . dealloc

instance IOResource Handle where dealloc = hClose
instance IOResource Fd where dealloc = System.Posix.IO.closeFd

instance (IOResource x, IOResource y) => IOResource (x, y) where
  dealloc (x, y) = noThrow (dealloc x) >> dealloc y

-- Nonempty lists

data NElist a = NElist a [a] deriving Eq

instance Functor NElist where fmap f (NElist x l) = NElist (f x) (f . l)
instance Convert (NElist a) [a] where convert (NElist x l) = x : l

maybe_ne :: [a] -> Maybe (NElist a)
maybe_ne [] = Nothing
maybe_ne (h:t) = Just $ NElist h t

nth_ne :: Ordinal -> NElist a -> Maybe a
nth_ne (Ordinal n) l | (- length (unne l)) <= n, n < length (unne l) = return $ unne l !! n
nth_ne _ _ = Nothing

reverse_ne :: NElist a -> NElist a
reverse_ne = work []
  where
    work l (NElist a []) = NElist a l
    work l (NElist a (h:t)) = work (a : l) (NElist h t)

unne :: NElist a -> [a]
unne (NElist x l) = x : l

nonne_ne_app :: [a] -> NElist a -> NElist a
nonne_ne_app [] l = l
nonne_ne_app (h:t) (NElist h' t') = NElist h (t ++ h' : t')

lastAndRest :: NElist a -> ([a], a)
lastAndRest (NElist x []) = ([], x)
lastAndRest (NElist x (h:t)) = first (x:) $ lastAndRest (NElist h t)

filter_ne :: (a -> Bool) -> NElist a -> NElist a
  -- Keeps an element if required to remain nonempty.
filter_ne p (NElist x y) = maybe_ne (filter p (x : y)) `orElse` NElist x []

-- Conversions

class Convert a b where convert :: a -> b

instance Convert a a where convert = id

instance (Functor f, Convert a b) => Convert (f a) (f b) where convert = fmap convert

instance (Convert x x', Convert y y') => Convert (Either x y) (Either x' y') where
  convert = either (Left . convert) (Right . convert)

-- List utilities

total_tail :: [a] -> [a]
total_tail [] = []
total_tail (_ : t) = t

stripSuffix :: String -> String -> Maybe String
stripSuffix x y = reverse . stripPrefix (reverse x) (reverse y)

takeBack :: Int -> [a] -> [a]
takeBack n = reverse . take n . reverse

sortByProperty :: Ord b => (a -> b) -> [a] -> [a]
sortByProperty f = sortBy $ \x y -> compare (f x) (f y)

findMaybe :: (a -> Maybe b) -> [a] -> Maybe b
findMaybe f = listToMaybe . mapMaybe f

elemBy :: (a -> a -> Bool) -> a -> [a] -> Bool
elemBy f x = or . (f x .)

none :: (a -> Bool) -> [a] -> Bool
none p = all (not . p)

maybeLast :: [a] -> Maybe a
maybeLast [] = Nothing
maybeLast [x] = Just x
maybeLast (_:t) = maybeLast t

maybeLastAndRest :: [a] -> Maybe ([a], a)
maybeLastAndRest [] = Nothing
maybeLastAndRest (h:t) = maybe (Just ([], h)) (Just . first (h:)) (maybeLastAndRest t)

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = map (\c -> if c == x then y else c)

(!!) :: [a] -> Int -> a
l !! i | i < 0 = (Prelude.!!) (reverse l) (-i - 1)
l !! i = (Prelude.!!) l i

erase_indexed :: [Int] -> [a] -> [a]
erase_indexed i l = f 0 l
 where
  f _ [] = []
  f n (_:t) | n `elem` i || (n - length l) `elem` i = f (n + 1) t
  f n (h:t) = h : f (n + 1) t

count :: (a -> Bool) -> [a] -> Int
count p = length . filter p

length_ge :: Int -> [a] -> Bool
length_ge 0 _ = True
length_ge (n+1) (_:t) = length_ge n t
length_ge _ _ = False
  -- length_ge is lazy in its list argument, which   length l >= n   is not.

take_atleast :: Int -> (a -> Int) -> [a] -> [a]
take_atleast _ _ [] = []
take_atleast n _ _ | n <= 0 = []
take_atleast n m (h:t) = h : take_atleast (n - m h) m t

replaceInfixM :: Eq a => [a] -> [a] -> [a] -> Maybe [a]
replaceInfixM what with l = (\(pre, post) -> pre ++ with ++ post) . stripInfix what l

replaceInfix :: Eq a => [a] -> [a] -> [a] -> [a]
replaceInfix what with l = fromMaybe l (replaceInfixM what with l)

replaceAllInfix :: Eq a => [a] -> [a] -> [a] -> [a]
replaceAllInfix what with l | Just r <- stripPrefix what l = with ++ replaceAllInfix what with r
replaceAllInfix what with (h:t) = h : replaceAllInfix what with t
replaceAllInfix _ _ [] = []

stripInfix :: Eq a => [a] -> [a] -> Maybe ([a], [a])
stripInfix p s | Just r <- stripPrefix p s = Just ([], r)
stripInfix p (h:t) = first (h:) . stripInfix p t
stripInfix _ _  = Nothing

partitionMaybe :: (a -> Maybe b) -> [a] -> ([a], [b])
partitionMaybe p = foldr (\x -> maybe (first (x:)) (second . (:)) (p x)) ([], [])

-- Test utilities

fail_test :: (Show a, Show b) => String -> a -> b -> IO ()
fail_test n x y = do
  putStr "Test failed: "; putStrLn n
  putStr "Expected: "; print x
  putStr "Actual:   "; print y
  putNewLn
  fail "test failure"

test_cmp :: (Eq a, Show a) => String -> a -> a -> IO ()
test_cmp n x y = when (x /= y) $ fail_test n x y

-- Finite

class Finite a where all_values :: [a]

instance (Finite a, Finite b) => Finite (Either a b) where
  all_values = Left . all_values ++ Right . all_values

instance (Enum a, Bounded a) => Finite a where all_values = enumFrom minBound

-- Ordinals

newtype Ordinal = Ordinal { ordinal_carrier :: Int }

instance Show Ordinal where
  show (Ordinal n) = case n of
    0 -> "first"; 1 -> "second"; 2 -> "third"; 3 -> "fourth"; 4 -> "fifth"
    5 -> "sixth"; 6 -> "seventh"; 7 -> "eighth"; 8 -> "ninth"; 9 -> "tenth"
    _ -> "<other ordinal>"

instance Invertible Ordinal where
  invert (Ordinal r) | r >= 0 = Ordinal (-r - 1)
  invert o = o

-- Cardinals

cardinals :: [String]
cardinals = words "zero one two three four five six seven eight nine ten"

-- Misc English/textual

once_twice_thrice :: Int -> String
once_twice_thrice i = case i of
  1 -> "once"; 2 -> "twice"; 3 -> "thrice"
  n -> show n ++ " times"

isVowel :: Char -> Bool
isVowel = (`elem` "aeoiu")

comma_enum :: String -> [String] -> String
comma_enum _ [] = ""
comma_enum _ [x] = x
comma_enum a [x, y] = x ++ (if length x > 25 then ", " else " ") ++ a ++ " " ++ y
comma_enum a [x, y, z] = x ++ ", " ++ y ++ ", " ++ a ++ " " ++ z
comma_enum z (x : y) = x ++ ", " ++ comma_enum z y

commas_and, commas_or :: [String] -> String
commas_and = comma_enum "and"
commas_or = comma_enum "or"

capitalize :: String -> String
capitalize (h:t) = toUpper h : t
capitalize [] = []

strip :: String -> String
strip = dropWhile isSpace . reverse . dropWhile isSpace . reverse

caselessStringEq :: String -> String -> Bool
caselessStringEq a b = (toLower . a) == (toLower . b)

-- Inversion

class Invertible a where invert :: a -> a

instance (Functor f, Invertible a) => Invertible (f a) where invert = fmap invert

-- Arrow utilities

snd_unit :: Arrow x => x () b -> x c (c, b)
snd_unit f = arr (\c -> (c, ())) >>> second f

liftA2 :: (Arrow a) => (c -> c' -> d) -> a b c -> a b c' -> a b d
liftA2 f a b = (a &&& b) >>> arr (uncurry f)

-- Options

class Option a where short :: a -> Char; long :: a -> String

show_long_opt :: Option o => o -> String
show_long_opt = ("--" ++) . long

show_long_opts :: Option o => [o] -> String
show_long_opts = concat . List.intersperse " " . map show_long_opt

instance (Option a, Option b) => Option (Either a b) where
  short = either short short; long = either long long

-- EitherOrBoth

data EitherOrBoth a b = LeftOnly a | RightOnly b | Both a b

instance (Show a, Show b) => Show (EitherOrBoth a b) where
  show (LeftOnly a) = "(LeftOnly " ++ show a ++ ")"
  show (RightOnly a) = "(RightOnly " ++ show a ++ ")"
  show (Both a b) = "(Both " ++ show a ++ " " ++ show b ++ ")"

-- Phantom

data Phantom a = Phantom

-- Approximate matching

type Cost = Float
data Op a = SkipOp a | InsertOp a | EraseOp a | ReplaceOp a a
data OpsWithCost a = OpsWithCost [Op a] Cost

instance Monoid (OpsWithCost a) where
  mempty = OpsWithCost [] 0
  mappend (OpsWithCost l c) (OpsWithCost l' c') = OpsWithCost (l ++ l') (c + c')

ops_cost :: OpsWithCost a -> Cost
ops_cost (OpsWithCost _ c) = c

opsWithCost :: (Op a -> Cost) -> [Op a] -> OpsWithCost a
opsWithCost co ops = OpsWithCost ops (sum $ map co ops)

addOp :: (Op a -> Cost) -> Op a -> OpsWithCost a -> OpsWithCost a
addOp co op (OpsWithCost ops c) = OpsWithCost (op : ops) (c + co op)

is_insertOp :: Op a -> Bool
is_insertOp (InsertOp _) = True
is_insertOp _ = False

approx_match :: Eq a => (Op a -> Cost) -> [a] -> [a] -> [(OpsWithCost a, Int, Int)]
approx_match co pattern text =
  sortBy (\(x,_,_) (y,_,_) -> compare (ops_cost x) (ops_cost y)) $
    zipWith (\owc@(OpsWithCost o _) z -> (owc, z, count (not . is_insertOp) o)) r [0..]
 where
  r = foldl f (replicate (length text + 1) mempty) (tail $ reverse $ tails pattern)
    -- r!!n contains the cheapest cost and corresponding replace-length of replacing the pattern at position n in the text.
    -- the nth intermediate list in r's fold stores at position p the cheapest cost and corresponding replace-length of matching the the last n elements of the pattern at position p in the text.
  f v pattern_tail = foldl g [opsWithCost co (map InsertOp pattern_tail)] [length text, length text - 1 .. 1]
   where
    c = head pattern_tail
    g w m = minimumBy (compare `on` ops_cost) candidates : w
     where
      d = text !! (m - 1)
      candidates =
        [ addOp co (if c == d then SkipOp c else ReplaceOp c d) (v!!m)
        , addOp co (InsertOp c) (v!!(m - 1))
        , addOp co (EraseOp d) (head w)
        ]

levenshtein :: String -> String -> Float
levenshtein s t = d !! length s !! length t
  where
    d = [[ distance m n | n <- [0 .. length t]] | m <- [0 .. length s]]
    distance :: Int -> Int -> Float
    distance i 0 = fromIntegral i
    distance 0 j = fromIntegral j
    distance i j = minimum [d!!(i-1)!!j+1, d!!i!!(j-1)+1, d!!(i-1)!!(j-1) + (if s!!(i-1)==t!!(j-1) then -0.4 else 1)]

-- Misc misc

prefixError :: String -> Either String a -> Either String a
prefixError _ (Right x) = Right x
prefixError s (Left s') = Left (s ++ s')

isIdChar :: Char -> Bool
isIdChar = isAlphaNum .||. (== '_')

instance (Functor m, Monad m) => Applicative m where pure = return; (<*>) = liftM2 ($)

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

kibi, mebi :: Integral a => a
kibi = 1024
mebi = kibi * kibi

readEither :: Read a => String -> Either String a
readEither s
  | [(x, r)] <- reads s, all isSpace r = return x
  | otherwise = fail "parse failure"

readTypedFile :: Read a => FilePath -> IO a
readTypedFile f = either (const $ fail $ "parsing \"" ++ f ++ "\"") return =<< readEither . readFile f

maybeM :: Monad m => Maybe a -> (a -> m ()) -> m ()
maybeM m a = maybe (return ()) a m

msapp :: (Data.Monoid.Monoid a, MonadState a m) => a -> m ()
msapp = modify . flip Data.Monoid.mappend

(.||.), (.&&.) :: (a -> Bool) -> (a -> Bool) -> (a -> Bool)
(f .||. g) x = f x || g x
(f .&&. g) x = f x && g x
  -- In applicative notation, these could be written: f .||. g = [[ f || g ]].

orElse :: Maybe a -> a -> a
orElse (Just x) _ = x
orElse Nothing x = x

putNewLn :: IO ()
putNewLn = putStrLn ""

(<<) :: Monad m => m a -> m b -> m a
x << y = x >>= \z -> y >> return z

(>+>) :: (Monad m, Monoid n) => m n -> m n -> m n
(>+>) = liftM2 mappend

parsep :: Char
parsep = '\x2029' -- U+2029 PARAGRAPH SEPARATOR

describe_new_output :: Maybe String -> String -> String
describe_new_output Nothing new = new
describe_new_output (Just prev) new =
  if prev /= new || length new <= 20 then new
  else if any (`isPrefixOf` new) ["error:", "internal compiler error:"] then "Same error."
  else if "warning:" `isPrefixOf` new then "Same warning."
  else "No change in output."

readState :: Monad y => StateT x y x
readState = StateT $ \x -> return (x, x)

mapState' :: Monad y => (x -> x) -> StateT x y ()
mapState' f = StateT $ \s -> return ((), f s)

data TriBool = Definitely | Indeterminate | DefinitelyNot

newtype MaybeEitherString a = MaybeEitherString (Maybe (Either String a)) deriving (Show, Typeable)

instance Monad MaybeEitherString where
  return = MaybeEitherString . return . return
  MaybeEitherString Nothing >>= _ = MaybeEitherString Nothing
  MaybeEitherString (Just (Left e)) >>= _ = MaybeEitherString $ Just $ Left e
  MaybeEitherString (Just (Right x)) >>= f = f x
  fail = MaybeEitherString . return . fail

instance MonadPlus MaybeEitherString where
  mzero = MaybeEitherString Nothing
  mplus (MaybeEitherString Nothing) = id
  mplus c = const c

instance Functor MaybeEitherString where
  fmap f (MaybeEitherString (Just (Right x))) = MaybeEitherString $ Just $ Right $ f x
  fmap _ (MaybeEitherString (Just (Left e))) = MaybeEitherString $ Just $ Left e
  fmap _ (MaybeEitherString Nothing) = MaybeEitherString Nothing
