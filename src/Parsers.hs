{-# LANGUAGE FunctionalDependencies, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}

module Parsers where

import qualified Data.List as List
import qualified Text.ParserCombinators.Parsec as PS
import qualified Editing.Basics
import qualified Editing.Show ()
import qualified Data.Char as Ch

import Control.Monad (liftM2)
import Control.Arrow (first)
import Data.List ((\\))
import Util ((.), NElist(..), unne, Finite(..), commas_or, Option(..), (.||.), isIdChar, (<<))

import Prelude hiding ((.))

-- To let us write parsers that work with both Parsec and our own Parser monad, we introduce the ParserLike class:

class (Functor m, Monad m) => ParserLike m t | m -> t where
  anySymbol :: m t
  anySymbol = satisfy $ const True
  symbols :: (Eq t, Show t) => [t] -> m [t]
  symbols [] = return []
  symbols (h:t) = liftM2 (:) (satisfy (== h)) (symbols t)
  satisfy :: (t -> Bool) -> m t
  (<|>) :: m a -> m a -> m a
  (<?>) :: m a -> String -> m a
  lookAhead :: m a -> m a
  pzero :: m a
  eof :: m ()
  try :: m a -> m a
  getInput :: m [t]
  getInput = many anySymbol << eof

infix 0 <?>
infixr 1 <|>

-- Parsec is clearly ParserLike:

instance ParserLike (PS.GenParser Char st) Char where
  (<?>) = (PS.<?>)
  lookAhead = PS.lookAhead
  (<|>) = (PS.<|>)
  symbols = PS.string
  anySymbol = PS.anyChar
  satisfy = PS.satisfy
  pzero = PS.pzero
  eof = PS.eof
  try = PS.try

-- Utility parser combinators:

manyTill :: ParserLike m t => m a -> m b -> m ([a], b)
manyTill p e = ((,) [] . e) <|> liftM2 (\x (y, z) -> (x:y, z)) p (manyTill p e)

many1Till' :: ParserLike m t => m a -> m b -> m (NElist a, b)
many1Till' p e = p >>= \v -> first (NElist v) . manyTill p e

optionMaybe :: ParserLike m t => m a -> m (Maybe a)
optionMaybe p = Just . p <|> return Nothing

optional :: ParserLike m t => m a -> m (Maybe a)
optional p = Just . p <|> return Nothing

option :: ParserLike m t => a -> m a -> m a
option x p = maybe x id . optional p

symbol :: (Show t, Eq t, ParserLike m t) => t -> m t
symbol x = satisfy (== x) <?> show x

choice :: ParserLike m t => [m a] -> m a
choice = foldl (<|>) pzero

many :: ParserLike m t => m a -> m [a]
many p = liftM2 (:) p (many p) <|> return []

many1' :: ParserLike m t => m a -> m (NElist a)
many1' p = liftM2 NElist p (manys p)

many1 :: ParserLike m t => m a -> m [a]
many1 p = unne . many1' p

spaces :: ParserLike m Char => m String
spaces = many $ symbol ' '

noneOf :: (Eq t, ParserLike m t) => [t] -> m t
noneOf l = satisfy $ not . (`elem` l)

oneOf :: (Eq t, ParserLike m t) => [t] -> m t
oneOf l = satisfy (`elem` l)

manys :: ParserLike m t => m a -> m [a]
manys p = liftM2 (:) p (manys p) <|> return []

sep :: ParserLike m t => m a -> m b -> m (a, [(b, a)])
sep p p' = liftM2 (,) p (many (liftM2 (,) p' p))

sepBy1' :: ParserLike m t => m a -> m b -> m (NElist a)
sepBy1' x y = (\(h,t) -> NElist h $ map snd t) . sep x y

sepBy1 :: ParserLike m t => m a -> m b -> m [a]
sepBy1 p e = liftM2 (:) p ((e >> sepBy1 p e) <|> return [])

kwd :: String -> Parser Char String
kwd s = try $ symbols s << notFollowedBy (satisfy isIdChar) << spaces

kwds :: [String] -> Parser Char String
kwds s = choice $ kwd . s

{-

Parsec's parse error descriptions aren't very good for parsers that backtrack. Consider

  parse (try (char 'x' >> char 'y') <|> char 'a') "" "x3"

This parse fails because (1) there is no 'y' at the second column, and (2) there is no 'a' at the first column. Intuitively, we feel that the first cause is the "real" cause, as it follows a longer partial match. Unfortunately, Parsec's errors do not reflect this:

  Left (line 1, column 1):
  unexpected "3"
  expecting "y" or "a"

Here, column numbers start at 1. Thus, while it /does/ mention the 'y' expectation and the unexpected '3', it fails to give the column number where 'y' was expected, and also does not show that the 'y' expectation followed a longer partial match than the 'a' expectation.

The parsing combinators in this module properly keep track of lengths of partial matches, to get intuitive error descriptions. For the parse above, they report only the 'y' expectation, at the second column.

Parsec mixes ordinary parse errors with custom parse errors. One can "fail" at any point. The parsers below can only fail due to unexpected symbols. Additional errors can be added by using parsers returning (Either String a)'s.

-}

data ParseResult t a
  = ParseSuccess a [t] Int (Maybe (Int, [String]))
  -- The Int is the length of the match. Hence, the [t] is strictly redundant, but there for efficiency. The Maybe is the furthest we've been able to parse successfully, with an indication of why we couldn't go even further.
  | ParseFailure Int [String] Bool deriving Show
  -- The Int and [String] serve as the Maybe above. The Bool indicates whether the failure is terminal.

instance Functor (ParseResult t) where
  fmap f (ParseSuccess x t i m) = ParseSuccess (f x) t i m; fmap _ (ParseFailure x y b) = ParseFailure x y b

newtype Parser t a = Parser { run_parser :: [t] -> ParseResult t a }

drain :: Parser t [t]
drain = Parser $ \s -> ParseSuccess s [] (length s) Nothing

-- Parser is a Functor, a Monad, and ParserLike:

instance Functor (Parser t) where fmap f p = p >>= return . f

instance Monad (Parser t) where
  return x = Parser $ \s -> ParseSuccess x s 0 Nothing
  Parser p >>= f = Parser $ \s -> case p s of
    ParseSuccess r s' n m -> case run_parser (f r) s' of
      ParseSuccess r' s'' n' m' -> ParseSuccess r' s'' (n + n') (furthest m (offset n m'))
      ParseFailure n' m' b -> (\(x, y) -> ParseFailure x y b) $ let u = (n + n', m') in maybe u (furthest' u) m
    ParseFailure n m b -> ParseFailure n m b
  fail = const pzero

instance ParserLike (Parser a) a where
  anySymbol = satisfy (const True) <?> "any symbol"
  satisfy p = Parser $ \s -> case s of
    h:t | p h -> ParseSuccess h t 1 Nothing
    _ -> ParseFailure 0 [] False
  symbols t = Parser $ \s -> case List.stripPrefix t s of
    Nothing -> ParseFailure 0 [show t] False
    Just s' -> ParseSuccess t s' (length t) Nothing
  Parser p <|> Parser q = Parser $ \s -> case p s of
    ParseFailure n m False -> case q s of
      ParseSuccess r u n' m' -> ParseSuccess r u n' (Just $ maybe (n, m) (furthest' (n, m)) m')
      ParseFailure n' m' b -> ParseFailure x y b where (x, y) = furthest' (n, m) (n', m')
    ps -> ps
  Parser p <?> m = Parser $ \s -> case p s of
    ParseFailure 0 _ b -> ParseFailure 0 [m] b
    b -> b
  lookAhead (Parser p) = Parser $ \s -> case p s of
    ParseSuccess r _ _ m -> ParseSuccess r s 0 m
    e -> e
  pzero = Parser $ const $ ParseFailure 0 [] False
  eof = Parser $ \s -> if null s then ParseSuccess () [] 0 Nothing else ParseFailure 0 ["EOF"] False
  try = id
  getInput = Parser $ \s -> ParseSuccess s s 0 Nothing

notFollowedBy :: Parser t a -> Parser t ()
notFollowedBy (Parser p) = Parser $ \s -> case p s of
  ParseFailure _ _ _ -> ParseSuccess () s 0 Nothing
  ParseSuccess _ _ _ _ -> ParseFailure 0 [] False

-- Some Parser-specific parsers:

guarded :: (a -> Bool) -> Parser t a -> Parser t a
guarded f (Parser p) = Parser $ \s -> case p s of
  ParseSuccess x _ _ _ | not (f x) -> ParseFailure 0 [] False
  k -> k

char :: (Show t, Eq t) => t -> Parser t t
char t = satisfy (== t) <?> show t

commit :: Parser t a -> Parser t a
commit (Parser p) = Parser $ \s -> case p s of
  ParseFailure x y _ -> ParseFailure x y True
  ps -> ps

silent :: Parser t a -> Parser t a
silent (Parser p) = Parser $ \s -> case p s of
  ParseFailure _ _ b -> ParseFailure 0 [] b
  ParseSuccess r t n _ -> ParseSuccess r t n Nothing

optParser :: (Monad m, Functor m, Finite o, Option o) => Parser Char (m [o])
optParser = (<?> "option") $ (char '-' >>) $ do
    char '-'
    n <- (many1 $ satisfy $ isIdChar .||. (== '-')) <?> "option name"
    spaces
    case List.find ((== n) . long) all_values of
      Nothing -> return $ fail $ "No such option: --" ++ n
      Just o -> ((o:) .) . option (return []) optParser
  <|> do
    x <- many1 $ do
      d <- satisfy Ch.isAlpha <?> "option letter"
      case List.find ((== d) . short) all_values of
        Nothing -> return $ fail $ "No such option: -" ++ [d]
        Just o -> return $ return o
    spaces
    y <- option (return []) optParser
    return (liftM2 (++) (sequence x) y)

-- Misc:

showParseError :: String -> String -> Int -> [String] -> String
showParseError subject_desc input column expectation =
  "Unexpected " ++ unexpectation ++ "." ++
  if null expectation' then "" else " Expected " ++ commas_or expectation' ++ "."
  where
    unexpectation
      | column < length input = show (take 1 $ drop column input) ++ " " ++ show (Editing.Basics.describe_position_after column input)
      | otherwise = "end of " ++ subject_desc
    expectation' = (List.nub expectation \\ ["EOF", "' '"]) ++
      if "EOF" `elem` expectation then ["end of " ++ subject_desc] else []

furthest' :: (Int, [String]) -> (Int, [String]) -> (Int, [String])
furthest' (n, s) (n', s')
  | n < n' = (n', s')
  | n' < n = (n, s)
  | otherwise = (n, s ++ s')

furthest :: Maybe (Int, [String]) -> Maybe (Int, [String]) -> Maybe (Int, [String])
furthest Nothing x = x
furthest x Nothing = x
furthest (Just x) (Just y) = Just $ furthest' x y

offset :: Int -> Maybe (Int, [String]) -> Maybe (Int, [String])
offset _ Nothing = Nothing
offset n (Just (i, s)) = Just (i + n, s)

parseOrFailE :: Parser Char (Either String a) -> String -> String -> Either String a
parseOrFailE p input desc = case run_parser p input of
  ParseSuccess (Left e) _ _ _ -> fail e
  ParseSuccess (Right x) _ _ _ -> return x
  ParseFailure x y _ -> fail $ showParseError desc input x y

parseOrFail :: Parser Char a -> String -> String -> Either String a
parseOrFail p input desc = case run_parser p input of
  ParseSuccess x _ _ _ -> return x
  ParseFailure x y _ -> fail $ showParseError desc input x y
