{-# LANGUAGE UnicodeSyntax, FunctionalDependencies, MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances, PatternGuards, RecordWildCards, NamedFieldPuns, ViewPatterns, TypeSynonymInstances #-}

module Parsers where

import qualified Data.List as List
import qualified Text.ParserCombinators.Parsec as PS
import qualified Editing.Commands
import qualified Editing.Show ()
import qualified Data.Char as Ch

import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Foldable (toList)
import Control.Monad (liftM, liftM2)
import Control.Arrow (first)
import Control.Monad.Except (MonadError(..))
import Data.List ((\\))
import Data.Maybe (fromMaybe)
import Editing.Basics (positionIn)
import Util ((.), Finite(..), commas_or, Option(..), (.∨.), isIdChar, (<<), NeList)

import Prelude hiding ((.))
import Prelude.Unicode

-- To let us write parsers that work with both Parsec and our own Parser monad, we introduce the ParserLike class:

class (Functor m, Monad m) ⇒ ParserLike m t | m → t where
  anySymbol :: m t
  anySymbol = satisfy $ const True
  symbols :: (Eq t, Show t) ⇒ [t] → m [t]
  symbols = foldr (\ h → liftM2 (:) (satisfy (== h))) (return [])
  satisfy :: (t → Bool) → m t
  (<|>) :: m a → m a → m a
  (<?>) :: m a → String → m a
  lookAhead :: m a → m a
  pzero :: m a
  eof :: m ()
  try :: m a → m a
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

manyTill :: ParserLike m t ⇒ m a → m b → m ([a], b)
manyTill p e = ((,) [] . e) <|> liftM2 (\x (y, z) → (x:y, z)) p (manyTill p e)

many1Till :: ParserLike m t ⇒ m a → m b → m (NeList a, b)
many1Till p e = p >>= \v → first (v :|) . manyTill p e

optionMaybe :: ParserLike m t ⇒ m a → m (Maybe a)
optionMaybe p = Just . p <|> return Nothing

optional :: ParserLike m t ⇒ m a → m (Maybe a)
optional p = Just . p <|> return Nothing

option :: ParserLike m t ⇒ a → m a → m a
option x p = fromMaybe x . optional p

symbol :: (Show t, Eq t, ParserLike m t) ⇒ t → m t
symbol x = satisfy (== x) <?> show x

choice :: ParserLike m t ⇒ [m a] → m a
choice = foldl (<|>) pzero

many :: ParserLike m t ⇒ m a → m [a]
many p = liftM2 (:) p (many p) <|> return []

many1 :: ParserLike m t ⇒ m a → m (NeList a)
many1 p = liftM2 (:|) p (many p)

spaces :: ParserLike m Char ⇒ m String
spaces = many $ symbol ' '

noneOf :: (Eq t, ParserLike m t) ⇒ [t] → m t
noneOf l = satisfy (∉ l)

oneOf :: (Eq t, ParserLike m t) ⇒ [t] → m t
oneOf l = satisfy (∈ l)

sep :: ParserLike m t ⇒ m a → m b → m (a, [(b, a)])
sep p p' = liftM2 (,) p (many (liftM2 (,) p' p))

sepBy1' :: ParserLike m t ⇒ m a → m b → m (NeList a)
sepBy1' x y = (\(h, t) → h :| map snd t) . sep x y

sepBy1 :: ParserLike m t ⇒ m a → m b → m [a]
sepBy1 p e = liftM2 (:) p ((e >> sepBy1 p e) <|> return [])

kwd :: String → Parser Char String
kwd s = try $ symbols s << notFollowedBy (satisfy isIdChar) << spaces

kwds :: [String] → Parser Char String
kwds = choice . (kwd .)

char_unit :: (Eq a, Show a) ⇒ a → Parser a ()
char_unit = (>> return ()) . char

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

data Expectation = Expected { expectedAt :: Int, expectedWhat :: [String] }

uninformativeExpectation :: Expectation
uninformativeExpectation = Expected 0 []

data ParseResult t a
  = ParseSuccess a [t] Int (Maybe Expectation)
  -- The Int is the length of the match. Hence, the [t] is strictly redundant, but there for efficiency. The Maybe is the furthest we've been able to parse successfully, with an indication of why we couldn't go even further.
  | ParseFailure Expectation Bool
  -- The Bool indicates whether the failure is terminal.

instance Functor (ParseResult t) where
  fmap f (ParseSuccess x t i m) = ParseSuccess (f x) t i m; fmap _ (ParseFailure e b) = ParseFailure e b

newtype Parser t a = Parser { run_parser :: [t] → ParseResult t a }

peek :: Parser t [t]
peek = Parser $ \s → ParseSuccess s s 0 Nothing

drain :: Parser t [t]
drain = Parser $ \s → ParseSuccess s [] (length s) Nothing

parseSuccess :: a → Parser t a
parseSuccess = return

-- Parser is a Functor, a Monad, and ParserLike:

instance Functor (Parser t) where fmap = liftM

instance Monad (Parser t) where
  return x = Parser $ \s → ParseSuccess x s 0 Nothing
  Parser p >>= f = Parser $ \s → case p s of
    ParseSuccess r s' n m → case run_parser (f r) s' of
      ParseSuccess r' s'' n' m' → ParseSuccess r' s'' (n + n') (furthest m (offset n . m'))
      ParseFailure (offset n → m') b → ParseFailure (maybe m' (furthest' m') m) b
    ParseFailure e b → ParseFailure e b
  fail = const pzero

instance ParserLike (Parser a) a where
  anySymbol = satisfy (const True) <?> "any symbol"
  satisfy p = Parser $ \s → case s of
    h:t | p h → ParseSuccess h t 1 Nothing
    _ → ParseFailure uninformativeExpectation False
  symbols t = Parser $ \s → case List.stripPrefix t s of
    Nothing → ParseFailure (Expected 0 [show t]) False
    Just s' → ParseSuccess t s' (length t) Nothing
  Parser p <|> Parser q = Parser $ \s → case p s of
    ParseFailure m False → case q s of
      ParseSuccess r u n' m' → ParseSuccess r u n' $ Just $ maybe m (furthest' m) m'
      ParseFailure m' b → ParseFailure (furthest' m m') b
    ps → ps
  Parser p <?> m = Parser $ \s → case p s of
    ParseFailure (Expected 0 _) b → ParseFailure (Expected 0 [m]) b
    b → b
  lookAhead (Parser p) = Parser $ \s → case p s of
    ParseSuccess r _ _ m → ParseSuccess r s 0 m
    e → e
  pzero = Parser $ const $ ParseFailure uninformativeExpectation False
  eof = Parser $ \s → if null s then ParseSuccess () [] 0 Nothing else ParseFailure (Expected 0 ["EOF"]) False
  try = id
  getInput = Parser $ \s → ParseSuccess s s 0 Nothing

notFollowedBy :: Parser t a → Parser t ()
notFollowedBy (Parser p) = Parser $ \s → case p s of
  ParseFailure{} → ParseSuccess () s 0 Nothing
  ParseSuccess{} → ParseFailure uninformativeExpectation False

-- Some Parser-specific parsers:

guarded :: (a → Bool) → Parser t a → Parser t a
guarded f (Parser p) = Parser $ \s → case p s of
  ParseSuccess x _ _ _ | not (f x) → ParseFailure uninformativeExpectation False
  k → k

char :: (Show t, Eq t) ⇒ t → Parser t t
char t = satisfy (== t) <?> show t

commit :: Parser t a → Parser t a
commit (Parser p) = Parser $ \s → case p s of
  ParseFailure x _ → ParseFailure x True
  ps → ps

silent :: Parser t a → Parser t a
silent (Parser p) = Parser $ \s → case p s of
  ParseFailure _ b → ParseFailure uninformativeExpectation b
  ParseSuccess r t n _ → ParseSuccess r t n Nothing

optParser :: (MonadError String m, Functor m, Finite o, Option o) ⇒ Parser Char (m [o])
optParser = (<?> "option") $ (char '-' >>) $ do
    char '-'
    n ← (<?> "option name") $ toList . many1 (satisfy $ isIdChar .∨. (== '-'))
    spaces
    case List.find ((== n) . long) all_values of
      Nothing → return $ throwError $ "No such option: --" ++ n
      Just o → ((o:) .) . option (return []) optParser
  <|> do
    x ← many1 $ do
      d ← satisfy Ch.isAlpha <?> "option letter"
      return $ case List.find ((== Just d) . short) all_values of
        Nothing → throwError $ "No such option: -" ++ [d]
        Just o → return o
    spaces
    y ← option (return []) optParser
    return (liftM2 (++) (sequence $ toList x) y)

-- Misc:

showParseError :: String → String → Int → [String] → String
showParseError subject_desc input column expectation =
  "Unexpected " ++ unexpectation ++ "." ++
  if null expectation' then "" else " Expected " ++ commas_or expectation' ++ "."
  where
    unexpectation
      | h:t ← drop column input =
        '`' : (if Ch.isAlphaNum h then h : takeWhile Ch.isAlphaNum t else [h]) ++ "` " ++
        show (Editing.Commands.describe_position_after (positionIn input column) input)
      | otherwise = "end of " ++ subject_desc
    expectation' = (List.nub expectation \\ ["EOF", "' '"]) ++
      ["end of " ++ subject_desc | "EOF" ∈ expectation]

furthest' :: Expectation → Expectation → Expectation
furthest' (Expected n s) (Expected n' s')
  | n < n' = Expected n' s'
  | n' < n = Expected n s
  | otherwise = Expected n (s ++ s')

furthest :: Maybe Expectation → Maybe Expectation → Maybe Expectation
furthest Nothing x = x
furthest x Nothing = x
furthest (Just x) (Just y) = Just $ furthest' x y

offset :: Int → Expectation → Expectation
offset n Expected{..} = Expected{expectedAt = expectedAt + n, ..}

parseOrFailE :: Parser Char (Either String a) → String → String → Either String a
parseOrFailE p input desc = case run_parser p input of
  ParseSuccess (Left e) _ _ _ → throwError e
  ParseSuccess (Right x) _ _ _ → return x
  ParseFailure (Expected x y) _ → throwError $ showParseError desc input x y

parseOrFail :: Parser Char a → String → String → Either String a
parseOrFail p input desc = case run_parser p input of
  ParseSuccess x _ _ _ → return x
  ParseFailure (Expected x y) _ → throwError $ showParseError desc input x y
