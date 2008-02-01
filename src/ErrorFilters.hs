-- Virtually everything here is GCC specific.

module ErrorFilters (cc1plus, as, ld, prog) where

import Control.Monad (liftM2, ap)
import Text.Regex (matchRegex, mkRegex, subRegex)
import Data.List (intersperse)
import Data.Char (isAlphaNum)
import Text.ParserCombinators.Parsec
  (string, sepBy, parse, char, try, getInput, (<|>), satisfy, spaces, manyTill, anyChar, noneOf, option, count, CharParser, notFollowedBy, choice)
import Text.ParserCombinators.Parsec.Prim (GenParser)
import Text.ParserCombinators.Parsec.Language (haskell)
import Text.ParserCombinators.Parsec.Token (charLiteral, stringLiteral)
import Control.Applicative (Applicative(..), (<*))

import Util
import Prelude hiding (catch, (.))

instance Applicative (Text.ParserCombinators.Parsec.Prim.GenParser Char st) where
  pure = return; (<*>) = ap

-- Using the following more general instance causes overlapping instance problems elsewhere:
--   instance (Monad m, Functor m) => Applicative m where pure = return; (<*>) = ap

as, ld, cc1plus, prog :: String -> String

as e = maybe e (!!1) $ matchRegex (mkRegex "\\b(Error|Warning): ([^\n]*)") e

ld e = maybe e head $ matchRegex (mkRegex "\\b(undefined reference to [^\n]*)") e

prog output = maybe (cleanup_types output) head $ matchRegex (mkRegex ":error: ([^\n]*)") output
  -- We apply cleanup_types even to successful output, to clean up assertion failures and {E}TYPE strings.

(>>>) :: CharParser st String -> CharParser st String -> CharParser st String
(>>>) = liftM2 (++)

(<<) :: Applicative f => f a -> f b -> f a
(<<) = (<*)

cxxArg :: CharParser st String
cxxArg = strip . ce
 where
  ce =
    try (show . charLiteral haskell >>> ce) <|>
    try (show . stringLiteral haskell >>> ce) <|>
      -- Haskell's char/string literals are much like C++'s.
    between '(' ')' <|> between '<' '>' <|> between '[' ']' <|>
    option [] ((:[]) . noneOf ")>],'\"" >>> ce)
  between open close =
    string [open] >>> (concat . intersperse "," . sepBy ce (string ",")) >>> string [close] >>> ce
    -- cxxArg can get confused when faced with sneaky uses of tokens like '>'.

class ExactParse a st | a -> st where exact :: a -> CharParser st String

instance ExactParse Char st where exact c = string [c]
instance ExactParse String st where exact = string
instance ExactParse [String] st where exact l = choice (try . string . l)
instance ExactParse (CharParser st String) st where exact = id
instance ExactParse (CharParser st Char) st where exact = ((:[]) .)

($>), (<$), ($>>) :: (ExactParse a st, ExactParse b st) => a -> b -> CharParser st String
x $> y = exact x >> spaces >> exact y
x <$ y = exact x << spaces << exact y
x $>> y = (exact x << spaces) >>> exact y

anyStringTill :: CharParser st String -> CharParser st String
anyStringTill end = scan ""
  where scan r = (reverse r ++) . end <|> (scan . (:r) =<< anyChar)

ioBasics, clutter_namespaces :: [String]
ioBasics = ["streambuf", "ofstream", "ifstream", "fstream", "filebuf", "ostream", "istream", "ostringstream", "istringstream", "stringstream", "iostream", "ios", "string"]
clutter_namespaces = ["std", "boost", "__debug", "__gnu_norm", "__gnu_debug_def", "__gnu_cxx", "__gnu_debug", "__norm"]

isIdChar :: Char -> Bool
isIdChar = isAlphaNum .||. (== '_')

type Replacer st = CharParser st String

localReplacer :: Replacer st -> Replacer st
localReplacer x = x <|> (anyStringTill $ try $ (:[]) . satisfy (not . isIdChar) >>> x)
  -- Turns a replacer that replaces X with Y into a replacer that replaces ZX with ZY for any Z.

defaulter :: [String] -> Int -> ([String] -> CharParser st a) -> Replacer st
defaulter names idx def = localReplacer $ try $
  names $>> '<' $>> (count idx (cxxArg <$ ',' << spaces) >>= \prec -> def prec >> return (concat $ intersperse ", " prec)) $>> '>'
    -- Hides default template arguments.

name :: String -> CharParser st String
name i = string i << notFollowedBy (satisfy isIdChar)
  -- For keywords and identifiers.

tmp :: String -> CharParser st a -> CharParser st a
tmp n a = n $> '<' >> a << char '>'

replacers :: [Replacer st]
replacers = (.) localReplacer
  [ clutter_namespaces $> "::" $> ""
  , string "basic_" >> ioBasics <$ '<' <$ "char" <$ '>'
  , string "basic_" >> ('w':) . exact ioBasics <$ '<' <$ "wchar_t" <$ '>'
  , tmp "_List_iterator" $ (\e -> "list<" ++ e ++ ">::iterator") . cxxArg
  , tmp "_List_const_iterator" $ (\e -> "list<" ++ e ++ ">::const_iterator") . cxxArg
  , tmp "__normal_iterator" $ spaces >> name "const" >> cxxArg >> ',' $> ((++ "::const_iterator") . cxxArg)
  , tmp "__normal_iterator" $ cxxArg >> char ',' >> ((++ "::iterator") . cxxArg)
      -- Last two are for vector/string. Next two for (multi)set/(multi)map.
  , tmp "_Safe_iterator" (tmp "_Rb_tree_iterator" cxxArg $> ',' >> ((++ "::iterator") . cxxArg))
  , tmp "_Safe_iterator" (tmp "_Rb_tree_const_iterator" cxxArg $> ',' >> ((++ "::const_iterator") . cxxArg))
    -- g++ displays both "set<T>::iterator" and "set<T>::const_iterator" as "_Safe_iterator<_Rb_tree_const_iterator<T>, set<T> >". Our replacers will produce "set<T>::iterator" for both.
  , tmp "_Safe_iterator" $ cxxArg << char ',' <$ cxxArg -- For vector/deque iterators.
  , tmp "_Deque_iterator" $ ((\e -> "deque<" ++ e ++ ">::const_iterator") . cxxArg) << char ',' <$ name "const" << cxxArg << char ',' << cxxArg
  , tmp "_Deque_iterator" $ ((\e -> "deque<" ++ e ++ ">::iterator") . cxxArg) << char ',' << cxxArg << char ',' << cxxArg
    -- g++ displays "deque<void(*)()>::const_iterator" as "_Deque_iterator<void (*)(), void (* const&)(), void (* const*)()>". Since our deque::const_iterator replacer requires the "const" modifier to be located at the start, the deque::iterator replacer is used, and the result is "deque<void(*)()>::iterator".

  , tmp "allocator" cxxArg $> "::" $> tmp "rebind" ((\e -> "allocator<" ++ e ++ ">") . cxxArg) <$ "::" <$ name "other"
  , tmp "allocator" ((++ "&") . cxxArg) <$ "::" <$ name "reference"
  , tmp "allocator" ((++ " const &") . cxxArg) <$ "::" <$ name "const_reference"
  , tmp "allocator" ((++ "*") . cxxArg) <$ "::" <$ name "pointer"
  , tmp "allocator" cxxArg $> "::" $> name "size_type" >> return "size_t"
  , name "typename" >> spaces >> return ""
      -- Shows up in assertion failures after replacements have been performed.
  ] ++
  [ defaulter ["list", "deque", "vector"] 1 (\[e] -> "allocator" $> '<' $> e $> '>')
  , defaulter ["set", "multiset", "basic_stringstream", "basic_string", "basic_ostringstream", "basic_istringstream"]
      2 (\[e, _] -> "allocator" $> '<' $> e $> '>')
  , defaulter ["map", "multimap"] 3
      (\[k, v, _] -> "allocator" $> '<' $> "pair" $> '<' $> "const " $> k $> ',' $> v $> '>' $> '>')
  , defaulter ["map", "multimap"] 3
      (\[k, v, _] -> "allocator" $> '<' $> "pair" $> '<' $> k $> "const" $> ',' $> v $> '>' $> '>')
  , defaulter ["set", "multiset"] 1 (\[e] -> "less" $> '<' $> e $> '>')
  , defaulter ["priority_queue"] 1 (\[e] -> "vector" $> '<' $> e $> '>')
  , defaulter ["map", "multimap", "priority_queue"] 2 (\[k, _] -> "less" $> '<' $> k $> '>')
  , defaulter ["queue", "stack"] 1 (\[e] -> "deque" $> '<' $> e $> '>')
  , defaulter ((("basic_" ++) . ioBasics) ++ ["ostreambuf_iterator", "istreambuf_iterator"])
      1 (\[e] -> "char_traits" $> '<' $> e $> '>')
  , defaulter ["istream_iterator"] 3 (const $ option [] (try $ string "long") $> "int")
      -- "int"/"long int" is what is printed for ptrdiff_t.
  , defaulter ["istream_iterator", "ostream_iterator"] 2 (\[_, c] -> "char_traits" $> '<' $> c $> '>')
  , defaulter ["istream_iterator", "ostream_iterator"] 1 (const $ string "char")

  , do
      s <- manyTill anyChar $ try $ string " [with "
      d <- (try $ (,) . (manyTill (satisfy isIdChar) $ string " = ") <*> cxxArg) `sepBy` string ", "
      char ']'
      return $ foldr with_subst s d
  ]

-- Security note: together, the replacers above must be strongly normalizing.

data RefKind = NoRef | LvalueRef | RvalueRef deriving Eq

stripRef :: String -> (String, RefKind)
stripRef s | Just s' <- stripSuffix "&&" s = (s', RvalueRef)
stripRef s | Just s' <- stripSuffix "&" s = (s', LvalueRef)
stripRef s = (s, NoRef)

with_subst :: (String, String) -> String -> String
with_subst (k, _) | or (not . isIdChar . k) = error "tried to match_subst non-name"
  -- The with-replacer must prevent this.
with_subst (k, v) =
  subRegex' (mkRegex $ "\\b" ++ k ++ "\\b") v .
  subRegex' (mkRegex $ "\\b" ++ k ++ "\\s*&") (v' ++ "&") .
  subRegex' (mkRegex $ "\\b" ++ k ++ "\\s*&&") (v' ++ if vrk == NoRef then "&&" else "&")
    -- Reference collapse rules are described in 7.1.3p9.
 where
  (v', vrk) = stripRef v
  subRegex' = flip . subRegex

-- With-substitution would fail if the following occurred in an error: "... T const ... [with T = int&]" (because it would be replaced with "... int& const ...". Fortunately, g++ places cv-qualifiers on the left side in these cases. For example, see the error message for: "template <typename T> std::string f(T const &); void g() { int i = 3; !f<int&>(i); }".

cleanup_types :: String -> String
cleanup_types s = either (const s) cleanup_types $ parse (choice (try . replacers) >>> getInput) "" s

cc1plus e = maybe e' (!!1) $ matchRegex (mkRegex "\\b(error|warning): ([^\n]*)") e'
  where e' = cleanup_types e
