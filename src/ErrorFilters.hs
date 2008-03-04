-- Virtually everything here is GCC specific.

module ErrorFilters (cc1plus, as, ld, prog) where

import Control.Monad (ap)
import Text.Regex (mkRegex, subRegex, matchRegexAll, Regex)
import Data.List (intersperse, isPrefixOf)
import Data.Char (isAlphaNum, toLower)
import Text.ParserCombinators.Parsec
  (string, sepBy, parse, char, try, getInput, (<|>), satisfy, spaces, manyTill, anyChar, noneOf, option, count, CharParser, notFollowedBy, choice)
import Text.ParserCombinators.Parsec.Prim (GenParser)
import Text.ParserCombinators.Parsec.Language (haskell)
import Text.ParserCombinators.Parsec.Token (charLiteral, stringLiteral)
import Control.Applicative (Applicative(..))

import Util
import Prelude hiding (catch, (.))

instance Applicative (Text.ParserCombinators.Parsec.Prim.GenParser Char st) where
  pure = return; (<*>) = ap

-- Using the following more general instance causes overlapping instance problems elsewhere:
--   instance (Monad m, Functor m) => Applicative m where pure = return; (<*>) = ap

as, ld, cc1plus, prog :: String -> String

as e = maybe e (\(_, (m:ms), _, _) -> toLower m : ms) $ matchRegexAll (mkRegex "\\b(Error|Warning): [^\n]*") e

ld e = maybe e (\(_, m, _, _) -> "error: " ++ m) $ matchRegexAll (mkRegex "\\bundefined reference to [^\n]*") e

prog = subRegex' (mkRegex "/usr/[^:]+:[[:digit:]]+:error: ") " error: " . cleanup_types
  -- We apply cleanup_types even to successful output, to clean up assertion failures and {E}TYPE strings. The subRegex cleans up libstdc++ debug mode errors.

cxxArg :: CharParser st String
cxxArg = strip . ce
 where
  ce =
    try (show . charLiteral haskell >+> ce) <|>
    try (show . stringLiteral haskell >+> ce) <|>
      -- Haskell's char/string literals are much like C++'s.
    between '(' ')' <|> between '<' '>' <|> between '[' ']' <|>
    option [] ((:[]) . noneOf ")>],'\"" >+> ce)
  between open close =
    string [open] >+> (concat . intersperse "," . sepBy ce (string ",")) >+> string [close] >+> ce
    -- cxxArg can get confused when faced with sneaky uses of tokens like '>'.

(<$) :: GenParser Char st a -> GenParser Char st b -> GenParser Char st a
a <$ b = a << spaces << b

($>) :: GenParser Char st a -> GenParser Char st b -> GenParser Char st b
a $> b = a >> spaces >> b

strings :: [String] -> GenParser Char st String
strings l = choice (try . string . l)

ioBasics, clutter_namespaces :: [String]
ioBasics = ["streambuf", "ofstream", "ifstream", "fstream", "filebuf", "ostream", "istream", "ostringstream", "istringstream", "stringstream", "iostream", "ios", "string"]
clutter_namespaces = ["std", "boost", "__debug", "__gnu_norm", "__gnu_debug_def", "__gnu_cxx", "__gnu_debug", "__norm"]

isIdChar :: Char -> Bool
isIdChar = isAlphaNum .||. (== '_')

type Replacer st = CharParser st String

localReplacer :: Replacer st -> Replacer st
localReplacer x = try x <|> scan
  where scan = anyChar >>= \c -> (c:) . if isIdChar c then scan else localReplacer x
    -- Turns a replacer that replaces X with Y into a replacer that replaces ZX with ZY for any Z.

defaulter :: [String] -> Int -> ([String] -> CharParser st a) -> Replacer st
defaulter names idx def =
  strings names $>> string "<" $>> (count idx (cxxArg <$ char ',' << spaces) >>= \prec -> def prec >> return (concat $ intersperse ", " prec)) $>> string ">"
    where x $>> y = (x << spaces) >+> y
    -- Hides default template arguments.

name :: String -> CharParser st String
name i = string i << notFollowedBy (satisfy isIdChar)
  -- For keywords and identifiers.

tmpl :: String -> [GenParser Char st a] -> GenParser Char st [a]
tmpl n ps = string n $> char '<' $> f ps
  where
    f [] = error "tmpl _ []"
    f [p] = (:[]) . (p <$ char '>' << spaces)
    f (p:pp) = (:) . (p << char ',' << spaces) <*> (f pp)

tmpi :: String -> Int -> GenParser Char st [String]
tmpi n p = tmpl n (replicate p cxxArg)

tmpls :: String -> [String] -> GenParser Char st [String]
tmpls n u = tmpl n (string . u)

replacer :: Replacer st
replacer = (try . localReplacer . choice) (try .
  [ strings clutter_namespaces $> string "::" $> return ""
  , string "basic_" >> strings ioBasics <$ char '<' <$ string "char" <$ char '>'
  , string "basic_" >> ('w':) . strings ioBasics <$ char '<' <$ string "wchar_t" <$ char '>'
  , (\[e] -> "list<" ++ e ++ ">::iterator") . tmpi "_List_iterator" 1
  , (\[e] -> "list<" ++ e ++ ">::const_iterator") . tmpi "_List_const_iterator" 1
  , (\[x, y] -> y ++ "::" ++ (if "const" `isPrefixOf` x then "const_" else "") ++ "iterator") . tmpi "__normal_iterator" 2
      -- For vector/string. Next two for (multi)set/(multi)map.
  , (++ "::iterator") . (!!1) . tmpl "_Safe_iterator" [head . tmpi "_Rb_tree_iterator" 1, cxxArg]
  , (++ "::const_iterator") . (!!1) . tmpl "_Safe_iterator" [head . tmpi "_Rb_tree_const_iterator" 1, cxxArg]
  , head . tmpi "_Safe_iterator" 2 -- For vector/deque.
  , (\[e,d,_] -> "deque<" ++ e ++ ">::" ++ (if "const " `isPrefixOf` d then "const_" else "") ++ "iterator") . tmpi "_Deque_iterator" 3
  , tmpi "allocator" 1 >> string "::" $> ((\[e] -> "allocator<" ++ e ++ ">") . tmpi "rebind" 1) <$ string "::" <$ name "other"
  , ((++ "&") . head . tmpi "allocator" 1) <$ string "::" <$ name "reference"
  , ((++ " const &") . head . tmpi "allocator" 1) <$ string "::" <$ name "const_reference"
  , ((++ "*") . head . tmpi "allocator" 1) <$ string "::" <$ name "pointer"
  , tmpi "allocator" 1 >> string "::" $> name "size_type" >> return "size_t"
  , name "typename " $> return ""
      -- Shows up in assertion failures after replacements have been performed.
  , defaulter ["list", "deque", "vector"] 1 $ tmpls "allocator"
  , defaulter ["set", "multiset", "basic_stringstream", "basic_string", "basic_ostringstream", "basic_istringstream"] 2 $ \[k, _] -> tmpls "allocator" [k]
  , defaulter ["map", "multimap"] 3 $ \[k, v, _] -> tmpl "allocator" [tmpl "pair" [try (string "const " $> string k) <|> (string k $> string "const"), string v]]
  , defaulter ["set", "multiset"] 1 $ tmpls "less"
  , defaulter ["priority_queue"] 1 $ tmpls "vector"
  , defaulter ["queue", "stack"] 1 $ tmpls "deque"
  , defaulter ["map", "multimap", "priority_queue"] 2 $ tmpls "less" . init
  , defaulter ((("basic_" ++) . ioBasics) ++ ["ostreambuf_iterator", "istreambuf_iterator"])
      1 $ tmpls "char_traits"
  , defaulter ["istream_iterator"] 3 $ const $ option [] (string "long") $> string "int"
      -- "int"/"long int" is what is printed for ptrdiff_t.
  , defaulter ["istream_iterator", "ostream_iterator"] 2 $ tmpls "char_traits" . tail
  , defaulter ["istream_iterator", "ostream_iterator"] 1 $ const $ string "char"
  ]) <|> (try $ do
    s <- anyChar `manyTill` try (string " [with ")
    d <- (try $ (,) . (satisfy isIdChar `manyTill` string " = ") <*> cxxArg) `sepBy` string ", "
    char ']'
    return $ foldr with_subst s d)

-- Security note: Together, the replacers above must be strongly normalizing.

-- Things that go wrong but are hard to fix:
--   set<T>::iterator displayed as const version. Same for multiset.
--   vector<int*>::const_iterator displayed as nonconst version

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

subRegex' :: Regex -> String -> String -> String
subRegex' = flip . subRegex

-- With-substitution would fail if the following occurred in an error: "... T const ... [with T = int&]" (because it would be replaced with "... int& const ...". Fortunately, g++ places cv-qualifiers on the left side in these cases. For example, see the error message for: "template <typename T> std::string f(T const &); void g() { int i = 3; !f<int&>(i); }".

cleanup_types :: String -> String
cleanup_types s = either (const s) cleanup_types $ parse (replacer >+> getInput) "" s

cc1plus e = maybe e' (\(_, m, _, _) -> m) $ matchRegexAll (mkRegex "\\b(error|warning): [^\n]*") e'
  where e' = cleanup_types e
