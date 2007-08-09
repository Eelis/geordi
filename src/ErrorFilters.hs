module ErrorFilters
  ( process_cc1plus_errors
  , process_as_errors
  , process_ld_errors
  , process_prog_errors
  ) where

import Prelude hiding (catch, (.))
import Control.Monad
import Control.Monad.Fix
import Text.Regex
import Util
import Data.List
import Data.Char
import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token
import Control.Applicative hiding ((<|>))

process_as_errors, process_ld_errors, process_cc1plus_errors :: String -> String

process_as_errors e = maybe e (!!1) $ matchRegex (mkRegex "\\b(Error|Warning): ([^\n]*)") e

process_ld_errors e = maybe e head $ matchRegex (mkRegex "\\b(undefined reference to [^\n]*)") e

process_prog_errors :: String -> String -> String
process_prog_errors output result =
  maybe (output ++ result) head $ matchRegex (mkRegex ":error: ([^\n]*)") output

-- cc1plus:

(>>>) :: CharParser st String -> CharParser st String -> CharParser st String
(>>>) = liftM2 (++)

strSepBy :: CharParser st String -> String -> CharParser st String
strSepBy x y = fmap (concat . intersperse y) $ sepBy x (string y)

cxxExpr :: CharParser st String
cxxExpr =
    try (fmap show (charLiteral haskell) >>> cxxExpr) <|>
    (oneOf "(<[" >>= \o -> return [o] >>> strSepBy cxxExpr "," >>> string [mirror o] >>> cxxExpr) <|>
    option [] (fmap (:[]) (noneOf ")>],'\"") >>> cxxExpr)
  where mirror '(' = ')'; mirror '[' = ']'; mirror '<' = '>'; mirror c = error $ "no mirror for " ++ [c]
    -- Can get confused when faced with sneaky uses of characters like '>'. Consequently, neither repl_withs nor hide_default_arguments works flawlessly in every imaginable case.

class Tok a where t :: a -> CharParser st String

instance Tok Char where t c = string [c] <* spaces
instance Tok String where t c = string c <* spaces
instance Tok [String] where t c = foldr1 (<|>) (try . t . c)

anyStringTill :: CharParser st String -> CharParser st String
anyStringTill end = fix $ \scan -> end <|> (((:[]) . anyChar) >>> scan)

ioBasics, clutter_namespaces :: [String]
ioBasics = ["streambuf", "ofstream", "ifstream", "fstream", "filebuf", "ostream", "istream", "ostringstream", "istringstream", "stringstream", "iostream", "ios", "string"]
clutter_namespaces = ["std", "boost", "__debug", "__gnu_norm", "__gnu_debug_def", "__gnu_cxx", "__gnu_debug"]

localReplacer :: CharParser st String -> CharParser st String
localReplacer x = anyStringTill $ try $ (:[]) . satisfy (not . isIdChar) >>> x
  where isIdChar c = isAlphaNum c || c == '_'
    -- Todo: Doesn't replace at start of input. (Situation does not occur in geordi's use, though.)

defaulter :: [String] -> Int -> ([String] -> CharParser st a) -> CharParser st String
defaulter names idx def = localReplacer $
  t names >>> t '<' >>> (count idx (cxxExpr <* t ',') >>= \prec -> def prec >> return (concat $ intersperse ", " prec)) >>> t '>'
    -- Hides default template arguments.

replacers :: [CharParser st String]
replacers = (.) localReplacer
  [ t clutter_namespaces >> t "::" >> return []
  , string "basic_" >> t ioBasics <* string "<char>"
  , (\e -> "list<" ++ e ++ ">::iterator") . (t "_List_iterator<" >> cxxExpr <* t '>')
  , (\e -> "list<" ++ e ++ ">::const_iterator") . (t "_List_const_iterator<" >> cxxExpr <* t '>')
  , (++ "::const_iterator") . (t "__normal_iterator<const " >> cxxExpr >> t ',' >> cxxExpr <* t '>')
  , (++ "::iterator") . (t "__normal_iterator<" >> cxxExpr >> t ',' >> cxxExpr <* t '>')
      -- Last two are for vector/string.
  , (++ "::const_iterator") . (t "_Safe_iterator<_Rb_tree_const_iterator<" >> cxxExpr >> t ">," >> cxxExpr <* t '>')
  , (++ "::iterator") . (t "_Safe_iterator<_Rb_tree_iterator<" >> cxxExpr >> t ">," >> cxxExpr <* t '>')
      -- Last two are for (multi)set/(multi)map.
  , t "_Safe_iterator<" >> cxxExpr <* t ',' <* cxxExpr <* t '>'
  -- Regarding deque iterators:   deque<void(*)() >::const_iterator   is written in errors as   _Deque_iterator<void (*)(), void (* const&)(), void (* const*)()>   . Detecting the const in there is too hard (for now).
  ] ++
  [ defaulter ["list", "deque", "vector"] 1 (\[e] -> t "allocator<" >> t e >> t '>')
  , defaulter ["set", "multiset", "basic_stringstream", "basic_string", "basic_ostringstream", "basic_istringstream"] 2 (\[e, _] -> t "allocator<" >> t e >> t '>')
  , defaulter ["map", "multimap"] 3 (\[k, v, _] -> t "allocator<pair<const " >> t k >> t ',' >> t v >> t '>' >> t '>')
  , defaulter ["map", "multimap"] 3 (\[k, v, _] -> t "allocator<pair<" >> t k >> t "const" >> t ',' >> t v >> t '>' >> t '>')
  , defaulter ["set", "multiset"] 1 (\[e] -> t "less<" >> t e >> t '>')
  , defaulter ["priority_queue"] 1 (\[e] -> t "vector<" >> t e >> t '>')
  , defaulter ["map", "multimap", "priority_queue"] 2 (\[k, _] -> t "less<" >> t k >> t '>')
  , defaulter ["queue", "stack"] 1 (\[e] -> t "deque<" >> t e >> t '>')
  , defaulter ((("basic_" ++) . ioBasics) ++ ["ostreambuf_iterator", "istreambuf_iterator"]) 1 (\[e] -> t "char_traits<" >> t e >> t '>')
  , defaulter ["istream_iterator"] 3 (const $ t "long int") -- "long int" is what is printed for ptrdiff_t, though probably not on all platforms.
  , defaulter ["istream_iterator", "ostream_iterator"] 2 (\[_, c] -> t "char_traits<" >> t c >> t '>')
  , defaulter ["istream_iterator", "ostream_iterator"] 1 (const $ t "char")

  , (foldr $ \(k, v) u -> subRegex (mkRegex $ "\\b" ++ k ++ "\\b") u v) . (manyTill anyChar $ try $ string " [with ") <*> (sepBy (try $ (,) . (manyTill anyChar $ t " =") <*> cxxExpr) (t ',') <* char ']')
  ]

process_cc1plus_errors e = maybe e' (!!1) $ matchRegex (mkRegex "\\b(error|warning): ([^\n]*)") e'
  where
    h s = either (const s) h $ parse (foldr1 (<|>) (try . replacers) >>> getInput) "" s
    e' = h e
