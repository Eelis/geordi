module ErrorFilters
  ( process_cc1plus_errors
  , process_as_errors
  , process_ld_errors
  , process_prog_errors
  ) where

import Prelude hiding (catch, (.))
import Control.Monad
import Text.Regex
import Util
import Data.List
import Data.Char
import Data.Maybe
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Token

process_as_errors, process_ld_errors, process_cc1plus_errors :: String -> String

process_as_errors e = maybe e (!!1) $ matchRegex (mkRegex "\\b(Error|Warning): ([^\n]*)") e

process_ld_errors e = maybe e head $ matchRegex (mkRegex "\\b(undefined reference to [^\n]*)") e

process_prog_errors :: String -> String -> String
process_prog_errors output result =
  maybe (output ++ result) head $ matchRegex (mkRegex ":error: ([^\n]*)") output

-- cc1plus:

(>>>) :: CharParser st String -> CharParser st String -> CharParser st String
(>>>) = liftM2 (++)

(<<) :: Monad m => m a -> m b -> m a
(<<) x y = x >>= \r -> y >> return r

strSepBy :: CharParser st String -> String -> CharParser st String
strSepBy x y = fmap (concat . intersperse y) $ sepBy x (string y)

cxxExpr :: CharParser st String
cxxExpr =
    try (fmap show (charLiteral haskell) >>> cxxExpr) <|>
    (oneOf "(<[" >>= \o -> return [o] >>> strSepBy cxxExpr "," >>> string [mirror o] >>> cxxExpr) <|>
    option [] (fmap (:[]) (noneOf ")>],'\"") >>> cxxExpr)
  where mirror '(' = ')'; mirror '[' = ']'; mirror '<' = '>'
    -- Can get confused when faced with sneaky uses of characters like '>'. Consequently, neither repl_withs nor hide_default_arguments works flawlessly in every imaginable case.

repl_withs :: String -> String
repl_withs s = either (const s) id $ parse (r "") "" s
  where
    r pre = ((try (string "[with ") >> sepBy ass (string ", ") << char ']') >>=
      r . foldr (\(k, v) u -> subRegex (mkRegex $ "\\b" ++ k ++ "\\b") u v) pre) <|>
        (anyChar >>= \x -> r $ pre ++ [x]) <|> return pre
    ass = try $ liftM2 (,) (manyTill anyChar $ string " = ") cxxExpr

hide_default_template_args :: String -> String
hide_default_template_args s = either (const s) id $ parse p "" s
  where
    p = foldr1 (<|>) (uncurry q . templates_with_default_arguments) >>> p <|> (:[]) . anyChar >>> p <|> return []
    q name initials = try $ (string (name ++ "<") << spaces) >>> (concat . intersperse ", " . (hide_default_template_args .) . take initials . sepBy1 cxxExpr (spaces >> char ',' >> spaces)) >>> (spaces >> string ">" << spaces)
    templates_with_default_arguments =
      [ ("vector", 1), ("list", 1), ("deque", 1), ("multiset", 1), ("set", 1), ("multimap", 2), ("map", 2), ("basic_string", 1), ("basic_iostream", 1), ("basic_istream", 1), ("basic_ostream", 1), ("basic_fstream", 1), ("basic_ofstream", 1), ("basic_ifstream", 1), ("basic_ostringstream", 1), ("basic_istringstream", 1), ("basic_iostringstream", 1), ("basic_filebuf", 1), ("basic_ios", 1), ("basic_streambuf", 1), ("ostream_iterator", 1), ("istream_iterator", 1) ]

pre_subst_regexps, post_subst_regexps :: [(String, String)]
post_subst_regexps = [("\\bbasic_(string|[io]?(f|string)?stream)<char>", "\\1")]
pre_subst_regexps = [("\\bstd::", ""), ("\\b__gnu_(norm|cxx|debug(_def)?)::", "")]

subst_regexps :: [(String, String)] -> String -> String
subst_regexps = flip $ foldl (\u (r, repl) -> subRegex (mkRegex r) u repl)

process_cc1plus_errors e = maybe e' (!!1) $ matchRegex (mkRegex "\\b(error|warning): ([^\n]*)") e'
  where e' = subst_regexps post_subst_regexps $ hide_default_template_args $ subst_regexps pre_subst_regexps $ repl_withs e
