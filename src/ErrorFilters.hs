{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances, UndecidableInstances #-}

-- Virtually everything here is GCC specific.

module ErrorFilters (cc1plus, as, ld, prog) where

import qualified Cxx.Parse
import Control.Monad (ap, liftM2, mzero, guard)
import Text.Regex (Regex, matchRegexAll, mkRegex, subRegex)
import Data.Char (toLower)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.List (intersperse, isPrefixOf, isSuffixOf, tails)
import Text.ParserCombinators.Parsec
  (string, sepBy, parse, char, try, getInput, (<|>), satisfy, spaces, manyTill, many1, anyChar, noneOf, option, count, CharParser, notFollowedBy, choice, setInput, eof, oneOf)
import Text.ParserCombinators.Parsec.Prim (GenParser)
import Control.Applicative (Applicative(..))
import Util ((.), (<<), isIdChar, (>+>), strip, replaceInfix, parsep, maybeLast, (!!))
import Prelude hiding (catch, (.), (!!))

subRegex' :: Regex → String → String → String
subRegex' = flip . subRegex

instance Applicative (GenParser Char st) where pure = return; (<*>) = ap

-- Using the following more general instance causes overlapping instance problems elsewhere:
--   instance (Monad m, Functor m) ⇒ Applicative m where pure = return; (<*>) = ap

cc1plus, as, ld, prog :: String → String

cc1plus e = cleanup_stdlib_templates $ replace_withs $ hide_clutter_namespaces
  $ fromMaybe e $ maybeLast $ flip mapMaybe (lines e) $ \l → do
    (_, _, x, _) ← matchRegexAll (mkRegex "(^|\n)[^:]+:([[:digit:]]+:)+ ") l
    guard $ not $ "note:" `isPrefixOf` x
    return x
  -- Even though we use -Wfatal-errors, we may still get several "instantiated from ..." lines. Only the last of these (the one we're interested in) actually says "error"/"warning". We used to have the regex match on that, greatly simplifying the above, but that broke when a language other than English was used.

as e = maybe e (\(_, m:ms, _, _) → toLower m : ms) $ matchRegexAll (mkRegex "\\b(Error|Warning): [^\n]*") e

ld e = maybe e (\(_, m, _, _) → "error: " ++ m) $ matchRegexAll (mkRegex "\\bundefined reference to [^\n]*") e

prog = replaceInfix "E7tKRJpMcGq574LY:" [parsep] . cleanup_stdlib_templates . replace_withs . hide_clutter_namespaces
  -- We also clean up successful output, because it might include dirty assertion failures and {E}TYPE strings. The "E7tKRJpMcGq574LY:" is for libstdc++ debug mode errors; see prelude.hpp.

cxxArg :: CharParser st String
cxxArg = strip . ce
 where
  ce =
    ((show . Cxx.Parse.charLit) >+> ce) <|> ((show . Cxx.Parse.stringLit) >+> ce) <|>
    between '(' ')' <|> between '<' '>' <|> between '[' ']' <|>
    option [] (((:[]) . noneOf ")>],'\"") >+> ce)
  between open close =
    string [open] >+> (concat . intersperse "," . sepBy ce (string ",")) >+> string [close] >+> ce
    -- cxxArg can get confused when faced with sneaky uses of tokens like '>'.

-- Todo: Use new C++ parser instead of cxxArg.

hide_clutter_namespaces :: String → String
hide_clutter_namespaces = subRegex' (mkRegex "(\\b|:: *)(std|boost|__(gnu_)?(debug(_def)?|norm|cxx))::") ""

replace_withs :: String → String
replace_withs s = either (const s) replace_withs $ parse (r >+> getInput) "" s
 where
  r :: CharParser st String
  r = do
    before ← anyChar `manyTill` try (string " [with ")
    k ← many1 (satisfy isIdChar)
    string " = "
    v ← cxxArg
    if "&" `isSuffixOf` v then mzero else do
      -- Replacing T in "T&" or "T&&" or even "const T&" with a reference type is too involved for now (see 14.3.1p4).
    c ← oneOf "],"
    let before' = subRegex (mkRegex $ "\\b" ++ k ++ "\\b") before (replaceInfix "\\" "\\\\" v)
    if before' == before then mzero else return $ before' ++ (if c == ']' then "" else " [with")

class Parser p st a | p → st, p → a where parser :: p → CharParser st a
instance Parser (CharParser st a) st a where parser = id
instance Parser String st String where parser = string
instance Parser [String] st String where parser = choice . ((try . string) .)
instance Parser Char st Char where parser = char

count_occs :: Eq a ⇒ [a] → [a] → Int
count_occs x = length . filter (isPrefixOf x) . tails

cleanup_stdlib_templates :: String → String
cleanup_stdlib_templates = either (const "cleanup_stdlib_templates parse failure") id .
  parse (recursive_replacer $ choice cleaners) ""
 where
  cleaners :: [CharParser st String]
  cleaners = try .
    [ string "basic_" >> parser ioBasics <$ '<' <$ "char" <$ '>'
    , string "basic_" >> (('w':) . parser ioBasics) <$ '<' <$ "wchar_t" <$ '>'
    , (\[e] → "list<" ++ e ++ ">::iterator") . tmpi "_List_iterator" 1
    , (\[e] → "list<" ++ e ++ ">::const_iterator") . tmpi "_List_const_iterator" 1
    , (++ "::iterator") . snd . tmpl "_Safe_iterator" (tmpi "_Rb_tree_iterator" 1 `comma` cxxArg)
    , (++ "::const_iterator") . snd . tmpl "_Safe_iterator" (tmpi "_Rb_tree_const_iterator" 1 `comma` cxxArg)
        -- Last two for (multi)set/(multi)map.
    , head . tmpi "_Safe_iterator" 2 -- For vector/deque.
    , (\[x, y] → y ++ "::" ++ (if count_occs "const" x > (count_occs "const" y `div` 2) then "const_" else "") ++ "iterator") . tmpi "__normal_iterator" 2
        -- Last one for vector/string.
    , (\[e,d,_] → "deque<" ++ e ++ ">::" ++ (if count_occs "const" d > count_occs "const" e then "const_" else "") ++ "iterator") . tmpi "_Deque_iterator" 3
    , tmpi "allocator" 1 >> "::" $> ((\[e] → "allocator<" ++ e ++ ">") . tmpi "rebind" 1) <$ "::" <$ "other" << noid
    , ((++ "&") . head . tmpi "allocator" 1) <$ "::" <$ "reference" << noid
    , ((++ " const &") . head . tmpi "allocator" 1) <$ "::" <$ "const_reference" << noid
    , ((++ "*") . head . tmpi "allocator" 1) <$ "::" <$ "pointer" << noid
        -- Last three only work properly for simple cases, and fail miserably for something like "allocator<void(*)()>::pointer".
    , tmpi "allocator" 1 >> "::" $> "size_type" >> noid >> return "size_t"
    , string "typename " >> return ""
        -- Shows up in assertion failures after replacements have been performed.
    , defaulter ["list", "deque", "vector"] 1 $ tmpl "allocator"
    , defaulter ["set", "multiset", "basic_stringstream", "basic_stringbuf", "basic_string", "basic_ostringstream", "basic_istringstream"] 2 $ tmpl "allocator" . head
    , defaulter ["map", "multimap"] 3 $ \[k, v, _] → tmpl "allocator" (tmpl "pair" (try (("const " $> k) <|> (k $> "const")) `comma` v))
    , defaulter ["set", "multiset"] 1 $ tmpl "less"
    , defaulter ["priority_queue"] 1 $ \[e] → tmpl "vector" (e `comma` tmpl "allocator" e)
    , defaulter ["queue", "stack"] 1 $ \[e] → tmpl "deque" (e `comma` tmpl "allocator" e)
    , defaulter ["map", "multimap", "priority_queue"] 2 $ tmpl "less" . head
    , defaulter ((("basic_" ++) . ioBasics) ++ ["ostreambuf_iterator", "istreambuf_iterator"]) 1 $ tmpl "char_traits"
    , defaulter ["istream_iterator"] 3 $ const $ option [] (string "long") $> "int"
        -- "int"/"long int" is what is printed for ptrdiff_t.
    , defaulter ["istream_iterator", "ostream_iterator"] 2 $ tmpl "char_traits" . (!!1)
    , defaulter ["istream_iterator", "ostream_iterator"] 1 $ const "char"
    ] -- Together, these must be strongly normalizing.

  -- Things that go wrong but are hard to fix:
  --   set<T>::iterator displayed as const version. Same for multiset.

  defaulter :: Parser p st a ⇒ [String] → Int → ([String] → p) → CharParser st String
  defaulter names idx def =
    names $>> "<" $>> (count idx (cxxArg <$ char ',' << spaces) >>= \prec → parser (def prec) >> return (concat $ intersperse ", " prec)) $>> ">"
      where x $>> y = (parser x << spaces) >+> parser y
        -- Hides default template arguments.

  noid = notFollowedBy (satisfy isIdChar)

  a <$ b = parser a << spaces << parser b
  a $> b = parser a >> spaces >> parser b

  comma :: (Parser p st a, Parser q st b) ⇒ p → q → CharParser st (a, b)
  comma x y = liftM2 (,) (x <$ char ',' << spaces) (parser y)

  tmpl n p = n $> '<' $> p <$ '>'

  tmpi :: String → Int → CharParser st [String]
  tmpi n i = tmpl n $ (:) . cxxArg <*> count (i - 1) (spaces >> ',' $> cxxArg)

  ioBasics = ["streambuf", "ofstream", "ifstream", "fstream", "filebuf", "stringbuf", "ostream", "istream", "ostringstream", "istringstream", "stringstream", "iostream", "ios", "string"]

  recursive_replacer :: CharParser st String → CharParser st String
  recursive_replacer r = ((r >+> getInput) >>= setInput >> recursive_replacer r) <|> scan
    where scan = (eof >> return "") <|> (anyChar >>= \c → (c:) . if isIdChar c then scan else recursive_replacer r)
