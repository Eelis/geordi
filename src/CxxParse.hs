
-- Extremely superficial C++ "parser". It parses just enough for Request.hs to be able to use it to find the split positions in "<< ...; ..." and "{ ... } ..." requests.

module CxxParse (Code(..), Chunk(..), code, charLit, stringLit, map_plain, map_chunks) where

import Control.Monad.Fix
import Text.ParserCombinators.Parsec (try, char, string, option, noneOf, (<|>), many, anyChar, manyTill, lookAhead, CharParser, many1)
import Util
import Prelude hiding ((.))

data Chunk
  = CharLiteral String
  | StringLiteral String
  | Plain String
  | SingleComment String
  | MultiComment String
  | Curlies Code
  | Parens Code
  | Squares Code

newtype Code = Code [Chunk]

map_chunks :: (Chunk -> Chunk) -> Code -> Code
map_chunks f (Code c) = Code (map f c)

map_plain :: (String -> String) -> Chunk -> Chunk
map_plain _ c@(CharLiteral _) = c
map_plain _ s@(StringLiteral _) = s
map_plain f (Plain s) = Plain $ f s
map_plain _ c@(SingleComment _) = c
map_plain _ m@(MultiComment _) = m
map_plain f (Curlies c) = Curlies $ map_chunks (map_plain f) c
map_plain f (Parens c) = Parens $ map_chunks (map_plain f) c
map_plain f (Squares c) = Squares $ map_chunks (map_plain f) c
  -- Todo: Perhaps some of this boilerplate can be scrapped.

instance Show Code where show (Code l) = concat $ show . l

instance Show Chunk where
  show (CharLiteral c) = "'" ++ c ++ "'"
  show (StringLiteral s) = "\"" ++ s ++ "\""
  show (Plain s) = s
  show (Parens c) = "(" ++ show c ++ ")"
  show (Curlies c) = "{" ++ show c ++ "}"
  show (Squares c) = "[" ++ show c ++ "]"
  show (MultiComment s) = "/*" ++ s ++ "*/"
  show (SingleComment s) = "//" ++ s

textLit :: Char -> CharParser st String
textLit q = (char q >>) $ fix $ \h -> do
  s <- many $ noneOf [q, '\\']
  c <- anyChar
  if c == '\\'
    then do d <- anyChar; r <- h; return $ s ++ ('\\':d:r)
    else return s

-- Parsec's Haskell char/string literal parsers consume whitespace, and save the value rather than the denotation.

charLit, stringLit, plain, parens, curlies, squares, multiComment, singleComment :: CharParser st Chunk

charLit = CharLiteral . textLit '\''
stringLit = StringLiteral . textLit '"'
plain = Plain . (string ";" <|> (many1 (noneOf "'\"{([])}/;" <|> (try $ char '/' << lookAhead (noneOf "*/"))) >+> option "" (string ";")))
parens = Parens . (char '(' >> code << char ')')
curlies = Curlies . (char '{' >> code << char '}')
squares = Squares . (char '[' >> code << char ']')
multiComment = MultiComment . (try (string "/*") >> manyTill anyChar (try $ string "*/"))
singleComment = SingleComment . (try (string "//") >> many anyChar)
  -- singleComment is unaware of "// ... \" funkiness.

code :: CharParser st Code
code = Code . many (multiComment <|> singleComment <|> plain <|> charLit <|> parens  <|> curlies <|> squares <|> stringLit)
