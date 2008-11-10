{-

Notes:

  Whether "(x)(y)" is a cast-expression or a function call depends on whether x is a type-id, which we cannot determine. We choose to simply not support cast-expressions.

Todo:

  - Expression syntax:
      "foo.template bar"
      Pseudo-destructor-names.
      Qualified names.
      New/delete-expressions.
      Lambda expressions.
      All the various kinds of literal, including those new extensible ones.
  - Add an expression pretty printer that uses as few parentheses as possible.
  - Add an expression verbose pretty printer that uses operator+(x, y) syntax.

-}

module CxxParse (Code, Chunk(..), code, charLit, stringLit, map_plain, parseExpr, Expr, test, shortcut_syntaxes, expand, blob, resume) where

import qualified Data.List as List
import qualified EditCommandParseError
import Control.Monad.Fix (fix)
import Control.Monad.Error ()
import qualified Text.ParserCombinators.Parsec as PS
import Text.ParserCombinators.Parsec (try, char, string, noneOf, (<|>), many, anyChar, manyTill, lookAhead, CharParser, many1, pzero, sepBy, (<?>), eof, parse, oneOf)
import Control.Monad (liftM2, guard)
import Data.Function (on)
import Data.Char (isSpace)
import ParsecUtil (spaces)
import Util ((<<), (.), isIdChar, test_cmp, fail_test, all_values)
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

type Code = [Chunk]

map_plain :: (String -> String) -> Chunk -> Chunk
map_plain f (Plain s) = Plain $ f s
map_plain f (Curlies c) = Curlies $ map (map_plain f) c
map_plain f (Parens c) = Parens $ map (map_plain f) c
map_plain f (Squares c) = Squares $ map (map_plain f) c
map_plain _ x = x

instance Show Chunk where
  show (CharLiteral c) = "'" ++ c ++ "'"
  show (StringLiteral s) = "\"" ++ s ++ "\""
  show (Plain s) = s
  show (Parens c) = "(" ++ show c ++ ")"
  show (Curlies c) = "{" ++ show c ++ "}"
  show (Squares c) = "[" ++ show c ++ "]"
  show (MultiComment s) = "/*" ++ s ++ "*/"
  show (SingleComment s) = "//" ++ s
  showList l s = concat (show . l) ++ s

textLit :: Char -> CharParser st String
textLit q = (char q >>) $ fix $ \h -> do
  s <- many $ noneOf [q, '\\']
  c <- anyChar
  if c == '\\'
    then do d <- anyChar; r <- h; return $ s ++ ('\\':d:r)
    else return s

splitSemicolon :: Code -> (Code, Code)
splitSemicolon [] = ([], [])
splitSemicolon (Plain ";" : r) = ([Plain ";"], r)
splitSemicolon (a : r) = (a : x, y) where (x, y) = splitSemicolon r

data ShortCode
  = Long Code
  | Print Code Code -- Does not include the semicolon.
  | Block Code Code -- Does not include Curlies.

expand :: ShortCode -> Code
expand (Long c) = c
expand (Block c c') = c' ++ [Plain "\nint main()", Curlies c]
expand (Print c c') = expand $ Block ([Plain "::std::cout << "] ++ c ++ [Plain "\n;"]) c'

cstyle_comments :: Code -> Code
cstyle_comments = map f
  where f (SingleComment s) = (MultiComment s); f c = c

expand_without_main :: ShortCode -> Code
expand_without_main (Long d) = erase_main d
  where
    erase_main (Plain s : Parens _ : Curlies _ : c) | "main" `List.isInfixOf` s = c
    erase_main (Plain s : Parens _ : Plain t : Curlies _ : c) | "main" `List.isInfixOf` s && all isSpace t = c
    erase_main (x : y) = (x :) $ erase_main y
    erase_main c = c
expand_without_main (Print _ c) = c
expand_without_main (Block _ c) = c

blob :: ShortCode -> Code
blob (Long c) = c
blob (Print c c') = [Plain "<<"] ++ c ++ [Plain ";"] ++ c'
blob (Block c c') = Curlies c : c'

resume :: ShortCode -> ShortCode -> ShortCode
resume old new = case new of
    Long c -> Long $ old' ++ c
    Print c c' -> Print c $ old' ++ c'
    Block c c' -> Block c $ old' ++ c'
  where old' = cstyle_comments $ expand_without_main old

shortcut_syntaxes :: Code -> ShortCode
shortcut_syntaxes (Curlies c : b) = Block c b
shortcut_syntaxes (Plain ('<':'<':x) : y) = uncurry Print $ splitSemicolon $ Plain x : y
shortcut_syntaxes c = Long c

-- Parsec's Haskell char/string literal parsers consume whitespace, and save the value rather than the denotation.

charLit, stringLit, plain, parens, curlies, squares, multiComment, singleComment :: CharParser st Chunk

charLit = CharLiteral . textLit '\''
stringLit = StringLiteral . textLit '"'
plain = Plain . ((:[]) . oneOf ";\\" <|> (many1 (noneOf "'\"{([])}/;\\" <|> (try $ char '/' << lookAhead (noneOf "*/")))))
parens = Parens . (char '(' >> code << char ')')
curlies = Curlies . (char '{' >> code << char '}')
squares = Squares . (char '[' >> code << char ']')
multiComment = MultiComment . (try (string "/*") >> manyTill anyChar (try $ string "*/"))
singleComment = SingleComment . (try (string "//") >> many anyChar)
  -- singleComment is unaware of "// ... \" funkiness.

code :: CharParser st Code
code = many (multiComment <|> singleComment <|> charLit <|> parens  <|> curlies <|> squares <|> stringLit <|> plain)
  -- Uncovers just enough structure for Request.hs to find the split positions in "<< ...; ..." and "{ ... } ..." requests and to implement --resume.

data UnaryOp
  = Dereference | PrefixIncrement | PostfixIncrement | PrefixDecrement | PostfixDecrement | AddressOf | Negate | Positive | Complement
 deriving (Eq, Enum, Bounded)

data BinaryOp
  = Plus | Minus | Multiply | Divide | Modulus | Less | Greater | LessEqual | GreaterEqual | EqualTo | NotEqualTo | LogicalAnd | LogicalOr | BitAnd | BitOr | BitXor | LeftShift | RightShift | PlusAssign | MinusAssign | MultiplyAssign | DivideAssign | ModulusAssign | BitAndAssign | BitOrAssign | BitXorAssign | LeftShiftAssign | RightShiftAssign | Assign | Index | Member | Comma | MemPtr | MemRef | Arrow
 deriving (Eq, Enum, Bounded)

data Expr
  = Identifier String
  | CharLiteralExpr String
  | StringLiteralExpr String
  | BinaryExpr BinaryOp Expr Expr
  | UnaryExpr UnaryOp Expr
  | FunctionCall Expr [Expr]
  | TernaryExpr Expr Expr Expr
 deriving Eq

instance Show UnaryOp where
  show Dereference = "*"
  show AddressOf = "&"
  show Negate = "-"
  show Complement = "~"
  show Positive = "+"
  show PrefixIncrement = "++"; show PostfixIncrement = "++"
  show PrefixDecrement = "--"; show PostfixDecrement = "--"

instance Show BinaryOp where
  show Plus = "+"; show Minus = "-"
  show Multiply = "*"; show Divide = "/"; show Modulus = "%"
  show Less = "<"; show Greater = ">"
  show LessEqual = "<="; show GreaterEqual = ">="
  show EqualTo = "=="; show NotEqualTo = "!="
  show LogicalAnd = "&&"; show LogicalOr = "||"
  show BitAnd = "&"; show BitOr = "|"; show BitXor = "^"
  show LeftShift = "<<"; show RightShift = ">>"
  show PlusAssign = "+="; show MinusAssign = "-="
  show MultiplyAssign = "*="; show DivideAssign = "/="; show ModulusAssign = "%="
  show BitAndAssign = "&="; show BitOrAssign = "|="; show BitXorAssign = "^="
  show LeftShiftAssign = "<<="; show RightShiftAssign = ">>="
  show Assign = "="; show Index = "[]"; show Member = "."; show Comma = ","; show Arrow = "->"
  show MemRef = ".*"; show MemPtr = "->*"

instance Show Expr where
  show e = case e of
    BinaryExpr Comma e' e'' -> f e' ++ ", " ++ f e''
    BinaryExpr Index e' e'' -> f e' ++ "[" ++ show e'' ++ "]"
    BinaryExpr o e' e'' | o `elem` [Member, Arrow, MemRef, MemPtr] -> f e' ++ show o ++ f e''
    BinaryExpr o e' e'' -> f e' ++ " " ++ show o ++ " " ++ f e''
    UnaryExpr o e' | o `elem` [PostfixIncrement, PostfixDecrement] -> f e' ++ show o
    UnaryExpr o e' -> show o ++ f e'
    TernaryExpr e' e'' e''' -> f e' ++ " ? " ++ f e'' ++ " : " ++ f e'''
    FunctionCall e' l -> f e' ++ "(" ++ concat (List.intersperse ", " $ f . l) ++ ")"
    x -> f x
   where
    f :: Expr -> String
    f (Identifier s) = s
    f (CharLiteralExpr s) = '\'' : s ++ "'"
    f (StringLiteralExpr s) = '"' : s ++ "\""
    f x = '(' : show x ++ ")"

binaryGroup :: [BinaryOp] -> CharParser st Expr -> CharParser st Expr
binaryGroup ops r = do
  e <- r
  foldl (\a (o, e') -> BinaryExpr o a e') e . (<?> "binary-operator") (many (liftM2 (,) (try $ do o <- op; case filter ((== o) . show) ops of [] -> pzero; (h:_) -> return h) r))

op :: CharParser st String
op = (<?> "operator") $ (<< spaces) $ PS.choice $ map (try . string) $ List.sortBy (flip compare `on` length) $
  show . (all_values :: [UnaryOp]) ++ show . (all_values :: [BinaryOp]) ++ words "? :"

specific_op :: String -> CharParser st ()
specific_op s = try (op >>= guard . (s ==)) <?> show s

assignmentOperator :: CharParser st BinaryOp
assignmentOperator =
  try (do o <- op; case filter ((== o) . show) [Assign, PlusAssign, MinusAssign, MultiplyAssign, DivideAssign, ModulusAssign, BitAndAssign, BitOrAssign, BitXorAssign, LeftShiftAssign, RightShiftAssign] of [] -> pzero; (h:_) -> return h) <?> "binary-operator"

idExpr, primaryExpr, postfixExpr, unaryExpr, pmExpr, multiplicativeExpr, additiveExpr, shiftExpr, relationalExpr, equalityExpr, andExpr, exclusiveOrExpr, inclusiveOrExpr, logicalAndExpr, logicalOrExpr, assignmentExpr, expr :: CharParser st Expr

idExpr = Identifier . many1 (PS.satisfy isIdChar) << spaces <?> "id-expression"
primaryExpr = (<?> "primary-expression") $ (char '(' >> spaces >> expr << char ')' << spaces) <|> idExpr <|> CharLiteralExpr . (textLit '\'' << spaces) <|> StringLiteralExpr . (textLit '"' << spaces)
postfixExpr = (<?> "postfix-expression") $ liftM2 (foldl $ flip ($)) primaryExpr $ many $ (<?> "postfix-operator") $
  (try (string "++") >> spaces >> return (UnaryExpr PostfixIncrement))
  <|> (try (string "--") >> spaces >> return (UnaryExpr PostfixDecrement))
  <|> (char '[' >> spaces >> flip (BinaryExpr Index) . expr << char ']' << spaces)
  <|> (char '(' >> spaces >> flip FunctionCall . sepBy assignmentExpr ({-dontexpect $-} char ',' >> spaces)  << char ')' << spaces)
  <|> (specific_op "->" >> flip (BinaryExpr Arrow) . idExpr)
  <|> (specific_op "." >> flip (BinaryExpr Member) . idExpr)
unaryExpr = (<?> "unary-expression") $
  liftM2 UnaryExpr
    (PS.choice $ map (\o -> try (string (show o)) >> spaces >> return o) [PrefixIncrement, PrefixDecrement, Negate, Positive, Complement, Dereference])
    unaryExpr
   <|> postfixExpr
pmExpr = binaryGroup [MemRef, MemPtr] unaryExpr <?> "pm-expression"
multiplicativeExpr = binaryGroup [Multiply, Divide, Modulus] pmExpr <?> "multiplicative-expression"
additiveExpr = binaryGroup [Plus, Minus] multiplicativeExpr <?> "additive-expression"
shiftExpr = binaryGroup [LeftShift, RightShift] additiveExpr <?> "shift-expression"
relationalExpr = binaryGroup [Less, Greater, LessEqual, GreaterEqual] shiftExpr <?> "relational-expression"
equalityExpr = binaryGroup [EqualTo, NotEqualTo] relationalExpr <?> "equality-expression"
andExpr = binaryGroup [BitAnd] equalityExpr <?> "and-expression"
exclusiveOrExpr = binaryGroup [BitXor] andExpr <?> "exclusive-or-expression"
inclusiveOrExpr = binaryGroup [BitOr] exclusiveOrExpr <?> "inclusive-or-expression"
logicalAndExpr = binaryGroup [LogicalAnd] inclusiveOrExpr <?> "logical-and-expression"
logicalOrExpr = binaryGroup [LogicalOr] logicalAndExpr <?> "logical-or-expr"
assignmentExpr = (<?> "assignment-expression") $ do
  e <- logicalOrExpr
  liftM2 (TernaryExpr e) ((<?> "ternary-operator") $ specific_op "?" >> expr) (specific_op ":" >> assignmentExpr) <|> liftM2 (flip BinaryExpr e) assignmentOperator assignmentExpr <|> return e
expr = binaryGroup [Comma] assignmentExpr <?> "expression"

parseExpr :: String -> Either String Expr
parseExpr s = case parse (expr << (eof <?> eof_desc)) "" s of
  Left e -> fail $ EditCommandParseError.showParseError "expression" s True e
  Right e -> Right e
 where
  eof_desc = "end of expression"

test :: IO ()
test = do
  s "*i-40?(*i-x?S(1,*i):a)+r(i+1,e,x,a):'('+(i[1]-46?r(i+1,o-1,x,a):'.'+r(i+2,o-1,x+1,a))+')'+r(o,e,x,a)" "((*i) - 40) ? ((((*i) - x) ? (S(1, (*i))) : a) + (r((i + 1), e, x, a))) : ((('(' + (((i[1]) - 46) ? (r((i + 1), (o - 1), x, a)) : ('.' + (r((i + 2), (o - 1), (x + 1), a))))) + ')') + (r(o, e, x, a)))" -- Taken from the lambda calculus snippet.
  s "a=b<c?d=e:f=g" "a = ((b < c) ? (d = e) : (f = g))" -- Example from TC++PL, section 6.2.
  s "x?y?z?a:b:c:d" "x ? (y ? (z ? a : b) : c) : d"
  s "x->x->*x.x.*x" "((x->x)->*(x.x)).*x"
  s "x || x && x | x ^ x & x == x < x << x" "x || (x && (x | (x ^ (x & (x == (x < (x << x)))))))"
  s "x---------x" "((((x--)--)--)--) - x"
  s "x + x *= x + x /= x + x" "(x + x) *= ((x + x) /= (x + x))"
  s "a+++a, b++ +b, c+ ++c, d+++ +d, e+ +++e" "(((((a++) + a), ((b++) + b)), (c + (++c))), ((d++) + (+d))), (e + (++(+e)))"
  s "x += a, b, c" "((x += a), b), c"
  s "x[y + 'z']" "x[y + 'z']" -- No parens around x + z or around 'z'.
  f "x--x" "Unexpected \"x\" after \"x--\". Expected postfix-operator, binary-operator, ternary-operator, or end of expression."
  f "x-" "Unexpected end of expression. Expected multiplicative-expression."
  f "x." "Unexpected end of expression. Expected id-expression."
  f "x( " "Unexpected end of expression. Expected assignment-expression or \")\"."
  f "x(y" "Unexpected end of expression. Expected postfix-operator, binary-operator, ternary-operator, \",\", or \")\"."
  f "x?y" "Unexpected end of expression. Expected postfix-operator, binary-operator, ternary-operator, or \":\"."
  f "x[" "Unexpected end of expression. Expected expression."
  putStrLn "No test failures."
 where
  s :: String -> String -> IO () -- Test for success.
  s i o = case parse (expr << eof) "" i of
    Right o' -> do
      test_cmp i o (show o')
      case parse (expr << eof) "" (show o') of
        Right o'' | o' == o'' -> return ()
        e -> fail_test (show o') o' e
    Left e -> fail_test i o e
  f :: String -> String -> IO () -- Test for failure.
  f t r = case parseExpr t of
    Left e -> test_cmp t r e
    Right y -> fail_test t r y
