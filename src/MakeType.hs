{- Todo:

  member functions
  "taking a vector of characters", "taking an optional bool", etc
  "pointer to anything" / "pointer to any object" / "pointer to object(s) of unspecified type" / "taking any object pointer" etc.
  parse C++ type syntax as well, so that "pointer to a void()" produces "void(*)()"
  "function taking and returning int"
-}

module MakeType (makeType, test) where

import Text.ParserCombinators.Parsec
  (CharParser, char, string, try, (<?>), (<|>), eof, optional, sepBy1, pzero, option, noneOf, sepBy, lookAhead, sourceColumn, errorPos, choice, optionMaybe)

import Text.ParserCombinators.Parsec.Language as PSL
import Text.ParserCombinators.Parsec.Token as PST
import qualified Text.ParserCombinators.Parsec.Error as PSE

import qualified Data.List as List
import Data.Char (isSpace)
import Control.Monad.Error ()

import qualified CxxParse

import Control.Monad (liftM, liftM2, when, msum)
import Control.Arrow (first)
import qualified Text.ParserCombinators.Parsec as PS

import Prelude hiding ((.))

import Util ((<<), (>+>), (.), strip)

data CV = CV Bool Bool
data LR = Lvalue | Rvalue

data Type
  = Pointer CV Type
  | Reference LR Type
  | Array (Maybe Integer) Type
  | Function Type [Type]
  | Other CV String

isFunc, isVoid :: Type -> Bool
isFunc (Function _ _) = True
isFunc _ = False
isVoid (Other _ "void") = True
isVoid _ = False

validType :: Type -> Maybe String
validType (Reference _ (Reference _ _)) = Just "cannot have reference to reference"
validType (Array _ (Reference _ _)) = Just "cannot have array of references"
validType (Array _ (Other _ "void")) = Just "cannot have array of void"
validType (Array _ (Array Nothing _)) = Just "cannot have array of arrays of indeterminate size"
validType (Reference _ (Other _ "void")) = Just "cannot have reference to void"
validType (Pointer _ (Reference _ _)) = Just "cannot have pointer to reference"
validType (Array _ (Function _ _)) = Just "cannot have array of functions"
validType (Function r l) | any isFunc (r : l) = Just "cannot have function taking or returning function by value"
validType (Function (Array _ _) _) = Just "cannot have function returning array by value"
validType (Function _ l) | any isVoid l = Just "cannot have function taking a void"
validType (Pointer _ t) = validType t
validType (Reference _ t) = validType t
validType (Array _ t) = validType t
validType (Function r l) = msum (validType r : map validType l)
validType _ = Nothing

-- Stringizers

instance Show CV where
  show (CV c v) = (if c then " const" else "") ++ (if v then " volatile" else "")

ref :: LR -> String
ref Lvalue = "&"
ref Rvalue = "&&"

showType :: Type -> (String, String)
showType (Pointer cv p@(Pointer _ _)) = first (++ " *" ++ show cv) (showType p)
showType (Pointer cv t) = let (x, y) = showType t in
  if null y then (x ++ "*" ++ show cv, "") else (x ++ "(*" ++ show cv, ")" ++ y)
showType (Reference lv p@(Pointer _ _)) = first (++ " " ++ ref lv) (showType p)
showType (Reference lv t) = if null y then (x ++ ref lv, "") else (x ++ "(" ++ ref lv, ")" ++ y)
  where (x, y) = showType t
showType (Other cv s) = (s ++ show cv, "")
showType (Array n t) = let (x, y) = showType t in (x, "[" ++ maybe "" show n ++ "]" ++ y)
showType (Function r a) = let (x, y) = showType r in
  (x, "(" ++ concat (List.intersperse ", " $ map show a) ++ ")" ++ y)

instance Show Type where show = uncurry (++) . showType

-- Parsers

cxxArg :: CharParser st String
cxxArg = do
  (strip . ce >>= \s -> if null s then pzero else return $ unraw s) <?> "raw type"
 where
  ce =
    (show . CxxParse.charLit >+> ce) <|> (show . CxxParse.stringLit >+> ce) <|>
    between '(' ')' <|> between '<' '>' <|> between '[' ']' <|>
    (lookAhead (choice $ try . string . [" and", " to"]) >> return []) <|>
      -- Todo: Use last line only at toplevel, because now things like "array<T, true and false>" are rejected.
    option [] ((:[]) . noneOf ")>],'\"" >+> ce)
  between open close =
    string [open] >+> (concat . List.intersperse "," . sepBy ce (string ",")) >+> string [close] >+> ce
    -- cxxArg can get confused when faced with sneaky uses of tokens like '>'.

unraw :: String -> String
unraw ('l':'o':'n':'g':' ':s) = "long " ++ unraw s
unraw ('s':'h':'o':'r':'t':' ':s) = "signed " ++ unraw s
unraw ('s':'i':'g':'n':'e':'d':' ':s) = "signed " ++ unraw s
unraw ('u':'n':'s':'i':'g':'n':'e':'d':' ':s) = "unsigned " ++ unraw s
unraw "integer" = "int"
unraw "integers" = "int"
unraw "chars" = "char"
unraw "character" = "char"
unraw "characters" = "char"
unraw "floats" = "float"
unraw "doubles" = "double"
unraw "shorts" = "short"
unraw "longs" = "long"
unraw "ints" = "int"
unraw "bools" = "bool"
unraw "boolean" = "bool"
unraw "booleans" = "bool"
unraw s = s

typeP :: Bool -> CharParser st Type
typeP plural = (
    (try cvP >>= \cv -> pointerP cv <|> liftM (Other cv) cxxArg)
  <|> referenceP
  <|> pointerP (CV False False)
  <|> arrayP
  <|> functionP
  <|> predicateP
  <|> (char '(' >> typeP plural << char ')')
  <|> liftM (Other $ CV False False) cxxArg
  ) <?> "type"

constP :: CharParser st ()
constP = (try (string "constant ") <|> string "const ") >> return ()

cvP :: CharParser st CV
cvP =
  try (constP >> liftM (CV True) (try (string "volatile " >> return True) <|> return False))
  <|> (string "volatile " >> liftM (flip CV True) ((try constP >> return True) <|> return False))

lrP :: CharParser st LR
lrP = (try (string "lvalue ") >> return Lvalue)
  <|> (try (string "rvalue ") >> return Rvalue)
  <|> return Lvalue

pointerP :: CV -> CharParser st Type
pointerP cv = liftM (Pointer cv) $ pluralP "pointer" >> ((try (string " to ") >> an) <|> return (nocv "T"))

delim :: CharParser st ()
delim = (char ',' >> optional (char ' ') >> optional (try $ string "and ")) <|> (string " and " >> return ())

pluralP :: String -> CharParser st ()
pluralP s = try (string s) >> optional (char 's') >> return ()

an :: CharParser st Type
an = (<?> "type") $ (try (char 'a' >> optional (char 'n') >> char ' ') >> typeP False) <|> typeP True

-- The plural arguments mean "plural is permitted". We always permit singular, even in cases like "array of 4 int".

nocv :: String -> Type
nocv = Other (CV False False)

referenceP :: CharParser st Type
referenceP = do
  lr <- lrP
  liftM (Reference lr) $ pluralP "reference" >> ((try (string " to ") >> an) <|> return (nocv "T"))

returningP :: Bool -> CharParser st Type
returningP sp = do
  try $ string $ (if sp then " " else "") ++ "returning "
  (try (string "nothing") >> return (nocv "void")) <|> an

takingClause :: CharParser st [Type]
takingClause = (do
  n <- arraySizeP
  if n > 10 then pzero else do
  replicate (fromInteger n) . typeP True)
    <|> (:[]) . an

takingP :: Bool -> CharParser st [Type]
takingP sp = do
  try $ string $ (if sp then " " else "") ++ "taking "
  try (string "nothing" >> return []) <|>
   try (string "no arguments" >> return []) <|>
   concat . sepBy1 takingClause (try $ delim >> ((try (string "returning ") >> pzero) <|> return ()))

functionP :: CharParser st Type
functionP = do
  pluralP "function"
  liftM2 (\x y -> Function y [x]) (try (string " from ") >> an) (string " to " >> an) <|>
   liftM2 Function (returningP True) (option [] (delim >> takingP False)) <|>
   liftM2 (flip Function) (takingP True) (option (nocv "void") (delim >> returningP False)) <|>
   return (Function (nocv "void") [])

predicateP :: CharParser st Type
predicateP =
  pluralP "predicate" >> char ' ' >> liftM (Function $ nocv "bool") (takingP False)

arraySizeP :: CharParser st Integer
arraySizeP = (<?> "array size") $ PST.natural PSL.haskell <|>
  (choice (zipWith (\n s -> try (string (s ++ " ")) >> return n) [1..] ss))
  where ss = words "one two three four five six seven eight nine ten"

arrayP :: CharParser st Type
arrayP = do
  pluralP "array"
  (try (string " of ") >> liftM2 Array (optionMaybe arraySizeP) (typeP True)) <|>
    return (Array Nothing (nocv "T"))

-- Exported interface

makeType :: Monad m => String -> m Type
makeType d = do
  case PS.parse (typeP False << eof) "" d of
    Left e -> fail $ "column " ++ show (sourceColumn $ errorPos e) ++ ": " ++
      (concatMap (++ ". ") $ filter (not . List.all isSpace) $ lines $ PSE.showErrorMessages "or" "unknown parse error" "expected:" "unexpected" "end of type description" $ PSE.errorMessages e)
    Right x -> maybe (return x) fail (validType x)

-- Testing

test :: IO ()
test = do
  t "function taking an int and returning nothing" $ Right "void(int)"
  t "function returning nothing and taking a pointer to a function returning a bool and taking an int" $ Right "void(bool(*)(int))"
  t "function taking a pointer to a (function returning a bool and taking an int), returning nothing" $ Right "void(bool(*)(int))"
  t "reference to a pointer to a function returning nothing, and taking an int" $ Right "void(* &)(int)"
  t "function taking an int, a char, a double, and returning nothing" $ Right "void(int, char, double)"
  t "function returning nothing and taking an int, a char, and a double" $ Right "void(int, char, double)"
  t "function returning a map<T, U>::value_type and taking no arguments" $ Right "map<T, U>::value_type()"
  t "function taking no arguments, returning a reference to an array of three pointers to functions taking integers and returning nothing" $ Right "void(*(&())[3])(int)"
  t "pointer to a constant pointer to an array of 3 ints" $ Right "int(* const *)[3]"
  t "function returning nothing and taking a reference to an array of three pointers to void" $ Right "void(void*(&)[3])"
  t "pointer to reference to void" $ Left "cannot have pointer to reference"
  t "pointer to function returning reference to array of pointers to functions returning arrays of six booleans" $ Left "cannot have function returning array by value"
  t "function taking void and returning void" $ Left "cannot have function taking a void"
  t "function taking two integers, three doubles, and returning a boolean" $ Right "bool(int, int, double, double, double)"
  t "function taking two pointers to integers and a pointer to a predicate taking two integers, and returning nothing" $ Right "void(int*, int*, bool(*)(int, int))"
  t "rvalue reference to function" $ Right "void(&&)()"
  t "pointer to a function from string to int" $ Right "int(*)(string)"
  t "pointer to " $ Left "column 12: unexpected end of type description. expected: type. "
  t "pointer to function taking pointer to function" $ Right "void(*)(void(*)())"
  t "pointer to int and double" $ Left "column 19: unexpected \" \". expected: end of input. " -- Todo: How come we don't get "end of type description" here?
  t "function returning " $ Left "column 20: unexpected end of type description. expected: \"nothing\" or type. "
  t "function taking a pointer and a reference and returning a reference to an array" $ Right "T(&(T*, T&))[]"
  t "array of " $ Left "column 10: unexpected end of type description. expected: array size or type. "
  t "array of seven characters" $ Right "char[7]"
 where
  t :: String -> Either String String -> IO ()
  t d o = let o' = show . makeType d in when (o' /= o) $ fail $ "test failed: " ++ show (d, o, o')

itest :: IO ()
itest = do
  l <- getLine
  print $ PS.parse (typeP False << eof) "" l
