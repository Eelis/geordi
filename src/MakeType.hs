{- Todo:

  "taking a vector of characters", "taking an optional bool", etc
  "pointer to anything" / "pointer to any object" / "pointer to object(s) of unspecified type" / "taking any object pointer" etc.
  new "->" syntax
  "function taking and returning int"
  "int(void)"
  textual description of member pointers
  variadic functions
-}

module MakeType (makeType, test) where

import qualified Text.ParserCombinators.Parsec as PS
import qualified Text.ParserCombinators.Parsec.Language as PSL
import qualified Text.ParserCombinators.Parsec.Token as PST
import qualified Text.ParserCombinators.Parsec.Error as PSE
import qualified Data.List as List

import Data.Char (isSpace, isAlphaNum)
import Control.Monad.Error ()
import Control.Monad (liftM, liftM2, when)
import Control.Arrow (first, second)
import Data.Monoid (Monoid(..))
import Text.ParserCombinators.Parsec
  (CharParser, char, string, try, (<?>), (<|>), eof, optional, sepBy1, pzero, option, sepBy, sourceColumn, errorPos, choice, optionMaybe, many1, satisfy, spaces, oneOf, notFollowedBy)

import Prelude hiding ((.))
import Util ((<<), (.), prefixError)

data CV = CV Bool Bool deriving Eq

instance Monoid CV where
  mempty = CV False False
  mappend (CV c v) (CV c' v') = CV (c || c') (v || v')

data LR = Lvalue | Rvalue deriving Eq

data UnconstrainedType -- Pretty much an AST. Produced by the parsers.
  = UnconstrainedPointer UnconstrainedType
  | UnconstrainedMemPointer String UnconstrainedType
  | UnconstrainedReference LR UnconstrainedType
  | UnconstrainedArray (Maybe Integer) UnconstrainedType
  | UnconstrainedFunction UnconstrainedType Bool [UnconstrainedType]
  | UnconstrainedAtomicType String
  | UnconstrainedCVQ CV UnconstrainedType

data Type -- Structurally enforces various constraints on C++ types.
  = T_Pointer Pointer
  | T_Reference Reference
  | T_DetArray DetArray
  | T_IndetArray IndetArray
  | T_Function Function
  | T_Atomic Atomic
  deriving Eq

data Pointer = Pointer CV Referee | MemPointer CV String MemPointee deriving Eq
data Reference = Reference LR Referee deriving Eq
data IndetArray = IndetArray ArrayElem deriving Eq
data DetArray = DetArray Integer ArrayElem deriving Eq
data Function = Function { funcRet :: FuncRet, funcVariadic :: Bool, funcArgs :: [FuncArg] } deriving Eq
  -- Todo: Stronger: a variadic function has at least one parameter (I think).
data Atomic = Atomic CV String deriving Eq

data Referee
  = Referee_Pointer Pointer
  | Referee_DetArray DetArray
  | Referee_IndetArray IndetArray
  | Referee_Function Function
  | Referee_Atomic Atomic deriving Eq

data MemPointee
  = MemPointee_Pointer Pointer
  | MemPointee_DetArray DetArray
  | MemPointee_IndetArray IndetArray
  | MemPointee_Function CV Function
  | MemPointee_Atomic Atomic
  deriving Eq

data ArrayElem
  = ArrayElem_Pointer Pointer
  | ArrayElem_DetArray DetArray
  | ArrayElem_Atomic Atomic
  deriving Eq

data FuncRet
  = FuncRet_Pointer Pointer
  | FuncRet_Reference Reference
  | FuncRet_Atomic Atomic
  deriving Eq

data FuncArg
  = FuncArg_Pointer Pointer
  | FuncArg_Reference Reference
  | FuncArg_Atomic Atomic
  deriving Eq

arrayElem_to_pointee :: ArrayElem -> Referee
arrayElem_to_pointee (ArrayElem_Pointer p) = Referee_Pointer p
arrayElem_to_pointee (ArrayElem_Atomic o) = Referee_Atomic o
arrayElem_to_pointee (ArrayElem_DetArray a) = Referee_DetArray a

applyCVarrayElem :: CV -> ArrayElem -> ArrayElem
applyCVarrayElem cv (ArrayElem_Pointer (Pointer cv' t)) = ArrayElem_Pointer $ Pointer (cv `mappend` cv') t
applyCVarrayElem cv (ArrayElem_Pointer (MemPointer cv' s t)) = ArrayElem_Pointer $ MemPointer (cv `mappend` cv') s t
applyCVarrayElem cv (ArrayElem_DetArray (DetArray n e)) = ArrayElem_DetArray $ DetArray n (applyCVarrayElem cv e)
applyCVarrayElem cv (ArrayElem_Atomic (Atomic cv' s)) = ArrayElem_Atomic $ Atomic (cv `mappend` cv') s

applyCV :: CV -> Type -> Either String Type
applyCV (CV False False) t = Right t
applyCV cv (T_Atomic (Atomic cv' t)) = return $ T_Atomic $ Atomic (cv `mappend` cv') t
applyCV _ (T_Function _) = fail "cannot cv-qualify function"
applyCV _ (T_Reference _) = fail "cannot cv-qualify reference"
applyCV cv (T_Pointer (Pointer cv' t)) = return $ T_Pointer $ Pointer (cv `mappend` cv') t
applyCV cv (T_Pointer (MemPointer cv' s t)) = return $ T_Pointer $ MemPointer (cv `mappend` cv') s t
applyCV cv (T_DetArray (DetArray n t)) =  return $ T_DetArray $ DetArray n (applyCVarrayElem cv t)
applyCV cv (T_IndetArray (IndetArray t)) =  return $ T_IndetArray $ IndetArray (applyCVarrayElem cv t)

checkConstraints :: UnconstrainedType -> Either String Type
checkConstraints (UnconstrainedCVQ cv ut) = checkConstraints ut >>= applyCV cv
checkConstraints (UnconstrainedMemPointer s (UnconstrainedCVQ cv ut@(UnconstrainedFunction _ _ _))) = checkConstraints ut >>= \t -> T_Pointer . MemPointer mempty s . case t of
  T_Function g -> return $ MemPointee_Function cv g
  _ -> error "impossible"
checkConstraints (UnconstrainedMemPointer s ut) =
  checkConstraints ut >>= \t -> T_Pointer . MemPointer mempty s . case t of
    T_Reference _ -> fail "cannot have pointer to reference"
    T_Pointer p -> return $ MemPointee_Pointer p
    T_Atomic o -> return $ MemPointee_Atomic o
    T_Function f -> return $ MemPointee_Function mempty f
    T_DetArray a -> return $ MemPointee_DetArray a
    T_IndetArray a -> return $ MemPointee_IndetArray a
checkConstraints (UnconstrainedPointer ut) =
  checkConstraints ut >>= \t -> T_Pointer . Pointer mempty . case t of
    T_Reference _ -> fail "cannot have pointer to reference"
    T_Pointer p -> return $ Referee_Pointer p
    T_Atomic o -> return $ Referee_Atomic o
    T_Function f -> return $ Referee_Function f
    T_DetArray a -> return $ Referee_DetArray a
    T_IndetArray a -> return $ Referee_IndetArray a
checkConstraints (UnconstrainedReference lr ut) =
  checkConstraints ut >>= \t -> prefixError "cannot have reference to " $ T_Reference . Reference lr . case t of
    T_Reference _ -> fail "reference"
    T_Atomic (Atomic _ "void") -> fail "void"
    T_Pointer p -> return $ Referee_Pointer p
    T_Atomic o -> return $ Referee_Atomic o
    T_Function f -> return $ Referee_Function f
    T_DetArray a -> return $ Referee_DetArray a
    T_IndetArray a -> return $ Referee_IndetArray a
checkConstraints (UnconstrainedArray ms ut) =
  checkConstraints ut >>= \t -> prefixError "cannot have array of " $ (case ms of Nothing -> T_IndetArray . IndetArray; Just i -> T_DetArray . DetArray i) . case t of
    T_Reference _ -> fail "references"
    T_Function _ -> fail "functions"
    T_IndetArray _ -> fail "arrays of indeterminate size"
    T_Atomic (Atomic _ "void") -> fail "void"
    T_Pointer p -> return $ ArrayElem_Pointer p
    T_DetArray a -> return $ ArrayElem_DetArray a
    T_Atomic o -> return $ ArrayElem_Atomic o
checkConstraints (UnconstrainedAtomicType s) = return $ T_Atomic (Atomic (CV False False) s)
checkConstraints (UnconstrainedFunction r v args) =
   T_Function . liftM2 (\x y -> Function x v y) checkRet (mapM checkArg args)
  where
    checkRet :: Either String FuncRet
    checkRet = checkConstraints r >>= \vt -> prefixError "cannot have function returning " $ case vt of
      T_Function _ -> fail "function"
      T_DetArray _ -> fail "array"
      T_IndetArray _ -> fail "array"
      T_Reference x -> return $ FuncRet_Reference x
      T_Pointer p -> return $ FuncRet_Pointer p
      T_Atomic o -> return $ FuncRet_Atomic o
    checkArg :: UnconstrainedType -> Either String FuncArg
    checkArg t = checkConstraints t >>= \vt -> prefixError "cannot have function taking " $ case vt of
      T_Function _ -> fail "function"
      T_Atomic (Atomic _ "void") -> fail "void"
      T_Pointer p -> return $ FuncArg_Pointer p
      T_Reference x -> return $ FuncArg_Reference x
      T_DetArray (DetArray _ x) -> return $ FuncArg_Pointer $ Pointer (CV False False) (arrayElem_to_pointee x)
      T_IndetArray (IndetArray x) -> return $ FuncArg_Pointer $ Pointer (CV False False) (arrayElem_to_pointee x)
      T_Atomic o -> return $ FuncArg_Atomic o

-- Pretty printers

instance Show CV where
  show (CV c v) = (if c then " const" else "") ++ (if v then " volatile" else "")

class NestShow t where nestShow :: t -> (String, String)

instance Show FuncArg where show = uncurry (++) . nestShow
instance Show Type where show = uncurry (++) . nestShow

instance NestShow Referee where
  nestShow (Referee_Pointer p) = nestShow p
  nestShow (Referee_Function f) = nestShow f
  nestShow (Referee_Atomic o) = nestShow o
  nestShow (Referee_DetArray a) = nestShow a
  nestShow (Referee_IndetArray a) = nestShow a

instance NestShow MemPointee where
  nestShow (MemPointee_Pointer p) = nestShow p
  nestShow (MemPointee_Function cv (Function r v a)) =
    second (("(" ++ concat (List.intersperse ", " $ map show a) ++ (if v then ", ..." else "") ++ ")" ++ show cv) ++) (nestShow r)
  nestShow (MemPointee_DetArray a) = nestShow a
  nestShow (MemPointee_IndetArray a) = nestShow a
  nestShow (MemPointee_Atomic o) = nestShow o

instance NestShow Atomic where nestShow (Atomic cv s) = (s ++ show cv, "")

instance NestShow ArrayElem where
  nestShow (ArrayElem_Pointer p) = nestShow p
  nestShow (ArrayElem_Atomic o) = nestShow o
  nestShow (ArrayElem_DetArray a) = nestShow a

instance NestShow FuncRet where
  nestShow (FuncRet_Pointer t) = nestShow t
  nestShow (FuncRet_Reference t) = nestShow t
  nestShow (FuncRet_Atomic t) = nestShow t

instance NestShow DetArray where
  nestShow (DetArray n t) = second (("[" ++ show n ++ "]") ++) (nestShow t)

instance NestShow IndetArray where
  nestShow (IndetArray t) = second ("[]" ++) (nestShow t)

instance NestShow FuncArg where
  nestShow (FuncArg_Pointer t) = nestShow t
  nestShow (FuncArg_Reference t) = nestShow t
  nestShow (FuncArg_Atomic t) = nestShow t

instance NestShow Function where
  nestShow (Function r v a) =
    second (("(" ++ concat (List.intersperse ", " $ map show a) ++ (if v then ", ..." else "") ++ ")") ++) (nestShow r)

instance NestShow Pointer where
  nestShow (Pointer cv p@(Referee_Pointer _)) =
    first (++ " *" ++ show cv) (nestShow p)
  nestShow (Pointer cv t) = let (x, y) = nestShow t in
    if null y then (x ++ "*" ++ show cv, "") else (x ++ "(*" ++ show cv, ")" ++ y)
  nestShow (MemPointer cv s p@(MemPointee_Pointer _)) =
    first (++ " " ++ s ++ "*" ++ show cv) (nestShow p)
  nestShow (MemPointer cv s t) = let (x, y) = nestShow t in
    if null y then (x ++ " " ++ s ++ "*" ++ show cv, "") else (x ++ "(" ++ s ++ "*" ++ show cv, ")" ++ y)

ref :: LR -> String
ref Lvalue = "&"
ref Rvalue = "&&"

instance NestShow Reference where
  nestShow (Reference lr p@(Referee_Pointer _)) = first (++ " " ++ ref lr) (nestShow p)
  nestShow (Reference lr t) = let (x, y) = nestShow t in
    if null y then (x ++ ref lr, "") else (x ++ "(" ++ ref lr, ")" ++ y)

instance NestShow Type where
  nestShow (T_Pointer p) = nestShow p
  nestShow (T_Reference r) = nestShow r
  nestShow (T_Function t) = nestShow t
  nestShow (T_DetArray t) = nestShow t
  nestShow (T_IndetArray t) = nestShow t
  nestShow (T_Atomic t) = nestShow t

-- Parsers

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

class_name_p :: CharParser st String
class_name_p = many1 (satisfy isAlphaNum) << spaces

typeP :: CharParser st UnconstrainedType
typeP = liftM2 (flip ($)) (referenceP <|> pointerP <|> arrayP <|> liftM2 UnconstrainedCVQ cv_qualifier_p typeP <|> functionP <|> predicateP <|> (op "(" >> typeP << op ")") <|> UnconstrainedAtomicType . simple_type_specifier_p) opt_type_adornment_p <?> "type"

kwd :: String -> CharParser st ()
kwd s = try (string s >> notFollowedBy (satisfy isAlphaNum)) >> spaces

constP :: CharParser st ()
constP = kwd "constant" <|> kwd "const"

lrP :: CharParser st LR
lrP = (kwd "rvalue" >> return Rvalue) <|> (optional (kwd "lvalue") >> return Lvalue)

pointerP :: CharParser st UnconstrainedType
pointerP = liftM UnconstrainedPointer $ pluralP "pointer" >> ((kwd "to" >> an) <|> return (nocv "T"))

delim :: CharParser st ()
delim = (op "," >> optional (kwd "and")) <|> kwd "and"

pluralP :: String -> CharParser st ()
pluralP s = kwd s <|> kwd (s ++ "s")

an :: CharParser st UnconstrainedType
an = (optional (kwd "an" <|> kwd "a") >> typeP) <?> "type"

nocv :: String -> UnconstrainedType
nocv = UnconstrainedAtomicType

referenceP :: CharParser st UnconstrainedType
referenceP = do
  lr <- lrP
  liftM (UnconstrainedReference lr) $ pluralP "reference" >> ((kwd "to" >> an) <|> return (nocv "T"))

returningP :: CharParser st UnconstrainedType
returningP = kwd "returning" >> ((kwd "nothing" >> return (nocv "void")) <|> an)

takingClause :: CharParser st [UnconstrainedType]
takingClause = (do
  n <- arraySizeP
  if n > 10 then pzero else do
  replicate (fromInteger n) . typeP)
    <|> (:[]) . an

takingP :: CharParser st [UnconstrainedType]
takingP = do
  kwd "taking"
  (kwd "nothing" >> return []) <|>
   (kwd "no arguments" >> return []) <|>
   concat . sepBy1 takingClause (try $ delim >> ((kwd "returning" >> pzero) <|> return ()))

functionP :: CharParser st UnconstrainedType
functionP = do
  pluralP "function"
  liftM2 (\x y -> UnconstrainedFunction y False [x]) (kwd "from" >> an) (kwd "to" >> an) <|>
   liftM2 (\x y -> UnconstrainedFunction x False y) returningP (option [] (delim >> takingP)) <|>
   liftM2 (\x y -> UnconstrainedFunction y False x) takingP (option (nocv "void") (delim >> returningP)) <|>
   return (UnconstrainedFunction (nocv "void") False [])

predicateP :: CharParser st UnconstrainedType
predicateP = pluralP "predicate" >> liftM (UnconstrainedFunction (nocv "bool") False) takingP

arraySizeP :: CharParser st Integer
arraySizeP = (<?> "array size") $ PST.natural PSL.haskell <|>
  (choice (zipWith (\n s -> kwd s >> return n) [1..] ss))
  where ss = words "one two three four five six seven eight nine ten"

arrayP :: CharParser st UnconstrainedType
arrayP = do
  pluralP "array"
  (kwd "of" >> liftM2 UnconstrainedArray (optionMaybe arraySizeP) typeP) <|> return (UnconstrainedArray Nothing (nocv "T"))

type_name_p :: CharParser st String
type_name_p = class_name_p

simple_type_specifier_p :: CharParser st String
simple_type_specifier_p = try $ do
  x <- option "" (op "::" >> return "::")
  y <- option "" nested_name_specifier_p
  z <- type_name_p
  return $ unraw $ x ++ y ++ z

nested_name_specifier_p :: CharParser st String
nested_name_specifier_p = try $ do
  s <- many1 $ satisfy isAlphaNum
  op "::"
  t <- option "" nested_name_specifier_p
  return (s ++ "::" ++ t)

cv_qualifier_p :: CharParser st CV
cv_qualifier_p = (constP >> return (CV True False)) <|> (kwd "volatile" >> return (CV False True))
  <?> "cv-qualifier"

cv_qualifier_seq_p :: CharParser st CV
cv_qualifier_seq_p = mconcat . many1 cv_qualifier_p

cv_qualifier_seq_opt_p :: CharParser st CV
cv_qualifier_seq_opt_p = option mempty cv_qualifier_seq_p

op :: String -> CharParser st ()
op "&&" = try (string "&&" >> notFollowedBy (char '=')) >> spaces
op "&" = try (char '&' >> notFollowedBy (oneOf "&=")) >> spaces
op "*" = try (char '*' >> notFollowedBy (char '=')) >> spaces
op [c] | c `elem` "[]()," = char c >> spaces
op "::" = try (string "::") >> spaces
op s = fail $ "internal error: no such operator: " ++ s

ref_qualifier_p :: CharParser st LR
ref_qualifier_p = (op "&&" >> return Rvalue) <|> (op "&" >> return Lvalue)

ptr_operator_p :: CharParser st (UnconstrainedType -> UnconstrainedType)
ptr_operator_p = (op "*" >> cv_qualifier_seq_opt_p >>= \s -> return $ UnconstrainedCVQ s . UnconstrainedPointer)
  <|> UnconstrainedReference . ref_qualifier_p
  <|> UnconstrainedMemPointer . liftM2 (++) (option "" $ op "::" >> return "::") (nested_name_specifier_p << op "*")
  <?> "ptr-operator"

opt_type_adornment_p :: CharParser st (UnconstrainedType -> UnconstrainedType)
opt_type_adornment_p = option id type_adornment_p

type_adornment_p :: CharParser st (UnconstrainedType -> UnconstrainedType)
type_adornment_p =
    liftM2 (flip (.)) (
      ptr_operator_p <|>
      UnconstrainedCVQ . cv_qualifier_seq_p <|>
      UnconstrainedArray . (op "[" >> optionMaybe arraySizeP << op "]")
    ) opt_type_adornment_p
  <|> do
    op "("
    try (liftM2 (.) (type_adornment_p << op ")") opt_type_adornment_p) <|> do
    args <- typeP `sepBy` op ","
    op ")"
    cv <- cv_qualifier_seq_opt_p
    return (\x -> UnconstrainedCVQ cv $ UnconstrainedFunction x False args)

-- Exported interface

makeType :: String -> Either String Type
makeType d = do
  case PS.parse (typeP << eotd) "" d of
    Left e -> fail $ "column " ++ show (sourceColumn $ errorPos e) ++ ": " ++
      (concatMap (++ ". ") $ filter (not . List.all isSpace) $ lines $ PSE.showErrorMessages "or" "unknown parse error" "expected:" "unexpected" eof_desc $ filter (not . isSpaceExpectation) $ PSE.errorMessages e)
    Right x -> checkConstraints x
 where
  eof_desc = "end of type description"
  eotd = eof <?> eof_desc
  isSpaceExpectation (PSE.Expect "space") = True
  isSpaceExpectation _ = False

-- Testing

test :: IO ()
test = do
  t "function taking an int and returning nothing" $ Right "void(int)"
  t "int const" $ Right "int const"
  t "void(int (* const (** (* const volatile (X::* volatile* (* )(int) )(char*)) [2])(long) ) [3])" $ Right "void(int(* const(* *(* const volatile(X::* volatile *(*)(int))(char*))[2])(long))[3])"
  t "function returning nothing and taking a pointer to a function returning a bool and taking an int" $ Right "void(bool(*)(int))"
  t "function taking a pointer to a (function returning a bool and taking an int), returning nothing" $ Right "void(bool(*)(int))"
  t "reference to a pointer to a function returning nothing, and taking an int" $ Right "void(* &)(int)"
  t "function taking an int, a char, a double, and returning nothing" $ Right "void(int, char, double)"
  t "function returning nothing and taking an int, a char, and a double" $ Right "void(int, char, double)"
--  t "function returning a map<T, U>::value_type and taking no arguments" $ Right "map<T, U>::value_type()"
  t "function taking no arguments, returning a reference to an array of three pointers to functions taking integers and returning nothing" $ Right "void(*(&())[3])(int)"
  t "pointer to a constant volatile pointer to an array of 3 ints" $ Right "int(* const volatile *)[3]"
  t "function returning nothing and taking a reference to an array of three pointers to void" $ Right "void(void*(&)[3])"
  t "pointer to reference to int" $ Left "cannot have pointer to reference"
  t "pointer to constant function" $ Left "cannot cv-qualify function"
  t "pointer to function returning reference to array of pointers to functions returning arrays of six booleans" $ Left "cannot have function returning array"
  t "function taking void and returning void" $ Left "cannot have function taking void"
  t "reference to pointer to const array" $ Right "T const(* &)[]"
  t "function taking two integers, three doubles, and returning a boolean" $ Right "bool(int, int, double, double, double)"
  t "function taking two pointers to integers and a pointer to a predicate taking two integers, and returning nothing" $ Right "void(int*, int*, bool(*)(int, int))"
  t "rvalue reference to function" $ Right "void(&&)()"
  t "constant constant constant pointer to volatile volatile volatile int" $ Right "int volatile* const"
  t "pointer to a function from string to int" $ Right "int(*)(string)"
  t "pointer to " $ Left "column 12: unexpected end of type description. expected: type. "
  t "pointer to function taking pointer to function" $ Right "void(*)(void(*)())"
  t "pointer to int and double" $ Left "column 16: unexpected \"a\". expected: ptr-operator, cv-qualifier, \"[\", \"(\" or end of type description. "
  t "function returning " $ Left "column 20: unexpected end of type description. expected: \"nothing\" or type. "
  t "function taking a pointer and a reference and returning a reference to a constant array" $ Right "T const(&(T*, T&))[]"
  t "array of " $ Left "column 10: unexpected end of type description. expected: array size or type. "
  t "array of seven characters" $ Right "char[7]"
  t "function taking a pointer to void() and returning a reference to an int[3]" $ Right "int(&(void(*)()))[3]"
  t "void(pointer to function)" $ Right "void(void(*)())"
  t "function*(const int)" $ Right "void(*(int const))()"
  t "(function taking an int[3] )*" $ Right "void(*)(int*)"
  t "(function*[3])&()" $ Right "void(*(&())[3])()"
  t "int(::T::U::V::**)" $ Right "int ::T::U::V::* *"
  t "int(::T::U::V)" $ Right "int(::T::U::V)"
  t "int(const*)()" $ Left "cannot cv-qualify function"
  t "int(X::*)()const volatile" $ Right "int(X::*)() const volatile"
  t "int volatile X::* Y::* const Z::*" $ Right "int volatile X::* Y::* const Z::*"
 where
  t :: String -> Either String String -> IO ()
  t d o = do
    let dt = makeType d
    let o' = show . dt
    when (o' /= o) $ fail $ "test failed: " ++ show (d, o, o')
    case dt of
      Right r -> do
        when (makeType (show r) /= dt) $ fail $ "secondary test failed for " ++ show r
      Left _ -> return ()

itest :: IO ()
itest = makeType . getLine >>= print
