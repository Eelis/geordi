{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, TypeSynonymInstances, MultiParamTypeClasses, FlexibleContexts, OverlappingInstances, GADTs, TypeOperators, TypeFamilies, ScopedTypeVariables, PatternGuards #-}

{- C++ is notoriously hard to parse. There are ambiguities in the grammar that can only be resolved through things like name lookup, which in turn require things like overloading and template instantiation, and so one basically has to implement a complete compiler front-end just to be able to parse.

However, this assumes that one insists on telling the ambiguous constructs apart. In the C++ parser defined in this module, we drop this requirement, and instead just accept that some ambiguous constructs will be parsed arbitrarily as one of their possible interpretations. This means that for something like:

  x*y;

we don't do name lookup to determine whether x is a type or a variable, in order to decide whether this statement is a declaration-statement or an expression-statement. We just parse it as a declaration, because we're especially interested in parsing declarations (since they have names and are natural subjects of edit commands).

Here are some more typical ambiguous constructs:

  int x(y); // If y is a type, this is a function declaration. Otherwise, it's a variable definition.
  x<y> z; // If x is a template, this is a variable declaration. Otherwise, it's an expression-statement.
  x(y); // If x is a type, this is a variable declaration. Otherwise, it's a function call.
  x+(y)(z); // if y is a type, (y)(z) is a cast. Otherwise, it's a function call.

We always prefer parsing things as declarations.

Note that even if we /could/ parse properly with name lookup and all the rest, we wouldn't want to, because we typically want to parse ill-formed code (with type errors, for example) in order to run edit commands on it to make it well-formed.

This C++ parser is probably extremely inefficient. Fortunately, geordi only ever runs it on tiny C++ snippets, so we just don't care.

-}

module Cxx.Parse (Code, Chunk(..), code, charLit, stringLit, makeType, precedence, parseRequest, makeDeclParser, declaratorIdParser, highlight) where

import qualified Data.List as List
import qualified Parsers as P
import qualified Cxx.Show

import qualified Data.Char as Char
import Control.Arrow (first, second)
import Control.Applicative (Applicative(..))
import Control.Monad.Fix (fix)
import Control.Monad.Instances ()
import Control.Monad.Error ()
import Control.Monad (liftM2, liftM3)
import Data.List ((\\))
import Data.Maybe (mapMaybe)
import Data.Function (on)
import Util ((<<), (.), Convert(..), isIdChar, NElist(..), Finite(..), Phantom(..), reverse_ne, cardinals, partitionMaybe, nonne_ne_app, unne, lastAndRest, TriBool(..), (.||.))
import Cxx.Basics
import Cxx.Show (pretty_with_precedence, Highlighter)
import Cxx.Operations (apply, squared, is_primary_TypeSpecifier, parenthesized, specT, split_all_decls, is_pointer_or_reference)
import Prelude hiding ((.))
import Control.Monad.Reader (ReaderT(..))
import Parsers ((<?>), (<|>), pzero, spaces, many, optional, choice, sep, many1, symbols, noneOf, lookAhead, symbol, satisfy, optionMaybe, many1', anySymbol, manys, manyTill, many1Till', oneOf, ParserLike, eof, ParseResult(..), getInput, sepBy1, option)
import MemoTrie (memo, Trie(..), PairTrie(..), BoolTrie(..))

-- Custom parsing monad:

data ParseOptions = ParseOptions { makeTypeExtensions :: Bool, pendingCloseAngleBracket :: Bool }

instance Trie ParseOptions (PairTrie BoolTrie BoolTrie) where
  trie f = trie (\(x, y) -> f $ ParseOptions x y)
  untrie f (ParseOptions x y) = untrie f (x, y)

defaultParseOptions :: ParseOptions
defaultParseOptions = ParseOptions { makeTypeExtensions = False, pendingCloseAngleBracket = False }

type Parser t = ReaderT ParseOptions (P.Parser t)

instance ParserLike (Parser a) a where
  anySymbol = ReaderT $ const anySymbol
  eof = ReaderT $ const eof
  pzero = ReaderT $ const pzero
  ReaderT p <?> s = ReaderT $ (<?> s) . p
  ReaderT p <|> ReaderT q = ReaderT $ \o -> p o <|> q o
  symbols = ReaderT . const . symbols
  satisfy = ReaderT . const . satisfy
  lookAhead (ReaderT p) = ReaderT $ lookAhead . p
  try = id

parseOptions :: Parser t ParseOptions
parseOptions = ReaderT return

run_parser :: Parser t a -> ParseOptions -> [t] -> ParseResult t a
run_parser p o = P.run_parser (runReaderT p o)

-- Some combinators:

silent :: Parser a b -> Parser a b
silent (ReaderT f) = ReaderT $ P.silent . f

guarded :: (a -> Bool) -> Parser t a -> Parser t a
guarded f (ReaderT p) = ReaderT $ P.guarded f . p

memoize :: Parser Char a -> Parser Char a
memoize (ReaderT p) = ReaderT $ \o -> P.Parser $ \s -> m (o, s)
  where m = memo $ \(o, s) -> P.run_parser (p o) s
    -- Note that we memoize over the whole input string, rather than just the position. This is very simplistic and inefficient, but we don't care. We only use memoization to prevent exponential explosions.

notFollowedBy :: Parser t a -> Parser t ()
notFollowedBy (ReaderT p) = ReaderT $ P.notFollowedBy . p

-- Primary module exports

precedence :: String -> Either String String
precedence s = either (pretty_with_precedence . split_all_decls) (pretty_with_precedence . split_all_decls) .
  P.parseOrFail p (dropWhile Char.isSpace s) "code"
  where p = runReaderT (parse << eof) defaultParseOptions :: P.Parser Char (Either Expression [Statement])

highlight :: Highlighter -> String -> String
highlight h s =
  case P.run_parser p (dropWhile Char.isSpace s) of
    ParseSuccess r _ _ _ -> Cxx.Show.show_pretty False h r
    ParseFailure _ _ _ -> s
  where p = runReaderT (parse << eof) defaultParseOptions :: P.Parser Char GeordiRequest

parseRequest :: String -> Either String GeordiRequest
parseRequest s = P.parseOrFail (runReaderT (parse << eof) defaultParseOptions) (dropWhile Char.isSpace s) "request"

makeType :: String -> Either String TypeId
makeType s = P.parseOrFail (runReaderT (parse << eof) (defaultParseOptions { makeTypeExtensions = True })) (dropWhile Char.isSpace s) "type description"

-- Chunk/Code parsers

textLit :: ParserLike m Char => Char -> m String
textLit q = (symbol q >>) $ fix $ \h -> do
  s <- manys $ noneOf [q, '\\']
  c <- anySymbol
  if c == '\\'
    then do d <- anySymbol; r <- h; return $ s ++ ('\\':d:r)
    else return s

-- Parsec's Haskell char/string literal parsers consume whitespace, and save the value rather than the denotation.

charLit, stringLit, plain, parens, curlies, squares, multiComment, singleComment :: ParserLike m Char => m Chunk

charLit = CharLiteral . textLit '\''
stringLit = StringLiteral' . textLit '"'
plain = Plain . ((:[]) . oneOf ";\\" <|> (many1 (noneOf "'\"{([])}/;\\" <|> (symbol '/' << lookAhead (noneOf "*/")))))
parens = Parens . (symbol '(' >> code << symbol ')')
curlies = Curlies . (symbol '{' >> code << symbol '}')
squares = Squares . (symbol '[' >> code << symbol ']')
multiComment = MultiComment . (symbols "/*" >> fst . manyTill anySymbol (symbols "*/"))
singleComment = SingleComment . (symbols "//" >> manys anySymbol)
  -- singleComment is unaware of "// ... \" funkiness.

code :: ParserLike m Char => m Code
code = manys (multiComment <|> singleComment <|> charLit <|> parens <|> curlies <|> squares <|> stringLit <|> plain)
  -- Uncovers just enough structure for Request.hs to find the split positions in "<< ...; ..." and "{ ... } ..." requests and to implement --resume.

-- Misc parsers.

kwd :: String -> Parser Char White
kwd x = symbols x >> notFollowedBy (satisfy isIdChar) >> parse

makeDeclParser :: P.Parser Char MakeDeclaration
makeDeclParser = runReaderT parse (defaultParseOptions { makeTypeExtensions = True })

declaratorIdParser :: P.Parser Char DeclaratorId
declaratorIdParser = runReaderT parse defaultParseOptions

anyOperator :: Parser Char (OperatorTok, White)
anyOperator = (<?> "operator") $ do
  o <- parseOptions
  liftM2 (,) (choice $ if pendingCloseAngleBracket o then pcab else normal) parse
  where
    normal = map (\x -> (symbols $ show x) >> return x) $ List.sortBy (flip compare `on` length . show) (all_values :: [OperatorTok])
    pcab = map (\x -> (symbols $ show x) >> return x) $ List.sortBy (flip compare `on` length . show) $ (all_values :: [OperatorTok]) \\ [CloseTwoAngles, CloseAngle]
      -- Profiling showed that having these two separate makes a huge difference in space/time efficiency.

op :: OperatorTok -> Parser Char White
op o = (<?> operatorTokName o) $ snd . guarded ((== o) . fst) anyOperator

class Parse a where parse :: Parser Char a

instance (Finite a, SingleTokenType a) => Parse (a, White) where parse = (<?> token_class_name (Phantom :: Phantom a)) $ choice $ (\v -> (,) v . either kwd op (token v)) . all_values
instance Parse White where
  parse = silent $ White . concat . (many $
    (symbols "/*" >> ("/*" ++) . (++ "*/") . fst . manyTill anySymbol (symbols "*/")) <|>
    (symbols "//" >> ("//" ++) . getInput) <|> symbols " ")

instance Parse a => Parse [a] where parse = many parse
instance Parse a => Parse (Maybe a) where parse = optionMaybe parse
instance Parse a => Parse (Enclosed a) where parse = Enclosed . parse
instance (Parse a, Parse b) => Parse (a, b) where parse = auto2 (,)
instance (Parse a, Parse b, Parse c) => Parse (a, b, c) where parse = auto3 (,,)
instance (Parse a, Parse b, Parse c, Parse d) => Parse (a, b, c ,d) where parse = auto4 (,,,)
instance Parse a => Parse (Commad a) where parse = liftM2 Commad parse (many (liftM2 (,) parse parse))
instance (Parse a, Parse b) => Parse (Either a b) where parse = Left . parse <|> Right . parse
instance Parse a => Parse (Angled a) where parse = liftM3 Angled parse (ReaderT $ \o -> runReaderT parse (o { pendingCloseAngleBracket = True })) (liftM2 (,) (symbol '>' >> return CloseAngle_) parse)
instance Parse a => Parse (Squared a) where parse = liftM3 Squared parse (ReaderT $ \o -> runReaderT parse $ o { pendingCloseAngleBracket = False }) parse
instance Parse a => Parse (Curlied a) where parse = liftM3 Curlied parse (ReaderT $ \o -> runReaderT parse $ o { pendingCloseAngleBracket = False }) parse
instance Parse a => Parse (Parenthesized a) where parse = parseParenthesized parse
instance Parse () where parse = return ()
instance Parse a => Parse (NElist a) where parse = many1' parse

parseParenthesized :: Parser Char (Enclosed a) -> Parser Char (Parenthesized a)
parseParenthesized p = liftM3 Parenthesized parse (ReaderT $ \o -> runReaderT p $ o { pendingCloseAngleBracket = False }) parse

an :: Parser Char (Maybe White)
an = silent $ optional $ kwd "an" <|> kwd "a"

pluralP :: String -> Parser Char ()
pluralP s = (>> return ()) $ kwd s <|> kwd (s ++ "s")

delim :: Parser Char ()
delim = (op CommaTok >> optional (kwd "and") >> return ()) <|> (kwd "and" >> return ())

takingP :: Parser Char ParameterDeclarationClause
takingP =
  ((kwd "nothing" <|> (kwd "no" >> kwd "arguments")) >> return (ParameterDeclarationClause Nothing Nothing)) <|> mkParameterDeclarationClause . concat . sepBy1 takingClause (delim >> ((kwd "returning" >> pzero) <|> return ()))

commad :: NElist x -> Commad x
commad (NElist h t) = Commad h $ (,) (CommaOp, White " ") . t

mkParameterDeclarationClause :: [ParameterDeclaration] -> ParameterDeclarationClause
mkParameterDeclarationClause l =
  ParameterDeclarationClause (case l of [] -> Nothing; h:t -> (Just $ commad $ NElist h t)) Nothing

instance Parse (CvQualifier, White) where
  parse = (<?> "cv-qualifier") $ do
    b <- makeTypeExtensions . parseOptions
    (if b then (,) Const . kwd "constant" else pzero) <|> (,) Const . kwd "const" <|> (,) Volatile . kwd "volatile"

takingClause :: Parser Char [ParameterDeclaration]
takingClause = (do
  IntegerLiteral s <- parse
  let (n :: Integer) = read s
  if n > 10 then pzero else replicate (fromInteger n) . parse) <|> (:[]) . (an >> parse)

instance Parse MakeDeclaration where
  parse = (<?> "type description") $ (an >>) $ (longlong .) $ flip fix [] $ \self specs -> do
    pspec <- parsePrimarySpec
    sspecs <- many parseSecondarySpec
    (\x -> MakeDeclaration (reverse specs ++ pspec : sspecs) x Indeterminate) . parse
    <|> (((: specs) . parseSecondarySpec) >>= self)
    <|> do
      kwd "pure"
      let virtSpec = MakeSpecifier_DeclSpecifier $ DeclSpecifier_FunctionSpecifier (Virtual, White " ")
      do { MakeDeclaration l m _ <- self specs; return $ MakeDeclaration (virtSpec : l) m Definitely } <|> return (MakeDeclaration (virtSpec : specs) Nothing Definitely)
    <|> do
      kwd "impure" <|> kwd "nonpure"
      do { MakeDeclaration l m _ <- self specs; return $ MakeDeclaration l m DefinitelyNot } <|> return (MakeDeclaration specs Nothing DefinitelyNot)
    <|> do
    let (noncvs, cvs) = partitionMaybe (convert :: MakeSpecifier -> Maybe (CvQualifier, White)) specs
    (x, y) <- type_desc
    return $ (\(p, q) -> MakeDeclaration p q Indeterminate) $ apply (map fst cvs) $ case y of
      Left s -> (noncvs ++ MakeSpecifier_DeclSpecifier . DeclSpecifier_TypeSpecifier . (x ++ [s]), Nothing)
      Right ad -> (noncvs ++ MakeSpecifier_DeclSpecifier . DeclSpecifier_TypeSpecifier . x, Just ad)
    <|> (\x -> MakeDeclaration specs x Indeterminate) . (if null specs then Just . parse else parse)
    where
      longlong m@(MakeDeclaration l x y)
        | (p, q) <- List.partition (== convert LongSpec) l, length p >= 2 = MakeDeclaration (LongLong : q) x y
        | otherwise = m

literalArrayBound :: IntegerLiteral -> ConditionalExpression
literalArrayBound = ConditionalExpression_LogicalOrExpression . LogicalOrExpression_LogicalAndExpression . LogicalAndExpression_InclusiveOrExpression . InclusiveOrExpression_ExclusiveOrExpression . ExclusiveOrExpression_AndExpression . AndExpression_EqualityExpression . EqualityExpression_RelationalExpression . RelationalExpression_ShiftExpression . ShiftExpression_AdditiveExpression . AdditiveExpression_MultiplicativeExpression . MultiplicativeExpression_PmExpression . PmExpression_CastExpression . CastExpression_UnaryExpression . UnaryExpression_PostfixExpression . PostfixExpression_PrimaryExpression . PrimaryExpression_Literal . flip Literal_IntegerLiteral (White "")

type_desc :: Parser Char ([TypeSpecifier], Either TypeSpecifier PtrAbstractDeclarator)
  -- Todo: Document this type
type_desc = (<?> "type description") $ do
      o <- (pluralP "pointer" >> return (PtrOperator_Ptr (StarOperator, White "") []))
        <|> (\k -> PtrOperator_Ref (k, White "")) . (((kwd "rvalue" >> return Rvalue) <|> (optional (kwd "lvalue") >> return Lvalue)) << pluralP "reference")
      (kwd "to" >> an >> second Right . apply o . specdDesc) <|> ((,) [] . Right . flip apply (PtrAbstractDeclarator o Nothing) . (parse :: Parser Char (Maybe PtrAbstractDeclarator)))
    <|> do
      pluralP "array"
      let d = PtrAbstractDeclarator_NoptrAbstractDeclarator . NoptrAbstractDeclarator Nothing . Right . squared
      do
        kwd "of"; n <- (d . Just . literalArrayBound . parse << spaces) <|> return (d Nothing)
        second Right . apply n . specdDesc
       <|> do
        mad <- parse :: Parser Char (Maybe PtrAbstractDeclarator)
        return ([], Right $ apply mad (d Nothing))
    <|> liftM2 (flip apply) (op OpenParen >> specdDesc << op CloseParen) (parse :: Parser Char (Maybe PtrAbstractDeclarator))
    <|> do
      pluralP "function"
      let function_declarator taking = PtrAbstractDeclarator_NoptrAbstractDeclarator $ NoptrAbstractDeclarator Nothing (Left $ ParametersAndQualifiers (parenthesized taking) [] Nothing Nothing)
      do
        taking <- kwd "taking" >> takingP
        do
          second Right . apply (function_declarator taking) . (optional delim >> kwd "returning" >> an >> specdDesc) <|> return ([], Right $ function_declarator taking)
        <|> do
        second Right . liftM2 (flip apply) (kwd "returning" >> an >> specdDesc) (function_declarator . ((optional delim >> kwd "taking" >> takingP) <|> return (mkParameterDeclarationClause [])))
        <|> do
        (,) [] . Right . flip apply (function_declarator $ mkParameterDeclarationClause []) . (parse :: Parser Char (Maybe PtrAbstractDeclarator))
  where
    specdDesc :: Parser Char ([TypeSpecifier], Either TypeSpecifier PtrAbstractDeclarator)
    specdDesc = (<?> "type description") $ flip fix [] $ \self specs -> do
      morespecs <- liftM2 NElist parsePrimarySpec (many parseSecondarySpec)
      let ne = nonne_ne_app specs morespecs
      mad <- parse :: Parser Char (Maybe PtrAbstractDeclarator)
      return $ case mad of
        Nothing -> let (x, y) = lastAndRest ne in (x, Left y)
        Just ad -> (unne ne, Right ad)
     <|> do
      sspec <- parseSecondarySpec
      self (specs ++ [sspec]) <|> return ([], Left sspec)
     <|> do
      let (noncvs, cvs) = partitionMaybe (convert :: TypeSpecifier -> Maybe (CvQualifier, White)) specs
      first (noncvs ++) . apply (map fst cvs) . type_desc

with_default :: [TypeSpecifier] -> NElist TypeSpecifier
with_default [] = NElist specT []
with_default l@(h:t) = if any is_primary_TypeSpecifier l then NElist h t else NElist specT l

instance Parse GeordiRequest where parse = auto3 GeordiRequest_Print <|> auto2 GeordiRequest_Block <|> auto1 GeordiRequest_TU

parseAnyMixOf :: Parser t a -> Parser t b -> Parser t (AnyMixOf a b)
parseAnyMixOf p q = (p >>= \x -> MixAB x . q <|> return (MixA x)) <|> (q >>= \y -> MixBA y . p <|> return (MixB y)) <|> return MixNone

instance (Parse a, Parse b) => Parse (AnyMixOf a b) where parse = parseAnyMixOf parse parse

instance Parse OptQualified where parse = auto2 OptQualified <?> "optional qualification"

auto1 :: (Parse a) => (a -> b) -> Parser Char b
auto1 f = f . parse
auto2 :: (Parse a, Parse b) => (a -> b -> c) -> Parser Char c
auto2 f = auto1 f <*> parse
auto3 :: (Parse a, Parse b, Parse c) => (a -> b -> c -> d) -> Parser Char d
auto3 f = auto2 f <*> parse
auto4 :: (Parse a, Parse b, Parse c, Parse d) => (a -> b -> c -> d -> e) -> Parser Char e
auto4 f = auto3 f <*> parse
auto5 :: (Parse a, Parse b, Parse c, Parse d, Parse e) => (a -> b -> c -> d -> e -> f) -> Parser Char f
auto5 f = auto4 f <*> parse
auto6 :: (Parse a, Parse b, Parse c, Parse d, Parse e, Parse f) => (a -> b -> c -> d -> e -> f -> g) -> Parser Char g
auto6 f = auto5 f <*> parse


-- Parse instances for all the grammar productions.

-- A.1 Keywords [gram.key]

instance Parse TemplateName where parse = auto1 TemplateName <?> "template-name"

-- A.2 Lexical conventions [gram.lex]

instance Parse Identifier where
  parse = (<?> "identifier") $ do
    b <- makeTypeExtensions . parseOptions
    let k = keywords ++ if b then make_type_keywords else []
    liftM2 Identifier (guarded (not . (`elem` k)) $ liftM2 (:) (satisfy $ Char.isAlpha .||. (== '_')) (many $ satisfy isIdChar)) parse

instance Parse ClassName where parse = auto1 ClassName_TemplateId <|> auto1 ClassName_Identifier <?> "class-name"
instance Parse TypeName where parse = auto1 TypeName_ClassName <?> "type-name"

instance Parse FloatingLiteral where
  parse = (<?> "floating-literal") $ (FloatingLiteral .) $ (>++> optSuffix) $
    (symbols "." >++> many1 digit >++> option "" exponentPart) <|>
    (many1 digit >++> ((symbols "." >++> many digit >++> option "" exponentPart >++> optSuffix) <|> exponentPart))
    where
      (>++>) = liftM2 (++)
      digit = satisfy Char.isDigit
      optSuffix = option "" $ (:[]) . (symbol 'f' <|> symbol 'l' <|> symbol 'F' <|> symbol 'L')
      exponentPart = option "" $ (symbols "e" <|> symbols "E") >++> option "" (symbols "+" <|> symbols "-") >++> many1 digit
instance Parse IntegerLiteral where
  parse = (<?> "integer-literal") $ (IntegerLiteral .) $ (p <|>) $ do
    b <- makeTypeExtensions . parseOptions
    if b then choice $ zipWith (\n s -> kwd s >> return (show n)) [0::Int ..] cardinals else pzero
   where p = liftM2 (:) (satisfy Char.isDigit) (many (satisfy Char.isAlphaNum) << notFollowedBy (satisfy isIdChar))
instance Parse StringLiteralKind where
  parse = (symbols "u8" >> return StringLiteral_u8) <|> (symbol 'u' >> return StringLiteral_u) <|> (symbol 'U' >> return StringLiteral_U) <|> (symbol 'L' >> return StringLiteral_L) <|> return StringLiteral_Plain
instance Parse SingleStringLiteral where parse = liftM2 SingleStringLiteral parse (textLit '"')
instance Parse StringLiteral where parse = StringLiteral . many1' (auto2 (,)) <?> "string-literal"
instance Parse CharacterLiteralKind where parse = (symbol 'u' >> return CharacterLiteralKind_u) <|> (symbol 'U' >> return CharacterLiteralKind_U) <|> (symbol 'L' >> return CharacterLiteralKind_L) <|> return CharacterLiteral_Plain
instance Parse CharacterLiteral where parse = liftM2 CharacterLiteral parse (textLit '\'')
instance Parse Literal where
  parse = (<?> "literal") $ auto2 Literal_CharacterLiteral <|> auto1 Literal_StringLiteral <|> auto2 Literal_FloatingLiteral <|> auto2 Literal_IntegerLiteral <|> liftM2 Literal_BooleanLiteral ((kwd "true" >> return True) <|> (kwd "false" >> return False)) parse <|> (kwd "nullptr" >> PointerLiteral . parse)

-- A.3 Basic concepts [gram.basic]

instance Parse TranslationUnit where parse = TranslationUnit . parse

-- A.4 Expressions [gram.expr]

instance Parse PrimaryExpression where
  parse = (<?> "primary-expression") $
    auto1 PrimaryExpression_This <|> auto1 PrimaryExpression_Literal <|>
    auto1 PrimaryExpression_Expression <|> auto1 PrimaryExpression_IdExpression
instance Parse IdExpression where parse = (<?> "id-expression") $ IdExpression . parse
instance Parse UnqualifiedId where
  parse = (<?> "unqualified-id") $
    (parse >>= \w -> (UnqualifiedId_OperatorFunctionId . OperatorFunctionId w . parse <|> UnqualifiedId_ConversionFunctionId . ConversionFunctionId w . parse))
    <|> auto1 UnqualifiedId_TemplateId <|> auto1 UnqualifiedId_Identifier <|> auto2 UnqualifiedId_Destructor
instance Parse QualifiedId where
  parse = (<?> "qualified-id") $ (do
    w <- parse
    GlobalIdentifier w . parse <|> GlobalOperatorFunctionId w . parse <|> GlobalTemplateId w . parse <|>
      auto3 (NestedUnqualifiedId (Just w))
   ) <|> auto3 (NestedUnqualifiedId Nothing)
instance Parse NestedNameSpecifier where
  parse = (<?> "nested-name-specifier") $ liftM2 (foldl (flip ($)))
    (auto2 NestedNameSpecifier_TypeName)
    (many $ auto3 (\x y z u -> NestedNameSpecifier_SimpleTemplateId u x y z) <|> auto2 (\x y z -> NestedNameSpecifier_Identifier z x y))
      -- We can't distinguish a simple class-name from a namespace-name anyway, so we only try to parse a type-name here.
instance Parse PostfixExpression where
  parse = (<?> "postfix-expression") $ liftM2 (foldl $ flip ($)) basic $ many $ (<?> "postfix operator") $
      auto3 (\o t e' e -> PostfixExpression_Member e o t e') <|> flip PostfixExpression_IncDec . parse <|> flip PostfixExpression_FunctionCall . parse <|> flip PostfixExpression_Squared . parse <|> auto2 (\o n e -> PostfixExpression_PseudoDestructor e o n)
    where
     basic = auto3 PostfixExpression_NewStyleCast <|> auto2 PostfixExpression_Conversion <|> auto2 PostfixExpression_TypeId <|> auto1 PostfixExpression_PrimaryExpression
instance Parse PseudoDestructorName where
  parse = (<?> "pseudo-destructor-name") $ do
    w <- parse
    mnns <- parse
    maybe pzero (\nns -> auto5 (PseudoDestructorName_InTemplate w nns)) mnns <|>
      auto2 (PseudoDestructorName (OptQualified w mnns)) <|>
      auto4 (PseudoDestructorName_InTypeName (OptQualified w mnns))
simpleBinaryGroup :: (Parse (b, White), Parse a) => (a1 -> (b, White) -> a -> a1) -> (a -> a1) -> Parser Char a1
simpleBinaryGroup c l = do
  (e, f) <- sep parse (parse <?> "binary operator")
  return $ foldl (\z ((o, w), y) -> c z (o, w) y) (l e) f
simplerBinaryGroup :: (Parse a) => Parser Char d -> (a1 -> d -> a -> a1) -> (a -> a1) -> Parser Char a1
simplerBinaryGroup p c l = do
    (e, f) <- sep parse (p <?> "binary operator")
    return $ foldl (\z (w, y) -> c z w y) (l e) f
instance Parse UnaryExpression where
  parse = (<?> "unary-expression") $
    (parse >>= \w -> (UnaryExpression_Sizeof_TypeId w . parse <|> (UnaryExpression_Sizeof_UnaryExpression w . parse))) <|>
    auto2 UnaryExpression_AlignOf <|> auto2 UnaryExpression <|> UnaryExpression_NewExpression . parse <|> UnaryExpression_DeleteExpression . parse <|> UnaryExpression_PostfixExpression . parse
instance Parse NewExpression where parse = auto5 NewExpression
instance Parse NewPlacement where parse = auto1 NewPlacement
instance Parse NewTypeId where parse = auto2 NewTypeId
instance Parse NewDeclarator where parse = auto2 NewDeclarator_PtrOperator <|> auto1 NewDeclarator_NoptrNewDeclarator
instance Parse NoptrNewDeclarator where parse = liftM2 NoptrNewDeclarator parse (reverse . parse)
instance Parse DeleteExpression where parse = auto4 DeleteExpression
instance Parse ThrowExpression where parse = (<?> "throw-expression") $ auto2 ThrowExpression
instance Parse CastExpression where parse = (<?> "cast-expression") $ auto2 CastExpression_Cast <|> CastExpression_UnaryExpression . parse
instance Parse PmExpression where parse = (<?> "pm-expression") $ simpleBinaryGroup PmExpression PmExpression_CastExpression
instance Parse MultiplicativeExpression where parse = (<?> "multiplicative-expression") $ simpleBinaryGroup MultiplicativeExpression MultiplicativeExpression_PmExpression
instance Parse (EqualityOperator, White) where parse = ((,) EqualityOperator_Equal . op IsIs) <|> ((,) EqualityOperator_Unequal . op ExclamationIs)
instance Parse AdditiveExpression where parse = (<?> "additive-expression") $ simpleBinaryGroup AdditiveExpression AdditiveExpression_MultiplicativeExpression
instance Parse ShiftExpression where parse = (<?> "shift-expression") $ simpleBinaryGroup ShiftExpression ShiftExpression_AdditiveExpression
instance Parse RelationalExpression where parse = (<?> "relational-expression") $ simpleBinaryGroup RelationalExpression RelationalExpression_ShiftExpression
instance Parse EqualityExpression where parse = (<?> "equality-expression") $ simpleBinaryGroup EqualityExpression EqualityExpression_RelationalExpression
instance Parse AndExpression where parse = (<?> "and-expression") $ simplerBinaryGroup parse AndExpression AndExpression_EqualityExpression
instance Parse ExclusiveOrExpression where parse = (<?> "exclusive-or-expression") $ simplerBinaryGroup parse ExclusiveOrExpression ExclusiveOrExpression_AndExpression
instance Parse InclusiveOrExpression where parse = (<?> "inclusive-or-expression") $ simplerBinaryGroup parse InclusiveOrExpression InclusiveOrExpression_ExclusiveOrExpression
instance Parse LogicalAndExpression where parse =  (<?> "logical-and-expression") $ simplerBinaryGroup parse LogicalAndExpression LogicalAndExpression_InclusiveOrExpression
instance Parse LogicalOrExpression where parse = (<?> "logical-or-expression") $ simplerBinaryGroup parse LogicalOrExpression LogicalOrExpression_LogicalAndExpression
instance Parse ConditionalExpression where parse = (<?> "conditional-expression") $ ConditionalExpression_LogicalOrExpression . parse -- Todo: This is no good, I think.
instance Parse AssignmentExpression where
  parse = (<?> "assignment-expression") $ (AssignmentExpression_ThrowExpression . parse <|>) $ do
    e <- parse
    AssignmentExpression_ConditionalExpression . auto4 (ConditionalExpression e) <|> auto2 (AssignmentExpression e) <|> return (AssignmentExpression_ConditionalExpression $ ConditionalExpression_LogicalOrExpression e)
instance Parse Expression where parse = (<?> "expression") $ simplerBinaryGroup parse Expression_Comma Expression_AssignmentExpression

-- A.5 Statements [gram.stmt]

instance Parse Statement where
  parse = (<?> "statement") $ auto1 Statement_CompoundStatement <|> auto1 Statement_JumpStatement <|> auto1 Statement_SelectionStatement <|> auto1 Statement_IterationStatement <|> auto1 Statement_DeclarationStatement <|> auto1 Statement_ExpressionStatement <|> auto1 Statement_TryBlock <|> auto1 Statement_Labeled

instance Parse LabelKind where parse = auto1 IdentifierLabel <|> auto2 CaseLabel <|> auto1 DefaultLabel
instance Parse LabeledStatement where parse = auto3 LabeledStatement
instance Parse IterationStatement where parse = auto3 WhileStatement <|> auto5 DoWhileStatement <|> auto3 ForStatement
instance Parse ForInitStatement where parse = auto1 ForInitStatement_SimpleDeclaration <|> auto1 ForInitStatement_ExpressionStatement
instance Parse SelectionStatement where parse = auto4 IfStatement <|> auto3 SwitchStatement
instance Parse JumpStatement where parse = auto2 BreakStatement <|> auto2 ContinueStatement <|> auto3 ReturnStatement
instance Parse ExpressionStatement where parse = auto2 ExpressionStatement <?> "expression-statement"
instance Parse Condition where parse = auto1 Condition_Expression <|> auto3 Condition_Declarator <?> "condition"
instance Parse CompoundStatement where parse = CompoundStatement . parse <?> "compound-statement"
instance Parse DeclarationStatement where parse = auto1 DeclarationStatement <?> "declaration-statement"

-- A.6 Declarations [gram.dcl]

instance Parse Declaration where parse = (<?> "declaration") $ auto1 Declaration_BlockDeclaration <|> auto1 Declaration_FunctionDefinition <|> auto1 Declaration_ExplicitSpecialization <|> auto1 Declaration_ExplicitInstantiation <|> auto1 Declaration_LinkageSpecification <|> auto1 Declaration_NamespaceDefinition <|> auto1 Declaration_TemplateDeclaration
instance Parse BlockDeclaration where parse = (<?> "block-declaration") $ auto1 BlockDeclaration_SimpleDeclaration <|> auto1 BlockDeclaration_AsmDefinition <|> auto1 BlockDeclaration_NamespaceAliasDefinition <|> auto1 BlockDeclaration_UsingDeclaration <|> auto1 BlockDeclaration_UsingDirective <|> auto1 BlockDeclaration_StaticAssertDeclaration <|> auto1 BlockDeclaration_AliasDeclaration
instance Parse UsingDirective where parse = auto5 UsingDirective
instance Parse AliasDeclaration where parse = auto5 AliasDeclaration
instance Parse StaticAssertDeclaration where parse = auto3 StaticAssertDeclaration
instance Parse NamespaceAliasDefinition where parse = auto6 NamespaceAliasDefinition
instance Parse NamespaceName where parse = NamespaceName_OriginalNamespaceName . OriginalNamespaceName . parse
instance Parse AsmDefinition where parse = auto3 AsmDefinition
instance Parse SimpleDeclaration where
  parse = do
    (specs, (decls, semicolon)) <- many1Till' parse (liftM2 (,) parse parse)
    return $ SimpleDeclaration specs decls semicolon

instance Parse UsingDeclaration where parse = parse >>= \w -> auto5 (UsingDeclaration_Nested w) <|> auto3 (UsingDeclaration_NonNested w)
instance Parse AlignmentSpecifier where parse = auto2 AlignmentSpecifier <?> "alignment-specifier"
instance Parse (BasicType, White) where
  parse = do
    b <- makeTypeExtensions . parseOptions
    if not b
      then choice $ (\v -> (,) v . either kwd op (token v)) . all_values
      else choice $ map (\(v, k) -> (,) v . kwd k) $
        ((,) Int' . ["ints", "integer", "integers"]) ++
        ((,) Char' . ["chars", "character", "characters"]) ++
        ((,) Bool' . ["bools", "boolean", "booleans"]) ++
        [(Float', "floats"), (Double', "doubles"), (Void, "nothing")] ++
        mapMaybe (\v -> either (Just . (,) v) (const Nothing) (token v)) all_values
instance Parse SimpleTypeSpecifier where
  parse = (<?> "simple-type-specifier") $
      SimpleTypeSpecifier_Auto . parse <|> SimpleTypeSpecifier_BasicType . parse
    <|> liftM2 SimpleTypeSpecifier_DeclType parse parse <|> LengthSpec . parse <|> SignSpec . parse
    <|> do
      w <- parse
      (parse >>= \nns -> (auto2 (SimpleTypeSpecifier_SimpleTemplateId w nns) <|> SimpleTypeSpecifier_TypeName (OptQualified w (Just nns)) . parse)) <|> SimpleTypeSpecifier_TypeName (OptQualified w Nothing) . parse
instance Parse TypeSpecifier where
  parse = (<?> "type-specifier") $ auto1 TypeSpecifier_CvQualifier <|> auto1 TypeSpecifier_SimpleTypeSpecifier <|> auto1 TypeSpecifier_TypenameSpecifier <|> auto1 TypeSpecifier_ClassSpecifier <|> auto1 TypeSpecifier_EnumSpecifier <|> auto1 TypeSpecifier_ElaboratedTypeSpecifier
instance Parse ElaboratedTypeSpecifier where parse = auto3 ElaboratedTypeSpecifier
instance Parse EnumHead where parse = auto3 EnumHead
instance Parse EnumBase where parse = auto2 EnumBase
instance Parse EnumeratorDefinition where parse = auto2 EnumeratorDefinition
instance Parse Enumerator where parse = auto1 Enumerator
instance Parse EnumKey where parse = parse >>= \e -> EnumKey_Class e . parse <|> EnumKey_Struct e . parse <|> return (EnumKey e)
instance Parse NamespaceDefinition where parse = auto4 NamespaceDefinition
instance Parse LinkageSpecification where parse = auto3 LinkageSpecification
instance Parse EnumSpecifier where parse = auto2 EnumSpecifier

-- A.7 Declarators [gram.decl]

instance Parse (RefQualifier, White) where parse = (<?> "ref-qualifier") $ ((,) Rvalue . op AmperAmper) <|> ((,) Lvalue . op Amper)
instance Parse PtrOperator where parse = (<?> "ptr-operator") $ auto2 PtrOperator_Ptr <|> PtrOperator_Ref . parse <|> auto4 PtrOperator_Nested
instance Parse PtrAbstractDeclarator where parse = auto2 PtrAbstractDeclarator <|> PtrAbstractDeclarator_NoptrAbstractDeclarator . parse
instance Parse NoptrAbstractDeclarator where parse = liftM2 (foldl $ \x y -> NoptrAbstractDeclarator (Just x) y) ((NoptrAbstractDeclarator_PtrAbstractDeclarator . parse) <|> NoptrAbstractDeclarator Nothing . parse) (many parse)
instance Parse AbstractDeclarator where parse = auto1 AbstractDeclarator_PtrAbstractDeclarator <?> "abstract-declarator"
instance Parse InitializerList where parse = auto1 InitializerList <?> "initializer-list"
instance Parse InitializerClause where parse = auto1 InitializerClause <?> "initializer-clause"
instance Parse InitDeclarator where
  parse = (<?> "init-declarator") $ do
    declarator <- parse
    if is_pointer_or_reference declarator
      then InitDeclarator declarator . (optionMaybe $ (<?> "initializer") $ Initializer_Parenthesized . parseParenthesized (Enclosed . InitializerList . flip Commad [] . parse) <|> auto1 Initializer_BraceOrEqualInitializer)
      else auto1 (InitDeclarator declarator)

instance Parse Declarator where parse = auto1 Declarator_PtrDeclarator <?> "declarator"
instance Parse PtrDeclarator where parse = liftM2 (flip $ foldl $ flip PtrDeclarator) (reverse . parse) (PtrDeclarator_NoptrDeclarator . parse) <?> "ptr-declarator"

instance Parse NoptrDeclarator where
  parse = liftM2 (foldl f) (NoptrDeclarator_Parenthesized . parse <|> NoptrDeclarator_Id . parse) parse
   where f d = either (NoptrDeclarator_WithParams d) (NoptrDeclarator_Squared d)

instance Parse DeclaratorId where parse = auto2 DeclaratorId_IdExpression <?> "declarator-id"
  -- We don't even try to parse a DeclaratorId_Nested, because we can't tell it apart from an IdExpression anyway.

instance Parse TypeId where parse = (<?> "type-id") $ typeP id (\x y -> TypeId x $ AbstractDeclarator_PtrAbstractDeclarator . y)

typeP :: (Parse c, Convert b (Maybe (CvQualifier, White)), Convert TypeSpecifier b, ParseSpecifier b) =>
  (Maybe PtrAbstractDeclarator -> c) -> (NElist b -> c -> a) -> Parser Char a
typeP g h = makeTypeExtensions . parseOptions >>= \b -> flip fix [] $ \self specs -> do
    pspec <- parsePrimarySpec; sspecs <- many parseSecondarySpec
    let (NElist p q) = reverse_ne (NElist pspec specs)
    r <- parse
    return $ h (NElist p (q ++ sspecs)) r
   <|> (((: specs) . parseSecondarySpec) >>= self)
   <|> if not b then pzero else do
    let (noncvs, cvs) = partitionMaybe (\x -> convert x :: Maybe (CvQualifier, White)) specs
    (x, y) <- apply (map fst cvs) . type_desc
    return $ uncurry h $ second g $ case y of
      Left s ->  (nonne_ne_app noncvs (fmap convert $ with_default (s:x)), Nothing)
      Right ad -> (nonne_ne_app noncvs (fmap convert $ with_default x), Just ad)

parseSpecs :: ParseSpecifier b => Parser Char (NElist b)
parseSpecs = do
  (l, p) <- manyTill parseSecondarySpec parsePrimarySpec
  l' <- many parseSecondarySpec
  return $ nonne_ne_app l (NElist p l')

instance Parse Initializer where parse = (<?> "initializer") $ auto1 Initializer_Parenthesized <|> auto1 Initializer_BraceOrEqualInitializer
instance Parse BraceOrEqualInitializer where parse = auto2 EqualInitializer <|> auto1 BraceInitializer
instance Parse BracedInitList where parse = auto1 BracedInitList <?> "braced-init-list"
instance Parse ParametersAndQualifiers where parse = memoize $ (<?> "parameters-and-qualifiers") $ auto4 ParametersAndQualifiers
  {- This memoize prevents an exponential explosion in
      { int b; a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(a(b+b)))))))))))))))))))))))); }
  -}

instance Parse ParameterDeclarationClause where
  parse = (<?> "parameter-declaration-clause") $
    ParameterDeclarationClause Nothing . Just . parse <|> (do
      l <- parse
      auto2 (ParameterDeclarationClauseWithEllipsis l) <|> ParameterDeclarationClause (Just l) . parse
      ) <|> return (ParameterDeclarationClause Nothing Nothing)

instance Parse ParameterDeclaration where parse = (<?> "parameter-declaration") $ typeP (Right . (AbstractDeclarator_PtrAbstractDeclarator .)) ParameterDeclaration >>= auto1

-- Consider the simple-declaration  x y();  . If we just start by parsing a list of decl/type-specifiers, we get [x, y], and consequently we will never succeed in parsing a simple-declaration. For this reason, we distinguish between primary and secondary specifiers, where the former may only occur once in specifier lists. We then parse specifier lists in such a way that we stop when a second primary specifier is encountered (and don't include it in the resulting list). In the example, both x and y are primary specifiers, and so only x becomes part of the list, leaving y to be (correctly) parsed as part of the declarator.
-- There is one context in which this is not sufficient: constructor declarations. In a constructor declaration  T();  , the T is not part of the specifier sequence, but part of the declarator. There, we use a manyTill to stop parsing specifiers as soon as what follows is a valid declarator.

class ParseSpecifier s where parsePrimarySpec, parseSecondarySpec :: Parser Char s

instance ParseSpecifier TypeSpecifier where
  parsePrimarySpec = (<?> "type-specifier") $ TypeSpecifier_SimpleTypeSpecifier . primarySimpleTypeSpecifier <|> TypeSpecifier_ClassSpecifier . parse <|> TypeSpecifier_TypenameSpecifier . parse <|> TypeSpecifier_EnumSpecifier . parse <|> TypeSpecifier_ElaboratedTypeSpecifier . parse
  parseSecondarySpec = (<?> "type-specifier") $ TypeSpecifier_CvQualifier . parse <|> TypeSpecifier_SimpleTypeSpecifier . LengthSpec . parse <|> TypeSpecifier_SimpleTypeSpecifier . SignSpec . parse <|> TypeSpecifier_SimpleTypeSpecifier . SimpleTypeSpecifier_BasicType . parse

instance ParseSpecifier DeclSpecifier where
  parsePrimarySpec = (<?> "decl-specifier") $ DeclSpecifier_TypeSpecifier . parsePrimarySpec
  parseSecondarySpec = (<?> "decl-specifier") $ DeclSpecifier_StorageClassSpecifier . parse <|> DeclSpecifier_FunctionSpecifier . parse <|> DeclSpecifier_Friend . parse <|> DeclSpecifier_Typedef . parse <|> DeclSpecifier_ConstExpr . parse <|> DeclSpecifier_AlignmentSpecifier . parse <|> DeclSpecifier_TypeSpecifier . parseSecondarySpec

primarySimpleTypeSpecifier :: Parser Char SimpleTypeSpecifier
primarySimpleTypeSpecifier = (<?> "simple-type-specifier") $ SimpleTypeSpecifier_BasicType . parse
  <|> auto2 SimpleTypeSpecifier_DeclType <|> auto2 SimpleTypeSpecifier_TypeName <|> auto1 LengthSpec <|> auto1 SignSpec <|> auto1 SimpleTypeSpecifier_Auto

instance ParseSpecifier MakeSpecifier where
  parsePrimarySpec = (<?> "make-specifier") $ MakeSpecifier_DeclSpecifier . parsePrimarySpec
  parseSecondarySpec = (<?> "make-specifier") $
    (symbols "non" >> NonFunctionSpecifier . fst . (parse :: Parser Char (FunctionSpecifier, White))) <|>
    (symbols "non" >> NonStorageClassSpecifier . fst . (parse :: Parser Char (StorageClassSpecifier, White))) <|>
    (symbols "non" >> NonCv . fst . (parse :: Parser Char (CvQualifier, White))) <|>
    (symbols "non" >> NonSign . fst . (parse :: Parser Char (Sign, White))) <|>
    (symbols "non" >> NonLength . fst . (parse :: Parser Char (LengthSpec, White))) <|>
    (symbols "implicit" >> return (NonFunctionSpecifier Explicit)) <|>
    MakeSpecifier_DeclSpecifier . parseSecondarySpec

instance Parse [DeclSpecifier] where
  parse = liftM2 (:) parsePrimarySpec (many parseSecondarySpec) <|> liftM2 (:) parseSecondarySpec parse

instance Parse DeclSpecifier where parse = parsePrimarySpec <|> parseSecondarySpec

instance Parse FunctionDefinition where
  parse = (<?> "function-definition") $ do
    (declspecs, (declarator, body)) <- manyTill parse (liftM2 (,) parse parse)
    return $ FunctionDefinition declspecs declarator body

instance Parse FunctionBody where parse = auto2 FunctionBody <?> "function-body"

-- A.8 Classes [gram.class]

instance Parse ClassSpecifier where parse = auto2 ClassSpecifier <?> "class-specifier"
instance Parse ClassHead where parse = auto3 ClassHead <?> "class-head"
instance Parse ClassHeadKind where parse = auto2 ClassHeadKind_SimpleTemplateId <|> auto2 ClassHeadKind_NestedIdentifier <|> auto1 ClassHeadKind_Identifier
instance Parse MemberSpecification where parse = auto1 MemberSpecification <?> "member-specification"
instance Parse MemberAccessSpecifier where parse = auto2 MemberAccessSpecifier <?> "member-access-specifier"
instance Parse MemberDeclaration where
  parse = (<?> "member-declaration") $ do
      (x, (y, z)) <- manyTill parse (liftM2 (,) parse parse)
      return $ MemberDeclaration x y z
    <|> auto2 MemberFunctionDefinition <|> auto1 MemberUsingDeclaration <|> auto1 MemberTemplateDeclaration
instance Parse MemberDeclarator where parse = auto3 BitField <|> auto2 MemberDeclarator <?> "member-declarator"
instance Parse PureSpecifier where parse = auto2 PureSpecifier <?> "pure-specifier"

-- A.9 Derived classes [gram.derived]

instance Parse BaseSpecifier where parse = auto3 BaseSpecifier <?> "base-specifier"
instance Parse BaseClause where parse = auto2 BaseClause <?> "base-clause"

-- A.10 Special member functions [gram.special]

instance Parse ConversionTypeId where parse = auto2 ConversionTypeId <?> "conversion-type-id"
instance Parse CtorInitializer where parse = auto2 CtorInitializer <?> "ctor-initializer"
instance Parse MemInitializer where parse = auto2 MemInitializer <?> "mem-initializer"
instance Parse MemInitializerId where parse = auto2 MemInitializerId_ClassName <|> auto1 MemInitializerId_Identifier <?> "mem-initializer-id"

-- A.11 Overloading [gram.over]

instance Parse OverloadableOperator where
  parse = (<?> "overloadable operator") $
    auto2 OverloadableOperator_New <|> auto2 OverloadableOperator_Delete <|>
    auto1 OverloadableOperator_Call <|> auto1 OverloadableOperator_Index <|> auto1 OverloadableUnaryOperator <|> auto1 OverloadableAssignmentOperator <|> auto1 OverloadableRelationalOperator <|> auto1 OverloadableMultiplicativeOperator <|> auto1 OverloadableShiftOperator <|> auto1 OverloadableAdditiveOperator <|> auto1 OverloadableEqualityOperator <|> auto1 OverloadableBitXor <|> auto1 OverloadableBitAnd <|> auto1 OverloadableBitOr <|> auto1 OverloadableLogicalAnd <|> auto1 OverloadableLogicalOr <|> auto1 OverloadableComma <|> auto1 OverloadablePmOperator

instance Parse OperatorFunctionId where parse = auto2 OperatorFunctionId <?> "operator-function-id"

-- A.12 Templates [gram.temp]

instance Parse TemplateArguments where
  parse = memoize (auto1 TemplateArguments) <?> "template-arguments"

{- Consider the following code:

  {a<a<a<a<a<a<a<a<a<a<a<a<a<a<a<a<a<a<a<a<a<a<a<a<a; int i; }

At each '<', the parser first tries parsing it as the beginning of a template argument list, before it falls back on treating it as operator<. Hence, without memoization, the above takes exponential time to parse. To solve this, we memoize parsing results. -}

instance Parse TemplateId where parse = auto2 TemplateId_OperatorFunctionId <|> auto1 TemplateId_SimpleTemplateId <?> "template-id"
instance Parse TypenameSpecifier where parse = auto4 TypenameSpecifier <?> "typename-specifier"
instance Parse SimpleTemplateId where parse = auto2 SimpleTemplateId <?> "simple-template-id"
instance Parse TemplateArgumentList where parse = auto1 TemplateArgumentList <?> "template-argument-list"
instance Parse TemplateArgument where parse = auto1 TemplateArgument_TypeId <|> auto1 TemplateArgument_ConstantExpression <|> auto1 TemplateArgument_IdExpression <?> "template-argument"
  -- Todo: There's probably potential for factoring here.
instance Parse TemplateDeclaration where parse = auto4 TemplateDeclaration <?> "template-declaration"
instance Parse TemplateParameter where parse = auto1 TemplateParameter_TypeParameter <|> auto1 TemplateParameter_ParameterDeclaration <?> "template-parameter"
instance Parse TypeParameter where parse = auto3 TypeParameter_Class <|> auto5 TypeParameter_Template <?> "type-parameter"
instance Parse ExplicitInstantiation where parse = auto3 ExplicitInstantiation <?> "explicit-instantiation"
instance Parse ExplicitSpecialization where parse = auto3 ExplicitSpecialization <?> "explicit-specialization"

-- A.13 Exception handling [gram.except]

instance Parse ExceptionSpecification where parse = auto2 ExceptionSpecification <?> "exception-specification"
instance Parse TypeIdList where parse = auto1 TypeIdList <?> "type-id-list"
instance Parse TryBlock where parse = auto3 TryBlock <?> "try-block"
instance Parse Handler where parse = auto3 Handler <?> "handler"
instance Parse ExceptionDeclaration where parse = auto1 ExceptionDeclaration_Ellipsis <|> liftM2 ExceptionDeclaration parseSpecs parse <?> "exception-declaration"

-- A.14 Preprocessing directives [gram.cpp]
