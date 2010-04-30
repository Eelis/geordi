{-# LANGUAGE Rank2Types, FlexibleInstances, UndecidableInstances, RelaxedPolyRec, PatternGuards, OverlappingInstances, CPP #-}

module Cxx.Show (pretty_with_precedence, show_simple, show_plural, show_pretty, Highlighter, Highlightable(..), noHighlighting, dataType_productionName, dataType_abbreviated_productionName) where

import qualified Data.List as List
import qualified Data.Char as Char
import Util ((.), strip)
import Control.Applicative (Applicative(..))
import Data.Generics (cast, Data, gmapQr, ext1Q, dataTypeName, Constr, DataType)
import Data.Foldable (toList)

import Cxx.Basics
import Prelude hiding ((.))
import Prelude.Unicode

instance Show Chunk where
  show (CharLiteral c) = "'" ++ c ++ "'"
  show (StringLiteral' s) = "\"" ++ s ++ "\""
  show (Plain s) = s
  show (Parens c) = "(" ++ show c ++ ")"
  show (Curlies c) = "{" ++ show c ++ "}"
  show (Squares c) = "[" ++ show c ++ "]"
  show (MultiComment s) = "/*" ++ s ++ "*/"
  show (SingleComment s) = "//" ++ s
  showList l s = concat (show . l) ++ s

type PrettyA a = PrettyOptions → a
data ExtraParenOpts = NoExtraParens | ExtraParens | ChildrenExtraParens

data Highlightable = Keyword | CurlyParen | RoundParen | Literal
type Highlighter = Highlightable → (String, String)

wrap :: (String, String) → String → String
wrap (x, y) s = x ++ s ++ y

noHighlighting :: Highlighter
noHighlighting = const ("", "")

data PrettyOptions = PrettyOptions
  { extra_parentheses :: ExtraParenOpts
  , highlighter :: Highlighter
  }

camel_components :: String → [String]
camel_components s = case s of
  (x:xs) → let (y, z) = span Char.isLower xs in (Char.toLower x : y) : camel_components z
  _ → []

camel_to_prod :: String → String
camel_to_prod = concat . List.intersperse "-" . camel_components

abbreviate :: String → String
abbreviate w = case w of
  "expression" → "expr"
  "statement" → "stmt"
  "initializer" → "init"
  "assignment" → "ass"
  "primary" → "prim"
  "qualified" → "qual"
  "qualifier" → "qual"
  "unqualified" → "unqual"
  "destructor" → "dtor"
  "operator" → "op"
  "definition" → "def"
  "function" → "func"
  "specifier" → "spec"
  "specification" → "spec"
  "declaration" → "decl'ion"
  "declarator" → "decl'or"
  "elaborated" → "elab"
  "enumerator" → "enum"
  "abstract" → "abs"
  "member" → "mem"
  "translation" → "trans"
  "parameter" → "param"
  "parameters" → "params"
  "argument" → "arg"
  "arguments" → "args"
  "identifier" → "ident"
  _ → w

dataType_to_camelProd :: DataType → String
dataType_to_camelProd = reverse . takeWhile (≠ '.') . reverse . dataTypeName

dataType_productionName :: DataType → String
dataType_productionName = camel_to_prod . dataType_to_camelProd

dataType_abbreviated_productionName :: DataType → String
dataType_abbreviated_productionName =
  concat . List.intersperse "-" . map abbreviate . camel_components . dataType_to_camelProd

constr_productionName :: Constr → String
constr_productionName = camel_to_prod . show

instance Show Findable where
  show (DeclarationOf d) = "free declaration of " ++ strip (show d)
  show (BodyOf d) = "body of " ++ strip (show d)
  show (FindableDataType dt) = dataType_productionName dt
  show (FindableConstr c) = constr_productionName c
  show Constructor = "free constructor"
  show Destructor = "free destructor"
  show ConversionFunction = "free conversion-function"
  show FindableParameterDeclaration = "parameter-declaration"
  show TemplateParameter = "template-parameter"
  show TemplateArgument = "template-argument"

show_plural :: Findable → String
show_plural (DeclarationOf did) = "free declarations of " ++ strip (show did)
show_plural (BodyOf did) = "bodies of " ++ strip (show did)
show_plural f = show f ++ "s"

-- SingleTokenType instances.

instance SingleTokenType AssignmentOperator where
  token_class_name _ = "binary operator"
  token AssignmentOperator_Assign = Right Is
  token AssignmentOperator_MultiplyAssign = Right StarIs
  token AssignmentOperator_DivideAssign = Right SlashIs
  token AssignmentOperator_PercentAssign = Right PercentIs
  token AssignmentOperator_PlusAssign = Right PlusIs
  token AssignmentOperator_MinusAssign = Right MinusIs
  token AssignmentOperator_RightShiftAssign = Right CloseTwoAnglesIs
  token AssignmentOperator_LeftShiftAssign = Right OpenTwoAnglesIs
  token AssignmentOperator_BitAndAssign = Right AmperIs
  token AssignmentOperator_BitXorAssign = Right HatIs
  token AssignmentOperator_BitOrAssign = Right PipeIs
  token AssignmentOperator_AltBitAndAssign = Left "and_eq"
  token AssignmentOperator_AltBitXorAssign = Left "xor_eq"
  token AssignmentOperator_AltBitOrAssign = Left "or_eq"
instance SingleTokenType ClassKey where
  token_class_name _ = "class-key"
  token Class = Left "class"; token Struct = Left "struct"; token Union = Left "union"
instance SingleTokenType MemberOperator where
  token_class_name _ = "binary-operator"
  token MemberPeriod = Right Period; token MemberArrow = Right Arrow
instance SingleTokenType UnaryOperator where
  token_class_name _ = "unary operator"
  token PrefixDecrement = Right MinusMinus; token PrefixIncrement = Right PlusPlus
  token Dereference' = Right Star; token AddressOf' = Right Amper
  token Negate' = Right MinusTok; token Positive' = Right PlusTok
  token LogicalNot' = Right Exclamation; token AltLogicalNot = Left "not"
  token Complement' = Right Tilde; token AltComplement = Left "compl"
instance SingleTokenType PmOperator where
  token_class_name _ = "binary operator"
  token PmOperator_MemPtr = Right PeriodStar; token PmOperator_PtrMemPtr = Right ArrowStar
instance SingleTokenType MultiplicativeOperator where
  token_class_name _ = "binary operator"
  token MultiplicativeOperator_Multiply = Right Star
  token MultiplicativeOperator_Divide = Right Slash
  token MultiplicativeOperator_Modulo = Right Percent
instance SingleTokenType AdditiveOperator where
  token_class_name _ = "binary operator"
  token AdditiveOperator_Plus = Right PlusTok
  token AdditiveOperator_Minus = Right MinusTok
instance SingleTokenType ShiftOperator where
  token_class_name _ = "binary operator"
  token ShiftOperator_Left = Right OpenTwoAngles; token ShiftOperator_Right = Right CloseTwoAngles
instance SingleTokenType RelationalOperator where
  token_class_name _ = "binary operator"
  token RelationalOperator_Less = Right OpenAngle; token RelationalOperator_Greater = Right CloseAngle
  token RelationalOperator_LessEqual = Right OpenAngleIs; token RelationalOperator_GreaterEqual = Right CloseAngleIs
instance SingleTokenType EqualityOperator where
  token_class_name _ = "binary operator"
  token EqualityOperator_Equal = Right IsIs
  token EqualityOperator_Unequal = Right ExclamationIs
  token EqualityOperator_AltUnequal = Left "not_eq"
instance SingleTokenType NewStyleCast where
  token_class_name _ = "new-style cast"
  token StaticCast = Left "static_cast"; token DynamicCast = Left "dynamic_cast"
  token ReinterpretCast = Left "reinterpret_cast"; token ConstCast = Left "const_cast"
instance SingleTokenType BasicType where
  token_class_name _ = "basic type"
  token Char' = Left "char"; token Wchar = Left "wchar_t"
  token Char32 = Left "char32_t"; token Char16 = Left "char16_t"
  token Void = Left "void"; token Bool' = Left "bool"; token Int' = Left "int"
  token Float' = Left "float"; token Double' = Left "double"
instance SingleTokenType StorageClassSpecifier where
  token_class_name _ = "storage-class-specifier"
  token Register = Left "register"; token Static = Left "static"
  token ThreadLocal = Left "thread_local"; token Extern = Left "extern"
  token Mutable = Left "mutable"
instance SingleTokenType FunctionSpecifier where
  token_class_name _ = "function-specifier"
  token Inline = Left "inline"; token Virtual = Left "virtual"; token Explicit = Left "explicit"
instance SingleTokenType Sign where
  token_class_name _ = "signedness"
  token Signed = Left "signed"; token Unsigned = Left "unsigned"
instance SingleTokenType LengthSpec where
  token_class_name _ = "integer length specifier"
  token LongSpec = Left "long"; token ShortSpec = Left "short"
instance SingleTokenType CvQualifier where
  token_class_name _ = "cv-qualifier"; token Const = Left "const"; token Volatile = Left "volatile"
instance SingleTokenType RefQualifier where
  token_class_name _ = "ref-qualifier"
  token Lvalue = Right Amper; token Rvalue = Right AmperAmper
instance SingleTokenType AccessSpecifier where
  token_class_name _ = "access-specifier"
  token Private = Left "private"; token Protected = Left "protected"; token Public = Left "public"

-- Show instances.

instance SingleTokenType t ⇒ Show t where show = either id show . token
instance Show White where show (White w) = w

pretty :: forall a. (Data a) ⇒ a → PrettyOptions → String
pretty =
  flip ext1Q (\(Enclosed x) → prettyEnclosed $ pretty x) $
  \x → let au = gmapQr (<++>) (pure "") pretty x in case () of

#define KWD(t) ()| Just s ← cast x → \o → wrap (highlighter o Keyword) (show (s :: t));
    KWD(KwdSizeof) KWD(KwdIf) KWD(KwdNew) KWD(KwdAlignof) KWD(KwdTypeid) KWD(KwdTemplate) KWD(KwdClass) KWD(KwdFor) KWD(KwdExtern) KWD(KwdWhile) KWD(KwdDo) KWD(KwdGoto) KWD(KwdContinue) KWD(KwdElse) KWD(KwdAuto) KWD(KwdDefault) KWD(KwdSwitch) KWD(KwdCase) KWD(KwdBreak) KWD(KwdNamespace) KWD(KwdTypedef) KWD(KwdThis) KWD(KwdFriend) KWD(KwdDelete) KWD(KwdStaticAssert) KWD(KwdConstexpr) KWD(KwdExport) KWD(KwdThrow) KWD(KwdUsing) KWD(KwdTry) KWD(KwdDecltype) KWD(KwdAsm) KWD(KwdCatch) KWD(KwdStruct) KWD(KwdEnum) KWD(KwdTypename) KWD(KwdOperator) KWD(KwdReturn) KWD(NewStyleCast) KWD(FunctionSpecifier) KWD(StorageClassSpecifier) KWD(CvQualifier) KWD(LengthSpec) KWD(BasicType) KWD(ClassKey) KWD(StorageClassSpecifier)
#undef KWD

    ()| Just s ← cast x → \o → wrap (highlighter o Literal) (show (s :: Literal))

    ()| Just s ← cast x → \o → wrap (highlighter o CurlyParen) (show (s :: OpenCurly_))
    ()| Just s ← cast x → \o → wrap (highlighter o CurlyParen) (show (s :: CloseCurly_))

    ()| Just s ← cast x → \o → wrap (highlighter o RoundParen) (show (s :: OpenParen_))
    ()| Just s ← cast x → \o → wrap (highlighter o RoundParen) (show (s :: CloseParen_))

#define SHOW(t) ()| Just s ← cast x → pure $ show (s :: t);
    SHOW(SemicolonOperator) SHOW(CommaOp) SHOW(ScopeRes) SHOW(UnaryOperator) SHOW(AssignmentOperator) SHOW(RelationalOperator) SHOW(QuestionOp) SHOW(ColonOp) SHOW(IncDecOperator) SHOW(AdditiveOperator) SHOW(MultiplicativeOperator) SHOW(PmOperator)  SHOW(MemberOperator) SHOW(IsOperator) SHOW(Tilde_) SHOW(Sign) SHOW(OpenSquare_) SHOW(CloseSquare_) SHOW(LeftShiftOp) SHOW(OpenAngle_) SHOW(CloseAngle_) SHOW(AccessSpecifier) SHOW(StarOperator) SHOW(AndOperator) SHOW(LogicalAndOperator) SHOW(LogicalOrOperator) SHOW(ExclusiveOrOperator) SHOW(InclusiveOrOperator) SHOW(EqualityOperator) SHOW(ShiftOperator) SHOW(RefQualifier) SHOW(StringLiteral) SHOW(KwdZero) SHOW(ArrowOp) SHOW(Ellipsis_) SHOW(EncodingPrefix)
#undef SHOW
    ()| Just (Identifier s (White w)) ← cast x → pure $ s ++ w
    ()| Just s ← cast x → pure s
    ()| Just (White w) ← cast x → pure w
    ()| Just (IfStatement w c s me) ← cast x → pretty w <++> pretty c <++> extraCurliesStmt s <++> maybe (pure "") (\(v, s') → pretty v <++> extraCurliesStmt s') me
    ()| Just (UnaryExpression _ _) ← cast x → extraParentheses au
    ()| Just (UnaryExpression_Sizeof_UnaryExpression _ _) ← cast x → extraParentheses au
    ()| Just (NewExpression _ _ _ _ _) ← cast x → extraParentheses au
    ()| Just (DeleteExpression _ _ _ _) ← cast x → extraParentheses au
    ()| Just (ConditionalExpression _ _ _ _ _) ← cast x → extraParentheses au
    ()| Just (LogicalOrExpression _ _ _) ← cast x → extraParentheses au
    ()| Just (CastExpression_Cast _ _) ← cast x → extraParentheses au
    ()| Just (PmExpression _ _ _) ← cast x → extraParentheses au
    ()| Just (MultiplicativeExpression _ _ _) ← cast x → extraParentheses au
    ()| Just (AdditiveExpression _ _ _) ← cast x → extraParentheses au
    ()| Just (ShiftExpression _ _ _) ← cast x → extraParentheses au
    ()| Just (RelationalExpression _ _ _) ← cast x → extraParentheses au
    ()| Just (EqualityExpression _ _ _) ← cast x → extraParentheses au
    ()| Just (AndExpression _ _ _) ← cast x → extraParentheses au
    ()| Just (ExclusiveOrExpression _ _ _) ← cast x → extraParentheses au
    ()| Just (InclusiveOrExpression _ _ _) ← cast x → extraParentheses au
    ()| Just (LogicalAndExpression _ _ _) ← cast x → extraParentheses au
    ()| Just (AssignmentExpression _ _ _) ← cast x → extraParentheses au
    ()| Just (PostfixExpression_Squared _ _) ← cast x → extraParentheses au
    ()| Just (PostfixExpression_FunctionCall _ _) ← cast x → extraParentheses au
    ()| Just (PostfixExpression_Conversion _ _) ← cast x → extraParentheses au
    ()| Just (PostfixExpression_Member _ _ _ _) ← cast x → extraParentheses au
    ()| Just (PostfixExpression_IncDec _ _) ← cast x → extraParentheses au
    ()| Just (Expression_Comma _ _ _) ← cast x → extraParentheses au
    ()| Just (ThrowExpression _ (Just _)) ← cast x → extraParentheses au
    ()| otherwise → au
  -- This pretty is very slow, probably due to all the list concatenations. Use the ShowS trick.

extraCurliesStmt :: Statement → PrettyA String
extraCurliesStmt (Statement_CompoundStatement s) = pretty s
extraCurliesStmt s = extraCurlies (pretty s)

pretty_with_precedence :: Data a ⇒ a → String
pretty_with_precedence e = pretty e PrettyOptions{ extra_parentheses = ChildrenExtraParens, highlighter = noHighlighting }

show_simple :: Data a ⇒ a → String
show_simple x = pretty x PrettyOptions{ extra_parentheses = NoExtraParens, highlighter = noHighlighting }

prettyEnclosed :: PrettyA String → PrettyA String
prettyEnclosed x o | ExtraParens ← extra_parentheses o = x (o { extra_parentheses = ChildrenExtraParens })
prettyEnclosed x o = x o

show_pretty :: Data a ⇒ Bool → Highlighter → a → String
show_pretty x y z = pretty z $ PrettyOptions (if x then ChildrenExtraParens else NoExtraParens) y

extraWrapping :: String → String → PrettyA String → PrettyA String
extraWrapping open close p o | ExtraParens ← extra_parentheses o =
  open ++ reverse y ++ close ++ x where (x, y) = span (== ' ') (reverse $ p o)
extraWrapping _ _ p o = p o

extraParentheses, extraCurlies :: PrettyA String → PrettyA String
extraParentheses = extraWrapping "(" ")"
extraCurlies = extraWrapping "{ " " }"

(<++>) :: PrettyA [a] → PrettyA [a] → PrettyA [a]
(<++>) x y o = x (if not $ null $ y o then o' else o) ++ y (if not $ null $ x o then o' else o)
  where o' = o { extra_parentheses = case extra_parentheses o of ChildrenExtraParens → ExtraParens; z → z }

instance Show Literal where
  show (Literal_CharacterLiteral l w) = show l ++ show w
  show (Literal_StringLiteral l) = show l
  show (Literal_FloatingLiteral l w) = show l ++ show w; show (Literal_IntegerLiteral l w) = show l ++ show w
  show (BooleanLiteral b w) = (if b then "true" else "false") ++ show w
  show (PointerLiteral w) = "nullptr" ++ show w
instance Show CharacterLiteral where show (CharacterLiteral k s) = show k ++ '\'' : s ++ "'"
instance Show SingleStringLiteral where show (SingleStringLiteral k s) = maybe "" show k ++ '"' : s ++ "\""
instance Show StringLiteral where show (StringLiteral l) = concatMap (\(x, y) → show x ++ show y) $ toList l
instance Show IntegerLiteral where show (IntegerLiteral s) = s
instance Show CharacterLiteralKind where
  show CharacterLiteral_Plain = ""; show CharacterLiteralKind_u = "u"
  show CharacterLiteralKind_U = "U"; show CharacterLiteralKind_L = "L"
instance Show FloatingLiteral where show (FloatingLiteral s) = s
instance Show EncodingPrefix where
  show EncodingPrefix_L = "L"; show EncodingPrefix_u8 = "u8"
  show EncodingPrefix_u = "u"; show EncodingPrefix_U = "U"

instance Show DeclaratorId where show = Cxx.Show.show_simple
