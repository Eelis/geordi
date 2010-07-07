{-# LANGUAGE DeriveDataTypeable, CPP #-}

{- Notes:

  * While we could leave things like static_assert and the new-style casts out of the grammar and let them be parsed as ordinary function calls, we incorporate them because recognizing them lets us disambiguate types from expressions.

  * (Either a b) is not equivalent to (Either b a) in grammar rule definitions, because we use a Parse (Either a b) instance which tries the left side first.

  * The Eq instances are all modulo whitespace.

  * We diverge from the grammar in the standard by "flattening" *-list and *-seq. In our parse trees, their recursion will be factored out, so that they always appear as a single node in paths, and can only be found as a whole in edit commands.

Missing:

  preprocessor stuff
  internal structure in literals
  alternative tokens
  backslash handling
  C++1x stuff:
    opaque-enum declarations
    range-for
    raw literals
    user-defined literals
    attributes
    variadic templates
    lambda expressions
    "->"-style declarators
    ellipsis in various lists
-}

module Cxx.Basics where

import Data.Generics (Data, Typeable, DataType, Constr)
import Util (Phantom(..), TriBool(..), NeList)
import Control.Arrow (second)

relational_ops, accessSpecifiers, classKeys, basic_simple_type_specifiers, casts, keywords, make_type_keywords, arithmetic_ops, ops, long_ops :: [String]
relational_ops = words "< > <= >= == !="
arithmetic_ops = concatMap (\x → [x, x ++ "="]) $ words "+ - * / << >> | & ^"
ops = relational_ops ++ arithmetic_ops ++ words "++ -- -> .* :: && || ! = ~ [ ] ( ) { } :"
long_ops = filter ((>1) . length) ops

accessSpecifiers = words "public private protected"
classKeys = words "class struct union"
basic_simple_type_specifiers = words "short auto bool double int signed unsigned void char wchar_t char32_t float long char16_t"
casts = words "reinterpret_cast dynamic_cast static_cast const_cast"
keywords = accessSpecifiers ++ classKeys ++ basic_simple_type_specifiers ++ casts ++ words "alignas continue friend typedef alignof decltype goto return typeid asm default if typename delete inline sizeof break do static_assert using case mutable virtual catch else namespace static enum new volatile explicit nullptr switch export operator template while extern this const false throw constexpr true for register try define elif include defined"

make_type_keywords = words "function functions pointer pointers reference references returning and lvalue rvalue constant chars character characters floats doubles bools booleans ints integer integers array arrays of from to noninline nonvirtual nonexplicit nonmutable nonstatic nonpure pure impure nonconst nonvolatile implicit nonlong nonshort nonsigned nonunsigned"

-- Todo: The stuff above is old, and could be expressed more properly.

data Findable = FindableDataType DataType | FindableConstr Constr | BodyOf DeclaratorId | DeclarationOf DeclaratorId | Constructor | Destructor | ConversionFunction | FindableParameterDeclaration | TemplateParameter | TemplateArgument
  -- Todo: ParameterDeclarationOf, TemplateParameterOf, ParameterDeclarationClauseOf, similar to BodyOf. Would be nice to do it in a generic fashion.

data ShortCode
  = LongForm Code
  | Print Code Code -- Does not include the semicolon.
  | Block Code Code -- Does not include Curlies.

data Chunk
  = CharLiteral String | StringLiteral' String
  | SingleComment String | MultiComment String
  | Curlies Code | Parens Code | Squares Code
  | Plain String
  deriving Eq

type Code = [Chunk]

-- Misc/Util

instance Show OperatorTok where
  show PlusTok = "+"; show PlusIs = "+="; show MinusTok = "-"; show MinusIs = "-="
  show Star = "*"; show StarIs = "*="; show Slash = "/"; show SlashIs = "/="
  show PlusPlus = "++"; show MinusMinus = "--"
  show OpenAngle = "<"; show OpenAngleIs = "<="; show CloseAngle = ">"; show CloseAngleIs = ">="
  show OpenTwoAngles = "<<"; show OpenTwoAnglesIs = "<<="
  show CloseTwoAngles = ">>"; show CloseTwoAnglesIs = ">>="
  show OpenCurly = "{"; show CloseCurly = "}"; show OpenSquare = "["; show CloseSquare = "]"
  show OpenParen = "("; show CloseParen = ")"; show Tilde = "~"
  show Question = "?"; show Exclamation = "!"; show ExclamationIs = "!="
  show Hat = "^"; show HatIs = "^="; show Percent = "%"; show PercentIs = "%="
  show Colon = ":"; show ColonColon = "::"; show Semicolon = ";"; show CommaTok = ","
  show Amper = "&"; show AmperIs = "&="; show AmperAmper = "&&"
  show Pipe = "|"; show PipeIs = "|="; show PipePipe = "||"
  show Is = "="; show IsIs = "=="; show Period = "."; show PeriodStar = ".*"
  show Arrow = "->"; show ArrowStar = "->*"; show Ellipsis = "..."

operatorTokName :: OperatorTok → String
operatorTokName Colon = "colon"
operatorTokName Semicolon = "semicolon"
operatorTokName CommaTok = "comma"
operatorTokName Ellipsis = "ellipsis"
operatorTokName o = '"' : show o ++ "\""

data SingleTokenType t ⇒ Kwd' t = Kwd' t White deriving (Data, Typeable)
class SingleTokenType t where
  token :: t → Either String OperatorTok
  token_class_name :: Phantom t → String
  token_class_name = const ""

#define KWD(x, n) \
  data x = x deriving (Data, Typeable, Enum, Bounded, Eq); \
  instance SingleTokenType x where token_class_name _ = show n; token _ = Left n

KWD(KwdZero, "0")
KWD(KwdGoto, "goto")
KWD(KwdBreak, "break")
KWD(KwdContinue, "continue")
KWD(KwdIf, "if")
KWD(KwdElse, "else")
KWD(KwdDo, "do")
KWD(KwdWhile, "while")
KWD(KwdReturn, "return")
KWD(KwdFor, "for")
KWD(KwdTemplate, "template")
KWD(KwdAlignof, "alignof")
KWD(KwdAlignas, "alignas")
KWD(KwdSwitch, "switch")
KWD(KwdTypename, "typename")
KWD(KwdTypeid, "typeid")
KWD(KwdOperator, "operator")
KWD(KwdInline, "inline")
KWD(KwdNew, "new")
KWD(KwdClass, "class")
KWD(KwdEnum, "enum")
KWD(KwdStruct, "struct")
KWD(KwdSizeof, "sizeof")
KWD(KwdDelete, "delete")
KWD(KwdVirtual, "virtual")
KWD(KwdDefault, "default")
KWD(KwdCase, "case")
KWD(KwdStaticAssert, "static_assert")
KWD(KwdUsing, "using")
KWD(KwdThis, "this")
KWD(KwdNamespace, "namespace")
KWD(KwdFriend, "friend")
KWD(KwdConstexpr, "constexpr")
KWD(KwdDecltype, "decltype")
KWD(KwdAuto, "auto")
KWD(KwdTypedef, "typedef")
KWD(KwdAsm, "asm")
KWD(KwdExtern, "extern")
KWD(KwdExport, "export")
KWD(KwdTry, "try")
KWD(KwdCatch, "catch")
KWD(KwdThrow, "throw")

#undef KWD

#define OP(n, t) \
  data n = n deriving (Data, Typeable, Enum, Bounded, Eq); \
  instance SingleTokenType n where token_class_name _ = operatorTokName t; token _ = Right t

OP(OpenAngle_, OpenAngle)
OP(OpenCurly_, OpenCurly)
OP(OpenParen_, OpenParen)
OP(OpenSquare_, OpenSquare)
OP(CloseAngle_, CloseAngle)
OP(CloseCurly_, CloseCurly)
OP(CloseParen_, CloseParen)
OP(CloseSquare_, CloseSquare)
OP(ColonOp, Colon)
OP(CommaOp, CommaTok)
OP(ArrowOp, Arrow)
OP(SemicolonOperator, Semicolon)
OP(StarOperator, Star)
OP(LeftShiftOp, OpenTwoAngles)
OP(IsOperator, Is)
OP(Tilde_, Tilde)
OP(Ellipsis_, Ellipsis)
OP(ScopeRes, ColonColon)

#undef OP

data AndOperator = AndOperator | AltAndOperator deriving (Data, Typeable, Enum, Bounded, Eq)
instance SingleTokenType AndOperator where
  token_class_name _ = "and-operator"
  token AndOperator = Right Amper
  token AltAndOperator = Left "bitand"

data LogicalOrOperator = LogicalOrOperator | AltLogicalOrOperator deriving (Data, Typeable, Enum, Bounded, Eq)
instance SingleTokenType LogicalOrOperator where
  token_class_name _ = "logical-or-operator"
  token LogicalOrOperator = Right PipePipe
  token AltLogicalOrOperator = Left "or"

data InclusiveOrOperator = InclusiveOrOperator | AltInclusiveOrOperator deriving (Data, Typeable, Enum, Bounded, Eq)
instance SingleTokenType InclusiveOrOperator where
  token_class_name _ = "inclusive-or-operator"
  token InclusiveOrOperator = Right Pipe
  token AltInclusiveOrOperator = Left "bitor"

data ExclusiveOrOperator = ExclusiveOrOperator | AltExclusiveOrOperator deriving (Data, Typeable, Enum, Bounded, Eq)
instance SingleTokenType ExclusiveOrOperator where
  token_class_name _ = "exclusive-or-operator"
  token ExclusiveOrOperator = Right Hat
  token AltExclusiveOrOperator = Left "xor"

data LogicalAndOperator = LogicalAndOperator | AltLogicalAndOperator deriving (Data, Typeable, Enum, Bounded, Eq)
instance SingleTokenType LogicalAndOperator where
  token_class_name _ = "logical-and-operator"
  token LogicalAndOperator = Right AmperAmper
  token AltLogicalAndOperator = Left "and"

data QuestionOp = QuestionOp deriving (Data, Typeable, Enum, Bounded, Eq)
instance SingleTokenType QuestionOp where token_class_name _ = "ternary operator"; token _ = Right Question
data IncDecOperator = IncrementOperator | DecrementOperator deriving (Data, Typeable, Enum, Bounded, Eq)
instance SingleTokenType IncDecOperator where
  token IncrementOperator = Right PlusPlus; token DecrementOperator = Right MinusMinus

data Enclosed a = Enclosed a deriving (Typeable, Eq, Data)
  -- Things wrapped in enclosed will not be wrapped in parentheses when shown for --precedence.

data Commad a = Commad a [((CommaOp, White), a)] deriving (Data, Typeable, Eq)
data Angled a = Angled (OpenAngle_, White) (Enclosed a) (CloseAngle_, White) deriving (Data, Typeable, Eq)
data Squared a = Squared (OpenSquare_, White) (Enclosed a) (CloseSquare_, White) deriving (Data, Typeable, Eq)
data Curlied a = Curlied (OpenCurly_, White) (Enclosed a) (CloseCurly_, White) deriving (Data, Typeable, Eq)
data Parenthesized a = Parenthesized (OpenParen_, White) (Enclosed a) (CloseParen_, White) deriving (Data, Typeable, Eq)
newtype White = White String deriving (Data, Typeable)
instance Eq White where _ == _ = True

instance Functor Commad where fmap f (Commad x l) = Commad (f x) $ map (second f) l

data AnyMixOf a b = MixNone | MixA a | MixB b | MixAB a b | MixBA b a deriving (Eq, Data, Typeable)

data OptQualified = OptQualified (Maybe (ScopeRes, White)) (Maybe NestedNameSpecifier) deriving (Data, Typeable, Eq)

data GeordiRequestWithoutWhite = GeordiRequest_TU TranslationUnit | GeordiRequest_Print (LeftShiftOp, White) AdditiveExpression (Maybe ((SemicolonOperator, White), TranslationUnit)) | GeordiRequest_Block FunctionBody TranslationUnit deriving (Data, Typeable)

type GeordiRequest = (White, GeordiRequestWithoutWhite)

data MakeSpecifier = NonStorageClassSpecifier StorageClassSpecifier | NonFunctionSpecifier FunctionSpecifier | MakeSpecifier_DeclSpecifier DeclSpecifier | NonSign Sign | NonLength LengthSpec | NonCv CvQualifier | LongLong deriving (Data, Typeable, Eq)
data MakeDeclaration = MakeDeclaration [MakeSpecifier] (Maybe PtrAbstractDeclarator) TriBool deriving (Data, Typeable)
  -- The tribool indicates purity.

data OperatorTok =
  PlusTok | PlusIs | MinusTok | MinusIs | PlusPlus | MinusMinus | Star | StarIs | Slash | SlashIs |
  OpenAngle | OpenAngleIs | CloseAngle | CloseAngleIs | OpenTwoAngles | CloseTwoAngles | OpenTwoAnglesIs | CloseTwoAnglesIs | OpenCurly | CloseCurly | OpenSquare | CloseSquare | OpenParen | CloseParen | Question | Colon | ColonColon | Semicolon | Amper | AmperIs | AmperAmper | Pipe | PipeIs | PipePipe | Percent | PercentIs | Hat | HatIs | Exclamation | ExclamationIs | Is | IsIs | Tilde | Arrow | ArrowStar | Period | PeriodStar | CommaTok | Ellipsis
 deriving (Eq, Enum, Bounded, Data, Typeable)

data NewStyleCast = DynamicCast | StaticCast | ReinterpretCast | ConstCast deriving (Bounded, Enum, Data, Typeable, Eq)
data MemberOperator = MemberPeriod | MemberArrow deriving (Enum, Bounded, Data, Typeable, Eq)
data PmOperator = PmOperator_MemPtr | PmOperator_PtrMemPtr deriving (Enum, Bounded, Data, Typeable, Eq)
data MultiplicativeOperator = MultiplicativeOperator_Multiply | MultiplicativeOperator_Divide | MultiplicativeOperator_Modulo deriving (Enum, Bounded, Data, Typeable, Eq)
data AdditiveOperator = AdditiveOperator_Plus | AdditiveOperator_Minus deriving (Bounded, Enum, Data, Typeable, Eq)
data ShiftOperator = ShiftOperator_Left | ShiftOperator_Right deriving (Bounded, Enum, Data, Typeable, Eq)
data RelationalOperator = RelationalOperator_Less | RelationalOperator_Greater | RelationalOperator_LessEqual | RelationalOperator_GreaterEqual deriving (Bounded, Enum, Data, Typeable, Eq)
data EqualityOperator = EqualityOperator_Equal | EqualityOperator_Unequal | EqualityOperator_AltUnequal deriving (Bounded, Enum, Data, Typeable, Eq)

-- A.1 Keywords [gram.key]

newtype TypedefName = TypedefName Identifier deriving (Data, Typeable, Eq)
data NamespaceName = NamespaceName_OriginalNamespaceName OriginalNamespaceName | NamespaceName_NamespaceAlias NamespaceAlias deriving (Data, Typeable, Eq)
newtype OriginalNamespaceName = OriginalNamespaceName Identifier deriving (Data, Typeable, Eq)
newtype NamespaceAlias = NamespaceAlias Identifier deriving (Data, Typeable, Eq)
data ClassName = ClassName_Identifier Identifier | ClassName_TemplateId TemplateId deriving (Data, Typeable, Eq)
newtype EnumName = EnumName Identifier deriving (Data, Typeable, Eq)
newtype TemplateName = TemplateName Identifier deriving (Data, Typeable, Eq)

-- A.2 Lexical conventions [gram.lex]

data Identifier = Identifier String White deriving (Data, Typeable, Eq)
data Literal = Literal_IntegerLiteral IntegerLiteral White | Literal_CharacterLiteral CharacterLiteral White | Literal_FloatingLiteral FloatingLiteral White | Literal_StringLiteral StringLiteral | BooleanLiteral Bool White | PointerLiteral White deriving (Data, Typeable, Eq)
newtype IntegerLiteral = IntegerLiteral String deriving (Data, Typeable, Eq)
data CharacterLiteralKind = CharacterLiteral_Plain | CharacterLiteralKind_u | CharacterLiteralKind_U | CharacterLiteralKind_L deriving (Data, Typeable, Eq)
data CharacterLiteral = CharacterLiteral CharacterLiteralKind String deriving (Data, Typeable, Eq)
newtype FloatingLiteral = FloatingLiteral String deriving (Data, Typeable, Eq)
data EncodingPrefix = EncodingPrefix_u8 | EncodingPrefix_u | EncodingPrefix_U | EncodingPrefix_L deriving (Data, Typeable, Eq)
data SingleStringLiteral = SingleStringLiteral (Maybe EncodingPrefix) String deriving (Data, Typeable, Eq)
data StringLiteral = StringLiteral (NeList (SingleStringLiteral, White)) deriving (Data, Typeable, Eq)

-- A.3 Basic concepts [gram.basic]

newtype TranslationUnit = TranslationUnit (Maybe DeclarationSeq) deriving (Data, Typeable)

-- A.4 Expressions [gram.expr]

data PrimaryExpression = PrimaryExpression_Literal Literal | PrimaryExpression_This (KwdThis, White) | PrimaryExpression_Expression (Parenthesized Expression) | PrimaryExpression_IdExpression IdExpression deriving (Data, Typeable, Eq)
newtype IdExpression = IdExpression (Either QualifiedId UnqualifiedId) deriving (Data, Typeable, Eq)
data UnqualifiedId = UnqualifiedId_Identifier Identifier | UnqualifiedId_OperatorFunctionId OperatorFunctionId | UnqualifiedId_ConversionFunctionId ConversionFunctionId | UnqualifiedId_Destructor (Tilde_, White) ClassName | UnqualifiedId_TemplateId TemplateId deriving (Data, Typeable, Eq)
data QualifiedId = NestedUnqualifiedId (Maybe (ScopeRes, White)) NestedNameSpecifier (Maybe (KwdTemplate, White)) UnqualifiedId | GlobalIdentifier (ScopeRes, White) Identifier | GlobalOperatorFunctionId (ScopeRes, White) OperatorFunctionId | GlobalTemplateId (ScopeRes, White) TemplateId deriving (Data, Typeable, Eq)
data NestedNameSpecifier = NestedNameSpecifier_TypeName TypeName (ScopeRes, White) | NestedNameSpecifier_NamespaceName NamespaceName (ScopeRes, White) | NestedNameSpecifier_Identifier NestedNameSpecifier Identifier (ScopeRes, White) | NestedNameSpecifier_SimpleTemplateId NestedNameSpecifier (Maybe White) SimpleTemplateId (ScopeRes, White) deriving (Data, Typeable, Eq)
data PostfixExpression
  = PostfixExpression_PrimaryExpression PrimaryExpression
  | PostfixExpression_Squared PostfixExpression (Squared (Either Expression BracedInitList))
  | PostfixExpression_FunctionCall PostfixExpression (Parenthesized (Maybe ExpressionList))
  | PostfixExpression_Conversion (Either SimpleTypeSpecifier TypenameSpecifier) (Either (Parenthesized (Maybe ExpressionList)) BracedInitList)
  | PostfixExpression_Member PostfixExpression (MemberOperator, White) (Maybe (KwdTemplate, White)) IdExpression
  | PostfixExpression_PseudoDestructor PostfixExpression (MemberOperator, White) PseudoDestructorName
  | PostfixExpression_IncDec PostfixExpression (IncDecOperator, White)
  | PostfixExpression_NewStyleCast (NewStyleCast, White) (Angled TypeId) (Parenthesized Expression)
  | PostfixExpression_TypeId (KwdTypeid, White) (Parenthesized (Either Expression TypeId))
  deriving (Data, Typeable, Eq)
newtype ExpressionList = ExpressionList InitializerList deriving (Data, Typeable, Eq)
data PseudoDestructorName = PseudoDestructorName_InTypeName OptQualified TypeName (ScopeRes, White) (Tilde_, White) TypeName | PseudoDestructorName_InTemplate (Maybe (ScopeRes, White)) NestedNameSpecifier (KwdTemplate, White) SimpleTemplateId (ScopeRes, White) (Tilde_, White) TypeName | PseudoDestructorName OptQualified (Tilde_, White) TypeName deriving (Data, Typeable, Eq)
data UnaryExpression = UnaryExpression_PostfixExpression PostfixExpression | UnaryExpression (UnaryOperator, White) CastExpression | UnaryExpression_Sizeof_UnaryExpression (KwdSizeof, White) UnaryExpression | UnaryExpression_Sizeof_TypeId (KwdSizeof, White) (Parenthesized TypeId) | UnaryExpression_Sizeof_Ellipsis (KwdSizeof, White) (Ellipsis_, White) (Parenthesized Identifier) | UnaryExpression_AlignOf (KwdAlignof, White) (Parenthesized TypeId) | UnaryExpression_NewExpression NewExpression | UnaryExpression_DeleteExpression DeleteExpression deriving (Data, Typeable, Eq)
data UnaryOperator = Dereference' | AddressOf' | Negate' | Positive' | LogicalNot' | AltLogicalNot | Complement' | AltComplement | PrefixIncrement | PrefixDecrement deriving (Bounded, Enum, Data, Typeable, Eq)
data NewExpression = NewExpression (Maybe (ScopeRes, White)) (KwdNew, White) (Maybe NewPlacement) (Either NewTypeId (Parenthesized TypeId)) (Maybe NewInitializer) deriving (Data, Typeable, Eq)
newtype NewPlacement = NewPlacement (Parenthesized ExpressionList) deriving (Data, Typeable, Eq)
data NewTypeId = NewTypeId TypeSpecifierSeq (Maybe NewDeclarator) deriving (Data, Typeable, Eq)
data NewDeclarator = NewDeclarator_PtrOperator PtrOperator (Maybe NewDeclarator) | NewDeclarator_NoptrNewDeclarator NoptrNewDeclarator deriving (Data, Typeable, Eq)
data NoptrNewDeclarator = NoptrNewDeclarator (Squared Expression) [Squared ConstantExpression] deriving (Data, Typeable, Eq)
data NewInitializer = NewInitializer_ExpressionList (Parenthesized (Maybe ExpressionList)) | NewInitializer_BracedInitList BracedInitList deriving (Data, Typeable, Eq)
data DeleteExpression = DeleteExpression (Maybe (ScopeRes, White)) (KwdDelete, White) (Maybe (Squared ())) CastExpression deriving (Data, Typeable, Eq)
data CastExpression = CastExpression_UnaryExpression UnaryExpression | CastExpression_Cast (Parenthesized TypeId) CastExpression deriving (Data, Typeable, Eq)
data PmExpression = PmExpression_CastExpression CastExpression | PmExpression PmExpression (PmOperator, White) CastExpression deriving (Data, Typeable, Eq)
data MultiplicativeExpression = MultiplicativeExpression_PmExpression PmExpression | MultiplicativeExpression MultiplicativeExpression (MultiplicativeOperator, White) PmExpression deriving (Data, Typeable, Eq)
data AdditiveExpression = AdditiveExpression_MultiplicativeExpression MultiplicativeExpression | AdditiveExpression AdditiveExpression (AdditiveOperator, White) MultiplicativeExpression deriving (Data, Typeable, Eq)
data ShiftExpression = ShiftExpression_AdditiveExpression AdditiveExpression | ShiftExpression ShiftExpression (ShiftOperator, White) AdditiveExpression deriving (Data, Typeable, Eq)
data RelationalExpression = RelationalExpression_ShiftExpression ShiftExpression | RelationalExpression RelationalExpression (RelationalOperator, White) ShiftExpression deriving (Data, Typeable, Eq)
data EqualityExpression = EqualityExpression_RelationalExpression RelationalExpression | EqualityExpression EqualityExpression (EqualityOperator, White) RelationalExpression deriving (Data, Typeable, Eq)
data AndExpression = AndExpression_EqualityExpression EqualityExpression | AndExpression AndExpression (AndOperator, White) EqualityExpression deriving (Data, Typeable, Eq)
data ExclusiveOrExpression = ExclusiveOrExpression_AndExpression AndExpression | ExclusiveOrExpression ExclusiveOrExpression (ExclusiveOrOperator, White) AndExpression deriving (Data, Typeable, Eq)
data InclusiveOrExpression = InclusiveOrExpression_ExclusiveOrExpression ExclusiveOrExpression | InclusiveOrExpression InclusiveOrExpression (InclusiveOrOperator, White) ExclusiveOrExpression deriving (Data, Typeable, Eq)
data LogicalAndExpression = LogicalAndExpression_InclusiveOrExpression InclusiveOrExpression | LogicalAndExpression LogicalAndExpression (LogicalAndOperator, White) InclusiveOrExpression deriving (Data, Typeable, Eq)
data LogicalOrExpression = LogicalOrExpression_LogicalAndExpression LogicalAndExpression | LogicalOrExpression LogicalOrExpression (LogicalOrOperator, White) LogicalAndExpression deriving (Data, Typeable, Eq)
data ConditionalExpression = ConditionalExpression_LogicalOrExpression LogicalOrExpression | ConditionalExpression LogicalOrExpression (QuestionOp, White) Expression (ColonOp, White) AssignmentExpression deriving (Data, Typeable, Eq)
data AssignmentExpression = AssignmentExpression_ConditionalExpression ConditionalExpression | AssignmentExpression LogicalOrExpression (AssignmentOperator, White) InitializerClause | AssignmentExpression_ThrowExpression ThrowExpression deriving (Data, Typeable, Eq)
data AssignmentOperator = AssignmentOperator_Assign | AssignmentOperator_MultiplyAssign | AssignmentOperator_DivideAssign | AssignmentOperator_PercentAssign | AssignmentOperator_PlusAssign | AssignmentOperator_MinusAssign | AssignmentOperator_RightShiftAssign | AssignmentOperator_LeftShiftAssign | AssignmentOperator_BitAndAssign | AssignmentOperator_AltBitAndAssign | AssignmentOperator_BitXorAssign | AssignmentOperator_AltBitXorAssign | AssignmentOperator_BitOrAssign | AssignmentOperator_AltBitOrAssign deriving (Bounded, Enum, Data, Typeable, Eq)
data Expression = Expression_AssignmentExpression AssignmentExpression | Expression_Comma Expression (CommaOp, White) AssignmentExpression deriving (Data, Typeable, Eq)
newtype ConstantExpression = ConstantExpression ConditionalExpression deriving (Data, Typeable, Eq)

-- A.5 Statements [gram.stmt]

data Statement = Statement_Labeled LabeledStatement | Statement_ExpressionStatement ExpressionStatement | Statement_DeclarationStatement DeclarationStatement | Statement_CompoundStatement CompoundStatement | Statement_SelectionStatement SelectionStatement | Statement_IterationStatement IterationStatement | Statement_JumpStatement JumpStatement | Statement_TryBlock TryBlock deriving (Data, Typeable, Eq)
data Label = IdentifierLabel Identifier | CaseLabel (KwdCase, White) ConstantExpression | DefaultLabel (KwdDefault, White) deriving (Data, Typeable, Eq)
data LabeledStatement = LabeledStatement Label (ColonOp, White) Statement deriving (Data, Typeable, Eq)
data ExpressionStatement = ExpressionStatement (Maybe Expression) (SemicolonOperator, White) deriving (Data, Typeable, Eq)
newtype CompoundStatement = CompoundStatement (Curlied (Maybe StatementSeq)) deriving (Data, Typeable, Eq)
newtype StatementSeq = StatementSeq (NeList Statement) deriving (Data, Typeable, Eq)
data SelectionStatement = IfStatement (KwdIf, White) (Parenthesized Condition) Statement (Maybe ((KwdElse, White), Statement)) | SwitchStatement (KwdSwitch, White) (Parenthesized Condition) Statement deriving (Data, Typeable, Eq)
data Condition = Condition_Expression Expression | Condition_Declaration TypeSpecifierSeq Declarator BraceOrEqualInitializer deriving (Data, Typeable, Eq)
  -- Why doesn't the draft use brace-or-equal-initializer here?
data IterationStatement = WhileStatement (KwdWhile, White) (Parenthesized Condition) Statement | DoWhileStatement (KwdDo, White) Statement (KwdWhile, White) (Parenthesized Expression) (SemicolonOperator, White) | ForStatement (KwdFor, White) (Parenthesized (ForInitStatement, Maybe Condition, (SemicolonOperator, White), Maybe Expression)) Statement deriving (Data, Typeable, Eq)
data ForInitStatement = ForInitStatement_ExpressionStatement ExpressionStatement | ForInitStatement_SimpleDeclaration SimpleDeclaration deriving (Data, Typeable, Eq)
data JumpStatement = BreakStatement (KwdBreak, White) (SemicolonOperator, White) | ContinueStatement (KwdContinue, White) (SemicolonOperator, White) | ReturnStatement (KwdReturn, White) (Maybe Expression) (SemicolonOperator, White) | GotoStatement (KwdGoto, White) Identifier (SemicolonOperator, White) deriving (Data, Typeable, Eq)
data DeclarationStatement = DeclarationStatement BlockDeclaration deriving (Data, Typeable, Eq)

-- A.6 Declarations [gram.dcl]

newtype DeclarationSeq = DeclarationSeq (NeList Declaration) deriving (Data, Typeable, Eq)
data Declaration = Declaration_BlockDeclaration BlockDeclaration | Declaration_FunctionDefinition FunctionDefinition | Declaration_TemplateDeclaration TemplateDeclaration | Declaration_ExplicitInstantiation ExplicitInstantiation | Declaration_ExplicitSpecialization ExplicitSpecialization | Declaration_LinkageSpecification LinkageSpecification | Declaration_NamespaceDefinition NamespaceDefinition deriving (Data, Typeable, Eq)
data BlockDeclaration = BlockDeclaration_SimpleDeclaration SimpleDeclaration | BlockDeclaration_AsmDefinition AsmDefinition | BlockDeclaration_NamespaceAliasDefinition NamespaceAliasDefinition | BlockDeclaration_UsingDeclaration UsingDeclaration | BlockDeclaration_UsingDirective UsingDirective | BlockDeclaration_StaticAssertDeclaration StaticAssertDeclaration | BlockDeclaration_AliasDeclaration AliasDeclaration deriving (Data, Typeable, Eq)
data AliasDeclaration = AliasDeclaration (KwdUsing, White) Identifier (IsOperator, White) TypeId (SemicolonOperator, White) deriving (Data, Typeable, Eq)
data SimpleDeclaration = SimpleDeclaration (Maybe DeclSpecifierSeq) (Maybe InitDeclaratorList) (SemicolonOperator, White) deriving (Data, Typeable, Eq)
data StaticAssertDeclaration = StaticAssertDeclaration (KwdStaticAssert, White) (Parenthesized (ConstantExpression, (CommaOp, White), StringLiteral)) (SemicolonOperator, White) deriving (Data, Typeable, Eq)
data DeclSpecifier = DeclSpecifier_StorageClassSpecifier (StorageClassSpecifier, White) | DeclSpecifier_TypeSpecifier TypeSpecifier | DeclSpecifier_FunctionSpecifier (FunctionSpecifier, White) | DeclSpecifier_Friend (KwdFriend, White) | DeclSpecifier_Typedef (KwdTypedef, White) | DeclSpecifier_ConstExpr (KwdConstexpr, White) | DeclSpecifier_AlignmentSpecifier AlignmentSpecifier deriving (Data, Typeable, Eq)
newtype DeclSpecifierSeq = DeclSpecifierSeq (NeList DeclSpecifier) deriving (Data, Typeable, Eq)
data StorageClassSpecifier = Register | Static | ThreadLocal | Extern | Mutable deriving (Enum, Bounded, Data, Typeable, Eq)
data FunctionSpecifier = Inline | Virtual | Explicit deriving (Enum, Bounded, Data, Typeable, Eq)
data Sign = Signed | Unsigned deriving (Eq, Bounded, Enum, Data, Typeable)
data LengthSpec = ShortSpec | LongSpec deriving (Eq, Bounded, Enum, Data, Typeable)
data BasicType = Char' | Char16 | Char32 | Wchar | Bool' | Int' | Float' | Double' | Void deriving (Enum, Bounded, Data, Typeable, Eq)
data TypeSpecifier = TypeSpecifier_SimpleTypeSpecifier SimpleTypeSpecifier | TypeSpecifier_ClassSpecifier ClassSpecifier | TypeSpecifier_EnumSpecifier EnumSpecifier | TypeSpecifier_ElaboratedTypeSpecifier ElaboratedTypeSpecifier | TypeSpecifier_TypenameSpecifier TypenameSpecifier | TypeSpecifier_CvQualifier (CvQualifier, White) deriving (Data, Typeable, Eq)
newtype TypeSpecifierSeq = TypeSpecifierSeq (NeList TypeSpecifier) deriving (Data, Typeable, Eq)
data SimpleTypeSpecifier = SimpleTypeSpecifier_BasicType (BasicType, White) | SimpleTypeSpecifier_Auto (KwdAuto, White) | SimpleTypeSpecifier_DeclType (KwdDecltype, White) (Parenthesized Expression) | LengthSpec (LengthSpec, White) | SignSpec (Sign, White) | SimpleTypeSpecifier_TypeName OptQualified TypeName | SimpleTypeSpecifier_SimpleTemplateId (Maybe (ScopeRes, White)) NestedNameSpecifier White SimpleTemplateId deriving (Data, Typeable, Eq)
data TypeName = TypeName_ClassName ClassName | TypeName_EnumName EnumName | TypeName_TypedefName TypedefName deriving (Data, Typeable, Eq)
data ElaboratedTypeSpecifier = ElaboratedTypeSpecifier (ClassKey, White) OptQualified (Either (Maybe (KwdTemplate, White), SimpleTemplateId) Identifier) deriving (Data, Typeable, Eq)
data EnumSpecifier = EnumSpecifier EnumHead (Curlied (Maybe (EnumeratorList, Maybe (CommaOp, White)))) deriving (Data, Typeable, Eq)
data EnumHead = EnumHead EnumKey (Maybe Identifier) (Maybe EnumBase) deriving (Data, Typeable, Eq)
data EnumKey = EnumKey (KwdEnum, White) | EnumKey_Class (KwdEnum, White) (KwdClass, White) | EnumKey_Struct (KwdEnum, White) (KwdStruct, White) deriving (Data, Typeable, Eq)
data EnumBase = EnumBase (ColonOp, White) TypeSpecifierSeq deriving (Data, Typeable, Eq)
  -- Todo: Make this "make"-able some day.
newtype EnumeratorList = EnumeratorList (Commad EnumeratorDefinition) deriving (Data, Typeable, Eq)
data EnumeratorDefinition = EnumeratorDefinition Enumerator (Maybe ((IsOperator, White), ConstantExpression)) deriving (Data, Typeable, Eq)
newtype Enumerator = Enumerator Identifier deriving (Data, Typeable, Eq)
data NamespaceDefinition = NamespaceDefinition (Maybe (KwdInline, White)) (KwdNamespace, White) (Maybe Identifier) (Curlied NamespaceBody) deriving (Data, Typeable, Eq)
newtype NamespaceBody = NamespaceBody (Maybe DeclarationSeq) deriving (Data, Typeable, Eq)
data NamespaceAliasDefinition = NamespaceAliasDefinition (KwdNamespace, White) Identifier (IsOperator, White) OptQualified NamespaceName (SemicolonOperator, White) deriving (Data, Typeable, Eq)
data UsingDeclaration = UsingDeclaration_Nested (KwdUsing, White) (Maybe (KwdTypename, White)) (Maybe (ScopeRes, White)) NestedNameSpecifier UnqualifiedId (SemicolonOperator, White) | UsingDeclaration_NonNested (KwdUsing, White) (ScopeRes, White) UnqualifiedId (SemicolonOperator, White) deriving (Data, Typeable, Eq)
data UsingDirective = UsingDirective (KwdUsing, White) (KwdNamespace, White) OptQualified NamespaceName (SemicolonOperator, White) deriving (Data, Typeable, Eq)
data AsmDefinition = AsmDefinition (KwdAsm, White) (Parenthesized StringLiteral) (SemicolonOperator, White) deriving (Data, Typeable, Eq)
data LinkageSpecification = LinkageSpecification (KwdExtern, White) StringLiteral (Either Declaration (Curlied (Maybe DeclarationSeq))) deriving (Data, Typeable, Eq)
data AlignmentSpecifier = AlignmentSpecifier (KwdAlignas, White) (Parenthesized (Either ConstantExpression TypeId)) deriving (Data, Typeable, Eq)

-- A.7 Declarators [gram.decl]

newtype InitDeclaratorList = InitDeclaratorList (Commad InitDeclarator) deriving (Data, Typeable, Eq)
data InitDeclarator = InitDeclarator Declarator (Maybe Initializer) deriving (Data, Typeable, Eq)
newtype Declarator = Declarator_PtrDeclarator PtrDeclarator deriving (Data, Typeable, Eq)
data PtrDeclarator = PtrDeclarator_NoptrDeclarator NoptrDeclarator | PtrDeclarator PtrOperator PtrDeclarator deriving (Data, Typeable, Eq)
data NoptrDeclarator = NoptrDeclarator_Id DeclaratorId | NoptrDeclarator_WithParams NoptrDeclarator ParametersAndQualifiers | NoptrDeclarator_Squared NoptrDeclarator (Squared (Maybe ConstantExpression)) | NoptrDeclarator_Parenthesized (Parenthesized PtrDeclarator) deriving (Data, Typeable, Eq)
data ParametersAndQualifiers = ParametersAndQualifiers (Parenthesized ParameterDeclarationClause) (Maybe CvQualifierSeq) (Maybe (RefQualifier, White)) (Maybe ExceptionSpecification) deriving (Data, Typeable, Eq)
data PtrOperator = PtrOperator_Ptr (StarOperator, White) (Maybe CvQualifierSeq) | PtrOperator_Ref (RefQualifier, White) | PtrOperator_Nested (Maybe (ScopeRes, White)) NestedNameSpecifier (StarOperator, White) (Maybe CvQualifierSeq) deriving (Data, Typeable, Eq)
newtype CvQualifierSeq = CvQualifierSeq (NeList (CvQualifier, White)) deriving (Data, Typeable, Eq)
data CvQualifier = Const | Volatile deriving (Bounded, Enum, Data, Typeable, Eq)
data RefQualifier = Lvalue | Rvalue deriving (Bounded, Enum, Data, Typeable, Eq)
data DeclaratorId = DeclaratorId_IdExpression (Maybe (Ellipsis_, White)) IdExpression | DeclaratorId_Nested OptQualified ClassName deriving (Data, Typeable, Eq)
data TypeId = TypeId TypeSpecifierSeq (Maybe AbstractDeclarator) deriving (Data, Typeable, Eq)
data AbstractDeclarator = AbstractDeclarator_PtrAbstractDeclarator PtrAbstractDeclarator | AbstractDeclarator_Ellipsis (Ellipsis_, White) deriving (Data, Typeable, Eq)
data PtrAbstractDeclarator = PtrAbstractDeclarator_NoptrAbstractDeclarator NoptrAbstractDeclarator | PtrAbstractDeclarator PtrOperator (Maybe PtrAbstractDeclarator) deriving (Data, Typeable, Eq)
data NoptrAbstractDeclarator = NoptrAbstractDeclarator (Maybe NoptrAbstractDeclarator) (Either ParametersAndQualifiers (Squared (Maybe ConstantExpression))) | NoptrAbstractDeclarator_PtrAbstractDeclarator (Parenthesized PtrAbstractDeclarator) deriving (Data, Typeable, Eq)
  -- In the latest draft, this constant expression isn't optional, which I think may be a bug. Todo: check again in the next draft.
data ParameterDeclarationClause = ParameterDeclarationClause (Maybe ParameterDeclarationList) (Maybe (Ellipsis_, White)) | ParameterDeclarationClauseWithEllipsis ParameterDeclarationList (CommaOp, White) (Ellipsis_, White) deriving (Data, Typeable, Eq)
newtype ParameterDeclarationList = ParameterDeclarationList (Commad ParameterDeclaration) deriving (Data, Typeable, Eq)
data ParameterDeclaration = ParameterDeclaration DeclSpecifierSeq (Either Declarator (Maybe AbstractDeclarator)) (Maybe ((IsOperator, White), AssignmentExpression)) deriving (Data, Typeable, Eq)
data FunctionDefinition = FunctionDefinition (Maybe DeclSpecifierSeq) Declarator FunctionBody deriving (Data, Typeable, Eq)
data FunctionBody = FunctionBody (Maybe CtorInitializer) CompoundStatement | FunctionBody_FunctionTryBlock FunctionTryBlock deriving (Data, Typeable, Eq)
data Initializer = Initializer_Parenthesized (Parenthesized ExpressionList) | Initializer_BraceOrEqualInitializer BraceOrEqualInitializer deriving (Data, Typeable, Eq)
data BraceOrEqualInitializer = EqualInitializer (IsOperator, White) InitializerClause | BraceInitializer BracedInitList deriving (Data, Typeable, Eq)
newtype InitializerClause = InitializerClause (Either BracedInitList AssignmentExpression) deriving (Data, Typeable, Eq)
newtype InitializerList = InitializerList (Commad InitializerClause) deriving (Data, Typeable, Eq)
newtype BracedInitList = BracedInitList (Curlied (Maybe (InitializerList, Maybe (CommaOp, White)))) deriving (Data, Typeable, Eq)

-- A.8 Classes [gram.class]

data ClassSpecifier = ClassSpecifier ClassHead (Curlied MemberSpecification) deriving (Data, Typeable, Eq)
data ClassHeadKind = ClassHeadKind_Identifier (Maybe Identifier) | ClassHeadKind_NestedIdentifier NestedNameSpecifier Identifier | ClassHeadKind_SimpleTemplateId (Maybe NestedNameSpecifier) SimpleTemplateId deriving (Data, Typeable, Eq)
data ClassHead = ClassHead (ClassKey, White) ClassHeadKind (Maybe BaseClause) deriving (Data, Typeable, Eq)
data ClassKey = Class | Struct | Union deriving (Enum, Bounded, Data, Typeable, Eq)
data MemberAccessSpecifier = MemberAccessSpecifier (AccessSpecifier, White) (ColonOp, White) deriving (Data, Typeable, Eq)
data MemberSpecification = MemberSpecification [Either MemberDeclaration MemberAccessSpecifier] deriving (Data, Typeable, Eq)
data MemberDeclaration = MemberDeclaration (Maybe DeclSpecifierSeq) (Maybe MemberDeclaratorList) (SemicolonOperator, White) | MemberFunctionDefinition FunctionDefinition (Maybe (SemicolonOperator, White)) | MemberUsingDeclaration UsingDeclaration | MemberTemplateDeclaration TemplateDeclaration deriving (Data, Typeable, Eq)
newtype MemberDeclaratorList = MemberDeclaratorList (Commad MemberDeclarator) deriving (Data, Typeable, Eq)
data MemberDeclarator = MemberDeclarator Declarator (Maybe (Either PureSpecifier BraceOrEqualInitializer)) | BitField (Maybe Identifier) (ColonOp, White) ConstantExpression deriving (Data, Typeable, Eq)
data PureSpecifier = PureSpecifier (IsOperator, White) (KwdZero, White) deriving (Data, Typeable, Eq)

-- A.9 Derived classes [gram.derived]

data BaseClause = BaseClause (ColonOp, White) BaseSpecifierList deriving (Data, Typeable, Eq)
newtype BaseSpecifierList = BaseSpecifierList (Commad BaseSpecifier) deriving (Data, Typeable, Eq)
data BaseSpecifier = BaseSpecifier (AnyMixOf (AccessSpecifier, White) (KwdVirtual, White)) OptQualified ClassName deriving (Data, Typeable, Eq)
data AccessSpecifier = Private | Protected | Public deriving (Bounded, Enum, Data, Typeable, Eq)

-- A.10 Special member functions [gram.special]

data ConversionFunctionId = ConversionFunctionId (KwdOperator, White) ConversionTypeId deriving (Data, Typeable, Eq)
data ConversionTypeId = ConversionTypeId TypeSpecifierSeq [PtrOperator] deriving (Data, Typeable, Eq)
data CtorInitializer = CtorInitializer (ColonOp, White) MemInitializerList deriving (Data, Typeable, Eq)
newtype MemInitializerList = MemInitializerList (Commad MemInitializer) deriving (Data, Typeable, Eq)
data MemInitializer = MemInitializer MemInitializerId (Either (Parenthesized (Maybe ExpressionList)) BracedInitList) deriving (Data, Typeable, Eq)
data MemInitializerId = MemInitializerId_ClassName OptQualified ClassName | MemInitializerId_Identifier Identifier deriving (Data, Typeable, Eq)

-- A.11 Overloading [gram.over]

data OperatorFunctionId = OperatorFunctionId (KwdOperator, White) OverloadableOperator deriving (Data, Typeable, Eq)
data OverloadableOperator = OverloadableOperator_New (KwdNew, White) (Maybe (Squared ())) | OverloadableOperator_Delete (KwdDelete, White) (Maybe (Squared ())) | OverloadableOperator_Call (Parenthesized ()) | OverloadableOperator_Index (Squared ()) | OverloadableUnaryOperator (UnaryOperator, White) | OverloadableAssignmentOperator (AssignmentOperator, White) | OverloadableRelationalOperator (RelationalOperator, White) | OverloadableMultiplicativeOperator (MultiplicativeOperator, White) | OverloadableShiftOperator (ShiftOperator, White) | OverloadableAdditiveOperator (AdditiveOperator, White) | OverloadableEqualityOperator (EqualityOperator, White) | OverloadableBitXor (ExclusiveOrOperator, White) | OverloadableBitAnd (AndOperator, White) | OverloadableBitOr (InclusiveOrOperator, White) | OverloadableLogicalAnd (LogicalAndOperator, White) | OverloadableLogicalOr (LogicalOrOperator, White) | OverloadableComma (CommaOp, White) | OverloadablePmOperator (PmOperator, White) | OverloadableArrowOperator (ArrowOp, White) deriving (Data, Typeable, Eq)

-- A.12 Templates [gram.temp]

data TemplateDeclaration = TemplateDeclaration (Maybe (KwdExport, White)) (KwdTemplate, White) (Angled TemplateParameterList) Declaration deriving (Data, Typeable, Eq)
newtype TemplateParameterList = TemplateParameterList (Commad TemplateParameter) deriving (Data, Typeable, Eq)
data TemplateParameter = TemplateParameter_TypeParameter TypeParameter | TemplateParameter_ParameterDeclaration ParameterDeclaration deriving (Data, Typeable, Eq)
data TypeParameter = TypeParameter_Class (Either (KwdClass, White) (KwdTypename, White)) (Maybe Identifier) (Maybe ((IsOperator, White), TypeId)) | TypeParameter_Template (KwdTemplate, White) (Angled TemplateParameterList) (KwdClass, White) (Maybe Identifier) (Maybe (White, IdExpression)) deriving (Data, Typeable, Eq)
data TemplateArguments = TemplateArguments (Angled (Maybe TemplateArgumentList)) deriving (Data, Typeable, Eq)
data SimpleTemplateId = SimpleTemplateId TemplateName TemplateArguments deriving (Data, Typeable, Eq)
data TemplateId = TemplateId_SimpleTemplateId SimpleTemplateId | TemplateId_OperatorFunctionId OperatorFunctionId TemplateArguments deriving (Data, Typeable, Eq)
newtype TemplateArgumentList = TemplateArgumentList (Commad TemplateArgument) deriving (Data, Typeable, Eq)
data TemplateArgument = TemplateArgument_ConstantExpression ConstantExpression | TemplateArgument_TypeId TypeId | TemplateArgument_IdExpression IdExpression deriving (Data, Typeable, Eq)
data TypenameSpecifier = TypenameSpecifier (KwdTypename, White) (Maybe (ScopeRes, White)) NestedNameSpecifier (Either Identifier (Maybe (KwdTemplate, White), SimpleTemplateId)) deriving (Data, Typeable, Eq)
data ExplicitInstantiation = ExplicitInstantiation (Maybe (KwdExtern, White)) (KwdTemplate, White) Declaration deriving (Data, Typeable, Eq)
data ExplicitSpecialization = ExplicitSpecialization (KwdTemplate, White) (Angled ()) Declaration deriving (Data, Typeable, Eq)

-- A.13 Exception handling [gram.except]

data TryBlock = TryBlock (KwdTry, White) CompoundStatement HandlerSeq deriving (Data, Typeable, Eq)
data FunctionTryBlock = FunctionTryBlock (KwdTry, White) (Maybe CtorInitializer) CompoundStatement HandlerSeq deriving (Data, Typeable, Eq)
newtype HandlerSeq = HandlerSeq (NeList Handler) deriving (Data, Typeable, Eq)
data Handler = Handler (KwdCatch, White) (Parenthesized ExceptionDeclaration) CompoundStatement deriving (Data, Typeable, Eq)
data ExceptionDeclaration = ExceptionDeclaration_Ellipsis (Ellipsis_, White) | ExceptionDeclaration TypeSpecifierSeq (Maybe (Either Declarator AbstractDeclarator)) deriving (Data, Typeable, Eq)
data ThrowExpression = ThrowExpression (KwdThrow, White) (Maybe AssignmentExpression) deriving (Data, Typeable, Eq)
data ExceptionSpecification = ExceptionSpecification (KwdThrow, White) (Parenthesized (Maybe TypeIdList)) deriving (Data, Typeable, Eq)
newtype TypeIdList = TypeIdList (Commad TypeId) deriving (Data, Typeable, Eq)

-- A.14 Preprocessing directives [gram.cpp]
