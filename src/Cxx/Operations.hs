{-# LANGUAGE DeriveDataTypeable, MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, FlexibleContexts, UndecidableInstances, PatternGuards, Rank2Types, OverlappingInstances, ScopedTypeVariables, ExistentialQuantification, TypeSynonymInstances, CPP, ViewPatterns #-}

module Cxx.Operations (apply, mapply, squared, parenthesized, is_primary_TypeSpecifier, split_all_decls, map_plain, shortcut_syntaxes, blob, resume, expand, line_breaks, specT, find, is_pointer_or_reference, namedPathTo, findable_productions, make_edits) where

import qualified Cxx.Show
import qualified Data.List as List
import Data.List.NonEmpty (neHead, neTail, (|:), (.:), toNonEmpty)
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import Util (Convert(..), (.), total_tail, strip, isIdChar, TriBool(..), MaybeEitherString(..), Phantom(..), neElim, NeList, orElse, neFilter)
import Cxx.Basics
import Editing.Basics (Range(..), Offsettable(..), Edit)
import Editing.Diff (diff_as_Edits)
import Data.Function (on)
import Data.Foldable (toList, any)
import Control.Arrow (first, second)
import Control.Monad.Identity
import Data.Generics (cast, gmapT, everywhere, Data, Typeable, gfoldl, dataTypeOf, toConstr, Constr, DataType, dataTypeName, constrType)

import Prelude hiding ((.), any)
import Prelude.Unicode

-- Operations on Chunks/Code

map_plain :: (String → String) → Chunk → Chunk
map_plain f (Plain s) = Plain $ f s
map_plain f (Curlies c) = Curlies $ map (map_plain f) c
map_plain f (Parens c) = Parens $ map (map_plain f) c
map_plain f (Squares c) = Squares $ map (map_plain f) c
map_plain _ x = x

expand :: ShortCode → Code
expand (LongForm c) = c
expand (Block c c') = c' ++ [Plain "\nint main(int argc, char * argv[])", Curlies c]
expand (Print c c') = expand $ Block ([Plain "::std::cout << "] ++ c ++ [Plain "\n;"]) c'
  -- The newline before the semicolon makes //-style comments work.

cstyle_comments :: Code → Code
cstyle_comments = map f where f (SingleComment s) = MultiComment s; f c = c

expand_without_main :: ShortCode → Code
expand_without_main (LongForm d) = erase_main d
  where
    erase_main (Plain s : Parens _ : Curlies _ : c) | "main" `List.isInfixOf` s = c
    erase_main (Plain s : Parens _ : Plain t : Curlies _ : c)
      | "main" `List.isInfixOf` s, all Char.isSpace t = c
    erase_main (x : y) = (x :) $ erase_main y
    erase_main c = c
expand_without_main (Print _ c) = c
expand_without_main (Block _ c) = c

blob :: ShortCode → Code
blob (LongForm c) = c
blob (Print c c') = [Plain "<<"] ++ c ++ [Plain ";"] ++ c'
blob (Block c c') = Curlies c : c'

resume :: ShortCode → ShortCode → ShortCode
resume old new = case new of
    LongForm c → LongForm $ old' ++ c
    Print c c' → Print c $ old' ++ c'
    Block c c' → Block c $ old' ++ c'
  where old' = cstyle_comments $ expand_without_main old

shortcut_syntaxes :: Code → ShortCode
shortcut_syntaxes (Curlies c : b) = Block c b
shortcut_syntaxes (Plain ('<':'<':x) : y) = uncurry Print $ second total_tail $ break (== Plain ";") $ Plain x : y
shortcut_syntaxes c = LongForm c

line_breaks ::Code → Code
line_breaks = map $ map_plain $ map $ \c → if c == '\\' then '\n' else c

-- Convenience constructors

squared :: a → Squared a
squared x = Squared (OpenSquare_, White "") (Enclosed x) (CloseSquare_, White "")

parenthesized :: a → Parenthesized a
parenthesized x = Parenthesized (OpenParen_, White "") (Enclosed x) (CloseParen_, White "")

specT :: TypeSpecifier
specT = TypeSpecifier_SimpleTypeSpecifier $ SimpleTypeSpecifier_TypeName (OptQualified Nothing Nothing) $ TypeName_ClassName $ ClassName_Identifier $ Identifier "T" $ White " "

-- Applying make-specifications.

apply_makedecl_to :: Data d ⇒ MakeDeclaration → d → MaybeEitherString d
apply_makedecl_to makedecl = Maybe.fromMaybe (const mzero) $ Maybe.listToMaybe . Maybe.catMaybes $
  [ cast ((\d → case d of
    SimpleDeclaration specs (Just (InitDeclaratorList (Commad (InitDeclarator x mi) []))) w  →
      case makedecl of
        MakeDeclaration _ _ Definitely → fail "Cannot purify simple-declaration."
        MakeDeclaration specs' mpad _ → return $ let (specs'', x') = apply (specs', mpad) (specs, x) in
          SimpleDeclaration specs'' (Just (InitDeclaratorList (Commad (InitDeclarator x' mi) []))) w
    _ → mzero) :: SimpleDeclaration → MaybeEitherString SimpleDeclaration)
  , cast ((\d → case d of
    ParameterDeclaration specs x m →
      case makedecl of
        MakeDeclaration _ _ Definitely → fail "Cannot purify parameter-declaration."
        MakeDeclaration specs' mpad _ → (\(specs'', x') → ParameterDeclaration specs'' x' m) . mapply (specs', mpad) (specs, x)
    ) :: ParameterDeclaration → MaybeEitherString ParameterDeclaration)
  , cast ((\d → case d of
    ExceptionDeclaration u (Just (Left e)) →
      case makedecl of
        MakeDeclaration _ _ Definitely → fail "Cannot purify exception-declaration."
        MakeDeclaration specs mpad _ →
          (\(u', e') → ExceptionDeclaration u' $ Just $ Left e') . mapply (specs, mpad) (u, e)
    _ → mzero) :: ExceptionDeclaration → MaybeEitherString ExceptionDeclaration)
  , cast ((\d → case d of
    MemberDeclaration specs (Just (MemberDeclaratorList (Commad (MemberDeclarator decl ps) []))) semicolon →
      return $ let (specs', decl', ps') = apply makedecl (specs, decl, ps) in
        MemberDeclaration specs' (Just (MemberDeclaratorList (Commad (MemberDeclarator decl' ps') []))) semicolon
    _ → mzero) :: MemberDeclaration → MaybeEitherString MemberDeclaration)
  , cast ((\d → case d of
    FunctionDefinition specs decl body →
      case makedecl of
        MakeDeclaration _ _ Definitely → fail "Cannot purify function-definition."
        MakeDeclaration specs' mpad _ → return $ let (specs'', decl') = apply (specs', mpad) (specs, decl) in
          FunctionDefinition specs'' decl' body
    ) :: FunctionDefinition → MaybeEitherString FunctionDefinition)
  , cast ((\d → case d of
    Condition_Declaration u e i →
      case makedecl of
        MakeDeclaration _ _ Definitely → fail "Cannot purify condition-declaration."
        MakeDeclaration specs mpad _ →
          (\(u', e') → Condition_Declaration u' e' i) . mapply (specs, mpad) (u, e)
    _ → mzero) :: Condition → MaybeEitherString Condition)
  ]

-- Getting declarator-ids out of things.

instance Convert ClassSpecifier (Maybe DeclaratorId) where convert (ClassSpecifier h _) = convert h
instance Convert ClassHead (Maybe DeclaratorId) where convert (ClassHead _ k _) = convert k
instance Convert Identifier DeclaratorId where convert = DeclaratorId_IdExpression Nothing . convert
instance Convert Identifier IdExpression where convert = IdExpression . Right . convert
instance Convert Identifier UnqualifiedId where convert = UnqualifiedId_Identifier
instance Convert (NestedNameSpecifier, Identifier) QualifiedId where convert (nns, i) = NestedUnqualifiedId Nothing nns Nothing (convert i)
instance Convert (NestedNameSpecifier, Identifier) IdExpression where convert = IdExpression . Left . convert
instance Convert SimpleTemplateId UnqualifiedId where convert = UnqualifiedId_TemplateId . convert
instance Convert SimpleTemplateId IdExpression where convert = IdExpression . Right . convert
instance Convert SimpleTemplateId DeclaratorId where convert = DeclaratorId_IdExpression Nothing . convert
instance Convert SimpleTemplateId TemplateId where convert = TemplateId_SimpleTemplateId
instance Convert (NestedNameSpecifier, SimpleTemplateId) DeclaratorId where convert = DeclaratorId_IdExpression Nothing . convert
instance Convert (NestedNameSpecifier, SimpleTemplateId) IdExpression where convert = IdExpression . Left . convert
instance Convert (NestedNameSpecifier, SimpleTemplateId) QualifiedId where convert (nns, tid) = NestedUnqualifiedId Nothing nns Nothing (convert tid)
instance Convert ClassHeadKind (Maybe DeclaratorId) where
  convert (ClassHeadKind_Identifier m) = convert . m
  convert (ClassHeadKind_NestedIdentifier nns i) = Just $ DeclaratorId_IdExpression Nothing $ convert (nns, i)
  convert (ClassHeadKind_SimpleTemplateId Nothing i) = Just $ convert i
  convert (ClassHeadKind_SimpleTemplateId (Just nns) i) = Just $ convert (nns, i)
instance Convert EnumSpecifier (Maybe DeclaratorId) where convert (EnumSpecifier x _) = convert x
instance Convert EnumHead (Maybe DeclaratorId) where convert (EnumHead _ m _) = convert . m
instance Convert DeclSpecifier (Maybe DeclaratorId) where
  convert (DeclSpecifier_TypeSpecifier (TypeSpecifier_ClassSpecifier c)) = convert c
  convert (DeclSpecifier_TypeSpecifier (TypeSpecifier_EnumSpecifier c)) = convert c
  convert (DeclSpecifier_TypeSpecifier (TypeSpecifier_ElaboratedTypeSpecifier c)) = Just $ convert c
  convert _ = Nothing
instance Convert SimpleDeclaration (Maybe DeclaratorId) where
  convert (SimpleDeclaration _ (Just (InitDeclaratorList (Commad (InitDeclarator d _) []))) _) = Just $ convert d
  convert (SimpleDeclaration (Just (DeclSpecifierSeq (neElim → (d, [])))) Nothing _) = convert d
  convert _ = Nothing
instance Convert NamespaceAliasDefinition DeclaratorId where convert (NamespaceAliasDefinition _ i _ _ _ _) = convert i
instance Convert BlockDeclaration (Maybe DeclaratorId) where
  convert (BlockDeclaration_SimpleDeclaration d) = convert d
  convert (BlockDeclaration_NamespaceAliasDefinition d) = Just $ convert d
  convert (BlockDeclaration_AliasDeclaration d) = Just $ convert d
  convert _ = Nothing
instance Convert ExplicitSpecialization (Maybe DeclaratorId) where convert (ExplicitSpecialization _ _ d) = convert d
instance Convert FunctionDefinition DeclaratorId where convert (FunctionDefinition _ d _) = convert d
instance Convert NamespaceDefinition (Maybe DeclaratorId) where convert (NamespaceDefinition _ _ m _) = convert . m
instance Convert AliasDeclaration DeclaratorId where convert (AliasDeclaration _ i _ _ _) = convert i
instance Convert Declaration (Maybe DeclaratorId) where
  convert (Declaration_BlockDeclaration d) = convert d
  convert (Declaration_FunctionDefinition d) = Just $ convert d
  convert (Declaration_TemplateDeclaration d) = convert d
  convert (Declaration_NamespaceDefinition d) = convert d
  convert (Declaration_ExplicitSpecialization d) = convert d
  convert _ = Nothing
instance Convert TemplateDeclaration (Maybe DeclaratorId) where convert (TemplateDeclaration _ _ _ d) = convert d
instance Convert ExplicitInstantiation (Maybe DeclaratorId) where convert (ExplicitInstantiation _ _ d) = convert d
instance Convert MemberDeclarator (Maybe DeclaratorId) where
  convert (MemberDeclarator d _) = Just $ convert d
  convert (BitField m _ _) = convert . m
instance Convert MemberDeclaration (Maybe DeclaratorId) where
  convert (MemberFunctionDefinition d _) = Just $ convert d
  convert (MemberUsingDeclaration _) = Nothing
  convert (MemberTemplateDeclaration d) = convert d
  convert (MemberDeclaration _ (Just (MemberDeclaratorList (Commad d []))) _) = convert d
  convert (MemberDeclaration (Just (DeclSpecifierSeq (neElim → (d, [])))) Nothing _) = convert d
  convert (MemberDeclaration _ _ _) = Nothing
instance Convert ExceptionDeclaration (Maybe DeclaratorId) where
  convert (ExceptionDeclaration _ (Just (Left d))) = Just $ convert d
  convert _ = Nothing
instance Convert ParameterDeclaration (Maybe DeclaratorId) where
  convert (ParameterDeclaration _ (Left d) _) = Just $ convert d
  convert _ = Nothing
instance Convert Condition (Maybe DeclaratorId) where
  convert (Condition_Declaration _ d _) = Just $ convert d
  convert _ = Nothing
instance Convert (OptQualified, Identifier) DeclaratorId where
  convert (OptQualified Nothing Nothing, i) = convert i
  convert (OptQualified (Just s) Nothing, i) = DeclaratorId_IdExpression Nothing $ IdExpression $ Left $ GlobalIdentifier s i
  convert (OptQualified ms (Just nns), i) = DeclaratorId_IdExpression Nothing $ IdExpression $ Left $ NestedUnqualifiedId ms nns Nothing $ convert i
instance Convert QualifiedId DeclaratorId where
  convert = DeclaratorId_IdExpression Nothing . IdExpression . Left
instance Convert (OptQualified, SimpleTemplateId) DeclaratorId where
  convert (OptQualified Nothing Nothing, stid) = convert stid
  convert (OptQualified (Just s) Nothing, stid) = DeclaratorId_IdExpression Nothing $ IdExpression $ Left $ GlobalTemplateId s $ convert stid
  convert (OptQualified ms (Just nns), stid) = DeclaratorId_IdExpression Nothing $ IdExpression $ Left $ NestedUnqualifiedId ms nns Nothing $ convert stid
instance Convert ElaboratedTypeSpecifier DeclaratorId where
  convert (ElaboratedTypeSpecifier _ optqualified (Right identifier)) = convert (optqualified, identifier)
  convert (ElaboratedTypeSpecifier _ optqualified (Left (_, stid))) = convert (optqualified, stid)
    -- Todo: Maybe using the (KwdTemplate, White) pair in the declarator-id would be better.
instance Convert Declarator DeclaratorId where convert (Declarator_PtrDeclarator p) = convert p
instance Convert PtrDeclarator DeclaratorId where
  convert (PtrDeclarator_NoptrDeclarator d) = convert d
  convert (PtrDeclarator _ d) = convert d
instance Convert NoptrDeclarator DeclaratorId where
  convert (NoptrDeclarator_Id did) = did
  convert (NoptrDeclarator_WithParams d _) = convert d
  convert (NoptrDeclarator_Squared d _) = convert d
  convert (NoptrDeclarator_Parenthesized (Parenthesized _ (Enclosed d) _)) = convert d
instance Convert ((ScopeRes, White), UnqualifiedId) (Maybe DeclaratorId) where
  convert (scoperes, UnqualifiedId_Identifier i) = Just $ convert (GlobalIdentifier scoperes i)
  convert (scoperes, UnqualifiedId_TemplateId i) = Just $ convert (GlobalTemplateId scoperes i)
  convert (scoperes, UnqualifiedId_OperatorFunctionId i) = Just $ convert (GlobalOperatorFunctionId scoperes i)
  convert (_, UnqualifiedId_Destructor _ _) = Nothing -- There are no global destructors.
  convert (_, UnqualifiedId_ConversionFunctionId _) = Nothing -- There are no global conversion operators.
instance Convert UsingDeclaration (Maybe DeclaratorId) where
  convert (UsingDeclaration_Nested _ _ msr nns i _) = Just $ convert $ NestedUnqualifiedId msr nns Nothing i
  convert (UsingDeclaration_NonNested _ s i _) = convert (s, i)

-- Finding declarations

gfoldl_with_lengths :: Data a ⇒ Int → (forall d. Data d ⇒ Int → d → [r]) → a → [r]
gfoldl_with_lengths i f = runIdentity . gfoldl_with_lengthsM i ((Identity .) . f)

gfoldl_with_ranges :: Data a ⇒ Int → (forall d. Data d ⇒ Range Char → d → [r]) → a → [r]
gfoldl_with_ranges i f = runIdentity . gfoldl_with_rangesM i ((Identity .) . f)

gfoldl_with_lengthsM :: (Data a, Monad m) ⇒ Int → (forall d. Data d ⇒ Int → d → m [r]) → a → m [r]
gfoldl_with_lengthsM i f = gfoldl_with_rangesM i (f . start)

data GfoldlWithLengthsIntermediary m r a = GfoldlWithLengthsIntermediary { gwli_result :: m [r], _off :: Int }

gfoldl_with_rangesM :: (Data a, Monad m) ⇒ Int → (forall d. Data d ⇒ Range Char → d → m [r]) → a → m [r]
gfoldl_with_rangesM i f = gwli_result . gfoldl (\(GfoldlWithLengthsIntermediary m o) y →
  let n = length (Cxx.Show.show_simple y) in
  GfoldlWithLengthsIntermediary (liftM2 (++) m (f (Range o n) y)) (o + n)) (\_ → GfoldlWithLengthsIntermediary (return []) i)

listElem :: forall a d . (Data a, Typeable a, Data d) ⇒ Phantom a → d → Maybe (Range Char, Range Char)
listElem _ d
  | Just (Commad x []) ← (cast d :: Maybe (Commad a)) =
    Just $ diag $ Range 0 $ length $ Cxx.Show.show_simple x
  | Just (Commad x ((cw, _):_)) ← (cast d :: Maybe (Commad a)) =
    Just (Range 0 $ length $ Cxx.Show.show_simple (x, cw), Range 0 $ length $ Cxx.Show.show_simple x)
  | Just x@(cw, r) ← (cast d :: Maybe ((CommaOp, White), a)) =
    Just (Range 0 $ length $ Cxx.Show.show_simple x,
      Range (length $ Cxx.Show.show_simple cw) (length $ Cxx.Show.show_simple r))
  | otherwise = Nothing

bodyOf :: Data d ⇒ d → DeclaratorId → Maybe (Range Char)
bodyOf x did
  | Just (GeordiRequest_Block (FunctionBody _ (CompoundStatement (Curlied o b _))) _) ← cast x, strip (show did) == "main" =
    Just $ Range (length $ Cxx.Show.show_simple o) (length $ Cxx.Show.show_simple b)
  | Just (ClassSpecifier classHead (Curlied o b _)) ← cast x, convert classHead == Just did =
    Just $ Range (length $ Cxx.Show.show_simple (classHead, o)) (length $ Cxx.Show.show_simple b)
  | Just (EnumSpecifier enumHead (Curlied o b _)) ← cast x, convert enumHead == Just did =
    Just $ Range (length $ Cxx.Show.show_simple (enumHead, o)) (length $ Cxx.Show.show_simple b)
  | Just (FunctionDefinition specs declarator (FunctionBody ctorInitializer (CompoundStatement (Curlied o b _)))) ← cast x, convert declarator == did =
    Just $ Range (length $ Cxx.Show.show_simple (specs, declarator, ctorInitializer, o)) (length $ Cxx.Show.show_simple b)
  | Just (NamespaceDefinition inline kwd (Just identifier) (Curlied o b _)) ← cast x, convert identifier == did =
    Just $ Range (length $ Cxx.Show.show_simple (inline, kwd, identifier, o)) (length $ Cxx.Show.show_simple b)
  | otherwise = Nothing

instance Eq DataType where (==) = (==) `on` dataTypeName

constr_eq :: Constr → Constr → Bool
  -- The existing Eq instance only compares constructor indices for algebraic data types, so for instance the first constructors of two unrelated algebraic data types are considered equal.
constr_eq c d = c == d ∧ constrType c == constrType d

data AnyData = forall d . Data d ⇒ AnyData d

type TreePath = NeList AnyData

applyAny :: (forall a. Data a ⇒ a → b) → (AnyData → b)
applyAny p (AnyData x) = p x

diag :: a → (a, a)
diag x = (x, x)

finder :: Findable → TreePath → Maybe (Range Char, Range Char)
finder f = case f of
  FindableDataType t → (diag .) . simpleFinder ((== t) . applyAny dataTypeOf . neHead)
  FindableConstr c → (diag .) . simpleFinder (constr_eq c . applyAny toConstr . neHead)
  BodyOf d → (diag .) . applyAny (`bodyOf` d) . neHead
  DeclarationOf d → (diag .) . simpleFinder (complete (`isDeclarationOf` d))
  Constructor → (diag .) . simpleFinder  (complete $ isSpecialFuncWith isConstructorId)
  Destructor → (diag .) . simpleFinder (complete $ isSpecialFuncWith isDestructorId)
  ConversionFunction → (diag .) . simpleFinder (complete $ isSpecialFuncWith isConversionFunctionId)
  FindableParameterDeclaration → applyAny (listElem (Phantom :: Phantom ParameterDeclaration)) . neHead
  TemplateParameter → applyAny (listElem (Phantom :: Phantom TemplateParameter)) . neHead
  TemplateArgument → applyAny (listElem (Phantom :: Phantom TemplateArgument)) . neHead
 where
  simpleFinder p t | AnyData x ← neHead t =
    if p t then Just $ Range 0 $ length $ Cxx.Show.show_simple x else Nothing

find :: Data d ⇒ Findable → d → [(Range Char, Range Char)]
find f = findRange (finder f) [] 0

complete :: (forall d . Data d ⇒ d → Bool) → TreePath → Bool
complete p d | AnyData x ← neHead d =
  p x ∧ case neTail d of [] → True; AnyData h : _ → not $ p h

wraps :: Range a → Range a → Bool
wraps (Range st si) (Range st' si') = st ≤ st' ∧ st' + si' ≤ st + si

pathTo :: Data d ⇒ d → Range Char → Int → TreePath
  -- Precondition: the range is entirely within [0, length (show d)]
pathTo x r i = AnyData x |: case gfoldl_with_ranges i f x of
  [] → []
  l : _ → toList l
  where f r'@(Range st _) y = [pathTo y r st | r' `wraps` r]

findable_productions, all_productions :: [DataType]
findable_productions =
#define P(n) dataTypeOf (undefined :: Cxx.Basics.n)
    -- A.1 Keywords [gram.key]
    [ P(TypedefName), P(NamespaceName), P(OriginalNamespaceName), P(NamespaceAlias)
    , P(ClassName), P(EnumName), P(TemplateName)

    -- A.2 Lexical conventions [gram.lex]
    , P(Identifier), P(Literal), P(IntegerLiteral), P(CharacterLiteral), P(FloatingLiteral), P(StringLiteral), P(EncodingPrefix)

    -- A.4 Expressions [gram.expr]
    , P(PrimaryExpression), P(IdExpression), P(UnqualifiedId), P(QualifiedId), P(NestedNameSpecifier), P(PostfixExpression)
    , P(ExpressionList), P(PseudoDestructorName), P(UnaryExpression), P(UnaryOperator), P(NewExpression), P(NewPlacement)
    , P(NewTypeId), P(NewDeclarator), P(NoptrNewDeclarator), P(NewInitializer), P(DeleteExpression)
    , P(CastExpression), P(PmExpression), P(MultiplicativeExpression), P(AdditiveExpression), P(ShiftExpression)
    , P(RelationalExpression), P(EqualityExpression), P(AndExpression), P(ExclusiveOrExpression)
    , P(InclusiveOrExpression), P(LogicalAndExpression), P(LogicalOrExpression), P(ConditionalExpression)
    , P(AssignmentExpression), P(AssignmentOperator), P(Expression), P(ConstantExpression)

    -- A.5 Statements [gram.stmt]
    , P(Statement), P(StatementSeq), P(Label), P(LabeledStatement), P(ExpressionStatement), P(CompoundStatement)
    , P(SelectionStatement), P(Condition), P(IterationStatement), P(ForInitStatement), P(JumpStatement), P(DeclarationStatement)

    -- A.6 Declarations [gram.dcl]
    , P(Declaration), P(DeclarationSeq), P(BlockDeclaration), P(AliasDeclaration), P(SimpleDeclaration), P(StaticAssertDeclaration)
    , P(DeclSpecifier), P(DeclSpecifierSeq), P(StorageClassSpecifier), P(FunctionSpecifier), P(TypeSpecifier), P(TypeSpecifierSeq), P(SimpleTypeSpecifier)
    , P(TypeName), P(ElaboratedTypeSpecifier), P(EnumSpecifier), P(EnumHead), P(EnumKey), P(EnumeratorList), P(EnumeratorDefinition)
    , P(Enumerator), P(NamespaceDefinition), P(NamespaceAliasDefinition), P(UsingDeclaration), P(UsingDirective), P(AsmDefinition), P(LinkageSpecification)
    , P(AlignmentSpecifier)

    -- A.7 Declarators [gram.decl]
    , P(InitDeclaratorList), P(InitDeclarator), P(Declarator), P(PtrDeclarator), P(NoptrDeclarator), P(ParametersAndQualifiers)
    , P(PtrOperator), P(CvQualifier), P(CvQualifierSeq), P(DeclaratorId), P(TypeId), P(AbstractDeclarator), P(PtrAbstractDeclarator)
    , P(NoptrAbstractDeclarator), P(ParameterDeclarationClause), P(ParameterDeclarationList), P(FunctionDefinition)
    , P(FunctionBody), P(Initializer), P(BraceOrEqualInitializer), P(InitializerClause), P(InitializerList), P(BracedInitList), P(NamespaceBody)

    -- A.8 Classes [gram.class]
    , P(ClassSpecifier), P(ClassHead), P(ClassKey), P(MemberAccessSpecifier)
    , P(MemberSpecification), P(MemberDeclaration), P(MemberDeclaratorList), P(MemberDeclarator), P(PureSpecifier)

    -- A.9 Derived classes [gram.derived]
    , P(BaseClause), P(BaseSpecifierList), P(BaseSpecifier), P(AccessSpecifier)

    -- A.10 Special member functions [gram.special]
    , P(ConversionFunctionId), P(ConversionTypeId), P(CtorInitializer), P(MemInitializerList), P(MemInitializer), P(MemInitializerId)

    -- A.11 Overloading [gram.over]
    , P(OperatorFunctionId)

    -- A.12 Templates [gram.temp]
    , P(TemplateDeclaration), P(TemplateParameterList), P(TypeParameter), P(TemplateArguments), P(SimpleTemplateId)
    , P(TemplateId), P(TemplateArgumentList), P(TemplateArgument), P(TypenameSpecifier), P(ExplicitInstantiation)
    , P(ExplicitSpecialization)

    -- A.13 Exception handling [gram.except]
    , P(TryBlock), P(FunctionTryBlock), P(Handler), P(HandlerSeq), P(ExceptionDeclaration), P(ThrowExpression), P(ExceptionSpecification)
    , P(TypeIdList) ]
all_productions = findable_productions ++ [P(ParameterDeclaration), P(TemplateParameter), P(TemplateArgument)]
  -- These three are not part of findable_productions because they get special Findable treatment.
#undef P

namedPathTo :: Data d ⇒ d → Range Char → [String]
namedPathTo d r = map Cxx.Show.dataType_abbreviated_productionName $
  filter (∈ all_productions) $ toList $ fmap (applyAny dataTypeOf) (pathTo d r 0)

findRange :: (Offsettable a, Data d) ⇒ (TreePath → Maybe a) → [AnyData] → Int → d → [a]
findRange p tp i x = Maybe.maybeToList (offset i . p (AnyData x |: tp)) ++ gfoldl_with_lengths i (findRange p (AnyData x : tp)) x

make_edits :: (Monad m, Data d) ⇒ Range Char → MakeDeclaration → Int → d → m [Edit]
make_edits r m i d = do
  ot ← gfoldl_with_lengthsM i (make_edits r m) d
  oi ← (if Range i (length $ strip $ Cxx.Show.show_simple d) == r
    then (case apply_makedecl_to m d of
      MaybeEitherString (Just (Right d')) → return $ offset i $ diff_as_Edits (Cxx.Show.show_simple d) (Cxx.Show.show_simple d')
      MaybeEitherString (Just (Left e)) → fail e
      MaybeEitherString Nothing → return [])
    else return [])
  return $ oi ++ ot

instance Convert [DeclSpecifier] [TypeSpecifier] where
  convert = Maybe.mapMaybe $ \ds →
    case ds of
      DeclSpecifier_TypeSpecifier t → Just t
      _ → Nothing

isSpecialFunc :: Data d ⇒ d → Maybe DeclaratorId
  -- "special" meaning: without any type-specifiers.
isSpecialFunc x
  | Just s@(MemberDeclaration l _ _) ← cast x, null (convert l :: [TypeSpecifier]) = convert s
  | Just (MemberFunctionDefinition f@(FunctionDefinition Nothing _ _) _) ← cast x = Just $ convert f
  | Just (MemberTemplateDeclaration d) ← cast x = isSpecialFunc d
  | Just f@(FunctionDefinition l _ _) ← cast x, null (convert l :: [TypeSpecifier]) = Just $ convert f
  | Just (TemplateDeclaration _ _ _ d) ← cast x = isSpecialFunc d
  | Just (Declaration_FunctionDefinition d) ← cast x = isSpecialFunc d
  | Just (Declaration_TemplateDeclaration d) ← cast x = isSpecialFunc d
  | Just (Declaration_BlockDeclaration d) ← cast x = isSpecialFunc d
  | Just (BlockDeclaration_SimpleDeclaration d) ← cast x = isSpecialFunc d
  | Just s@(SimpleDeclaration l _ _) ← cast x, null (convert l :: [TypeSpecifier]) = convert s
  | otherwise = Nothing

isSpecialFuncWith :: Data d ⇒ (DeclaratorId → Bool) → d → Bool
isSpecialFuncWith p x
  | Just did ← isSpecialFunc x = p did
  | otherwise = False

isConstructorId :: DeclaratorId → Bool
isConstructorId i = not $ isDestructorId i ∨ isConversionFunctionId i

isDestructorId :: DeclaratorId → Bool
isDestructorId (DeclaratorId_IdExpression Nothing (IdExpression e)) = case e of
  Right (UnqualifiedId_Destructor _ _) → True
  Left (NestedUnqualifiedId _ _ _ (UnqualifiedId_Destructor _ _)) → True
  _ → False
isDestructorId _ = False

isConversionFunctionId :: DeclaratorId → Bool
isConversionFunctionId = Maybe.isJust . (convert :: DeclaratorId → Maybe ConversionFunctionId)

isDeclarationOf :: Data d ⇒ d → DeclaratorId → Bool
isDeclarationOf x did = Just did == case () of { ()
  | Just s ← cast x → convert (s :: Declaration)
  | Just s ← cast x → convert (s :: BlockDeclaration)
  | Just s ← cast x → Just $ convert (s :: FunctionDefinition)
  | Just s ← cast x → convert (s :: TemplateDeclaration)
  | Just s ← cast x → convert (s :: ExplicitInstantiation)
  | Just s ← cast x → convert (s :: ExplicitSpecialization)
  | Just s ← cast x → convert (s :: NamespaceDefinition)
  | Just s ← cast x → convert (s :: SimpleDeclaration)
  | Just s ← cast x → Just $ convert (s :: NamespaceAliasDefinition)
  | Just s ← cast x → convert (s :: UsingDeclaration)
  | Just s ← cast x → Just $ convert (s :: AliasDeclaration)
  | Just s ← cast x → convert (s :: MemberDeclaration)
  | Just s ← cast x → convert (s :: ExceptionDeclaration)
  | Just s ← cast x → convert (s :: ParameterDeclaration)
  | Just s ← cast x → convert (s :: Condition)
  | otherwise → Nothing }

-- Specifier/qualifier compatibility.

class Compatible a b where compatible :: a → b → Bool
  -- For instances where a=b, compatible should be symmetric.

instance Compatible CvQualifier CvQualifier where compatible = (≠)

instance Compatible CvQualifier TypeSpecifier where
  compatible cv (TypeSpecifier_CvQualifier (cv', _)) = compatible cv cv'
  compatible _ _ = True

instance Compatible CvQualifier DeclSpecifier where
  compatible cv (DeclSpecifier_TypeSpecifier t) = compatible cv t
  compatible _ _ = True

instance Compatible SimpleTypeSpecifier SimpleTypeSpecifier where
  compatible (SignSpec _) (LengthSpec _) = True
  compatible (LengthSpec _) (SignSpec _) = True
  compatible (LengthSpec (LongSpec, _)) (SimpleTypeSpecifier_BasicType (Int', _)) = True
  compatible (LengthSpec (LongSpec, _)) (SimpleTypeSpecifier_BasicType (Double', _)) = True
  compatible x@(SimpleTypeSpecifier_BasicType _) y@(LengthSpec _) = compatible y x
  compatible (SignSpec _) (SimpleTypeSpecifier_BasicType (Int', _)) = True
  compatible (SimpleTypeSpecifier_BasicType (Int', _)) (SignSpec _) = True
  compatible _ _ = False

instance Compatible TypeSpecifier TypeSpecifier where
  compatible (TypeSpecifier_CvQualifier (cv, _)) (TypeSpecifier_CvQualifier (cv', _)) = compatible cv cv'
  compatible (TypeSpecifier_CvQualifier _) _ = True
  compatible _ (TypeSpecifier_CvQualifier _) = True
  compatible (TypeSpecifier_SimpleTypeSpecifier x) (TypeSpecifier_SimpleTypeSpecifier y) = compatible x y
  compatible _ _ = False

instance Compatible MakeSpecifier MakeSpecifier where
  compatible (MakeSpecifier_DeclSpecifier d) (MakeSpecifier_DeclSpecifier d') = compatible d d'
  compatible x@(MakeSpecifier_DeclSpecifier _) y = compatible y x
  compatible (NonStorageClassSpecifier scs) (MakeSpecifier_DeclSpecifier (DeclSpecifier_StorageClassSpecifier (scs', _))) = scs ≠ scs'
  compatible _ _ = True

instance Compatible DeclSpecifier DeclSpecifier where
  compatible (DeclSpecifier_TypeSpecifier x) (DeclSpecifier_TypeSpecifier y) = compatible x y
  compatible (DeclSpecifier_StorageClassSpecifier _) (DeclSpecifier_StorageClassSpecifier _) = False
  compatible (DeclSpecifier_FunctionSpecifier (s, _)) (DeclSpecifier_FunctionSpecifier (s', _)) | s == s' = False
  compatible (DeclSpecifier_Typedef _) (DeclSpecifier_Typedef _) = False
  compatible (DeclSpecifier_ConstExpr _) (DeclSpecifier_ConstExpr _) = False
  compatible (DeclSpecifier_AlignmentSpecifier _) (DeclSpecifier_AlignmentSpecifier _) = False
  compatible (DeclSpecifier_FunctionSpecifier (Virtual, _)) (DeclSpecifier_StorageClassSpecifier (Static, _)) = False
  compatible (DeclSpecifier_StorageClassSpecifier (Static, _)) (DeclSpecifier_FunctionSpecifier (Virtual, _)) = False
  compatible _ _ = True

-- Making sure things end with whitespace.

data WithAlternate a = WithoutAlternate a | WithAlternate { _wa_primary :: a, _wa_alternate :: a } deriving Typeable

instance Functor WithAlternate where
  fmap f (WithoutAlternate x) = WithoutAlternate $ f x
  fmap f (WithAlternate x y) = WithAlternate (f x) (f y)

with_trailing_white :: Data d ⇒ d → d
with_trailing_white = \x → case f x of WithoutAlternate y → y; WithAlternate _ y → y
  where
    f :: Data d ⇒ d → WithAlternate d
    f | Just h ← cast (\w@(White s) → WithAlternate w (White $ if null s then " " else s)) = h
      | otherwise = flip gfoldl WithoutAlternate $ \e d → case e of
        (WithAlternate h i) → case f d of
          WithoutAlternate x → WithAlternate (h x) (i x)
          WithAlternate x y → WithAlternate (h x) (h y)
        (WithoutAlternate h) → h . f d

-- Specifier/qualifier conversion

instance Convert (BasicType, White) TypeSpecifier where convert = TypeSpecifier_SimpleTypeSpecifier . SimpleTypeSpecifier_BasicType
instance Convert (BasicType, White) DeclSpecifier where convert = (convert :: TypeSpecifier → DeclSpecifier) . convert
instance Convert CvQualifier TypeSpecifier where convert cvq = TypeSpecifier_CvQualifier (cvq, White " ")
instance Convert CvQualifier DeclSpecifier where convert = convert . (convert :: CvQualifier → TypeSpecifier)
instance Convert CvQualifier MakeSpecifier where convert = convert . (convert :: TypeSpecifier → DeclSpecifier) . convert
instance Convert SimpleTypeSpecifier (Maybe Sign) where convert (SignSpec (s, _)) = Just s; convert _ = Nothing
instance Convert SimpleTypeSpecifier (Maybe LengthSpec) where convert (LengthSpec (s, _)) = Just s; convert _ = Nothing
instance Convert SimpleTypeSpecifier TypeSpecifier where convert = TypeSpecifier_SimpleTypeSpecifier
instance Convert TypeSpecifier DeclSpecifier where convert = DeclSpecifier_TypeSpecifier
instance Convert TypeSpecifier (Maybe Sign) where convert x = convert x >>= (convert :: SimpleTypeSpecifier → Maybe Sign)
instance Convert TypeSpecifier (Maybe SimpleTypeSpecifier) where convert (TypeSpecifier_SimpleTypeSpecifier s) = Just s; convert _ = Nothing
instance Convert TypeSpecifier (Maybe LengthSpec) where convert x = convert x >>= (convert :: SimpleTypeSpecifier → Maybe LengthSpec)
instance Convert TypeSpecifier (Maybe (CvQualifier, White)) where convert (TypeSpecifier_CvQualifier cvq) = Just cvq; convert _ = Nothing
instance Convert TypeSpecifier (Maybe CvQualifier) where convert x = fst . (convert x :: Maybe (CvQualifier, White))
instance Convert DeclSpecifier (Maybe TypeSpecifier) where convert (DeclSpecifier_TypeSpecifier s) = Just s; convert _ = Nothing
instance Convert DeclSpecifier (Maybe StorageClassSpecifier) where convert (DeclSpecifier_StorageClassSpecifier (s, _)) = Just s; convert _ = Nothing
instance Convert DeclSpecifier (Maybe FunctionSpecifier) where convert (DeclSpecifier_FunctionSpecifier (s, _)) = Just s; convert _ = Nothing
instance Convert DeclSpecifier MakeSpecifier where convert = MakeSpecifier_DeclSpecifier
instance Convert DeclSpecifier (Maybe Sign) where convert x = convert x >>= (convert :: TypeSpecifier → Maybe Sign)
instance Convert DeclSpecifier (Maybe LengthSpec) where convert x = convert x >>= (convert :: TypeSpecifier → Maybe LengthSpec)
instance Convert DeclSpecifier (Maybe (CvQualifier, White)) where convert (DeclSpecifier_TypeSpecifier t) = convert t; convert _ = Nothing
instance Convert DeclSpecifier (Maybe CvQualifier) where convert x = fst . (convert x :: Maybe (CvQualifier, White))
instance Convert MakeSpecifier (Maybe (CvQualifier, White)) where convert (MakeSpecifier_DeclSpecifier t) = convert t; convert _ = Nothing
instance Convert LengthSpec SimpleTypeSpecifier where convert x = LengthSpec (x, White " ")
instance Convert LengthSpec TypeSpecifier where convert = convert . (convert :: LengthSpec → SimpleTypeSpecifier)
instance Convert LengthSpec DeclSpecifier where convert = convert . (convert :: LengthSpec → TypeSpecifier)
instance Convert LengthSpec MakeSpecifier where convert = convert . (convert :: LengthSpec → DeclSpecifier)

-- Misc conversions

instance Convert PtrDeclarator Declarator where convert = Declarator_PtrDeclarator
instance Convert NoptrDeclarator PtrDeclarator where convert = PtrDeclarator_NoptrDeclarator
instance Convert NoptrDeclarator Declarator where convert = convert . (convert :: NoptrDeclarator → PtrDeclarator)
instance Convert NoptrDeclarator InitDeclarator where convert = flip InitDeclarator Nothing . convert

instance Convert DeclaratorId (Maybe ConversionFunctionId) where
  convert (DeclaratorId_IdExpression Nothing (IdExpression e)) =
    case e of
      Left (NestedUnqualifiedId _ _ _ (UnqualifiedId_ConversionFunctionId i)) → Just i
      Right (UnqualifiedId_ConversionFunctionId i) → Just i
      _ → Nothing
  convert _ = Nothing

-- Declaration splitting

class SplitDecls a where split_decls :: a → NeList a

instance SplitDecls Declaration where
  split_decls (Declaration_BlockDeclaration bd) = fmap Declaration_BlockDeclaration $ split_decls bd
  split_decls d = return d

instance SplitDecls BlockDeclaration where
  split_decls (BlockDeclaration_SimpleDeclaration sd) = fmap BlockDeclaration_SimpleDeclaration $ split_decls sd
  split_decls d = return d

instance SplitDecls SimpleDeclaration where
  split_decls d@(SimpleDeclaration _ Nothing _) = return d
  split_decls (SimpleDeclaration specs (Just (InitDeclaratorList (Commad x l))) w) =
    (\y → SimpleDeclaration specs (Just (InitDeclaratorList (Commad y []))) w) . (x |: (snd . l))

instance SplitDecls Statement where
  split_decls (Statement_DeclarationStatement (DeclarationStatement d)) =
    Statement_DeclarationStatement . DeclarationStatement . split_decls d
  split_decls (Statement_CompoundStatement (CompoundStatement (Curlied x (Enclosed (Just (StatementSeq l))) y))) =
    return (Statement_CompoundStatement $ CompoundStatement $ Curlied x (Enclosed $ Just $ StatementSeq $ l >>= split_decls) y)
  split_decls (Statement_SelectionStatement (IfStatement k c s Nothing)) =
    return (Statement_SelectionStatement $ IfStatement k c (compound_split_decls s) Nothing) -- todo: do else part as well
    -- todo: do while and do-loops as well.
  split_decls s = return $ gmapT split_all_decls s

instance SplitDecls MemberDeclaration where
  split_decls (MemberDeclaration specs (Just (MemberDeclaratorList (Commad d ds))) s) =
    (\d' → MemberDeclaration specs (Just (MemberDeclaratorList (Commad d' []))) s) . (d |: (snd . ds))
  split_decls d = return $ gmapT split_all_decls d

compound_split_decls :: Statement → Statement
compound_split_decls s
  | null (neTail l) = neHead l
  | otherwise = Statement_CompoundStatement $ CompoundStatement $ Curlied (OpenCurly_, White "") (Enclosed $ Just $ StatementSeq l) (CloseCurly_, White "")
  where l = split_decls s

split_all_decls :: Data a ⇒ a → a
split_all_decls = everywhere $ Maybe.fromMaybe id $ Maybe.listToMaybe . Maybe.catMaybes $
  [ cast ((>>= split_decls) :: NeList Declaration → NeList Declaration)
  , cast ((>>= split_decls) :: NeList Statement → NeList Statement)
  , cast (compound_split_decls :: Statement → Statement)
  , cast (concatMap (either (map Left . toList . split_decls) ((:[]) . Right)) :: [Either MemberDeclaration MemberAccessSpecifier] → [Either MemberDeclaration MemberAccessSpecifier])
  ]

-- Qualifier/specifier classification

is_primary_TypeSpecifier :: TypeSpecifier → Bool
is_primary_TypeSpecifier (TypeSpecifier_CvQualifier _) = False
is_primary_TypeSpecifier _ = True

is_primary_DeclSpecifier :: DeclSpecifier → Bool
is_primary_DeclSpecifier (DeclSpecifier_TypeSpecifier t) = is_primary_TypeSpecifier t
is_primary_DeclSpecifier _ = False

is_primary_MakeSpecifier :: MakeSpecifier → Bool
is_primary_MakeSpecifier (MakeSpecifier_DeclSpecifier t) = is_primary_DeclSpecifier t
is_primary_MakeSpecifier _ = False

-- Natural applications

class Apply a b c | a b → c where apply :: a → b → c
class MaybeApply a b where mapply :: (Functor m, Monad m) ⇒ a → b → m b

instance Apply a b b ⇒ Apply (Maybe a) b b where apply m x = maybe x (flip apply x) m
instance Apply a b b ⇒ Apply [a] b b where apply = flip $ foldl $ flip apply
instance Apply a b c ⇒ Apply a (Enclosed b) (Enclosed c) where apply x (Enclosed y) = Enclosed $ apply x y
instance MaybeApply a b ⇒ MaybeApply a (Enclosed b) where mapply x (Enclosed y) = Enclosed . mapply x y

-- Id application

instance Apply DeclaratorId PtrAbstractDeclarator PtrDeclarator where
  apply i (PtrAbstractDeclarator_NoptrAbstractDeclarator npad) = PtrDeclarator_NoptrDeclarator $ apply i npad
  apply i (PtrAbstractDeclarator o Nothing) = PtrDeclarator (with_trailing_white o) $ PtrDeclarator_NoptrDeclarator $ NoptrDeclarator_Id i
  apply i (PtrAbstractDeclarator o (Just pad)) = let pd = apply i pad in PtrDeclarator (case Cxx.Show.show_simple pd of
    (h:_) | not (isIdChar h) → o; _ → with_trailing_white o) pd

instance Apply DeclaratorId NoptrAbstractDeclarator NoptrDeclarator where
  apply i (NoptrAbstractDeclarator Nothing (Right s)) = NoptrDeclarator_Squared (NoptrDeclarator_Id i) s
  apply i (NoptrAbstractDeclarator (Just npad) (Right s)) = NoptrDeclarator_Squared (apply i npad) s
  apply i (NoptrAbstractDeclarator Nothing (Left params)) = NoptrDeclarator_WithParams (NoptrDeclarator_Id i) params
  apply i (NoptrAbstractDeclarator (Just npad) (Left params)) = NoptrDeclarator_WithParams (apply i npad) params
  apply i (NoptrAbstractDeclarator_PtrAbstractDeclarator (Parenthesized w (Enclosed pad) w')) =
    NoptrDeclarator_Parenthesized $ Parenthesized w (Enclosed $ apply i pad) w'

-- TypeSpecifier application

-- Here and elsewhere, we always keep specifiers in the order they appeared in the source text as much as possible.

instance Apply TypeSpecifier (NeList TypeSpecifier) (NeList TypeSpecifier) where
  apply d = (with_trailing_white d |:) . filter (compatible d) . toList

-- DeclSpecifier application

instance Apply DeclSpecifier [DeclSpecifier] [DeclSpecifier] where
  apply d = (with_trailing_white d :) . filter (compatible d)

instance Apply DeclSpecifier (NeList DeclSpecifier) (NeList DeclSpecifier) where
  apply d = (with_trailing_white d |:) . filter (compatible d) . toList

instance Apply [DeclSpecifier] (NeList DeclSpecifier) (NeList DeclSpecifier) where
  apply [] l = l
  apply l@(h:t) x = h |: (with_trailing_white t ++ filter (\s → all (compatible s) l) (toList x))

instance MaybeApply DeclSpecifier (NeList TypeSpecifier) where
  mapply (DeclSpecifier_TypeSpecifier x) typespecs = return $ apply x typespecs
  mapply x _ = fail $ "Invalid decl-specifier for type-specifier-seq: " ++ Cxx.Show.show_simple x

instance MaybeApply [DeclSpecifier] (NeList TypeSpecifier) where
  mapply = flip $ foldM $ flip mapply

-- MakeSpecifier application

type M = ([MakeSpecifier], Maybe PtrAbstractDeclarator)

instance MaybeApply M (DeclSpecifierSeq, Either Declarator (Maybe AbstractDeclarator)) where
  mapply x (l, Left d) | (l', d') ← apply x (l, d) = return (l', Left d')
  mapply x (DeclSpecifierSeq l, Right Nothing) = return $
    first DeclSpecifierSeq $ second (Right . (AbstractDeclarator_PtrAbstractDeclarator .)) $
      apply x (l, Nothing :: Maybe PtrAbstractDeclarator)
  mapply x (DeclSpecifierSeq l, Right (Just (AbstractDeclarator_PtrAbstractDeclarator d))) = return $
    first DeclSpecifierSeq $ second (Right . (AbstractDeclarator_PtrAbstractDeclarator .)) $ apply x (l, Just d)
  mapply _ (_, Right (Just (AbstractDeclarator_Ellipsis _))) = fail "Sorry, make-application to abstract-declarator with ellipsis not yet implemented."

instance MaybeApply M (TypeSpecifierSeq, Declarator) where
  mapply x (l, Declarator_PtrDeclarator d) = second Declarator_PtrDeclarator . mapply x (l, d)

instance Apply M (Maybe DeclSpecifierSeq, Declarator) (Maybe DeclSpecifierSeq, Declarator) where
  apply x (y, Declarator_PtrDeclarator d) = second Declarator_PtrDeclarator $ apply x (y, d)

instance Apply M (NeList DeclSpecifier, Declarator) (NeList DeclSpecifier, Declarator) where
  apply x (l, Declarator_PtrDeclarator d) = second Declarator_PtrDeclarator $ apply x (l, d)

instance Apply M (DeclSpecifierSeq, Declarator) (DeclSpecifierSeq, Declarator) where
  apply x (DeclSpecifierSeq l, d)= first DeclSpecifierSeq $ apply x (l, d)

instance Apply M (Maybe DeclSpecifierSeq, PtrDeclarator) (Maybe DeclSpecifierSeq, PtrDeclarator) where
  apply x (m, d) = first convert $ apply x (convert m :: [DeclSpecifier], d)

instance (Apply [MakeSpecifier] (l DeclSpecifier) (l DeclSpecifier), Apply MakeSpecifier (l DeclSpecifier, PtrDeclarator) (l DeclSpecifier, PtrDeclarator)) ⇒ Apply M (l DeclSpecifier, PtrDeclarator) (l DeclSpecifier, PtrDeclarator) where
  apply (l, Nothing) (l', x) =
    if any is_primary_MakeSpecifier l
      then (apply l l', PtrDeclarator_NoptrDeclarator $ NoptrDeclarator_Id $ convert x)
      else foldl (flip apply) (l', x) l
  apply (l, Just pad) (l', x) = (apply l l', apply (convert x :: DeclaratorId) pad)

instance Apply M (NeList DeclSpecifier, Maybe PtrAbstractDeclarator) (NeList DeclSpecifier, Maybe PtrAbstractDeclarator) where
  apply (l, Nothing) (l', Just x) =
    if any is_primary_MakeSpecifier l
      then (apply l l', Just x)
      else second Just $ foldl (flip apply) (l', x) l
  apply (l, m) (l', Nothing) = (apply l l', m)
  apply (l, Just x) (l', _) = (apply l l', Just x)

instance MaybeApply M (TypeSpecifierSeq, PtrDeclarator)  where
  mapply (l, Nothing) (l', x) =
    if any is_primary_MakeSpecifier l
      then flip (,) (PtrDeclarator_NoptrDeclarator $ NoptrDeclarator_Id $ convert x) . mapply l l'
      else foldM (flip mapply) (l', x) l
  mapply (l, Just pad) (l', x) = flip (,) (apply (convert x :: DeclaratorId) pad) . mapply l l'

instance MaybeApply [MakeSpecifier] TypeSpecifierSeq where mapply l l' = foldM (flip mapply) l' l

instance Apply MakeSpecifier ([DeclSpecifier], PtrDeclarator) ([DeclSpecifier], PtrDeclarator) where
  apply s (x, y) = maybe (apply s x, y) ((,) x) (mapply s y)

instance Apply MakeSpecifier (NeList DeclSpecifier, PtrAbstractDeclarator) (NeList DeclSpecifier, PtrAbstractDeclarator) where
  apply s (x, y) = maybe (apply s x, y) ((,) x) (mapply s y)

instance MaybeApply MakeSpecifier (TypeSpecifierSeq, PtrDeclarator) where
  mapply s (x, y) = maybe (flip (,) y . mapply s x) (return . (,) x) (mapply s y)

instance MaybeApply MakeSpecifier PtrDeclarator where
  mapply s (PtrDeclarator_NoptrDeclarator d) = PtrDeclarator_NoptrDeclarator . mapply s d
  mapply s (PtrDeclarator o d) = maybe (flip PtrDeclarator d . mapply s o) (return . PtrDeclarator o) (mapply s d)

instance MaybeApply MakeSpecifier PtrAbstractDeclarator where
  mapply s (PtrAbstractDeclarator_NoptrAbstractDeclarator d) =
    PtrAbstractDeclarator_NoptrAbstractDeclarator . mapply s d
  mapply _ _ = fail "Sorry, not yet implemented."

instance MaybeApply MakeSpecifier PtrOperator where
  mapply s (PtrOperator_Ptr o cvs) = PtrOperator_Ptr o . mapply s cvs
  mapply s (PtrOperator_Nested x y z cvs) = PtrOperator_Nested x y z . mapply s cvs
  mapply _ (PtrOperator_Ref _) = fail "Cannot apply make-specifier to reference ptr-operator."

eraseCv :: CvQualifier → Maybe CvQualifierSeq → Maybe CvQualifierSeq
eraseCv _ Nothing = Nothing
eraseCv q (Just (CvQualifierSeq l)) = CvQualifierSeq . toNonEmpty (neFilter ((≠ q) . fst) l)

instance MaybeApply MakeSpecifier (Maybe CvQualifierSeq) where
  mapply (MakeSpecifier_DeclSpecifier (DeclSpecifier_TypeSpecifier (TypeSpecifier_CvQualifier (cvq, _)))) =
    return . apply cvq
  mapply (NonCv cvq) = return . eraseCv cvq
  mapply _ = const $ fail "Cannot apply non-cv make-specifier to cv-qualifier-seq."

instance MaybeApply MakeSpecifier NoptrAbstractDeclarator where
  mapply s (NoptrAbstractDeclarator_PtrAbstractDeclarator (Parenthesized w (Enclosed d) w')) = do
    d' ← mapply s d
    return (NoptrAbstractDeclarator_PtrAbstractDeclarator (Parenthesized w (Enclosed d') w'))
  mapply _ _ = fail "Sorry, not yet implemented."

instance MaybeApply MakeSpecifier NoptrDeclarator where
  mapply _ (NoptrDeclarator_Id _) = fail "Cannot apply make-specifier to declarator-id."
  mapply s (NoptrDeclarator_Parenthesized (Parenthesized w (Enclosed d) w')) = do
    d' ← mapply s d
    return (NoptrDeclarator_Parenthesized (Parenthesized w (Enclosed d') w'))
  mapply s (NoptrDeclarator_Squared d ce) = do
    d' ← mapply s d
    return $ NoptrDeclarator_Squared d' ce
  mapply s (NoptrDeclarator_WithParams d p) =
    case mapply s d of
      Just d' → return $ NoptrDeclarator_WithParams d' p
      Nothing → NoptrDeclarator_WithParams d . mapply s p

instance MaybeApply MakeSpecifier ParametersAndQualifiers where
  mapply (MakeSpecifier_DeclSpecifier (DeclSpecifier_TypeSpecifier (TypeSpecifier_CvQualifier (cvq, _)))) (ParametersAndQualifiers c cvqs m e) =
    return $ ParametersAndQualifiers c (apply cvq cvqs) m e
  mapply (NonCv cvq) (ParametersAndQualifiers c cvqs m e) =
    return $ ParametersAndQualifiers c (eraseCv cvq cvqs) m e
  mapply _ _ = fail "Cannot apply non-cv make-specifier to parameters-and-qualifiers (yet)."

instance Apply MakeSpecifier (NeList DeclSpecifier, PtrDeclarator) (NeList DeclSpecifier, PtrDeclarator) where
  apply s (x, y) = maybe (apply s x, y) ((,) x) (mapply s y)

nonIntSpec :: (Eq s, Eq t, Convert t (Maybe s), Convert (BasicType, White) t) ⇒ s → NeList t → NeList t
nonIntSpec s l = case neFilter ((≠ Just s) . convert) l of
    l'@(h:t) | (convert (Int', White "") ∈ l') ∨ (convert (Double', White "") ∈ l') → h |: t
    l' → convert (Int', White " ") |: l'

instance MaybeApply MakeSpecifier (NeList TypeSpecifier) where
  mapply (MakeSpecifier_DeclSpecifier s) = mapply s
  mapply (NonStorageClassSpecifier _) = return
  mapply (NonFunctionSpecifier _) = return
  mapply (NonCv cvq) = return . filter_but_keep_nonempty ((≠ Just cvq) . convert)
  mapply (NonSign s) = return . nonIntSpec s
  mapply (NonLength s) = return . nonIntSpec s
  mapply LongLong = return . (convert LongSpec |:) . (convert LongSpec :) . filter (compatible (convert LongSpec :: TypeSpecifier)) . toList

instance MaybeApply MakeSpecifier TypeSpecifierSeq where
  mapply s (TypeSpecifierSeq l) = TypeSpecifierSeq . mapply s l

-- decl-specifier-seqs (which are always nonempty) always contain at least one primary specifier. Hence, removing, say, all cv-qualifiers, will always produce a proper (nonempty) decl-specifier-seq. However, the type system does not know that, and so we use the following ugly function:

filter_but_keep_nonempty :: forall a . (a → Bool) → NeList a → NeList a
filter_but_keep_nonempty p l = toNonEmpty (filter p $ toList l) `orElse` return (neHead l)

instance Apply MakeSpecifier (NeList DeclSpecifier) (NeList DeclSpecifier) where
  apply (MakeSpecifier_DeclSpecifier s) = apply s
  apply (NonStorageClassSpecifier scs) = filter_but_keep_nonempty $ (≠ Just scs) . convert
  apply (NonFunctionSpecifier fs) = filter_but_keep_nonempty $ (≠ Just fs) . convert
  apply (NonCv cvq) = filter_but_keep_nonempty $ (≠ Just cvq) . convert
  apply (NonSign s) = nonIntSpec s
  apply (NonLength s) = nonIntSpec s
  apply LongLong = (convert LongSpec |:) . (convert LongSpec :) . filter (compatible (convert LongSpec :: DeclSpecifier)) . toList

instance Apply MakeSpecifier [DeclSpecifier] [DeclSpecifier] where
  apply (MakeSpecifier_DeclSpecifier d) = apply d
  apply (NonStorageClassSpecifier scs) = filter $ (≠ Just scs) . convert
  apply (NonFunctionSpecifier fs) = filter $ (≠ Just fs) . convert
  apply (NonCv cvq) = filter $ (≠ Just cvq) . convert
  apply (NonSign s) = maybe [] (toList . nonIntSpec s) . toNonEmpty
  apply (NonLength s) = maybe [] (toList . nonIntSpec s) . toNonEmpty
  apply LongLong = (convert LongSpec :) . (convert LongSpec :) . filter (compatible (convert LongSpec :: DeclSpecifier))

-- PtrOperator application

instance MaybeApply PtrOperator (Maybe AbstractDeclarator) where
  mapply o Nothing = return $ Just $ AbstractDeclarator_PtrAbstractDeclarator $ PtrAbstractDeclarator o Nothing
  mapply o (Just (AbstractDeclarator_PtrAbstractDeclarator pad)) =
    return $ Just $ AbstractDeclarator_PtrAbstractDeclarator $ apply o pad
  mapply _ (Just (AbstractDeclarator_Ellipsis _)) = fail "Cannot apply ptr-operator to ellipsis."

instance Apply PtrOperator PtrAbstractDeclarator PtrAbstractDeclarator where
  apply o (PtrAbstractDeclarator_NoptrAbstractDeclarator npad) =
    PtrAbstractDeclarator_NoptrAbstractDeclarator (apply o npad)
  apply o (PtrAbstractDeclarator o' Nothing) =
    PtrAbstractDeclarator o' $ Just $ PtrAbstractDeclarator o Nothing
  apply o (PtrAbstractDeclarator o' (Just pad)) = PtrAbstractDeclarator o' $ Just $ apply o pad

instance Apply PtrOperator NoptrAbstractDeclarator NoptrAbstractDeclarator where
  apply o (NoptrAbstractDeclarator_PtrAbstractDeclarator (Parenthesized w pad w')) =
      NoptrAbstractDeclarator_PtrAbstractDeclarator (Parenthesized w (apply o pad) w')
  apply o (NoptrAbstractDeclarator (Just npad) e) = NoptrAbstractDeclarator (Just $ apply o npad) e
  apply o (NoptrAbstractDeclarator Nothing e) =
    NoptrAbstractDeclarator (Just $ NoptrAbstractDeclarator_PtrAbstractDeclarator $ parenthesized (PtrAbstractDeclarator o Nothing)) e

instance Apply PtrOperator ([TypeSpecifier], Either TypeSpecifier PtrAbstractDeclarator)
    ([TypeSpecifier], PtrAbstractDeclarator) where
  apply o (specs, Left spec) = (specs ++ [spec], PtrAbstractDeclarator o Nothing)
  apply o (specs, Right ad) = (specs, apply o ad)

-- Declarator application

instance Apply (Maybe PtrAbstractDeclarator) ([TypeSpecifier], Either TypeSpecifier PtrAbstractDeclarator)
    ([TypeSpecifier], Either TypeSpecifier PtrAbstractDeclarator) where
  apply Nothing = id
  apply (Just ad) = second Right . apply ad

instance Apply PtrAbstractDeclarator ([TypeSpecifier], Either TypeSpecifier PtrAbstractDeclarator)
    ([TypeSpecifier], PtrAbstractDeclarator) where
  apply pad (specs, Left spec) = (specs ++ [spec], pad)
  apply pad (specs, Right pad') = (specs, apply pad pad')

instance Apply PtrAbstractDeclarator PtrAbstractDeclarator PtrAbstractDeclarator where
  apply pad (PtrAbstractDeclarator o Nothing) = PtrAbstractDeclarator o $ Just pad
  apply pad (PtrAbstractDeclarator o (Just pad')) = PtrAbstractDeclarator o $ Just $ apply pad pad'
  apply pad (PtrAbstractDeclarator_NoptrAbstractDeclarator npad') = PtrAbstractDeclarator_NoptrAbstractDeclarator (apply pad npad')

instance Apply PtrAbstractDeclarator NoptrAbstractDeclarator NoptrAbstractDeclarator where
  apply pad (NoptrAbstractDeclarator_PtrAbstractDeclarator (Parenthesized w pad' w')) =
    NoptrAbstractDeclarator_PtrAbstractDeclarator (Parenthesized w (apply pad pad') w')
  apply (PtrAbstractDeclarator_NoptrAbstractDeclarator npad) npad' = apply npad npad'
  apply pad (NoptrAbstractDeclarator Nothing e) = NoptrAbstractDeclarator (Just $ NoptrAbstractDeclarator_PtrAbstractDeclarator $ parenthesized pad) e
  apply pad (NoptrAbstractDeclarator (Just npad) e) = NoptrAbstractDeclarator (Just $ apply pad npad) e

instance Apply NoptrAbstractDeclarator (Maybe NoptrAbstractDeclarator) NoptrAbstractDeclarator where
  apply x = maybe x (apply x)

instance Apply NoptrAbstractDeclarator NoptrAbstractDeclarator NoptrAbstractDeclarator where
  apply npad (NoptrAbstractDeclarator_PtrAbstractDeclarator (Parenthesized w pad w')) =
    NoptrAbstractDeclarator_PtrAbstractDeclarator (Parenthesized w (apply (PtrAbstractDeclarator_NoptrAbstractDeclarator npad) pad) w')
  apply npad (NoptrAbstractDeclarator m e) = NoptrAbstractDeclarator (Just $ apply npad m) e

-- MakeDeclaration application

instance Convert [a] (Maybe (NeList a)) where convert = toNonEmpty

instance Convert [DeclSpecifier] (Maybe DeclSpecifierSeq) where convert = fmap DeclSpecifierSeq . convert
instance Convert (Maybe DeclSpecifierSeq) [DeclSpecifier] where
  convert Nothing = []
  convert (Just (DeclSpecifierSeq l)) = toList l

instance Convert (Maybe DeclSpecifierSeq) [TypeSpecifier] where
  convert = convert . (convert :: Maybe DeclSpecifierSeq → [DeclSpecifier])

instance Apply MakeDeclaration (Maybe DeclSpecifierSeq, Declarator, Maybe (Either PureSpecifier BraceOrEqualInitializer)) (Maybe DeclSpecifierSeq, Declarator, Maybe (Either PureSpecifier BraceOrEqualInitializer)) where
  apply (MakeDeclaration specs m b) (specs', d, p) = (convert specs'', d', pure)
    where
      (specs'', d') = first (convert :: Maybe DeclSpecifierSeq → [DeclSpecifier]) $ apply (specs, m) (specs', d)
      pure = if any (\ms → case ms of NonFunctionSpecifier Virtual → True; MakeSpecifier_DeclSpecifier (DeclSpecifier_StorageClassSpecifier (Static, _)) → True; _ → False) specs then Nothing else case b of Definitely → Just $ Left $ PureSpecifier (IsOperator, White " ") (KwdZero, White " "); Indeterminate → p; DefinitelyNot → Nothing

-- cv-qualifier application

instance Apply CvQualifier (Maybe CvQualifierSeq) (Maybe CvQualifierSeq) where
  apply cvq Nothing = Just $ CvQualifierSeq $ return (cvq, White " ")
  apply cvq (Just x) = Just $ apply cvq x
instance Apply CvQualifier CvQualifierSeq CvQualifierSeq where
  apply cvq (CvQualifierSeq l) = CvQualifierSeq $ if any ((== cvq) . fst) l then l else (cvq, White " ") .: l

instance MaybeApply CvQualifier a ⇒ MaybeApply [CvQualifier] a where
  mapply l x = foldM (flip mapply) x l

instance MaybeApply CvQualifier PtrOperator where
  mapply cvq (PtrOperator_Nested mw n w cvq') = return $ PtrOperator_Nested mw n w $ apply cvq cvq'
  mapply cvq (PtrOperator_Ptr w cvq') = return $ PtrOperator_Ptr w $ apply cvq cvq'
  mapply _ (PtrOperator_Ref _) = fail "Cannot cv-qualify reference."

instance (Convert CvQualifier t, Compatible t t) ⇒ Apply CvQualifier [t] [t] where
  apply cvq l = let x = convert cvq in if any (not . compatible x) l then l else x : l

instance (Convert CvQualifier t, Compatible t t) ⇒ Apply CvQualifier (NeList t) (NeList t) where
  apply cvq l = let x = convert cvq in if any (not . compatible x) (toList l) then l else x .: l
  -- todo: merge last two using ListLike

instance Apply CvQualifier x x ⇒ Apply CvQualifier (x, Maybe PtrAbstractDeclarator) (x, Maybe PtrAbstractDeclarator) where
  apply cvq (l, Just ad) | Just ad' ← mapply cvq ad = (l, Just ad')
  apply cvq (l, mad) = (apply cvq l, mad)

instance MaybeApply CvQualifier InitDeclarator where
  mapply cvq (InitDeclarator d mi) = flip InitDeclarator mi . mapply cvq d

instance MaybeApply CvQualifier Declarator where
  mapply cvq (Declarator_PtrDeclarator d) = Declarator_PtrDeclarator . mapply cvq d

instance MaybeApply CvQualifier PtrDeclarator where
  mapply cvq (PtrDeclarator_NoptrDeclarator d) = PtrDeclarator_NoptrDeclarator . mapply cvq d
  mapply cvq (PtrDeclarator o d) = case mapply cvq d of
    Just d' → return $ PtrDeclarator o d'
    Nothing → flip PtrDeclarator d . mapply cvq o

instance MaybeApply CvQualifier NoptrDeclarator where
  mapply cvq (NoptrDeclarator_WithParams d p) = return $ case mapply cvq d of
      Just d' → NoptrDeclarator_WithParams d' p
      Nothing → NoptrDeclarator_WithParams d $ apply cvq p
  mapply cvq (NoptrDeclarator_Parenthesized (Parenthesized w d w'))
    = NoptrDeclarator_Parenthesized . (\x → Parenthesized w x w') . mapply cvq d
  mapply cvq (NoptrDeclarator_Squared d s) = flip NoptrDeclarator_Squared s . mapply cvq d
  mapply _ (NoptrDeclarator_Id _) = fail "Cannot cv-qualify declarator-id."

instance Apply CvQualifier ParametersAndQualifiers ParametersAndQualifiers where
  apply cvq (ParametersAndQualifiers d cvq' m e) = ParametersAndQualifiers d (apply cvq cvq') m e

instance Apply CvQualifier ([TypeSpecifier], Either TypeSpecifier PtrAbstractDeclarator)
    ([TypeSpecifier], Either TypeSpecifier PtrAbstractDeclarator) where
  apply cvq (l, Right ad)
    | Just ad' ← mapply cvq ad = (l, Right ad')
    | otherwise = (apply cvq l, Right ad)
  apply cvq (l, Left s) = let (s', l') = neElim $ apply cvq (s |: l) in (l', Left s')

instance MaybeApply CvQualifier PtrAbstractDeclarator where
  mapply cvq (PtrAbstractDeclarator_NoptrAbstractDeclarator d) =
    PtrAbstractDeclarator_NoptrAbstractDeclarator . mapply cvq d
  mapply cvq (PtrAbstractDeclarator o Nothing) = flip PtrAbstractDeclarator Nothing . mapply cvq o
  mapply cvq (PtrAbstractDeclarator o (Just a)) = case mapply cvq a of
    Just a' → return $ PtrAbstractDeclarator o (Just a')
    Nothing → flip PtrAbstractDeclarator (Just a) . mapply cvq o

instance MaybeApply CvQualifier NoptrAbstractDeclarator where
  mapply cvq (NoptrAbstractDeclarator (Just d) (Right t)) = flip NoptrAbstractDeclarator (Right t) . Just . mapply cvq d
  mapply _ (NoptrAbstractDeclarator Nothing (Right _)) = fail "Cannot cv-qualify leaf array noptr-abstract-declarator."
  mapply cvq (NoptrAbstractDeclarator m (Left p)) = return $ case m >>= mapply cvq of
      Nothing → NoptrAbstractDeclarator m $ Left $ apply cvq p
      Just m' → NoptrAbstractDeclarator (Just m') $ Left p
  mapply cvq (NoptrAbstractDeclarator_PtrAbstractDeclarator (Parenthesized w d w')) =
    NoptrAbstractDeclarator_PtrAbstractDeclarator . (\x → Parenthesized w x w') . mapply cvq d

-- Determination of whether declarators declare pointers/references

class IsPointerOrReference t r | t → r where is_pointer_or_reference :: t → r

instance IsPointerOrReference Declarator Bool where
  is_pointer_or_reference (Declarator_PtrDeclarator d) = case is_pointer_or_reference d of
    Definitely → True; _ → False
instance IsPointerOrReference PtrDeclarator TriBool where
  is_pointer_or_reference (PtrDeclarator_NoptrDeclarator d) = is_pointer_or_reference d
  is_pointer_or_reference (PtrDeclarator _ d) = case is_pointer_or_reference d of
    DefinitelyNot → DefinitelyNot; _ → Definitely
instance IsPointerOrReference NoptrDeclarator TriBool where
  is_pointer_or_reference (NoptrDeclarator_Id _) = Indeterminate
  is_pointer_or_reference (NoptrDeclarator_WithParams d _) = case is_pointer_or_reference d of
    Definitely → Definitely; _ → DefinitelyNot
  is_pointer_or_reference (NoptrDeclarator_Squared d _) = case is_pointer_or_reference d of
    Definitely → Definitely; _ → DefinitelyNot
  is_pointer_or_reference (NoptrDeclarator_Parenthesized (Parenthesized _ (Enclosed d) _)) = is_pointer_or_reference d
