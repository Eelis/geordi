{-# LANGUAGE OverlappingInstances, UndecidableInstances, Arrows, FlexibleInstances, TypeSynonymInstances, CPP, PatternGuards #-}

module Editing.Parse (commandsP, finalCommandP) where

import qualified Cxx.Parse
import qualified Cxx.Basics
import qualified Cxx.Operations
import qualified Parsers as P
import Data.Foldable (toList)
import Data.List.NonEmpty (( |:))
import Control.Monad.Error ()
import Control.Category (Category, (.), id)
import Control.Arrow (Arrow, (>>>), first, second, arr, ArrowChoice(..), returnA)
import Data.Generics (DataType, Constr, toConstr, Data)
import Parsers (choice, eof, (<|>), (<?>), symbols, char, anySymbol, lookAhead, notFollowedBy, many1Till, optParser, try, many, satisfy, spaces)
import Util ((<<), liftA2, Ordinal(..), apply_if, cardinals, plural, indefinite, neElim)
import Cxx.Basics (DeclaratorId)
import Request (EvalOpt)

import Prelude hiding ((.), id, and)
import Prelude.Unicode
import Editing.Basics

data Terminators = Terminators { term_eof :: Bool, term_keywords :: [String], and_conts :: [String] } deriving Eq
  -- term_eof states whether eof is a valid termination of a directly preceding verbatim stings.
  -- term_keywords lists keywords that should terminate directly preceding verbatim strings.
  -- and_conts lists keywords that should terminate directly preceding "and"-lists (when appearing after "and ").

data Parser a b = Parser { terminators :: Terminators, _p :: (Terminators → a → P.Parser Char (Either String b)) }

commit :: Parser a b → Parser a b
commit (Parser t f) = Parser t $ \t' → P.commit . f t'

instance Category Parser where
  id = Parser (Terminators True [] []) $ const $ return . return
  p' . Parser (Terminators b t na) f =
    Parser (Terminators (b ∧ term_eof (terminators p')) (if null t then term_keywords $ terminators p' else t)
        (if null t then and_conts $ terminators p' else na))
     $ \(Terminators b'' t'' a) x → do
      let (Parser (Terminators b' t' na') f') = p'
        -- Todo: This match is delayed for a reason, I think. Document that reason.
      u ← f (Terminators (b' ∧ b'') (if b' then t' ++ t'' else t') (if b' then a++na' else na')) x
      case u of
        Left e → return $ Left e
        Right u' → f' (Terminators b'' t'' a) u'
  -- I suspect this composition might not actually be associative. Would be neat to check in Coq.

instance Arrow Parser where
  arr f = Parser (Terminators True [] []) $ \_ → return . return . f
  first (Parser ac f) = Parser ac $ \y (b, d) → fmap (fmap (\c' → (c', d))) (f y b)
  second (Parser ac f) = Parser ac $ \y (d, b) → fmap (fmap (\c' → (d, c'))) (f y b)

instance ArrowChoice Parser where
  left (Parser t p) = Parser t $ \t' x →
    case x of
      Left y → fmap Left `fmap` p t' y
      Right y → return $ Right $ Right y
  right (Parser t p) = Parser t $ \t' x →
    case x of
      Left y → return $ Right $ Left y
      Right y → fmap Right `fmap` p t' y

class Parse a where parse :: Parser x a
class ParsePlural a where parsePlural :: Parser x a

kwd :: [String] → Parser a String
kwd s = Parser (Terminators False s []) $ \(Terminators b _ _) _ → fmap Right $ try $
  choice (try `fmap` symbols `fmap` s) << apply_if b (eof <|>) (P.char_unit ' ')

label :: String → Parser a b → Parser a b
label s (Parser t f) = Parser t $ \l x → f l x <?> s

(<||>) :: Parser a b → Parser a b → Parser a b
Parser (Terminators b t a) f <||> Parser (Terminators b' t' a') f' =
  Parser (Terminators (b ∨ b') (t ++ t') (a ++ a')) $ \y x → f y x <|> f' y x

named_characters :: [(String, String)]
named_characters = [("comma", ","), ("space", " "), ("colon", ":"), ("semicolon", ";"), ("ampersand", "&"), ("tilde", "~"), ("slash", "/"), ("backslash", "\\"), ("asterisk", "*"), ("caret", "^"), ("period", "."), ("dot", "."), ("ellipsis", "...")]

instance Parse String where
  parse = label "verbatim string" $ (parsePlural <||>) $ (select cs <||>) $
    Parser (Terminators False [] []) $ \t _ → quoted <|> unquoted t
   where
    quoted = char '`' >> fmap Right (many $ satisfy (/= '`')) << char '`' << spaces
    unquoted t = fmap (Right . toList . fst) $ many1Till (P.silent anySymbol) $ try $ apply_if (term_eof t) (eof <|>) $ lookAhead
      (choice ((\k → try $ symbols (' ': k) >> (eof <|> P.char_unit ' ')) `fmap` term_keywords t)) >> P.char_unit ' '
    cs :: [([String], String)]
    cs = map (\(x, y) → ([x, indefinite x], y)) named_characters

instance ParsePlural String where
  parsePlural = label "verbatim string" $ select $ map (first ((:[]) . plural)) named_characters

instance Parse a ⇒ Parse (AndList a) where
  parse = liftA2 ( |:) parse ((and parse >>> arr (toList . andList)) <||> arr (const [])) >>> arr AndList

pzero :: Parser x a
pzero = Parser (Terminators False [] []) $ \_ _ → P.pzero

and :: Parser x a → Parser x a
and (Parser (Terminators _ t _) p) = Parser (Terminators False ["and"] t) $
  \t' x → try $ do
    symbols "and "
    notFollowedBy (choice $ try `fmap` symbols `fmap` and_conts t')
    p t' x

instance (Parse a, Parse b) ⇒ Parse (Either a b) where parse = (parse >>> arr Left) <||> (parse >>> arr Right)
instance (ParsePlural a, ParsePlural b) ⇒ ParsePlural (Either a b) where
  parsePlural = (parsePlural >>> arr Left) <||> (parsePlural >>> arr Right)

select :: [([String], a)] → Parser x a
select = foldl1 (<||>) . map (\(s, r) → if null s then arr (const r) else kwd s >>> arr (const r))

optional :: Parser a b → Parser a ()
optional p = (p >>> arr (const ())) <||> arr (const ())

option :: Parser a b → Parser a (Maybe b)
option p = (p >>> arr Just) <||> arr (const Nothing)

auto1 :: Parse a ⇒ (a → b) → Parser x b
auto1 f = parse >>> arr f

auto2 :: (Parse a, Parse b) ⇒ (a → b → c) → Parser x c
auto2 f = liftA2 f parse parse

till, begin, end_kwds :: [String]
till = ["till", "until"]
begin = ["beginning", "begin", "front", "start"]
end_kwds = ["end", "back"]

instance Parse a ⇒ Parse (Ranked a) where parse = auto2 Ranked <||> auto1 Sole

instance Parse BefAft where parse = select [(["before"], Before), (["after"], After)]

instance Parse (AndList BefAft) where
  parse =
    (kwd ["around"] >>> arr (const $ AndList $ Before |: [After])) <||>
    (liftA2 ( |:) parse ((and parse >>> arr (toList . andList)) <||> arr (const [])) >>> arr AndList)

instance Parse RelativeBound where
  parse = select [(end_kwds, Back), (begin, Front)] <||> auto2 RelativeBound

instance Parse Ordinal where
  parse = label "ordinal" $ (>>> arr Ordinal) $ proc _ → do
    optional $ kwd ["the"] -< ()
    (select [(["last"], -1), (["first"], 0)] -< ()) <||> do
    n ← select $ fmap (\n → ([show (Ordinal n)], n)) [1..9] -< ()
    b ← select [(["last"], True), ([], False)] -< ()
    returnA -< if b then - n - 1 else n

parseSmallPositiveCardinal :: Parser a Int
parseSmallPositiveCardinal = select $ map (\(x,y) → ([x {-, show y-}], y)) $ tail $ zip cardinals [0..]

instance Parse OccurrencesClause where
  parse = label "ordinal" $ (>>> arr OccurrencesClause) $  -- Todo: FIx inappropriate label.
    (optional (kwd ["the"]) >>> kwd ["first"] >>> parseSmallPositiveCardinal >>> arr (\n → fmap Ordinal $ 0 |: [1 .. n-1])) <||>
    (optional (kwd ["the"]) >>> kwd ["last"] >>> parseSmallPositiveCardinal >>> arr (\n → fmap Ordinal $ (-1) |: [-n .. -2])) <||>
    auto1 return

everything_kwds :: [String]
everything_kwds = ["everything", "code"]

instance Parse a ⇒ Parse (EverythingOr a) where
  parse = select [(everything_kwds, Everything)] <||> auto1 NotEverything

relative :: Parser a (Relative a)
relative = proc a → do x ← parse -< (); y ← parse -< (); returnA -< Relative a x y
  <||> do b ← parse -< (); returnA -< Between a b
  <||> do returnA -< Absolute a

instance Parse a ⇒ Parse (In a) where parse = auto2 In

instance Parse Bound where parse = select [(begin, front), (end_kwds, back)] <||> auto2 Bound

instance Parse Betw where
  parse = (kwd ["between"] >>>) $ proc _ → do
      Wrapping b a ← select namedWrappings -< ()
      returnA -< Betw (Bound Nothing $ NotEverything $ Sole $ Right b) (RelativeBound Nothing $ Absolute $ NotEverything $ Sole $ Right a)
    <||> do
      y ← parse -< ()
      do
        rank' ← and parse -< (); s ← parse -< ()
        returnA -< Betw (Bound Nothing $ NotEverything $ Ranked y s) $ RelativeBound Nothing $ Absolute $ NotEverything $ Ranked rank' s
       <||> do
        x ← parse -< (); v ← and parse -< ()
        returnA -< Betw (Bound Nothing (NotEverything $ Ranked y x)) v
    <||> do liftA2 Betw parse (and parse) -< ()

relative_everything_orA :: Parser y (Relative (EverythingOr a))
relative_everything_orA =
    (kwd till >>> auto1 (Between Everything . Betw front))
  <||> liftA2 FromTill (kwd ["from"] >>> parse) ((kwd till >>> parse) <||> arr (const Back))
  <||> (kwd everything_kwds >>>
    (liftA2 (\x → Between Everything . Betw x) (kwd ["from"] >>> parse) (kwd till >>> parse)
      <||> (kwd till >>> auto1 (Between Everything . Betw front))
      <||> (arr (const Everything) >>> relative)))
  <||> (proc _ → do
    kwd ["before"] -< ()
    y ← auto1 NotEverything -< ()
    x ← do z ← kwd till >>> parse -< (); returnA -< Betw (Bound (Just Before) y) z
      <||> do returnA -< Betw front $ RelativeBound (Just Before) $ Absolute y
    returnA -< Between Everything x)
  <||> auto1 (Between Everything)
  <||> proc _ → do
    x ← kwd ["after"] >>> parse -< ()
    y ← (kwd till >>> parse -< ()) <||> (returnA -< Back)
    returnA -< Between Everything (Betw (Bound (Just After) x) y)

instance Parse (Relative Substr) where
  parse = (relative_everything_orA <||>) $ parse >>> proc x → do
      u ← kwd till >>> parse -< ()
      returnA -< Between Everything $ Betw (Bound (Just Before) $ NotEverything x) u
    <||> (relative -< NotEverything x)

instance Parse a ⇒ Parse (Relative a) where parse = parse >>> relative

instance Parse (Relative (EverythingOr (Rankeds (Either Cxx.Basics.Findable String)))) where
  parse = (relative_everything_orA <||>) $ (parse >>>) $ proc x → case x of
    Rankeds (AndList l) s | (OccurrencesClause l', []) ← neElim l, (r, []) ← neElim l' → do
      -- Writing this with view patterns causes GHC to panic..
        u ← kwd till >>> parse -< ()
        returnA -< Between Everything (Betw (Bound (Just Before) $ NotEverything $ Ranked r s) u)
      <||> (relative -< NotEverything x)
    Sole' s → do
        u ← kwd till >>> parse -< ()
        returnA -< Between Everything (Betw (Bound (Just Before) $ NotEverything $ Sole s) u)
      <||> (relative -< NotEverything x)
    _ → (relative -< NotEverything x)

instance Parse ImplicitDeclarationOf where parse = auto1 ImplicitDeclarationOf
instance Parse ImplicitBodyOf where parse = auto1 ImplicitBodyOf
instance ParsePlural ImplicitBodyOf where parsePlural = pzero
instance ParsePlural ImplicitDeclarationOf where parsePlural = pzero

instance Parse Cxx.Basics.Findable where
  parse = (parsePlural <||>) $
    (kwd ["declaration"] >>> kwd ["of"] >>> auto1 Cxx.Basics.DeclarationOf) <||>
    (kwd ["body"] >>> kwd ["of"] >>> auto1 Cxx.Basics.BodyOf) <||>
    label "production-name" (
      auto1 Cxx.Basics.FindableDataType <||>
      auto1 Cxx.Basics.FindableConstr <||>
      (kwd ["constructor", "ctor"] >>> arr (const Cxx.Basics.Constructor)) <||>
      (kwd ["destructor", "dtor"] >>> arr (const Cxx.Basics.Destructor)) <||>
      (kwd ["conversion-function"] >>> arr (const Cxx.Basics.ConversionFunction)) <||>
      (kwd ["parameter-declaration"] >>> arr (const Cxx.Basics.FindableParameterDeclaration)) <||>
      (kwd ["template-parameter"] >>> arr (const Cxx.Basics.TemplateParameter)) <||>
      (kwd ["template-argument"] >>> arr (const Cxx.Basics.TemplateArgument)))

instance ParsePlural Cxx.Basics.Findable where
  parsePlural =
    (label "\"declaration\"" (kwd ["declarations"]) >>> kwd ["of"] >>> auto1 Cxx.Basics.DeclarationOf) <||>
    (label "\"body\"" (kwd ["bodies"]) >>> kwd ["of"] >>> auto1 Cxx.Basics.BodyOf) <||>
    label "production-name" (
      (parsePlural >>> arr Cxx.Basics.FindableDataType) <||>
      (parsePlural >>> arr Cxx.Basics.FindableConstr) <||>
      (kwd ["constructors", "ctors"] >>> arr (const Cxx.Basics.Constructor)) <||>
      (kwd ["destructors", "dtors"] >>> arr (const Cxx.Basics.Destructor)) <||>
      (kwd ["conversion-functions"] >>> arr (const Cxx.Basics.ConversionFunction)) <||>
      (kwd ["parameter-declarations"] >>> arr (const Cxx.Basics.FindableParameterDeclaration)) <||>
      (kwd ["template-parameters"] >>> arr (const Cxx.Basics.TemplateParameter)) <||>
      (kwd ["template-arguments"] >>> arr (const Cxx.Basics.TemplateArgument)))

class Constructor a where to_constr :: a → Constr
instance Data a ⇒ Constructor a where to_constr x = toConstr x
instance Constructor b ⇒ Constructor (a → b) where to_constr f = to_constr $ f undefined

instance Parse DataType where
  parse = parsePlural <||> select (map (\t → ([show $ Cxx.Basics.FindableDataType t], t)) Cxx.Operations.findable_productions)
instance ParsePlural DataType where
  parsePlural = select $ map (\t → ([show (Cxx.Basics.FindableDataType t) ++ "s"], t)) Cxx.Operations.findable_productions

parseableConstructors :: [Constr]
parseableConstructors =
#define P(n) to_constr Cxx.Basics.n
  [ P(BooleanLiteral), P(PointerLiteral), P(IfStatement), P(SwitchStatement), P(WhileStatement), P(DoWhileStatement)
  , P(ForStatement), P(BreakStatement), P(ContinueStatement), P(ReturnStatement), P(GotoStatement) ]
#undef P

instance Parse Constr where
  parse = parsePlural <||> select (map (\c → ([show $ Cxx.Basics.FindableConstr c], c)) parseableConstructors)
instance ParsePlural Constr where
  parsePlural = select $ map (\c → ([show (Cxx.Basics.FindableConstr c) ++ "s"], c)) parseableConstructors

instance Parse Position where
  parse = (select [(begin, Before), (end_kwds, After)] >>> arr (flip Position $ flip In Nothing $ Absolute Everything)) <||> auto2 Position

instance (Parse a, ParsePlural a) ⇒ Parse (Rankeds a) where
  parse =
    (parsePlural >>> arr All) <||> (kwd ["all"] >>> ((kwd ["except", "but"] >>> auto2 AllBut) <||> auto1 All))
    <||> (kwd ["any", "every", "each"] >>> auto1 All) <||> auto2 Rankeds <||> auto1 Sole'

instance Parse InClause where parse = kwd ["in"] >>> auto1 InClause

instance Parse AppendPositionsClause where
  parse = auto1 AppendIn <||> auto1 NonAppendPositionsClause
instance Parse PrependPositionsClause where
  parse = auto1 PrependIn <||> auto1 NonPrependPositionsClause

instance Parse Substrs where parse = auto1 Substrs
instance Parse MakeSubject where parse = auto1 MakeSubject

instance Parse PositionsClause where
  parse = (kwd ["at"] >>> select [(begin, Before), (end_kwds, After)] >>> arr (\ba → PositionsClause (and_one ba) $ Substrs $ and_one $ flip In Nothing $ Absolute Everything)) <||> auto2 PositionsClause

instance Parse Replacer where
  parse = liftA2 ReplaceOptions parse (wb >>> parse) <||> liftA2 Replacer parse (wb >>> parse)
    where wb = kwd ["with", "by"]

instance Parse Changer where
  parse = liftA2 ChangeOptions parse (wb >>> parse) <||> liftA2 Changer parse (wb >>> parse)
    where wb = kwd ["to"]

instance Parse Eraser where
  parse = liftA2 EraseAround parse (zw $ kwd ["around"] >>> parse >>> arr Around) <||>
      auto1 EraseOptions <||> auto1 EraseText
    where zw (Parser (Terminators _ x y) z) = Parser (Terminators True x y) z
      -- This use of 'zw' is a dirty hack.
instance Parse Mover where parse = liftA2 Mover parse (kwd ["to"] >>> parse)
instance Parse [EvalOpt] where parse = Parser (Terminators False [] []) $ \_ _ → optParser
instance Parse a ⇒ Parse (Around a) where parse = kwd ["around"] >>> auto1 Around
instance Parse UsePattern where parse = auto1 UsePattern
instance Parse (Relative UsePattern) where parse = parse >>> relative

instance Parse UseClause where parse = auto1 UseOptions <||> (parse {- >>> relative-} >>> arr UseString)

namedWrappings :: [([String], Wrapping)]
namedWrappings =
  [ (["curlies", "braces", "curly brackets"], Wrapping "{" "}")
  , (["parentheses", "parens", "round brackets"], Wrapping "(" ")")
  , (["square brackets"], Wrapping "[" "]")
  , (["angle brackets"], Wrapping "<" ">")
  , (["single quotes"], Wrapping "'" "'")
  , (["double quotes"], Wrapping "\"" "\"")
  , (["spaces"], Wrapping " " " ") ]

instance Parse Wrapping where
  parse = label "wrapping description" $ select namedWrappings <||> liftA2 Wrapping parse (and parse)

instance Parse Insertee where
  parse = label "wrapping description" (select namedWrappings >>> arr WrapInsert) <||>
    liftA2 (\x y → case y of Nothing → SimpleInsert x; Just y' → WrapInsert (Wrapping x y')) parse (option (and parse))

instance Parse a ⇒ Parse (Maybe a) where parse = (parse >>> arr Just) <||> arr (const Nothing)

instance Parse Command where
  parse = label "edit command" $
    (kwd ["insert", "add"] >>> commit ((parse >>> arr (Use `fmap` fmap UseOptions)) <||> auto2 Insert)) <||>
    (kwd ["append"] >>> commit (auto2 Append)) <||>
    (kwd ["prepend"] >>> commit ((parse >>> arr (Use `fmap` fmap UseOptions)) <||> auto2 Prepend)) <||>
    (kwd ["erase", "remove", "kill", "cut", "omit", "delete", "drop"] >>> commit (auto1 Erase)) <||>
    (kwd ["replace"] >>> commit (auto1 Replace)) <||>
    (kwd ["change"] >>> commit (auto1 Change)) <||>
    (kwd ["make"] >>> commit (auto2 Make)) <||>
    (kwd ["use"] >>> commit (auto1 Use)) <||>
    (kwd ["move"] >>> commit (auto1 Move)) <||>
    (kwd ["swap"] >>> commit (liftA2 Swap parse (option (kwd ["with"] >>> parse))))

instance Parse Cxx.Basics.MakeDeclaration where
  parse = Parser (Terminators False ["a", "an", "const"] []) $ \_ _ → Right `fmap` Cxx.Parse.makeDeclParser

instance Parse DeclaratorId where
  parse = Parser (Terminators False [] []) $ \_ _ → Right `fmap` Cxx.Parse.declaratorIdParser

instance Parse MakeClause where parse = auto2 MakeClause

instance Parse FinalCommand where
  parse = label "edit command" $
    (kwd ["show", "display"] >>> commit (option parse >>> arr Show)) <||>
    (kwd ["identify"] >>> commit (auto1 Identify)) <||>
    (kwd ["parse"] >>> arr (const Parse)) <||>
    (kwd ["diff"] >>> arr (const Diff))

instance Parse ([Command], Maybe FinalCommand) where
  parse = liftA2 (,) (parse >>> arr (toList . andList)) (option $ and parse)

uncool :: Parser () a → Terminators → P.Parser Char (Either String a)
uncool (Parser _ f) t = f t ()

commandsP :: P.Parser Char (Either String ([Command], Maybe FinalCommand))
commandsP = uncool parse $ Terminators True [] []

finalCommandP :: P.Parser Char (Either String FinalCommand)
finalCommandP = uncool parse $ Terminators True [] []
