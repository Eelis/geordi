{-# LANGUAGE Arrows, FlexibleInstances, TypeSynonymInstances #-}

module Editing.Parse (commandsP) where

import qualified Cxx.Parse
import qualified Cxx.Basics
import qualified Parsers as P
import Control.Monad (liftM2)
import Control.Monad.Error ()
import Control.Category (Category, (.), id)
import Control.Arrow (Arrow, (>>>), first, second, arr, ArrowChoice(..), returnA)
import Data.Either (partitionEithers)
import Parsers (choice, eof, (<|>), (<?>), symbols, char, anySymbol, lookAhead, notFollowedBy, sepBy1', many1Till', optParser, try, many, satisfy, spaces)
import Util (isVowel, (<<), NElist(..), unne, snd_unit, liftA2, Ordinal(..))
import Cxx.Basics (DeclaratorId)
import Request (EvalOpt)

import Prelude hiding ((.), id)
import Editing.Basics

data Terminators = Terminators { term_eof :: Bool, term_keywords :: [String] } deriving Eq
  -- term_eof states whether eof is a valid termination

type AndCont = String

data Parser a b = Parser Terminators (Terminators -> [AndCont] -> a -> P.Parser Char (Either String b))

fl :: String -> Parser a b
fl s = Parser (Terminators False []) $ \_ _ _ -> P.drain >> return (Left s)

zero_width :: Parser a b -> Bool
zero_width (Parser (Terminators b _) _) = b

terminators :: Parser a b -> [String]
terminators (Parser (Terminators _ t) _) = t

commit :: Parser a b -> Parser a b
commit (Parser t f) = Parser t $ \t' n v -> P.commit (f t' n v)

instance Category Parser where
  id = Parser (Terminators True []) $ \_ _ x -> return $ return x
  p' . Parser (Terminators b t) f =
    Parser (Terminators (b && zero_width p') (if null t then terminators p' else t))
     $ \(Terminators b'' t'') a'' x -> do
      let (Parser (Terminators b' t') f') = p'
        -- Todo: This match is delayed for a reason, I think. Document that reason.
      u <- f (Terminators (b' && b'') (if b' then t' ++ t'' else t')) (if null t' then a'' else []) x
      case u of
        Left e -> return $ Left e
        Right u' -> f' (Terminators b'' t'') a'' u'

instance Arrow Parser where
  arr f = Parser (Terminators True []) $ \_ _ -> return . return . f
  first (Parser ac f) = Parser ac $ \y ac' (b, d) -> fmap (fmap (\c' -> (c', d))) (f y ac' b)
  second (Parser ac f) = Parser ac $ \y ac' (d, b) -> fmap (fmap (\c' -> (d, c'))) (f y ac' b)

instance ArrowChoice Parser where
  left (Parser (Terminators b t) p) = Parser (Terminators b t) $ \(Terminators b' t') a x ->
    case x of
      Left y -> fmap Left `fmap` p (Terminators b' t') a y
      Right y -> return $ Right $ Right y
  right (Parser (Terminators b t) p) = Parser (Terminators b t) $ \(Terminators b' t') a x ->
    case x of
      Left y -> return $ Right $ Left y
      Right y -> fmap Right `fmap` p (Terminators b' t') a y

class Parse a where parse :: Parser x a

kwd :: [String] -> Parser a String
kwd s = Parser (Terminators False s) $ \(Terminators b _) _ _ -> fmap Right $ try $
  choice (try `fmap` symbols `fmap` s) << (if b then (eof <|>) else id) (char ' ' >> return ())

label :: String -> Parser a b -> Parser a b
label s (Parser t f) = Parser t $ \l a x -> f l a x <?> s

(<||>) :: Parser a b -> Parser a b -> Parser a b
Parser (Terminators b t) f <||> Parser (Terminators b' t') f' =
  Parser (Terminators (b || b') (t ++ t')) $ \y t'' x -> f y t'' x <|> f' y t'' x

instance Parse String where
  parse = label "verbatim string" $ (select cs <||>) $
    Parser (Terminators False []) $ \t _ _ -> quoted <|> unquoted t
   where
    quoted = char '`' >> fmap Right (many $ satisfy (/= '`')) << char '`' << spaces
    unquoted t = fmap (Right . unne . fst) $ many1Till' (P.silent anySymbol) $ try $ (if term_eof t then (eof <|>) else id) $ lookAhead (choice ((try . symbols . (' ':)) `fmap` term_keywords t)) >> char ' ' >> return ()
    cs :: [([String], String)]
    cs = first opt_an `fmap` [("comma", ","), ("space", " "), ("colon", ":"), ("semicolon", ";"), ("ampersand", "&"), ("tilde", "~")]

andP :: Parser a ()
andP = (kwd ["and"] >>>) $ Parser (Terminators True []) $ \_ a _ -> fmap Right $ notFollowedBy (choice $ try `fmap` symbols `fmap` a) >> return ()

instance Parse a => Parse (AndList a) where
  parse = sepP parse andP >>> arr AndList
    where
      sepP :: Parser a t -> Parser a t1 -> Parser a (NElist t)
      sepP (Parser u@(Terminators _ t) p) (Parser (Terminators _ t') p') = Parser (Terminators False t) $
        \(Terminators b'' t'') a v -> fmap f $ p (Terminators b'' (t' ++ t'')) (t ++ t'' ++ a) v `sepBy1'` p' u a v
      f :: NElist (Either String t) -> Either String (NElist t)
      f (NElist x l) = liftM2 NElist x $ case l of [] -> return []; (h : t) -> fmap unne $ f (NElist h t)

instance (Parse a, Parse b) => Parse (Either a b) where
  parse = (parse >>> arr Left) <||> (parse >>> arr Right)

semipure :: (a -> Either String b) -> Parser a b
semipure f = Parser (Terminators True []) $ \_ _ -> return . f

select :: [([String], a)] -> Parser x a
select = foldl1 (<||>) . map (\(s, r) -> if null s then arr (const r) else kwd s >>> arr (const r))

optional :: Parser a b -> Parser a ()
optional p = (p >>> arr (const ())) <||> arr (const ())

auto1 :: Parse a => (a -> b) -> Parser x b
auto1 f = parse >>> arr f

auto2 :: (Parse a, Parse b) => (a -> b -> c) -> Parser x c
auto2 f = proc x -> do a <- parse -< x; b <- parse -< x; returnA -< f a b

till, begin, end_kwds :: [String]
till = ["till", "until"]
begin = ["beginning", "begin", "front", "start"]
end_kwds = ["end", "back"]

opt_an :: String -> [String]
opt_an s@(c:_) | isVowel c = [s, "an " ++ s]
opt_an s = [s, "a " ++ s]

uncool :: Parser () a -> Terminators -> [AndCont] -> P.Parser Char (Either String a)
uncool (Parser _ f) t a = f t a ()

instance Parse a => Parse (Ranked a) where parse = auto2 Ranked <||> auto1 Sole

instance Parse BefAft where parse = select [(["before"], Before), (["after"], After)]

instance Parse RelativeBound where
  parse = select [(end_kwds, Back), (begin, Front)] <||> auto2 RelativeBound

instance Parse Ordinal where
  parse = label "ordinal" $ (>>> arr Ordinal) $ proc _ -> do
    optional $ kwd ["the"] -< ()
    (select [(["last"], -1), (["first"], 0)] -< ()) <||> do
    n <- select $ fmap (\n -> ([show (Ordinal n)], n)) [1..9] -< ()
    b <- select [(["last"], True), ([], False)] -< ()
    returnA -< if b then - n - 1 else n

instance Parse a => Parse (EverythingOr a) where
  parse = select [(["everything"], Everything)] <||> auto1 NotEverything

relative :: Parser a (Relative a)
relative = proc a -> do x <- parse -< (); y <- parse -< (); returnA -< Relative a x y
  <||> do b <- parse -< (); returnA -< Between a b
  <||> do returnA -< absolute a

instance Parse Bound where parse = select [(begin, front), (end_kwds, back)] <||> auto2 Bound

instance Parse Betw where
  parse = (kwd ["between"] >>>) $ proc _ -> do
      y <- parse -< ()
      do
        rank' <- andP >>> parse -< (); s <- parse -< ()
        returnA -< Betw (Bound Nothing $ NotEverything $ Ranked y s) $ RelativeBound Nothing $ absolute $ NotEverything $ Ranked rank' s
       <||> do
        x <- parse -< (); v <- andP >>> parse -< ()
        returnA -< Betw (Bound Nothing (NotEverything $ Ranked y x)) v
    <||> do x <- parse -< (); y <- andP >>> parse -< (); returnA -< Betw x y

relative_everything_orA :: Parser y (Relative (EverythingOr a))
relative_everything_orA =
    (kwd till >>> auto1 (Between Everything . Betw front))
  <||> liftA2 FromTill (kwd ["from"] >>> parse) ((kwd till >>> parse) <||> arr (const Back))
  <||> (kwd ["everything"] >>>
    (liftA2 (\x y -> Between Everything (Betw x y)) (kwd ["from"] >>> parse) (kwd till >>> parse)
      <||> (kwd till >>> auto1 (\x -> Between Everything (Betw front x)))
      <||> (arr (const Everything) >>> relative)))
  <||> (kwd ["begin"] >>> kwd till >>> auto1 (Between Everything . Betw front))
  <||> (proc _ -> do
    kwd ["before"] -< ()
    y <- auto1 NotEverything -< ()
    x <- do z <- kwd till >>> parse -< (); returnA -< Betw (Bound (Just Before) y) z
      <||> do returnA -< Betw front $ RelativeBound (Just Before) $ absolute y
    returnA -< Between Everything x)
  <||> auto1 (Between Everything)
  <||> proc _ -> do
    x <- kwd ["after"] >>> parse -< ()
    y <- (kwd till >>> parse -< ()) <||> (returnA -< Back)
    returnA -< Between Everything (Betw (Bound (Just After) x) y)

instance Parse (Relative (EverythingOr (Rankeds String))) where
  parse = (relative_everything_orA <||>) $ (parse >>>) $ proc x -> case x of
    Rankeds (AndList (NElist r [])) s -> do
        u <- kwd till >>> parse -< ()
        returnA -< Between Everything (Betw (Bound (Just Before) $ NotEverything $ Ranked r s) u)
      <||> (relative -< NotEverything x)
    Sole' s -> do
        u <- kwd till >>> parse -< ()
        returnA -< Between Everything (Betw (Bound (Just Before) $ NotEverything $ Sole s) u)
      <||> (relative -< NotEverything x)
    _ -> (relative -< NotEverything x)

instance Parse (Relative (EverythingOr (Ranked String))) where
  parse = (relative_everything_orA <||>) $ parse >>> proc x -> do
      u <- kwd till >>> parse -< ()
      returnA -< Between Everything $ Betw (Bound (Just Before) $ NotEverything x) u
    <||> (relative -< NotEverything x)

instance Parse Declaration where
  parse = k >>> kwd ["of"] >>> parse >>> arr DeclarationOf
    where k = Parser (Terminators False ["declaration"]) $ \(Terminators b _) _ _ -> fmap Right $ symbols "declaration" >> P.optional (char 's') >> (if b then (eof <|>) else id) (char ' ' >> return ())

instance Parse Position where
  parse = (select [(begin, Before), (end_kwds, After)] >>> arr (flip Position Everything)) <||> auto2 Position

instance Parse a => Parse (Rankeds a) where
  parse = (kwd ["all"] >>> ((kwd ["except", "but"] >>> auto2 AllBut) <||> auto1 All))
    <||> (kwd ["any", "every", "each"] >>> auto1 All) <||> auto2 Rankeds <||> auto1 Sole'

instance Parse PositionsClause where
  parse = (kwd ["at"] >>> select [(begin, Before), (end_kwds, After)] >>> arr (\ba -> PositionsClause ba $ and_one $ Right $ absolute Everything)) <||> auto2 PositionsClause

instance Parse Replacer where
  parse = liftA2 ReplaceOptions parse (wb >>> parse) <||> liftA2 Replacer parse (wb >>> parse)
    where wb = kwd ["with", "by"]

instance Parse Eraser where parse = auto1 EraseOptions <||> auto1 EraseText
instance Parse Mover where parse = liftA2 Mover parse (kwd ["to"] >>> parse)
instance Parse Swapper where parse = liftA2 Swapper parse (andP >>> parse)
instance Parse [EvalOpt] where parse = Parser (Terminators False []) $ \_ _ _ -> optParser
instance Parse Around where parse = kwd ["around"] >>> auto1 Around
instance Parse UseClause where parse = auto1 UseOptions <||> auto1 UseString

instance Parse Wrapping where
  parse =
    (kwd ["curlies", "braces", "curly brackets"] >>> arr (const (Wrapping "{" "}")))
    <||> (kwd ["parentheses", "parens", "round brackets"] >>> arr (const (Wrapping "(" ")")))
    <||> (kwd ["square brackets"] >>> arr (const (Wrapping "[" "]")))
    <||> (kwd ["angle brackets"] >>> arr (const (Wrapping "<" ">")))
    <||> (kwd ["single quotes"] >>> arr (const (Wrapping "'" "'")))
    <||> (kwd ["double quotes"] >>> arr (const (Wrapping "\"" "\"")))
    <||> liftA2 Wrapping parse (andP >>> parse)
  -- Todo: This is duplicated below.

instance Parse a => Parse (Maybe a) where parse = (parse >>> arr Just) <||> arr (const Nothing)

instance Parse Command where
  parse = label "edit command" $
    (kwd ["insert", "add"] >>> commit ((parse >>> arr (Use `fmap` (fmap UseOptions))) <||> auto2 Insert)) <||>
    (kwd ["append"] >>> commit (auto2 Append)) <||>
    (kwd ["prepend"] >>> commit ((parse >>> arr (Use `fmap` (fmap UseOptions))) <||> auto2 Prepend)) <||>
    (kwd ["erase", "remove", "kill", "cut", "omit", "delete", "drop"] >>> commit (auto1 Erase)) <||>
    (kwd ["replace"] >>> commit (auto1 Replace)) <||>
    (kwd ["use"] >>> commit (auto1 Use)) <||>
    (kwd ["move"] >>> commit (auto1 Move)) <||>
    (kwd ["swap"] >>> commit (auto1 Swap)) <||>
    (kwd ["wrap"] >>> commit (parse >>> snd_unit (auto1 Left <||> (kwd ["in"] >>> auto1 Right)) >>> semipure (uncurry wc)))
    where
      wc :: (AndList Substrs) -> Either (AndList Around) Wrapping -> Either String Command
      wc what (Right wrapping) = return $ WrapIn what wrapping
      wc (AndList (NElist (Right (Between (NotEverything (Sole' x)) (Betw (Bound (Just Before) Everything) Back))) [])) (Left what) =
        (\q -> WrapAround q what) `fmap` case () of
          ()| x `elem` ["curlies", "braces", "curly brackets"] -> return $ Wrapping "{" "}"
          ()| x `elem` ["parentheses", "parens", "round brackets"] -> return $ Wrapping "(" ")"
          ()| x `elem` ["square brackets"] -> return $ Wrapping "[" "]"
          ()| x `elem` ["angle brackets"] -> return $ Wrapping "<" ">"
          ()| x `elem` ["single quotes"] -> return $ Wrapping "'" "'"
          ()| x `elem` ["double quotes"] -> return $ Wrapping "\"" "\""
          ()| otherwise -> fail "Unrecognized wrapping description."
      wc (AndList (NElist (Right (Between (NotEverything (Sole' x)) (Betw (Bound (Just Before) Everything) Back)))
        [Right (Between (NotEverything (Sole' y)) (Betw (Bound (Just Before) Everything) Back))])) (Left what) =
          return $ WrapAround (Wrapping x y) what
      wc _ (Left _) = fail "Malformed wrap command."

instance Parse Cxx.Basics.MakeDeclaration where
  parse = Parser (Terminators False []) $ \_ _ _ -> Right `fmap` Cxx.Parse.makeDeclParser

instance Parse DeclaratorId where
  parse = Parser (Terminators False []) $ \_ _ _ -> Right `fmap` Cxx.Parse.declaratorIdParser

instance Parse SemCommand where
  parse = label "edit command" $ kwd ["make"] >>> commit (auto2 Make)

commandsP :: P.Parser Char (Either String (([Command], [SemCommand]), Bool))
commandsP = uncool p (Terminators True []) []
  where
    p = liftA2 (,)
      (parse >>> arr (partitionEithers . unne . andList))
      ((andP >>> kwd ["show"] >>> arr (const True)) <||> arr (const False))
