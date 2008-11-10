module EditCommandParse (Terminators(..), commandP, commandsP) where

import Control.Monad.Error ()
import Control.Arrow (Arrow(..), ArrowChoice(..), returnA)
import Text.ParserCombinators.Parsec (choice, char, string, try, lookAhead, eof, (<|>), (<?>), CharParser, anyChar, spaces)
import Util (isVowel, (<<), NElist(..), unne, snd_unit, (.), show_ordinal, liftA2)
import ParsecUtil (sepBy1', notFollowedBy, many1Till', optParser)
import Request (EvalOpt)

import Prelude hiding ((.))
import EditCommandGrammar

data Terminators = Terminators { term_eof :: Bool, term_keywords :: [String] } deriving Eq
  -- term_eof states whether eof is a valid termination

type AndCont = String

data Parser st a b = Parser Terminators (Terminators -> [AndCont] -> a -> CharParser st b)

zero_width :: Parser st a b -> Bool
zero_width (Parser (Terminators b _) _) = b

terminators :: Parser st a b -> [String]
terminators (Parser (Terminators _ t) _) = t

instance Arrow (Parser st) where
  pure f = Parser (Terminators True []) $ \_ _ -> return . f
  Parser (Terminators b t) f >>> p' =
    Parser (Terminators (b && zero_width p') (if null t then terminators p' else t))
     $ \(Terminators b'' t'') a'' x -> do
      let (Parser (Terminators b' t') f') = p'
      u <- f (Terminators (b' && b'') (if b' then t' ++ t'' else t')) (if null t' then a'' else []) x
      f' (Terminators b'' t'') a'' u
  first (Parser ac f) = Parser ac $ \y ac' (b, d) -> do c <- f y ac' b; return (c, d)
  second (Parser ac f) = Parser ac $ \y ac' (d, b) -> do c <- f y ac' b; return (d, c)

instance ArrowChoice (Parser st) where
  left (Parser (Terminators b t) p) = Parser (Terminators b t) $ \(Terminators b' t') a ->
    either ((Left .) . p (Terminators b' t') a) (return . Right)
  right (Parser (Terminators b t) p) = Parser (Terminators b t) $ \(Terminators b' t') a ->
    either (return . Left) ((Right .) . p (Terminators b' t') a)

class Parse a where parse :: Parser st x a

kwd :: [String] -> Parser st a String
kwd s = Parser (Terminators False s) $ \(Terminators b _) _ _ -> try $
  choice (try . string . s) << (if b then (eof <|>) else id) (char ' ' >> return ())

label :: String -> Parser st a b -> Parser st a b
label s (Parser t f) = Parser t $ \l a x -> f l a x <?> s

(<||>) :: Parser st a b -> Parser st a b -> Parser st a b
Parser (Terminators b t) f <||> Parser (Terminators b' t') f' =
  Parser (Terminators (b || b') (t ++ t')) $ \y t'' x -> f y t'' x <|> f' y t'' x

instance Parse String where
  parse = label "verbatim string" $ (select cs <||>) $
    Parser (Terminators False []) $ \t _ _ -> (fst .) $ many1Till' anyChar $ try $ (if term_eof t then (eof <|>) else id) $ lookAhead (choice (try . string . (' ':) . term_keywords t)) >> char ' ' >> return ()
   where
    cs :: [([String], String)]
    cs = first opt_an . [("comma", ","), ("space", " "), ("colon", ":"), ("semicolon", ";"), ("ampersand", "&"), ("tilde", "~")]

andP :: Parser st a ()
andP = tryP $ (kwd ["and"] >>>) $ Parser (Terminators True []) $ \_ a _ -> notFollowedBy (choice $ try . string . a)
  where
    tryP :: Parser st a b -> Parser st a b
    tryP (Parser t f) = Parser t $ \x y z -> try (f x y z)

instance Parse a => Parse (AndList a) where
  parse = sepP parse andP >>> pure AndList
    where
      sepP :: Parser st a t -> Parser st a t1 -> Parser st a (NElist t)
      sepP (Parser u@(Terminators _ t) p) (Parser (Terminators _ t') p') = Parser (Terminators False t) $
        \(Terminators b'' t'') a v -> p (Terminators b'' (t' ++ t'')) (t ++ t'' ++ a) v `sepBy1'` p' u a v

semipure :: (a -> Either String b) -> Parser st a b
semipure f = Parser (Terminators True []) $ \_ _ -> either fail return . f

select :: [([String], a)] -> Parser st x a
select = foldl1 (<||>) . map (\(s, r) -> if null s then pure (const r) else kwd s >>> pure (const r))

optional :: Parser st a b -> Parser st a ()
optional p = (p >>> pure (const ())) <||> pure (const ())

auto1 :: Parse a => (a -> b) -> Parser st x b
auto1 f = parse >>> pure f

auto2 :: (Parse a, Parse b) => (a -> b -> c) -> Parser st x c
auto2 f = proc x -> do a <- parse -< x; b <- parse -< x; returnA -< f a b

till, begin, end_kwds :: [String]
till = ["till", "until"]
begin = ["begin", "front"]
end_kwds = ["end", "back"]

opt_an :: String -> [String]
opt_an s@(c:_) | isVowel c = [s, "an " ++ s]
opt_an s = [s, "a " ++ s]

uncool :: Parser st () a -> Terminators -> [AndCont] -> CharParser st a
uncool (Parser _ f) t a = f t a ()

instance Parse (Ranked String) where parse = auto2 Ranked <||> auto1 Sole

instance Parse BefAft where parse = select [(["before"], Before), (["after"], After)]

instance Parse RelativeBound where
  parse = select [(end_kwds, Back), (begin, Front)] <||> auto2 RelativeBound

instance Parse Ordinal where
  parse = label "ordinal" $ (>>> pure Ordinal) $ proc _ -> do
    optional $ kwd ["the"] -< ()
    (select [(["last"], -1), (["first"], 0)] -< ()) <||> do
    n <- select $ (\n -> ([show_ordinal n], n)) . [1..9] -< ()
    b <- select [(["last"], True), ([], False)] -< ()
    returnA -< if b then - n - 1 else n

instance Parse (EverythingOr (Ranked String)) where
  parse = select [(["everything"], Everything)] <||> auto1 NotEverything

relative :: Parser st a (Relative a)
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

relative_everything_orA :: Parser st y (Relative (EverythingOr a))
relative_everything_orA =
    (kwd till >>> auto1 (Between Everything . Betw front))
  <||> liftA2 FromTill (kwd ["from"] >>> parse) ((kwd till >>> parse) <||> pure (const Back))
  <||> (kwd ["everything"] >>>
    (liftA2 (\x y -> Between Everything (Betw x y)) (kwd ["from"] >>> parse) (kwd till >>> parse)
      <||> (kwd till >>> auto1 (\x -> Between Everything (Betw front x)))
      <||> (pure (const Everything) >>> relative)))
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

instance Parse Position where
  parse = (select [(begin, Before), (end_kwds, After)] >>> pure (flip Position Everything)) <||> auto2 Position

instance Parse (Rankeds String) where
  parse = (kwd ["all"] >>> ((kwd ["except", "but"] >>> auto2 AllBut) <||> auto1 All))
    <||> (kwd ["any", "every", "each"] >>> auto1 All) <||> auto2 Rankeds <||> auto1 Sole'

instance Parse PositionsClause where
  parse = (kwd ["at"] >>> select [(begin, Before), (end_kwds, After)] >>> pure (\ba -> PositionsClause ba $ and_one $ absolute Everything)) <||> auto2 PositionsClause

instance Parse Replacer where
  parse = liftA2 ReplaceOptions parse (wb >>> parse) <||> liftA2 Replacer parse (wb >>> parse)
    where wb = kwd ["with", "by"]

instance Parse Eraser where parse = auto1 EraseOptions <||> auto1 EraseText
instance Parse Mover where parse = liftA2 Mover parse (kwd ["to"] >>> parse)
instance Parse [EvalOpt] where parse = Parser (Terminators False []) $ \_ _ _ -> optParser << spaces
instance Parse Around where parse = kwd ["around"] >>> auto1 Around
instance Parse UseClause where parse = auto1 UseOptions <||> auto1 UseString

instance Parse Wrapping where
  parse =
    (kwd ["curlies", "braces", "curly brackets"] >>> pure (const (Wrapping "{" "}")))
    <||> (kwd ["parentheses", "parens", "round brackets"] >>> pure (const (Wrapping "(" ")")))
    <||> (kwd ["square brackets"] >>> pure (const (Wrapping "[" "]")))
    <||> (kwd ["angle brackets"] >>> pure (const (Wrapping "<" ">")))
    <||> (kwd ["single quotes"] >>> pure (const (Wrapping "'" "'")))
    <||> (kwd ["double quotes"] >>> pure (const (Wrapping "\"" "\"")))
    <||> liftA2 Wrapping parse (andP >>> parse)
  -- Todo: This is duplicated below.

instance Parse a => Parse (Maybe a) where parse = (parse >>> pure Just) <||> pure (const Nothing)

instance Parse Command where
  parse = label "edit command" $
    (kwd ["insert", "add"] >>> auto2 Insert) <||>
    (kwd ["append"] >>> auto2 Append) <||>
    (kwd ["prepend"] >>> ((parse >>> pure (Use . (UseOptions .))) <||> auto2 Prepend)) <||>
    (kwd ["erase", "remove", "kill", "cut", "omit", "delete"] >>> auto1 Erase) <||>
    (kwd ["replace"] >>> auto1 Replace) <||>
    (kwd ["use"] >>> auto1 Use) <||>
    (kwd ["move"] >>> auto1 Move) <||>
    (kwd ["wrap"] >>> parse >>> snd_unit (auto1 Left <||> (kwd ["in"] >>> auto1 Right)) >>> semipure (uncurry wc))
    where
      wc :: (AndList (Relative (EverythingOr (Rankeds String)))) -> Either (AndList Around) Wrapping -> Either String Command
      wc what (Right wrapping) = return $ WrapIn what wrapping
      wc (AndList (NElist (Between (NotEverything (Sole' x)) (Betw (Bound (Just Before) Everything) Back)) [])) (Left what) =
        (\q -> WrapAround q what) . case () of
          ()| x `elem` ["curlies", "braces", "curly brackets"] -> return $ Wrapping "{" "}"
          ()| x `elem` ["parentheses", "parens", "round brackets"] -> return $ Wrapping "(" ")"
          ()| x `elem` ["square brackets"] -> return $ Wrapping "[" "]"
          ()| x `elem` ["angle brackets"] -> return $ Wrapping "<" ">"
          ()| x `elem` ["single quotes"] -> return $ Wrapping "'" "'"
          ()| x `elem` ["double quotes"] -> return $ Wrapping "\"" "\""
          ()| otherwise -> fail "Unrecognized wrapping description."
      wc (AndList (NElist (Between (NotEverything (Sole' x)) (Betw (Bound (Just Before) Everything) Back))
        [Between (NotEverything (Sole' y)) (Betw (Bound (Just Before) Everything) Back)])) (Left what) =
          return $ WrapAround (Wrapping x y) what
      wc _ (Left _) = fail "Malformed wrap command."

commandP :: CharParser st Command
commandP = uncool parse (Terminators True []) []

commandsP :: CharParser st [Command]
commandsP = unne . andList . uncool parse (Terminators True []) []
