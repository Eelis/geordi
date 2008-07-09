module EditCmds (exec, commands) where

import Control.Monad.Error

import Data.Char (isSpace)
import Text.ParserCombinators.Parsec
  (choice, CharParser, char, string, try, (<?>), (<|>), eof, anyChar, errorPos, sourceColumn, lookAhead, sepBy1, unexpected)
import qualified Text.ParserCombinators.Parsec as PS
import qualified Text.ParserCombinators.Parsec.Error as PSE

import qualified Prelude
import Prelude hiding (last, (.), all, (!!))
import qualified Data.List as List
import Util

-- Positions/ranges:

type Pos = Int
data Range = Range { start :: Pos, size :: Int } deriving Eq

selectRange :: Range -> String -> String
selectRange (Range st si) = take si . drop st

overlap :: Range -> Range -> Int
overlap (Range x s) (Range x' s') = max 0 $ min (x + s) (x' + s') - max x x'

find_occs :: Eq a => [a] -> [a] -> [Pos]
find_occs _ [] = []
find_occs x y | Just z <- stripPrefix x y = 0 : (+ length x) . find_occs x z
find_occs x (_:ys) = (+ 1) . find_occs x ys

nth :: (Monad m, Show a, Eq a) => Int -> [a] -> [a] -> m Range
nth _ x y | [] <- find_occs x y = fail $ "String " ++ show x ++ " does not occur."
nth n x y | l <- find_occs x y, (- length l) <= n, n < length l = return $ Range (l !! n) (length x)
nth n x _ = fail $ "String " ++ show x ++ " does not occur " ++ once_twice_thrice (if n < 0 then -n else (n+1)) ++ "."

-- Edits:

data Edit = Edit { range :: Range, replacement :: String } deriving Eq

showEdit :: String -> Edit -> String
showEdit _ (Edit (Range 0 0) r) = "prepend " ++ show r
showEdit s (Edit (Range t _) r) | t == length s = "append " ++ show r
showEdit _ (Edit (Range _ 0) r) = "insert " ++ show r
showEdit s (Edit r "") = "erase " ++ show (selectRange r s)
showEdit s (Edit r s') = "replace " ++ show (selectRange r s) ++ " with " ++ show s'

adjustRange :: Edit -> Edit -> Maybe Range
  -- Returns an adjusted Range, or Nothing if the edits conflict.
adjustRange e@(Edit r@(Range st si) repl) e'@(Edit r'@(Range st' si') repl') =
  case () of
    ()| st + si <= st' -> Just $ Range (st' - si + length repl) si'
    ()| st' + si' <= st || e == e' -> Just $ Range st' si'
    ()| null repl && null repl' -> -- Overlapping erase-edits do not conflict.
      if st <= st' then Just $ Range st (max 0 $ (st' + si') - (st + si))
      else Just $ Range st' (si' - overlap r r')
    ()| otherwise -> Nothing

adjustEdits :: Edit -> [Edit] -> Either Edit [Edit]
  -- Returns either a conflicting Edit or the list of adjusted Edits.
adjustEdits _ [] = Right []
adjustEdits e (e'@(Edit _ repl) : t) =
  case adjustRange e e' of
    Just r -> (Edit r repl :) . adjustEdits e t
    Nothing -> Left e'

exec_edits :: Monad m => [Edit] -> String -> m String
exec_edits [] s = return s
exec_edits (e@(Edit (Range st si) repl) : t) s =
  case adjustEdits e t of
    Left e' -> fail $ "Overlapping edits: " ++ showEdit s e ++ " and " ++ showEdit s e' ++ "."
    Right t' -> do
      let (x, y) = splitAt st s
      let (_, b) = splitAt si y
      exec_edits t' $ x ++ repl ++ b

-- English:

once_twice_thrice :: Int -> String
once_twice_thrice i = case i of
  1 -> "once"; 2 -> "twice"; 3 -> "thrice"
  n -> show n ++ " times"

rank :: Int -> String
rank n = case n of
  0 -> "zeroth"; 1 -> "first"; 2 -> "second"; 3 -> "third"; 4 -> "fourth"; 5 -> "fifth"
  6 -> "sixth"; 7 -> "seventh"; 8 -> "eighth"; 9 -> "ninth"; 10 -> "tenth"
  _ -> "<high rank>"

-- Edit command structure:

data EverythingOr a = Everything | NotEverything a
data RangeOr a = RangeOcc RangeBegin RangeEnd | NoRange a
newtype AndList a = AndList { andList :: [a] }

type Rank = Int -- 0 = first, 1 = second, -1 = last, -2 = second last, etc
data Ranked a = Ranked Rank a | Sole a
data Rankeds a = Rankeds (AndList Rank) a | Sole' a | All a
data RangeBegin = Front | RangeBegin BefAft (Ranked String)
data RangeEnd = Back | RangeEnd BefAft (Relative (Ranked String))
data Relative a = Relative a BefAft (Ranked String) | Absolute a
data PositionsClause = PositionsClause BefAft (AndList (Relative (EverythingOr (Rankeds String))))
data Position = Position BefAft (EverythingOr (Ranked String))
type Positions = AndList PositionsClause
data Replacer = Replacer (AndList (RangeOr (Relative (EverythingOr (Rankeds String))))) String
data Mover = Mover (RangeOr (Relative (EverythingOr (Ranked String)))) Position
data BefAft = Before | After

data Command
  = Insert String Positions
  | Append String
  | Prepend String
  | Replace (AndList Replacer)
  | Move (AndList Mover)
  | Wrap String String (EverythingOr (Ranked String))

-- Parsers:

notFollowedBy :: Show a => CharParser st a -> CharParser st ()
notFollowedBy p = ((try p >>= return . unexpected . show) <|> return (return ())) >>= id

kwd :: [String] -> CharParser st String
kwd s = choice $ try . string . s

select_act :: [([String], a)] -> CharParser st a
select_act = choice . map (\(s, r) -> choice (try . string . s) >> return r)

befAftP :: CharParser st BefAft
befAftP = select_act [(["before "], Before), (["after "], After)]

data Terminators = Terminators { term_eof :: Bool, term_keywords :: [String] }
type AndCont = String

add_terms :: [String] -> Terminators -> Terminators
add_terms s t = t { term_keywords = s ++ term_keywords t }

eof_or_space :: Bool -> CharParser st ()
eof_or_space b = (if b then (eof <|>) else id) (char ' ' >> lookAhead anyChar >> return ())

everything :: Terminators -> CharParser st ()
everything t = try $ string "everything" >> eof_or_space (term_eof t)

till :: [String]
till = ["till ", "until "]

verbatim :: Terminators -> CharParser st String
verbatim t = try (select_act cs << x) <|> fst . many1Till' anyChar (try x) <?> "verbatim string"
 where
  x = (if term_eof t then (eof <|>) else id) $ lookAhead (choice (try . string . (" " ++) . term_keywords t)) >> char ' ' >> return ()
  cs = [(["comma"], ","), (["space"], " "), (["colon"], ":"), (["semicolon"], ";"), (["ampersand"], "&"), (["tilde"], "~")]

class Parse t where parse :: Terminators -> [AndCont] -> CharParser st t

instance Parse (Ranked String) where
  parse k a = liftM2 Ranked (parse k a) (verbatim k) <|> Sole . verbatim k

instance Parse RangeEnd where
  parse t a = (try (string "end" >> eof_or_space (term_eof t)) >> return Back) <|>
    liftM2 RangeEnd (befAftP <|> return Before) (parse t a)

instance Parse Rank where
  parse _ _ = (<?> "ordinal") $ (try (string "last ") >> return (-1)) <|> do
    n <- select_act ((\n -> ([rank (n+1)], n)) . [0..9]) << char ' '
    (try (string "last ") >> return (- n - 1)) <|> return n

instance Parse (EverythingOr (Ranked String)) where
  parse t a = (everything t >> return Everything)
    <|> NotEverything . parse (add_terms ("before " : "after " : till) t) a

instance Parse (EverythingOr (Rankeds String)) where
  parse t a = (everything t >> return Everything)
    <|> NotEverything . parse (add_terms ("before " : "after " : till) t) a

relative :: a -> Terminators -> [AndCont] -> CharParser st (Relative a)
relative x t a = liftM2 (Relative x) befAftP (parse t a) <|> return (Absolute x)

instance Parse (RangeOr (Relative (EverythingOr (Rankeds String)))) where
  parse t a = do
      everything t
      (RangeOcc Front . (kwd till >> parse t a) <|> NoRange . relative Everything t a)
    <|> (try (string "begin ") >> kwd till >> RangeOcc Front . parse t a)
    <|> (try (string "before ") >> RangeOcc Front . RangeEnd Before . parse t a)
    <|> do
      try (string "after ")
      rankedString <- parse (add_terms till t) a
      RangeOcc (RangeBegin After rankedString) . ((kwd till >> parse t a) <|> return Back)
    <|> do
      x <- parse (add_terms ("before " : "after " : till) t) a
      case x of
        Rankeds (AndList [r]) s -> RangeOcc (RangeBegin Before (Ranked r s)) . (kwd till >> parse t a) <|> NoRange . relative (NotEverything x) t a
        Sole' s -> RangeOcc (RangeBegin Before (Sole s)) . (kwd till >> parse t a) <|> NoRange . relative (NotEverything x) t a
        _ -> NoRange . relative (NotEverything x) t a

instance Parse (RangeOr (Relative (EverythingOr (Ranked String)))) where
  parse t a = do
      everything t
      (RangeOcc Front . (kwd till >> parse t a) <|> NoRange . relative Everything t a)
    <|> do
      rankedString <- parse (add_terms ("before " : "after " : till) t) a
      RangeOcc (RangeBegin Before rankedString) . (kwd till >> parse t a) <|> NoRange . relative (NotEverything rankedString) t a

instance Parse (EverythingOr (Relative (Ranked String))) where
  parse t a = (everything t >> return Everything)
    <|> (parse (add_terms ("before " : "after " : till) t) a >>= \x -> NotEverything . relative x t a)

instance Parse Position where
  parse t a = (flip Position Everything . select_act [(["begin", "front"], Before), (["end", "back"], After)] << eof_or_space True) <|> liftM2 Position befAftP (parse t a)

instance Parse a => Parse (Relative a) where
  parse t a = do x <- parse (add_terms  ["before ", "after "] t) a; relative x t a

instance Parse (Rankeds String) where
  parse k a = (kwd ["all ", "any ", "every ", "each "] >> All . verbatim k)
    <|> liftM2 Rankeds (parse k a) (verbatim $ add_terms till k)
    <|> Sole' . verbatim k

instance Parse PositionsClause where
  parse t a =
    (\ba -> PositionsClause ba (AndList [Absolute Everything])) . (select_act [(["begin", "front"], Before), (["end", "back"], After)] << eof_or_space True)
    <|> liftM2 PositionsClause befAftP (parse t ("before " : "after " : a))

instance Parse Replacer where
  parse t _ = liftM2 Replacer (parse (Terminators False wb) []) (kwd wb >> verbatim t)
    where wb = ["with ", "by "]

instance Parse a => Parse (AndList a) where
  parse t a = AndList . (parse (add_terms ["and "] t) a `sepBy1` try (string "and " >> notFollowedBy (kwd a)))

instance Parse Mover where
  parse t a = liftM2 Mover (parse (Terminators False ["to "]) a) (string "to " >> parse t a)

instance Parse Command where
  parse t a = select_act l >>= id
   where
    l =
      [ (["insert ", "add "], liftM2 Insert (verbatim (Terminators False ["after ", "before "])) (parse t a'))
      , (["append "], Append . verbatim t)
      , (["prepend "], Prepend . verbatim t)
      , (["erase ", "remove ", "kill ", "cut ", "omit ", "delete "], do
          s <- parse t a'; return $ Replace $ AndList [Replacer s ""])
      , (["replace "], Replace . parse t a')
      , (["move "], Move . parse t a')
      , (["wrap "], do
          f <- (kwd ["curlies ", "braces ", "curly brackets "] >> return (Wrap "{" "}")) <|>
            (kwd ["parentheses ", "parens ", "round brackets "] >> return (Wrap "(" ")")) <|>
            (liftM2 Wrap (verbatim (Terminators False ["and "]) << string "and ") (verbatim (Terminators False ["around "])))
          string "around "
          f . parse t a')
      ]
    a' = concat (fst . l) ++ a

commandsP :: CharParser st [Command]
commandsP = andList . parse (Terminators True []) []

-- Resolving positions/occurrences/edits in the subject string:

class Offsettable a where offset :: Int -> a -> a

instance Offsettable Range where offset x (Range y z) = Range (y + x) z
instance Offsettable [Range] where offset x l = offset x . l

class FindInStr a b | a -> b where findInStr :: (Functor m, Monad m) => String -> a -> m b

instance FindInStr (Ranked String) Range where
  findInStr t (Ranked r s) = nth r s t
  findInStr y (Sole x) = case find_occs x y of
    [z] -> return $ Range z (length x)
    [] -> fail $ "String " ++ show x ++ " does not occur."
    _ -> fail $ "String " ++ show x ++ " occurs multiple times."

class Invertible a where invert :: a -> a

instance Invertible Rank where
  invert r | r >= 0 = -r - 1
  invert r = r

instance Invertible a => Invertible (EverythingOr a) where
  invert Everything = Everything
  invert (NotEverything x) = NotEverything (invert x)

instance Invertible (Ranked String) where
  invert (Ranked r s) = Ranked (invert r) s
  invert x = x

instance Invertible (Rankeds String) where
  invert (Rankeds (AndList r) s) = Rankeds (AndList $ invert . r) s
  invert x = x

instance (Offsettable b, Invertible a, FindInStr a b) => FindInStr (Relative a) b where
  findInStr s (Relative o ba w) = do
    Range st si <- findInStr s w
    case ba of
      Before -> findInStr (take st s) (invert o)
      After -> offset (st + si) . findInStr (drop (st + si) s) o
  findInStr s (Absolute o) = findInStr s o

class FromRange a where fromRange :: Range -> a
instance FromRange Range where fromRange = id
instance FromRange [Range] where fromRange = (:[])

instance (FromRange b, FindInStr a b) => FindInStr (EverythingOr a) b where
  findInStr s Everything = return $ fromRange $ Range 0 (length s)
  findInStr s (NotEverything x) = findInStr s x

instance FindInStr (Rankeds String) [Range] where
  findInStr y (All x) = case find_occs x y of
    [] -> fail $ "String " ++ show x ++ " does not occur."
    l -> return $ (flip Range (length x)) . l
  findInStr y (Sole' x) = case find_occs x y of
    [z] -> return [Range z (length x)]
    [] -> fail $ "String " ++ show x ++ " does not occur."
    _ -> fail $ "String " ++ show x ++ " occurs multiple times."
  findInStr x (Rankeds (AndList rs) s) = sequence $ (\r -> findInStr x (Ranked r s)) . rs

instance (FindInStr a b, FromRange b) => FindInStr (RangeOr a) b where
  findInStr s (NoRange o) = findInStr s o
  findInStr s (RangeOcc x y) = do
    Range a b <- findInStr s x
    e <- findInStr (drop (a + b) s) y
    return $ fromRange $ Range a (b + e)

instance FindInStr PositionsClause [Pos] where
  findInStr s (PositionsClause Before (AndList o)) = (start .) . concat . sequence (findInStr s . o)
  findInStr s (PositionsClause After (AndList o)) = ((\(Range x y) -> x + y) .) . concat . sequence (findInStr s . o)

instance FindInStr a b => FindInStr (AndList a) [b] where
  findInStr s (AndList l) = sequence (findInStr s . l)

instance FindInStr Replacer [Edit] where
  findInStr s (Replacer p r) = (flip Edit r .) . concat . findInStr s p

instance FindInStr RangeBegin Range where
  findInStr _ Front = return (Range 0 0)
  findInStr s (RangeBegin ba p) = do
    r@(Range x y) <- findInStr s p
    return $ case ba of Before -> r; After -> Range (x + y) 0

instance FindInStr RangeEnd Int where
  findInStr s Back = return (length s)
  findInStr s (RangeEnd ba p) = do
    (Range x y) <- findInStr s p
    return $ case ba of Before -> x; After -> x + y

instance FindInStr Mover [Edit] where
  findInStr s (Mover o p) = do
    r <- findInStr s o
    i <- findInStr s p
    return [Edit r "", Edit (Range i 0) (selectRange r s)]

instance FindInStr Position Pos where
  findInStr _ (Position Before Everything) = return 0
  findInStr s (Position After Everything) = return (length s)
  findInStr s (Position ba (NotEverything p)) = do
    (Range x y) <- findInStr s p
    return $ case ba of Before -> x; After -> x + y

instance FindInStr Command [Edit] where
  findInStr s (Append x) = return [Edit (Range (length s) 0) x]
  findInStr _ (Prepend x) = return [Edit (Range 0 0) x]
  findInStr s (Replace (AndList l)) = concat . sequence (findInStr s . l)
  findInStr s (Insert r p) = ((\x -> Edit (Range x 0) r) .) . concat . findInStr s p
  findInStr s (Move (AndList movers)) = concat . sequence (findInStr s . movers)
  findInStr s (Wrap x y z) = do
    Range a b <- findInStr s z
    return [Edit (Range a 0) x, Edit (Range (a + b) 0) y]

-- Main:

commands :: [String]
commands = words "append prepend erase remove cut omit kill delete replace remove add insert move wrap"

exec :: (Functor m, Monad m) => String -> String -> m String
exec cmd_str str = do
  cmds <- case PS.parse (commandsP << eof) "" cmd_str of
    Left e -> fail $ "column " ++ show (sourceColumn $ errorPos e) ++ ": " ++
      (concatMap (++ ". ") $ filter (not . List.all isSpace) $ lines $ PSE.showErrorMessages "or" "unknown parse error" "expected:" "unexpected" "end of command" $ PSE.errorMessages e)
    Right x -> return x
  edits <- concat . sequence (findInStr str . cmds)
  exec_edits edits str

-- Testing:

itest :: IO ()
itest = do
  l <- getLine
  case (exec l "1 2 3 2 3 4 5" :: Either String String) of
    Left x -> putStr "syntax error: " >> putStrLn x
    Right x -> putStrLn x

test :: IO ()
test = do
  t "erase all 2 and insert x before second 3 and prepend y" $ Right "y1  3  x3 4 5"
  t "erase everything before last 2 and replace everything after 4 with x" $ Right "2 3 4x"
  t "insert x after all 2 and append y" $ Right "1 2x 3 2x 3 4 5y"
  t "erase first 2 and replace second 2 with x and omit last  " $ Right "1  3 x 3 45"
  t "erase first and last 2 and all 3 and 4 and everything before first 2 and everything after 4" $ Right "    "
  t "erase third last space and kill everything after second last  " $ Right "1 2 3 23 "
  t "insert x after 1 2 and before all 3 and erase 5 and prepend y" $ Right "y1 2x x3 2 x3 4 "
  t "erase everything before 4 and everything after 5" $ Right "4 5"
  t "insert x before 3 4 and 5" $ Right "1 2 3 2 x3 4 x5"
  t "insert x after 1 and all 3 and before 5 and erase 4" $ Right "1x 2 3x 2 3x  x5"
  t "erase second space before 4" $ Right "1 2 3 23 4 5"
  t "erase begin until second 3" $ Right "3 4 5"
  t "erase first 2 3 and 3 2 and 4 5 and last  " $ Right "1  3 "
  t "move 1 till second 3 to before 5" $ Right "3 4 1 2 3 2 5"
  t "move 1 till after second 3 to before 5" $ Right " 4 1 2 3 2 35"
  t "replace 2 after second space with x" $ Right "1 2 3 x 3 4 5"
  t "move everything till 4  to end and erase 4 " $ Right "51 2 3 2 3 "
  t "erase everything" $ Right ""
  t "insert x after second space before 4" $ Right "1 2 3 2 x3 4 5"
  t "wrap < and > around second 3" $ Right "1 2 3 2 <3> 4 5"
  t "wrap braces around everything" $ Right "{1 2 3 2 3 4 5}"
  t "move everything after second 3 to begin" $ Right " 4 51 2 3 2 3"
  t "move second 3 to end and 5 to begin" $ Right "51 2 3 2  4 3"
  t "erase first space until second space before 4" $ Right "1 3 4 5"
  t "erase second last space before 4" $ Right "1 2 3 23 4 5"
  t "erase after second 2" $ Right "1 2 3 2"
  t "erase after second 2 until before 5" $ Right "1 2 3 25"
  t "erase before second last space" $ Right " 4 5"
  t "move 4 5 to before 1 and erase everything after second 2" $ Right "4 51 2 3 2"
  t "move 4 till end to front" $ Right "4 51 2 3 2 3 "
  t "erase all space after first 2" $ Right "1 232345"
  t "add x before first 3 after second 2" $ Right "1 2 3 2 x3 4 5"
  t "add x before second space after second 3" $ Right "1 2 3 2 3 4x 5"
  t "add x after all space before first 3" $ Right "1 x2 x3 2 3 4 5"
  t "erase all space before last 3" $ Right "12323 4 5"
  t "erase second 2 till end" $ Right "1 2 3 "
  t "move everything after second 3 to begin" $ Right " 4 51 2 3 2 3"
  t "replace first 2 with x and replace all 2 with x" $ Right "1 x 3 x 3 4 5"
  t "erase everything until 4 and 5" $ Right "4 "
  t "erase 3 2 till 5" $ Right "1 2 5"
  t "move second 2 to back and prepend x" $ Right "x1 2 3  3 4 52"
  t "cut everything before first 2 and first and second 3 and everything after 4 and prepend x" $ Right "x2  2  4"
  t "replace all 2 with 3 and erase all 3 and add x after second 2" $ Right "1 3  3x  4 5"
  t "insert spacer before 4 and insert semicolon after 1 and erase last space" $ Right "1; 2 3 2 3 spacer45"
  t "erase first and second last  " $ Right "12 3 2 34 5"
  t "replace 1 with x and all 2 with y and erase second 3" $ Right "x y 3 y  4 5"
  -- Edit errors:
  t "move second 2 to x" $ Left "column 18: unexpected \"x\". expected: \"begin\", \"front\", \"end\", \"back\", \"before \" or \"after \". "
  t "replace alligators with chickens" $ Left "String \"alligators\" does not occur."
  t "erase 2" $ Left "String \"2\" occurs multiple times."
  t "replace 1 and erase with 4" $ Left "String \"erase\" does not occur."
  t "replace tenth last 2 by x" $ Left "String \"2\" does not occur 10 times."
  t "erase second 9" $ Left "String \"9\" does not occur."
  t "replace all 2 with 3 and replace second 2 with x" $ Left "Overlapping edits: replace \"2\" with \"3\" and replace \"2\" with \"x\"."
  t "erase everything before first 3 and replace first 2 with x" $ Left "Overlapping edits: erase \"1 2 \" and replace \"2\" with \"x\"."
  -- Syntax errors:
  t "move 4 to back " $ Left "column 16: unexpected end of command. "
  t "isnert 3 before 4" $ Left "column 1: unexpected \"s\". expected: \"insert \", \"add \", \"append \", \"prepend \", \"erase \", \"remove \", \"kill \", \"cut \", \"omit \", \"delete \", \"replace \", \"move \" or \"wrap \". "
  t "insert " $ Left "column 8: unexpected end of command. expected: verbatim string. "
  t "insert kung fu" $ Left "column 15: unexpected end of command. expected: \" after \" or \" before \". "
  t "move " $ Left "column 6: unexpected end of command. expected: \"everything\", ordinal or verbatim string. "
  t "move x " $ Left "column 8: unexpected end of command. expected: \" before \", \" after \", \" till \", \" until \" or \" to \". "
  t "move x to "$ Left "column 11: unexpected end of command. expected: \"begin\", \"front\", \"end\", \"back\", \"before \" or \"after \". "
  t "insert x after " $ Left "column 16: unexpected end of command. expected: \"everything\", \"all \", \"any \", \"every \", \"each \", ordinal or verbatim string. "
  t "wrap x and y" $ Left $ "column 13: unexpected end of command. expected: \" around \". "
  t "append x and erase first " $ Left "column 26: unexpected end of command. expected: \"last \", \"and \" or verbatim string. "
  t "erase all 2 and " $ Left "column 17: unexpected end of command. expected: \"insert \", \"add \", \"append \", \"prepend \", \"erase \", \"remove \", \"kill \", \"cut \", \"omit \", \"delete \", \"replace \", \"move \", \"wrap \", \"everything\", \"begin \", \"before \", \"after \", \"all \", \"any \", \"every \", \"each \", ordinal or verbatim string. "
  putStrLn "No test failures."
 where
  t :: String -> Either String String -> IO ()
  t c o = let o' = exec c "1 2 3 2 3 4 5" in when (o' /= o) $ fail $ "test failed: " ++ show (c, o, o')

{- Command grammar:

  command = (insert | append | prepend | erase | replace | move | wrap)*

  insert = ("insert" | "add") ... positions*
  append = "append" ...
  prepend = "prepend" ...
  erase = ("erase" | "remove" | "delete" | "cut" | "omit" | "kill") substrs*
  replace = "replace" (substrs* ("with" | "by") ...)*
  move = "move" (substr "to" position)*
  wrap = "wrap" (... "and" ... | "parentheses" | "curlies") "around" ("everything" | ranked)

  substrs = range | ("everything" | rankeds) [befaft ranked]
  substr = range | ("everything" | ranked) [befaft ranked]
  position = befaft (everything | ranked)
  befaft = "before" | "after"
  positions = befaft (("everything" | rankeds) [befaft ranked])*
  rank = "first" | ("second" | "third" | etc) ["last"] | "last"
  ranked = [rank] ...
  rankeds = ["all" | "each" | "every" | "any" | rank*] ...
  range = (range-begin | "everything") ("till" | "until") range-end
  range-begin = "begin" | "front" | befaft (everything | ranked)
  range-end = "end" | "back" | befaft (everything | ranked)

  Ellipsis denote a verbatim string, and x* = x ["and" x*].

Design notes:

  Giving moves a single target makes "move 4 to end and 5 to begin" work, because otherwise it would be parsed as a move with two targets, the second of which, 5, is not a valid target.

  Should "second last x before y" in "xxaxxy" designate the 'x' before or after 'a'?

    We choose the latter, because it seems more natural, despite the curious result that "first x before y" now means the same as "last x before y".

  Should "erase all x and y" mean "erase all x and all y" or "erase all x and the sole occurrence of y"?

    We choose the latter, because:
    - it's easier to implement, because we don't need "and"-repetition nested under "all";
    - it's safe, because if y occurs multiple times, a clear "y occurs multiple times" error will be emitted, whereas the former solution could result in unintended edit results.

  Should "first and second last x" mean "(first and second) last x" or "first and (second last) x"?

    I have no strong feelings on the matter. Currently it is interpreted to mean the second.

  Grammar choices:

    - Ranges are never relative (but their bounds may be).
    - Position specifications never mention ranges (this means that "everything" must not be a range).

Todo:

  There are small mistakes in the grammar. It doesn't reflect the fact that "erase before 4" is allowed while "move before 4 to end" isn't.

  "prepend z and wrap parens around everything" -- currently produces "z(...)", should produce "(z...)"
  bad error: "wrap x and y around first space a"
  bad error: "move second 3 to end "
  "wrap parentheses around x+y and y+x"
  "move 1 after 4" sounds nice, but probably too ambiguous
  "replace first two x with y"
  "erase everything from x until y"
  "insert x between x and y"
  "erase everything between x and y"
  "erase from after first 3 till 5"
  "erase block/statement at second {/;" (using CxxParse)
  "erase from first 2 to last 3"

-}
