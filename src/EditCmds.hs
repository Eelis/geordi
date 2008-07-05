module EditCmds (exec) where

import Control.Monad.Error

import Data.Char (isSpace)
import Text.ParserCombinators.Parsec
  (choice, CharParser, char, string, try, (<?>), (<|>), eof, anyChar, parse, errorPos, sourceColumn, lookAhead, sepBy1, unexpected)
import qualified Text.ParserCombinators.Parsec.Error as PSE

import qualified Prelude
import Data.Monoid (Monoid(..))
import Prelude hiding (last, (.), all, (!!))
import qualified Data.List as List
import Util

-- Positions/ranges:

type Pos = Int
data Range = Range { start :: Pos, size :: Int } deriving Show

selectRange :: Range -> String -> String
selectRange (Range st si) = take si . drop st

overlap :: Range -> Range -> Int
overlap (Range x s) (Range x' s') = max 0 $ min (x + s) (x' + s') - max x x'

find_occs :: Eq a => [a] -> [a] -> [Pos]
find_occs _ [] = []
find_occs x y | Just z <- stripPrefix x y = 0 : (+ length x) . find_occs x z
find_occs x (_:ys) = (+ 1) . find_occs x ys

sole :: (Show a, Eq a, Monad m) => [a] -> [a] -> m Range
sole x y = case find_occs x y of
  [z] -> return $ Range z (length x)
  [] -> fail $ "String " ++ show x ++ " does not occur."
  _ -> fail $ "String " ++ show x ++ " occurs multiple times."

all :: (Show a, Eq a, Monad m) => [a] -> [a] -> m [Range]
all x y = case find_occs x y of
  [] -> fail $ "String " ++ show x ++ " does not occur."
  l -> return $ (flip Range (length x)) . l

nth :: (Monad m, Show a, Eq a) => Int -> [a] -> [a] -> m Range
nth _ x y | [] <- find_occs x y = fail $ "String " ++ show x ++ " does not occur."
nth n x y | l <- find_occs x y, (- length l) <= n, n < length l = return $ Range (l !! n) (length x)
nth n x _ = fail $ "String " ++ show x ++ " does not occur " ++ once_twice_thrice (if n < 0 then -n else (n+1)) ++ "."

last :: (Monad m, Show a, Eq a) => [a] -> [a] -> m Range
last x y | l@(_:_) <- find_occs x y = return $ Range (List.last l) (length x)
last x _ = fail $ "String " ++ show x ++ " does not occur."

-- Edits:

data Edit = Edit { range :: Range, replacement :: String }

showEdit :: String -> Edit -> String
showEdit _ (Edit (Range 0 0) r) = "prepend " ++ show r
showEdit s (Edit (Range t _) r) | t == length s = "append " ++ show r
showEdit _ (Edit (Range _ 0) r) = "insert " ++ show r
showEdit s (Edit r "") = "erase " ++ show (selectRange r s)
showEdit s (Edit r s') = "replace " ++ show (selectRange r s) ++ " with " ++ show s'

adjustStart :: Edit -> Range -> Maybe Pos
adjustStart (Edit (Range st si) repl) (Range st' si') =
  case () of
    ()| st + si <= st' -> Just $ (st' - si + length repl)
    ()| st' + si' <= st -> Just $ st'
    ()| otherwise -> Nothing

adjustRange :: Edit -> Edit -> Maybe Range
  -- Returns an adjusted Range, or Nothing if the edits conflict.
adjustRange (Edit r@(Range st si) repl) (Edit r'@(Range st' si') repl') =
  case () of
    ()| st + si <= st' -> Just $ Range (st' - si + length repl) si'
    ()| st' + si' <= st -> Just $ Range st' si'
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

type Rank = Int -- 0 = first, 1 = second, -1 = last, -2 = second last, etc
data Occ = RankedOcc Rank String | SoleOcc String deriving Show
data Occs = Occs { allsOccs :: [String], singleOccs :: [Occ] } deriving Show
data Substrs = Substrs { substr_occs :: Occs, substrs_before, substrs_after :: [Occ] } deriving Show
data Positions = Positions { pos_bef, pos_aft :: Occs } deriving Show

data Command
  = Insert String Positions
  | Append String
  | Prepend String
  | Replace Substrs String
  | Move Occ Positions
  deriving Show

instance Monoid Occs where
  mempty = Occs [] []
  mappend (Occs x y) (Occs x' y') = Occs (x ++ x') (y ++ y')

instance Monoid Substrs where
  mempty = Substrs mempty [] []
  mappend (Substrs x y z) (Substrs x' y' z') = Substrs (mappend x x') (y ++ y') (z ++ z')

-- Parsers:

nfb :: Show a => CharParser st a -> CharParser st ()
nfb p = ((try p >>= return . Just) <|> return Nothing) >>= maybe (return ()) (unexpected . show)
  {- Alternative weird implementation with nested try's:
    nfb' :: Show a => CharParser st a -> CharParser st ()
    nfb' p = try ((try p >>= unexpected . show) <|> return ()) -}

nfbk :: [String] -> CharParser st ()
nfbk k = nfb $ choice (try . string . k)

data Terminators = Terminators { term_eof :: Bool, term_keywords :: [String] }
type AndCont = String

strarg :: Terminators -> CharParser st String
strarg t = try (select_act cs << x) <|> fst . many1Till' anyChar (try x) <?> "verbatim string"
 where
  x = (if term_eof t then (eof <|>) else id) $ lookAhead (choice (try . string . (" "++) . term_keywords t)) >> char ' ' >> return ()
  cs = [(["comma"], ","), (["space"], " "), (["colon"], ":"), (["semicolon"], ";"), (["ampersand"], "&"), (["tilde"], "~")]

select_act :: [([String], a)] -> CharParser st a
select_act = choice . map (\(s, r) -> choice (try . string . s) >> return r)

add_term :: String -> Terminators -> Terminators
add_term s t = t { term_keywords = s : term_keywords t }

rankP :: CharParser st Rank
rankP = (try (string "last ") >> return (-1)) <|> do
  n <- select_act ((\n -> ([rank (n+1)], n)) . [0..9]) << char ' ' <?> "ordinal"
  (try (string "last ") >> return (- n - 1)) <|> return n

ranksP :: CharParser st [Rank]
ranksP = rankP `sepBy1` try (string "and ")

occP :: Terminators -> CharParser st Occ
occP k = liftM2 RankedOcc rankP (strarg k) <|> SoleOcc . strarg k

occsP :: Terminators -> [AndCont] -> CharParser st Occs
occsP k a = mconcat . (h  `sepBy1` try (string "and " >> nfbk a))
 where
  strarg' = strarg (add_term "and " k)
  h = (try (choice $ try . string . ["all ", "any ", "every ", "each "]) >> (\x -> Occs [x] []) . strarg')
    <|> (do r <- ranksP; x <- strarg'; return $ Occs [] $ flip RankedOcc x . r)
    <|> (\x -> Occs [] [SoleOcc x]) . strarg'

substrsP :: Terminators -> [AndCont] -> CharParser st Substrs
substrsP k a = mconcat . (f  `sepBy1` try (string "and " >> nfbk a))
  where
    k' = add_term "and " k
    f = (try (string "after ") >> (\x -> Substrs mempty [] [x]) . occP k')
      <|> (try (string "before ") >> (\x -> Substrs mempty [x] []) . occP k')
      <|> (\x -> Substrs x [] []) . occsP k' ("after " : "before " : a)

positionsP :: [AndCont] -> CharParser st Positions
positionsP a = (\(x, y) -> Positions (mconcat x) (mconcat y)) . either_part . ((select_act [(["before "], Left), (["after "], Right)] >>= (. occsP (Terminators True []) ("before " : "after " : a))) `sepBy1` try (string "and " >> nfbk a))

commandsP :: CharParser st [Command]
commandsP = (select_act l >>= id) `sepBy1` try (string "and ")
 where
  l =
    [ (["insert ", "add "], liftM2 Insert (strarg (Terminators False ["after ", "before "])) (positionsP pr))
    , (["append "], Append . strarg (Terminators True pr))
    , (["prepend "], Prepend . strarg (Terminators True pr))
    , (["erase ", "remove ", "kill ", "cut ", "omit "], flip Replace "" . substrsP (Terminators True []) pr)
    , (["replace "], liftM2 Replace (substrsP (Terminators False ["with ", "by "]) pr) ((string "with " <|> string "by ") >> strarg (Terminators True pr)))
    , (["move "], liftM2 Move (occP $ Terminators False ["to "]) (string "to " >> positionsP pr))
    ]
  pr = "and " : concat (fst . l)

-- Resolving positions/occurrences/edits in the subject string:

class FindInStr a b | a -> b where findInStr :: (Functor m, Monad m) => String -> a -> m b

instance FindInStr Occ Range where
  findInStr t (RankedOcc r s) = nth r s t
  findInStr t (SoleOcc s) = sole s t

instance FindInStr Occs [Range] where
  findInStr s (Occs alls soles) = liftM2 (++)
    (concat . sequence (flip all s . alls))
    (sequence $ findInStr s . soles)

instance FindInStr Substrs [Range] where
  findInStr s (Substrs occs befores afters) = do
    x <- findInStr s occs
    y <- ((\(Range b _) -> Range 0 b)  .) . sequence (findInStr s . befores)
    z <- ((\(Range b l) -> Range (b + l) (length s - b - l)) .) . sequence (findInStr s . afters)
    return (x ++ y ++ z)

instance FindInStr Positions [Pos] where
  findInStr s (Positions befores afters) = liftM2 (++)
    ((start .) . findInStr s befores)
    (((\(Range x y) -> x + y) .) . findInStr s afters)

instance FindInStr Command [Edit] where
  findInStr s (Append x) = return [Edit (Range (length s) 0) x]
  findInStr _ (Prepend x) = return [Edit (Range 0 0) x]
  findInStr s (Replace p r) = (flip Edit r .) . findInStr s p
  findInStr s (Insert r p) = ((\x -> Edit (Range x 0) r) .) . findInStr s p
  findInStr s (Move o p) = do
    r <- findInStr s o
    let w = selectRange r s
    i <- findInStr s p
    return $ Edit r "" : map (\x -> Edit (Range x 0) w) i

-- Main:

exec :: (Functor m, Monad m) => String -> String -> m String
exec cmd_str str = do
  cmds <- case parse commandsP "" cmd_str of
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
  t "erase before last 2 and replace after 4 with x" $ Right "2 3 4x"
  t "insert x after all 2 and append y" $ Right "1 2x 3 2x 3 4 5y"
  t "erase first 2 and replace second 2 with x and omit last  " $ Right "1  3 x 3 45"
  t "erase first and last 2 and all 3 and 4 and before first 2 and after 4" $ Right "    "
  t "erase third last   and kill after second last  " $ Right "1 2 3 23 "
  t "insert x after 1 2 and before all 3 and erase 5 and prepend y" $ Right "y1 2x x3 2 x3 4 "
  t "erase before 4 and after 5" $ Right "4 5"
  t "insert x before 3 4 and 5" $ Right "1 2 3 2 x3 4 x5"
  t "insert x after 1 and all 3 and before 5 and erase 4" $ Right "1x 2 3x 2 3x  x5"
  t "move 4 5 to before first 2 and after first 3" $ Right "1 4 52 34 5 2 3 "
  t "erase first 2 3 and 3 2 and 4 5 and last  " $ Right "1  3 "
  t "move 4 5 to before 1 and erase after second 2" $ Right "4 51 2 3 2"
  t "cut before first 2 and first and second 3 and after 4 and prepend x" $ Right "x2  2  4"
  t "replace all 2 with 3 and erase all 3 and add x after second 2" $ Right "1 3  3x  4 5"
  t "insert spacer before 4 and insert semicolon after 1 and erase last space" $ Right "1; 2 3 2 3 spacer45"
  t "erase first and second last  " $ Right "12 3 2 34 5"
  -- Edit errors:
  t "replace alligators with chickens" $ Left "String \"alligators\" does not occur."
  t "erase 2" $ Left "String \"2\" occurs multiple times."
  t "replace tenth last 2 by x" $ Left "String \"2\" does not occur 10 times."
  t "erase second 9" $ Left "String \"9\" does not occur."
  t "replace all 2 with 3 and replace second 2 with x" $ Left "Overlapping edits: replace \"2\" with \"3\" and replace \"2\" with \"x\"."
  t "erase before first 3 and replace first 2 with x" $ Left "Overlapping edits: erase \"1 2 \" and replace \"2\" with \"x\"."
  -- Syntax errors:
  t "isnert 3 before 4" $ Left "column 1: unexpected \"s\". expected: \"insert \", \"add \", \"append \", \"prepend \", \"erase \", \"remove \", \"kill \", \"cut \", \"omit \", \"replace \" or \"move \". "
  t "insert " $ Left "column 8: unexpected end of command. expected: verbatim string. "
  t "insert kung fu" $ Left "column 15: unexpected end of command. expected: \" after \" or \" before \". "
  t "insert bla after " $ Left "column 18: unexpected end of command. expected: \"all \", \"any \", \"every \", \"each \", \"last \", ordinal or verbatim string. "
  t "append x and erase first " $ Left "column 26: unexpected end of command. expected: \"last \", \"and \" or verbatim string. "
  t "erase all 2 and " $ Left "column 17: unexpected end of command. expected: \"after \", \"before \", \"and \", \"insert \", \"add \", \"append \", \"prepend \", \"erase \", \"remove \", \"kill \", \"cut \", \"omit \", \"replace \", \"move \", \"all \", \"any \", \"every \", \"each \", \"last \", ordinal or verbatim string. "
    -- Todo: the "and " expectancy is a bit awkward here.
 where
  t :: String -> Either String String -> IO ()
  t c o = let o' = exec c "1 2 3 2 3 4 5" in when (o' /= o) $ fail $ "test failed: " ++ show (c, o, o')

{-

Command grammar:

  command = (insert | append | prepend | erase | replace | move) ["and" command]

  insert = ("insert" | "add") ... positions
  append = "append" ...
  prepend = "prepend" ...
  erase = ("erase" | "remove" | "cut" | "omit" | "kill") substrings
  replace = "replace" substrings ("with" | "by") ...
  move = "move" occurrence "to" positions

  positions = (("after" | "before") occurrences) ["and" positions]
  substrings = (("after" | "before") occurrence | occurrences) ["and" substrings]
  occurrences = (("all" | "any" | "every" | "each") ... | ranks ... | ...) ["and" occurrences]
  occurrence = rank ... | ...
  ranks = rank ["and" ranks]
  rank = "last" ... | ordinal ["last"] ...
  ordinal = "first" | "second" | "third" | etc

Design decisions:

  Should "erase all x and y" mean "erase all x and all y" or "erase all x and the sole occurrence of y"?

    We choose the latter, because:
    - it's easier to implement, because we don't need "and"-repetition nested under "all";
    - it's safe, because if y occurs multiple times, a clear "y occurs multiple times" error will be emitted, whereas the former solution could result in unintended edit results.

  Should "first and second last x" mean "(first and second) last x" or "first and (second last) x"?

    I have no strong feelings on the matter. Currently it is interpreted to mean the second.

Todo:

  "erase block at second {" (using CxxParse)
  "erase from first 2 to last 3"
  "wrap ( and ) around x", "wrap { and } around everything"

-}
