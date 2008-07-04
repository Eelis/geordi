module EditCmds (exec) where

import qualified List
import qualified Control.Arrow as Arrow

import Control.Monad.Error

import Data.Char (isSpace)
import Text.ParserCombinators.Parsec
  (choice, CharParser, char, string, try, (<?>), (<|>), eof, anyChar, parse, errorPos, sourceColumn)
import qualified Text.ParserCombinators.Parsec.Error as PSE

import Prelude hiding (last, (.), all)
import Util

-- Positions/ranges:

type Pos = Int
data Range = Range { start :: Pos, size :: Int }

selectRange :: Range -> String -> String
selectRange (Range st si) = take si . drop st

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

nth :: (Monad m, Show a, Eq a) => Bool -> Int -> [a] -> [a] -> m Range
nth inv n x y | l <- find_occs x y, n < length l =
  return $ Range ((if inv then reverse l else l) !! n) (length x)
nth _ n x _ = fail $ "String " ++ show x ++ " does not occur" ++
  (if n == 0 then "." else ' ' : once_twice_thrice (n + 1) ++ ".")

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

adjustEdits :: Edit -> [Edit] -> Either Edit [Edit]
  -- returns either a conflicting Edit or the list of adjusted Edits
adjustEdits _ [] = Right []
adjustEdits e (e'@(Edit rng repl) : t) =
  case adjustStart e rng of
    Just s -> (Edit (Range s (size rng)) repl :) . adjustEdits e t
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

-- Parsers:

select_act :: [([String], a)] -> CharParser st a
select_act = choice . map (\(s, r) -> choice (try . string . s) >> return r)

verbatim :: CharParser st a -> CharParser st (String, a)
verbatim cont = many1Till' anyChar cont <?> "verbatim string"

occurrence :: Monad m => CharParser st a -> CharParser st (String -> m Range, a)
occurrence cont =
    (try (string "last") >> char ' ' >> Arrow.first last . verbatim cont)
  <|> do
    n <- select_act $ (\n -> ([rank (n+1)], n)) . [0..9]; char ' '
    inv <- (try (string "last") >> char ' ' >> return True) <|> return False
    Arrow.first (nth inv n) . verbatim cont
  <?> "occurrence specification"

occurrences :: (Functor m, Monad m) => CharParser st a -> CharParser st (String -> m [Range], a)
occurrences = auto_and True $ \cont ->
    Arrow.first (((:[]) .) .) . occurrence cont
  <|> (try (string "all" >> char ' ') >> Arrow.first all . verbatim cont)
  <|> Arrow.first ((((:[]) .) .) . sole) . verbatim cont

auto_and :: (Functor m, Monad m) => Bool ->
  (forall a . (CharParser st a -> CharParser st (String -> m [x], a))) ->
  (CharParser st b -> CharParser st (String -> m [x], b))
auto_and space_after_and f cont = do
  (x, g) <- f (Left . cont <|> Right . (try (string " and" >> (if space_after_and then char ' ' else return ' ') >> auto_and space_after_and f cont)))
  case g of
    Left y -> return (x, y)
    Right (a, b) -> return (\s -> liftM2 (++) (x s) (a s), b)

substrings :: (Monad m, Functor m) => CharParser st a -> CharParser st (String -> m [Range], a)
substrings = auto_and True $ \cont -> do
    f <- try $ select_act [
      (["after"], (\s r -> Range (start r + size r) (length s - start r - size r))),
      (["before"], (\_ r -> Range 0 (start r)))]
    char ' '
    Arrow.first (\x s -> x s >>= \r -> return [f s r]) . (occurrence cont <|> Arrow.first sole . verbatim cont)
  <|> occurrences cont -- <?> "substring specification"

positions :: (Functor m, Monad m) => CharParser st a -> CharParser st (String -> m [Pos], a)
positions = auto_and False $ \cont -> (do
  f <- try $ char ' ' >> select_act [(["before", "preceding"], start), (["after", "following"], liftM2 (+) start size)]
  char ' '
  Arrow.first (((f .) .) .) . occurrences cont
  ) <?> "position specification"

edit :: (Monad m, Functor m) => CharParser st (String -> m [Edit])
edit = fst . auto_and True e eof
 where
  e cont = select_act
    [ (["replace"], do
      (ranges_finder, _) <- substrings ((try (string " with") <|> try (string " by")) >> char ' ')
      Arrow.first (\repl -> (((flip Edit repl) .) .) . ranges_finder) . verbatim cont)
    , (["append"], Arrow.first (\r x -> return [Edit (Range (length x) 0) r]) . verbatim cont)
    , (["prepend"], Arrow.first (\r _ -> return [Edit (Range 0 0) r]) . verbatim cont)
    , (["erase", "remove", "kill", "omit", "cut"], Arrow.first ((((flip Edit "") .) .) .) . substrings cont)
    , (["insert", "add"], do
      (what, (pos_finder, y)) <- verbatim (positions cont)
      return $ ((((\p -> Edit (Range p 0) what) .) .) . pos_finder, y))
    ] >>= (char ' ' >>)

exec :: (Functor m, Monad m) => String -> String -> m String
exec cmd str = do
  f <- case parse edit "" cmd of
    Left e -> fail $ "column " ++ show (sourceColumn $ errorPos e) ++ ": " ++
      (concatMap (++ ". ") $ filter (not . List.all isSpace) $ lines $ PSE.showErrorMessages "or" "unknown parse error" "expected:" "unexpected" "end of command" $ PSE.errorMessages e)
    Right x -> return x
  e <- f str
  exec_edits e str

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
  t "erase 2" $ Left "String \"2\" occurs multiple times."
  t "erase before first 3 and replace first 2 with x" $ Left "Overlapping edits: erase \"1 2 \" and replace \"2\" with \"x\"."
  t "erase first 2 and replace second 2 with x and omit last  " $ Right "1  3 x 3 45"
  t "replace tenth last 2 by x" $ Left "String \"2\" does not occur 10 times."
  t "erase third last   and kill after second last  " $ Right "1 2 3 23 "
  t "insert x after 1 2 and before all 3 and erase 5 and prepend y" $ Right "y1 2x x3 2 x3 4 "
  t "erase before 4 and after 5" $ Right "4 5"
  t "insert x before 3 4 and 5" $ Right "1 2 3 2 x3 4 x5"
  t "insert x after 1 and all 3 and before 5 and erase 4" $ Right "1x 2 3x 2 3x  x5"
  t "insert x after " $ Left "column 16: unexpected end of command. expected: occurrence specification, \"all\" or verbatim string. "
  t "replace d with " $ Left "column 16: unexpected end of command. expected: verbatim string. "
  t "replace all 2 with 3 and erase all 3 and add x after second 2" $ Right "1 3  3x  4 5"
  t "replace all 2 with 3 and replace second 2 with x" $ Left "Overlapping edits: replace \"2\" with \"3\" and replace \"2\" with \"x\"."
 where
  t :: String -> Either String String -> IO ()
  t c o = let o' = exec c "1 2 3 2 3 4 5" in when (o' /= o) $ fail $ "test failed: " ++ show (c, o, o')

{-

Edit command grammar:

  occurrence = "last" ... | ("first" | "second" | "third" | etc) ["last"] ...
  occurrences = (occurrence | ["all"] ...) ["and" occurrences]
  substrings = (("after" | "before") (occurrence | ...) | occurrences) ["and" substrings]
  positions = (("after" | "before") occurrences) ["and" positions]

  insert = ("insert" | "add") ... positions
  append = "append" ...
  prepend = "prepend" ...
  erase = ("erase" | "remove" | "cut" | "omit" | "kill") substrings
  replace = "replace" strings ("with" | "by") ...

  command = (insert | append | prepend | erase | replace) ["and" command]

Design decisions:

  Should "erase all x and y" mean "erase all x and all y" or "erase all x and the sole occurrence of y"?

  We choose the latter, because:
  - it's easier to implement, because we don't need "and"-repetition nested under "all";
  - it's safe, because if y occurs multiple times, a clear "y occurs multiple times" error will be emitted, whereas the former solution could result in unintended edit results.

Todo:

  - Support "erase first and last x". (Hm, but is "first and second last x" the same as "(first and second) last x" or "first and (second last) x"?)
  - Fix crappy error for "erase all 2 and " (String "2 and " does not occur). Same for "erase all 2 and".
  - This "continuation" crap hurts my brain. A simpler alternative could be to use something like: verbatim kwds = many1Till anyChar (try $ char ' ' >> lookAhead (choice $ try . string . kwds)), where kwds is passed around as continuations are, now.

-}
