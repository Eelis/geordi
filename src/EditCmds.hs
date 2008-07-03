module EditCmds (exec) where

import qualified List
import qualified Control.Arrow as Arrow

import Data.List (intersperse)
import Text.ParserCombinators.Parsec (choice, CharParser, char, string, try, (<|>), eof, anyChar)

import Prelude hiding (last, (.))
import Util

-- List substring/position splitting/searching stuff:

splitOn' :: Eq a => [a] -> [a] -> ([a], [[a]])
splitOn' _ [] = ([], [])
splitOn' s x | Just y <- stripPrefix s x = ([], uncurry (:) $ splitOn' s y)
splitOn' s (x:y) = Arrow.first (x:) $ splitOn' s y

splitOn :: Eq a => [a] -> [a] -> [[a]]
splitOn x = uncurry (:) . splitOn' x

prop_splitOn :: Eq a => [a] -> [a] -> Bool
prop_splitOn r s = (concat $ intersperse r $ splitOn r s) == s

strictSplit :: (Monad m, Eq a, Show a) => [a] -> [a] -> m (SubLists a)
strictSplit x y | (a, b:c) <- splitOn' x y = return (a, x, b, map (\v -> (x, v)) c)
strictSplit x _ = fail $ "String " ++ show x ++ " does not occur."

type SubList a = ([a], [a], [a])
type SubLists a = ([a], [a], [a], [([a], [a])])
type Positions a = ([a], [[a]])

sublist_as_sublists :: SubList a -> SubLists a
sublist_as_sublists (n, u, m) = (n, u, m, [])

positions_before, positions_after :: SubLists a -> Positions a
positions_before (a, b, c, d) = (a, (b ++ c) : map (uncurry (++)) d)
positions_after (a, b, c, d) = (a ++ b, g c d)
  where g z [] = [z]; g z ((x, y) : t) = (z ++ x) : g y t

replaceSublists :: [a] -> SubLists a -> [a]
replaceSublists r (a, _, c, d) = a ++ r ++ c ++ concatMap (\(_, x) -> r ++ x) d

first, second, third, last, sole :: (Show a, Eq a, Monad m) => [a] -> [a] -> m (SubList a)

first s x | (h, t@(_:_)) <- splitOn' s x = return (h, s, concat $ intersperse s t)
first s _ = fail $ "String " ++ show s ++ " does not occur."

second s x | (h, y : z@(_:_)) <- splitOn' s x = return (h ++ s ++ y, s, concat $ intersperse s z)
second s _ = fail $ "String " ++ show s ++ " does not occur twice."

third s x | (h, y : z : p@(_:_)) <- splitOn' s x = return (h ++ s ++ y ++ s ++ z, s, concat $ intersperse s p)
third s _ = fail $ "String " ++ show s ++ " does not occur thrice."

last s x | l@(_:_:_) <- splitOn s x = return (concat $ intersperse s (init l), s, List.last l)
last s _ = fail $ "String " ++ show s ++ " does not occur."

sole x y = do
  (a, b, c, d) <- strictSplit x y
  if null d then return (a, b, c) else fail $ "String " ++ show x ++ " occurs multiple times."

-- Parsers:

str_select :: [(String, a)] -> CharParser st a
str_select = choice . map (\(s, r) -> try (string s) >> return r)

specific :: Monad m => CharParser st a -> CharParser st (String -> m (SubList Char), a)
specific cont = do
  f <- str_select [("first ", first), ("second ", second), ("third ", third), ("last ", last)]
  Arrow.first f . manyTill' anyChar cont

specorall :: (Functor m, Monad m) => CharParser st a -> CharParser st (String -> m (SubLists Char), a)
specorall cont = (Arrow.first ((sublist_as_sublists .) .) . specific cont) <|> do
  f <- (try (string "all ") >> return strictSplit) <|> return (((sublist_as_sublists .) .) . sole)
  Arrow.first f . manyTill' anyChar cont

substrs :: (Monad m, Functor m) => CharParser st a -> CharParser st (String -> m (SubLists Char), a)
substrs cont = ba <|> specorall cont
 where
  ba = do
    f <- str_select [("before ", \(a, b, c) -> ("", a, b ++ c)), ("after ", \(a, b, c) -> (a ++ b, c, ""))];
    Arrow.first (\s x -> sublist_as_sublists . f . s x) .
      (specific cont <|> Arrow.first sole . manyTill' anyChar cont)

positions :: (Functor m, Monad m) => CharParser st a -> CharParser st (String -> m (Positions Char), a)
positions cont = try $ do
  char ' '
  f <- str_select [("before ", positions_before), ("after ", positions_after)]
  Arrow.first (\s x -> f . s x) . specorall cont

edit :: (Monad m, Functor m) => CharParser st (String -> m String)
edit = let cont = (eof >> return return) <|> (try (string " and ") >> edit) in choice
  [ do try $ string "append "; (r, y) <- manyTill' anyChar cont; return $ \s -> y (s ++ r)
  , do try $ string "prepend "; (r, y) <- manyTill' anyChar cont; return $ \s -> y (r ++ s)
  , do
      try $ string "replace "
      (x, _) <- substrs (try $ char ' ' >> (string "with " <|> string "by"))
      (r, y) <- manyTill' anyChar cont
      return $ \s -> replaceSublists r . (x s) >>= y
  , do
      try $ string "insert "
      (x, (p, q)) <- manyTill' anyChar (positions cont)
      return $ \s -> p s >>= \(u, s') -> q $ u ++ x ++ concat (intersperse x s')
  , do
      try $ string "erase " <|> string "remove "
      (x, y) <- substrs cont
      return $ \s -> replaceSublists "" . (x s) >>= y
  ]

exec :: (Functor m, Monad m) => String -> String -> m String
exec cmd str = parseOrFail edit cmd >>= ($ str)

{- Edit command grammar:

  specific = "first" ... | "second" ... | "third" ... | "last" ...
  substrs = ("after" | "before") (specific | ...) | (specific | ["all"] ...)
  positions = ("after" | "before") (specific | ["all"] ...)

  insert = "insert" ... positions
  append = "append" ...
  prepend = "prepend" ...
  erase = ("erase" | "remove") substrs
  replace = "replace" substrs ("with" | "by") ...

  command = (insert | append | prepend | erase | replace) ["and" command]
-}
