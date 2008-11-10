module ParsecUtil where

import qualified Data.List as List

import Text.ParserCombinators.Parsec ((<|>), (<?>), CharParser, GenParser, unexpected, optional, labels, char, satisfy, many1, option, try, sepBy1)
import Util (Option(..), (.||.), Finite(..), NElist(..), (.))
import Control.Monad (liftM2)
import Control.Arrow (first)
import Data.Char (isAlpha)

import Prelude hiding ((.))

optParser :: (Finite o, Option o) => CharParser st [o]
optParser = (try (spaces >> char '-') >>) $ do
    char '-'
    n <- (many1 $ satisfy $ isAlpha .||. (== '-')) <?> "option name"
    case List.find ((== n) . long) all_values of
      Nothing -> fail $ "No such option: --" ++ n
      Just o -> (o:) . option [] optParser
  <|> liftM2 (++)
    (many1 $ do
      d <- satisfy isAlpha <?> "option letter"
      maybe (fail $ "No such option: -" ++ [d]) return (List.find ((== d) . short) all_values))
    (option [] optParser)

spaces :: CharParser st ()
spaces = optional (char ' ' >> spaces) `labels` []

notFollowedBy :: Show a => CharParser st a -> CharParser st ()
notFollowedBy p = ((try p >>= return . unexpected . show) <|> return (return ())) >>= id

sepBy1' :: CharParser st a -> CharParser st b -> CharParser st (NElist a)
sepBy1' x y = (\(h:t) -> NElist h t) . sepBy1 x y

many1Till' :: GenParser tok st a -> GenParser tok st end -> GenParser tok st ([a], end)
many1Till' p end = p >>= \v -> first (v:) . scan
  where scan = ((\r -> ([], r)) . end) <|> (do { x <- p; (xs, e) <- scan; return (x:xs, e) })
