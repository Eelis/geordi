module Request {- (is_addressed_request, is_short_request, EditableRequest, Context(..), Response(..))-} where

import qualified Text.ParserCombinators.Parsec as PS
import Control.Exception ()
import Data.Char (isAlpha, isDigit)
import Control.Monad.Error ()
import Text.ParserCombinators.Parsec (getInput, (<|>), oneOf, lookAhead, spaces, satisfy, CharParser, many1, string, parse)
import Util (Option(..), (.), (.||.), all_values)
import Prelude hiding (catch, (.))

data EvalOpt = CompileOnly | Terse | NoWarn deriving (Eq, Enum, Bounded)

instance Option EvalOpt where
  short CompileOnly = 'c'
  short Terse = 't'
  short NoWarn = 'w'
  long CompileOnly = "compile-only"
  long Terse = "terse"
  long NoWarn = "no-warn"

instance Option o => Show o where show = long

instance (Option a, Option b) => Option (Either a b) where
  short = either short short; long = either long long

data EphemeralOpt = Resume deriving (Eq, Enum, Bounded)

instance Option EphemeralOpt where short Resume = 'r'; long Resume = "resume"

type Nick = String

nickP :: CharParser st Nick
nickP = many1 $ satisfy $ isAlpha .||. isDigit .||. (`elem` "[]\\`_^|}-")
  -- We don't include '{' because it messes up "geordi{...}", and no sane person would use it in a nick for a geordi bot anyway.

is_short_request :: String -> Maybe String
is_short_request txt =
  either (const Nothing) Just (parse (spaces >> lookAhead (string "{" <|> string "<<") >> getInput) "" txt)

is_addressed_request :: String -> Maybe (Nick, String)
is_addressed_request txt = either (const Nothing) Just (parse p "" txt)
  where
   p = do
    spaces
    nick <- nickP
    oneOf ":," <|> (spaces >> lookAhead (oneOf "<{-"))
    r <- getInput
    return (nick, r)

data Context = Context { request_history :: [EditableRequest] }

data EditableRequestKind = MakeType | Precedence | Evaluate (EvalOpt -> Bool)
instance Show EditableRequestKind where
  show MakeType = "make type"
  show Precedence = "precedence"
  show (Evaluate f) = case filter f all_values of [] -> ""; s -> '-' : (short . s) ++ " "

data EditableRequest = EditableRequest { kind :: EditableRequestKind, editable_body :: String }

instance Show EditableRequest where show (EditableRequest k s) = show k ++ " " ++ s

data Response = Response
  { response_add_history :: Maybe EditableRequest
  , response_output :: String }
