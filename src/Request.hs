{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module Request (is_addressed_request, is_short_request, EditableRequest(..), EditableRequestKind(..), Context(..), Response(..), EvalOpt(..), EphemeralOpt(..)) where

import Data.Set (Set)
import qualified Data.Set as Set
import qualified Text.ParserCombinators.Parsec as PS
import Control.Exception ()
import Data.Char (isAlpha, isDigit)
import Control.Monad.Error ()
import Text.ParserCombinators.Parsec (getInput, (<|>), oneOf, lookAhead, spaces, satisfy, CharParser, many1, string, parse)
import Util (Option(..), (.), (.||.))
import Prelude hiding (catch, (.))

data EvalOpt = CompileOnly | Terse | NoWarn deriving (Eq, Enum, Bounded, Ord)

instance Option EvalOpt where
  short CompileOnly = 'c'
  short Terse = 't'
  short NoWarn = 'w'
  long CompileOnly = "compile-only"
  long Terse = "terse"
  long NoWarn = "no-warn"

data EphemeralOpt = Resume deriving (Eq, Enum, Bounded)

instance Option EphemeralOpt where short Resume = 'r'; long Resume = "resume"

type Nick = String

nickP :: CharParser st Nick
nickP = many1 $ satisfy $ isAlpha .||. isDigit .||. (`elem` "[]\\`_^|}-")
  -- We don't include '{' because it messes up "geordi{...}", and no sane person would use it in a nick for a geordi bot anyway.

is_short_request :: String -> Maybe String
is_short_request =
  either (const Nothing) Just . parse (spaces >> lookAhead (string "{" <|> string "<<") >> getInput) ""

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

data EditableRequestKind = MakeType | Precedence | Evaluate (Set EvalOpt)
instance Show EditableRequestKind where
  show MakeType = "make type"
  show Precedence = "precedence"
  show (Evaluate s) = if Set.null s then "" else '-' : (short . Set.elems s)

data EditableRequest = EditableRequest { kind :: EditableRequestKind, editable_body :: String }

instance Show EditableRequest where
  show (EditableRequest (Evaluate f) s) | Set.null f = s
  show (EditableRequest k s) = show k ++ (if null s then "" else ' ' : s)

data Response = Response
  { response_new_history :: Maybe (EditableRequest, Bool) -- The Bool indicates whether the new request replaces the most recent historic request.
  , response_output :: String }
