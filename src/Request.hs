{-# LANGUAGE UnicodeSyntax, FlexibleInstances, UndecidableInstances, OverlappingInstances, ViewPatterns #-}

module Request (is_addressed_request, is_nickless_request, RequestEdit(..), EditableRequest(..), EditableRequestKind(..), Context(..), Response(..), EvalOpt(..), EphemeralOpt(..), HistoryModification(..), modify_history, popContext, addEvalOpt) where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (liftM2)
import Control.Monad.Except (throwError)
import Cxx.Show (Highlighter)
import Control.Exception ()
import Data.Char (isAlpha, isDigit, isSpace)
import Data.List (intercalate)
import Text.ParserCombinators.Parsec (getInput, (<|>), oneOf, lookAhead, spaces, satisfy, CharParser, many1, parse)
import Util (Option(..), (.), (.∨.), total_tail, partitionMaybe, E)
import Editing.Basics (TextEdit)
import Prelude hiding ((.))
import Prelude.Unicode

data EvalOpt
  = CompileOnly
  | PreprocessOnly
  | Tracing
  | NoWarn
  | NoUsingStd
  | Clang
  | Gcc
  | Std98
  | Std03
  | Std11
  | Std14
  deriving (Eq, Enum, Bounded, Ord)

data RequestEdit
  = TextEdit (TextEdit Char)
  | AddOptions [Request.EvalOpt]
  | RemoveOptions [Request.EvalOpt]

instance Option EvalOpt where
  short CompileOnly = Just 'c'
  short NoWarn = Just 'w'
  short _ = Nothing
  long CompileOnly = "compile-only"
  long NoWarn = "no-warn"
  long PreprocessOnly = "preprocess"
  long Tracing = "trace"
  long NoUsingStd = "no-using-std"
  long Clang = "clang"
  long Gcc = "gcc"
  long Std98 = "1998"
  long Std03 = "2003"
  long Std11 = "2011"
  long Std14 = "2014"

data EphemeralOpt = Resume | Help | Version deriving (Eq, Enum, Bounded)

instance Option EphemeralOpt where
  long Resume = "resume"; long Help = "help"; long Version = "version"
  short Resume = Just 'r'; short Help = Just 'h'; short Version = Just 'v'

type Nick = String

nickP :: CharParser st Nick
nickP = many1 $ satisfy $ isAlpha .∨. isDigit .∨. (∈ "[]\\`_^|}-")
  -- We don't include '{' because it messes up "geordi{...}", and no sane person would use it in a nick for a geordi bot anyway.

is_nickless_request :: String → Maybe String
is_nickless_request (dropWhile isSpace → s) = case s of
  '{' : s' | not $ all isSpace s' → Just s
    -- A '{' on a line of its own can occur as part of a small code fragments pasted in a channel. Of course, so can a '{' followed by more code on the same line, but for a '{' on a line of its own, we /know/ it's not intended for geordi.
  '<' : '<' : _ → Just s
  _ → Nothing

is_addressed_request :: String → Maybe (Nick, String)
is_addressed_request txt = either (const Nothing) Just (parse p "" txt)
  where p = liftM2 (,) (spaces >> nickP) (spaces >> (oneOf ":," <|> lookAhead (oneOf "<{-(")) >> getInput)

data Context = Context
  { highlighter :: Highlighter
  , clangByDefault :: Bool
  , previousRequests :: [HistoricalRequest] }

popContext :: Context → E (HistoricalRequest, Context)
popContext c@Context{previousRequests=x:xs} = return (x, c{previousRequests=xs})
popContext _ = throwError "History exhausted."

data EditableRequestKind = MakeType | Precedence | Evaluate (Set EvalOpt)
instance Show EditableRequestKind where
  show MakeType = "make type"
  show Precedence = "precedence"
  show (Evaluate s) = intercalate " " $ (if null shorts then id else (('-' : shorts) :) ) $ ("--"++) . long . longs
    where (longs, shorts) = partitionMaybe short (Set.elems s)

data EditableRequest = EditableRequest { kind :: EditableRequestKind, editable_body :: String }

type HistoricalRequest = (EditableRequest, Maybe (TextEdit Char) {- a fix-it -})
data HistoryModification = ReplaceLast HistoricalRequest | AddLast HistoricalRequest | DropLast

modify_history :: HistoryModification → Context → Context
modify_history m (Context h cbd l) = Context h cbd $ case m of
  ReplaceLast e → e : total_tail l
  AddLast e → e : l
  DropLast → total_tail l

data Response = Response
  { response_history_modification :: Maybe HistoryModification
  , response_output :: String }

data EvalOptKind = StageOpt | StdOpt | CompilerOpt | MiscOpt
  deriving Eq

evalOptKind :: EvalOpt -> EvalOptKind
evalOptKind x
  | x `elem` [Std98,Std03,Std11,Std14] = StdOpt
  | x `elem` [Gcc, Clang] = CompilerOpt
  | x `elem` [CompileOnly, PreprocessOnly] = StageOpt
  | otherwise = MiscOpt

replaces :: EvalOpt -> EvalOpt -> Bool
x `replaces` y = evalOptKind x == evalOptKind y && evalOptKind x /= MiscOpt

addEvalOpt :: EvalOpt -> Set EvalOpt -> Set EvalOpt
addEvalOpt o = Set.insert o . Set.filter (not . (o `replaces`))
