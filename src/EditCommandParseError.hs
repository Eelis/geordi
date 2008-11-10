module EditCommandParseError (showParseError) where

import qualified Text.ParserCombinators.Parsec as PS
import qualified Text.ParserCombinators.Parsec.Error as PSE
import qualified Data.List as List

import Data.Maybe (mapMaybe)
import EditCommandBasics (describe_position_after)
import EditCommandShow ()
import Util (capitalize, orElse, findMaybe, commas_or)

isUnexpectation, isExpectation, isMessage :: PSE.Message -> Maybe String

isUnexpectation (PSE.SysUnExpect s) = Just s
isUnexpectation (PSE.UnExpect s) = Just s
isUnexpectation _ = Nothing

isExpectation (PSE.Expect s@(_:_)) = Just s
isExpectation _ = Nothing

isMessage (PSE.Message s@(_:_)) = Just s
isMessage _ = Nothing

showParseError :: String -> String -> Bool -> PSE.ParseError -> String
showParseError subject_desc input show_expectation e =
  findMaybe isMessage (PSE.errorMessages e) `orElse`
    if null input then maybe "Parse error." capitalize expectation else
      case findMaybe isUnexpectation (PSE.errorMessages e) of
        Just u -> "Unexpected " ++
          (case u of "" -> "end of " ++ subject_desc; "\" \"" -> "whitespace " ++ pos; _ -> u ++ " " ++ pos) ++ "." ++
          maybe "" ((" " ++) . capitalize) expectation
        Nothing -> maybe "Parse error." ((capitalize pos ++ ", ") ++) expectation
  where
    pos = show (describe_position_after (PS.sourceColumn (PS.errorPos e) - 1) input)
    expectation = if not show_expectation then Nothing else
      case List.nub $ mapMaybe isExpectation $ PSE.errorMessages e of
        [] -> Nothing; l -> Just $ "expected " ++ commas_or l ++ "."
