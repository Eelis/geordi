module RequestEval (evaluator) where

import qualified MakeType
import qualified EvalCxx
import qualified EditCommandBasics
import qualified EditCommandParseError
import qualified EditCommandParse
import qualified EditCommandDiff
import qualified CxxParse as Cxx
import qualified Data.List as List
import Data.Char (isPrint)
import Data.Maybe (listToMaybe)
import Text.ParserCombinators.Parsec (getInput, (<|>), spaces, satisfy, eof, CharParser, string, try, choice, parse, notFollowedBy, option)
import ParsecUtil (optParser)
import Util ((.), (<<), (.||.), commas_and, isIdChar, capitalize, all_values, lefts, orElse, length_ge, replace, maybe_nonempty, unne)
import Request (Context(..), EvalOpt(..), Response(..), EditableRequest(..), EditableRequestKind(..), EphemeralOpt(..))
import Prelude hiding (catch, (.))

no_break_space :: Char
no_break_space = '\x00A0' -- NO-BREAK SPACE

line_breaks ::Cxx.Code -> Cxx.Code
line_breaks = map $ Cxx.map_plain $ map $ \c -> if c == '\\' then '\n' else c

diff :: EditableRequest -> EditableRequest -> String
diff (EditableRequest MakeType y) (EditableRequest MakeType x) = pretty $ show . EditCommandDiff.diff x y
diff (EditableRequest Precedence y) (EditableRequest Precedence x) = pretty $ show . EditCommandDiff.diff x y
diff (EditableRequest (Evaluate flags) y) (EditableRequest (Evaluate flags') x) =
  pretty $ f "removed" flags' flags ++ f "added" flags flags' ++ show . EditCommandDiff.diff x y
    where f n fl fl' = maybe [] (\l -> [n ++ " " ++ concat (List.intersperse " and " $ map show $ unne l)]) (maybe_nonempty $ filter (\o -> fl o && not (fl' o)) all_values)
diff _ _ = "Requests differ in kind."

pretty :: [String] -> String
pretty [] = "Requests are identical."
pretty l = capitalize (commas_and l) ++ "."

kwd :: [String] -> CharParser st ()
kwd s = try (choice (try . string . s) >> notFollowedBy (satisfy isIdChar)) >> spaces

evaluator :: IO (String -> Context -> IO Response)
evaluator = do
  (ev, compile_cfg) <- EvalCxx.evaluator
  let
    evf :: EvalCxx.Request -> IO String
    evf r = filter (isPrint .||. (== '\n')) . show . ev r
    -- Filtering using isPrint works properly because (1) the EvalCxx evaluator returns proper Unicode Strings, not mere byte blobs; and (2) to print filtered strings we will use System.IO.UTF8's hPutStrLn which properly UTF-8-encodes the filtered String.
    -- Possible problem: terminals which have not been (properly) UTF-8 configured might interpret bytes that are part of UTF-8 encoded characters as control characters.
  return $ \r (Context prevs) -> do
  let
    prel = "#include \"prelude.hpp\"\n"
    respond :: EditableRequest -> IO String
    respond (EditableRequest MakeType d) = return $ either ("error: " ++) show $ MakeType.makeType d
    respond (EditableRequest Precedence t) = return $ either ("error: " ++) show $ Cxx.parseExpr t
    respond (EditableRequest (Evaluate opts) code) =
      case parse (Cxx.code << eof) "" code of
        Right sc -> do
          evf $ EvalCxx.Request (prel ++ (if opts Terse then "#include \"terse.hpp\"\n" else "") ++ show (Cxx.expand $ Cxx.shortcut_syntaxes $ line_breaks sc)) (not $ opts CompileOnly) (opts NoWarn)
        Left e -> return $ "error: " ++ EditCommandParseError.showParseError "request" code False e
    p :: CharParser st (IO Response)
    p =
      (kwd ["show"] >> eof >> return (return $ Response Nothing $ show . listToMaybe prevs `orElse` "<none>"))
      <|> (kwd ["--precedence", "precedence"] >> getInput >>= \i -> return $
        let er = EditableRequest Precedence i in Response (Just er) . respond er)
      <|> (kwd ["--make-type", "make type"] >> getInput >>= \i -> return $
        let er = EditableRequest MakeType i in Response (Just er) . respond er)
      <|> (kwd ["--help", "-h", "help"] >> eof >> return (Response Nothing . evf (EvalCxx.Request (prel ++ "int main() { cout << help; }") True False)))
      <|> (kwd ["--version", "-v", "version"] >> eof >> return (Response Nothing . evf (EvalCxx.Request (prel ++ "int main() { cout << \"g++ (GCC) \" << __VERSION__; }") True False)))
      <|> (kwd ["--show-compile-flags"] >> eof >> return (return $ Response Nothing $ unwords $ EvalCxx.compileFlags compile_cfg))
      <|> (kwd ["diff", "diffs", "differences", "change", "changes"] >> eof >> case prevs of
        y : x : _ -> return $ return $ Response Nothing $ diff y x
        _ -> fail "I have not yet seen two comparable requests."
      ) <|> (do
        kwd ["again", "try again"]; eof
        case prevs of
          [] -> fail "There is no repeatable request."
          er : _ -> return $ Response Nothing . respond er
      ) <|> (do
        cs <- EditCommandParse.commandsP; eof
        case prevs of
          [] -> fail "There is no previous editable request."
          er : _ -> case EditCommandBasics.execute cs er of
            Left e -> fail e
            Right (EditableRequest _ h') | length_ge 1000 h' -> fail "Request would become too large."
            Right e' -> return $ Response (Just e') . respond e'
      ) <|> (do
        opts <- option [] optParser
        spaces
        let evalopts = (`elem` lefts opts)
        er <- if Right Resume `elem` opts
          then case prevs of
            [] -> fail "There is no previous resumable request."
            EditableRequest (Evaluate oldopts) oldcodeblob : _ -> case parse (Cxx.code << eof) "" oldcodeblob of
              Right oldcode -> do
                code <- Cxx.code; eof
                return $ EditableRequest (Evaluate $ evalopts .||. oldopts) $ show $ Cxx.blob $ Cxx.resume (Cxx.shortcut_syntaxes oldcode) (Cxx.shortcut_syntaxes code)
              Left _ -> fail "Previous request too malformed to resume."
            _ -> fail "Last (editable) request was not resumable."
          else EditableRequest (Evaluate evalopts) . getInput
        return $ Response (Just er) . respond er)
  let b = replace no_break_space ' ' r
  either (return . Response Nothing . ("error: " ++) . EditCommandParseError.showParseError "request" b True) id (parse p "" b)
