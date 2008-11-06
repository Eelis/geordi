module Request (is_addressed_request, is_short_request, EditableRequest, evaluator, Context(..), Response(..)) where

import qualified EvalCxx
import qualified CxxParse as Cxx
import qualified MakeType
import qualified Text.ParserCombinators.Parsec as PS
import qualified EditCmds
import qualified Data.List as List

import Control.Exception ()
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Char (isPrint, isAlpha, isDigit)
import Control.Monad (guard, liftM2)
import Control.Monad.Error ()
import Text.ParserCombinators.Parsec (getInput, (<|>), (<?>), oneOf, lookAhead, spaces, satisfy, eof, CharParser, many1, string, try, choice, char, parse, notFollowedBy)

import Prelude hiding (catch, (.))
import Util

optParser :: (Char -> Maybe o) -> (String -> Maybe o) -> CharParser st [o]
optParser c s = (spaces >>) $ (<|> return []) $ (char '-' >>) $ do
    char '-'
    n <- (many1 $ satisfy $ isAlpha .||. (== '-')) <?> "option name"
    case s n of
      Nothing -> fail $ "No such option: --" ++ n
      Just o -> (o:) . optParser c s
  <|> liftM2 (++)
    (many1 $ do
      d <- satisfy isAlpha <?> "letter"
      maybe (fail $ "No such option: -" ++ [d]) return (c d))
    (optParser c s)

data EvalOpt = CompileOnly | Terse | NoWarn deriving (Eq, Enum, Bounded)

short_eval_opt :: EvalOpt -> Char
short_eval_opt CompileOnly = 'c'
short_eval_opt Terse = 't'
short_eval_opt NoWarn = 'w'

long_eval_opt :: EvalOpt -> String
long_eval_opt CompileOnly = "compile-only"
long_eval_opt Terse = "terse"
long_eval_opt NoWarn = "no-warn"

evalOptParser :: CharParser st [EvalOpt]
evalOptParser = optParser (\c -> List.find ((== c) . short_eval_opt) enumAll) (\s -> List.find ((== s) . long_eval_opt) enumAll)

wrapPrePost :: String -> String -> String
wrapPrePost t c = "GEORDI_" ++ t ++ "_PRE " ++ c ++ "\nGEORDI_" ++ t ++ "_POST"

wrapPrint, wrapStmts :: String -> String
wrapPrint = wrapPrePost "PRINT"
wrapStmts = wrapPrePost "STATEMENTS"

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

splitSemicolon :: Cxx.Code -> (Cxx.Code, Cxx.Code)
splitSemicolon (Cxx.Code []) = (Cxx.Code [], Cxx.Code [])
splitSemicolon (Cxx.Code (Cxx.Plain s : r)) | maybeLast s == Just ';' =
  (Cxx.Code [Cxx.Plain s], Cxx.Code r)
splitSemicolon (Cxx.Code (a : r)) = (Cxx.Code $ a : x, y)
  where (Cxx.Code x,y) = splitSemicolon (Cxx.Code r)

line_breaks ::Cxx.Code -> Cxx.Code
line_breaks = Cxx.map_chunks $ Cxx.map_plain $ map $ \c -> if c == '\\' then '\n' else c

shortcut_syntaxes :: Cxx.Code -> String
shortcut_syntaxes (Cxx.Code (Cxx.Curlies c : b)) = show (Cxx.Code b) ++ "\n" ++ wrapStmts (show c)
shortcut_syntaxes (Cxx.Code (Cxx.Plain ('<':'<':x) : y)) =
    let (a, b) = splitSemicolon $ Cxx.Code $ Cxx.Plain x : y in
    show b ++ "\n" ++ wrapPrint (show a)
shortcut_syntaxes c = show c

data Context = Context { request_history :: [EditableRequest] }

data EditableRequestKind = MakeType | Precedence | Evaluate (EvalOpt -> Bool)
instance Show EditableRequestKind where
  show MakeType = "make type "
  show Precedence = "precedence "
  show (Evaluate f) = case filter f enumAll of [] -> ""; s -> '-' : (short_eval_opt . s) ++ " "

data EditableRequest = EditableRequest EditableRequestKind String
instance Show EditableRequest where show (EditableRequest kind s) = show kind ++ s

data Response = Response
  { response_add_history :: Maybe EditableRequest
  , response_output :: String }

no_break_space :: Char
no_break_space = '\x00A0' -- NO-BREAK SPACE

diff :: EditableRequest -> EditableRequest -> String
diff r r' = case (r, r') of
    (EditableRequest MakeType y, EditableRequest MakeType x) -> pretty $ show . EditCmds.diff x y
    (EditableRequest Precedence y, EditableRequest Precedence x) -> pretty $ show . EditCmds.diff x y
    (EditableRequest (Evaluate flags) y, EditableRequest (Evaluate flags') x) -> pretty $
      mapMaybe (\o -> if flags o && not (flags' o) then Just $ "added --" ++ long_eval_opt o else if flags' o && not (flags o) then Just $ "removed --" ++ long_eval_opt o else Nothing) enumAll ++ show . EditCmds.diff x y
    _ -> "Requests differ in kind."
  where
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
          ou <- evf $ EvalCxx.Request (prel ++ (if opts Terse then "#include \"terse.hpp\"\n" else "") ++ shortcut_syntaxes (line_breaks sc)) (not $ opts CompileOnly) (opts NoWarn)
          return $ ou
        Left e -> return $ EditCmds.showParseError "request" code False e
    p :: CharParser st (IO Response)
    p =
      (kwd ["show"] >> eof >> return (return $ Response Nothing $ show . listToMaybe prevs `orElse` "<none>"))
      <|> (kwd ["--precedence", "precedence"] >> getInput >>= \i -> return $
        let er = EditableRequest Precedence i in Response (Just er) . respond er)
      <|> (kwd ["--make-type", "make type"] >> getInput >>= \i -> return $
        let er = EditableRequest MakeType i in Response (Just er) . respond er)
      <|> (kwd ["--help", "-h", "help"] >> eof >> return (Response Nothing . evf (EvalCxx.Request (prel ++ wrapPrint "help") True False)))
      <|> (kwd ["--version", "-v", "version"] >> eof >> return (Response Nothing . evf (EvalCxx.Request (prel ++ wrapPrint "\"g++ (GCC) \" << __VERSION__") True False)))
      <|> (kwd ["--show-compile-flags"] >> eof >> return (return $ Response Nothing $ unwords $ EvalCxx.compileFlags compile_cfg))
      <|> (kwd ["diff", "diffs", "differences", "change", "changes"] >> eof >> case prevs of
        y : x : _ -> return $ return $ Response Nothing $ diff y x
        _ -> fail "I have not yet seen two comparable requests."
      ) <|> (do
        opts <- try $ do
          kwd ["erase", "remove", "delete", "cut", "omit", "kill"]
          o <- evalOptParser; eof; guard $ not $ null o; return o
        case prevs of
          EditableRequest (Evaluate opts') c : _ -> return $
            let er = EditableRequest (Evaluate ((not . (`elem` opts)) .&&. opts')) c in Response (Just er) . respond er
          _ -> fail "Last (editable) request was not an evaluation request."
      ) <|> (do
        opts <- try $ do
          kwd ["use", "add", "insert", "prepend"]
          o <- evalOptParser; eof; guard $ not $ null o; return o
        case prevs of
          EditableRequest (Evaluate opts') c : _ -> return $
            let er = EditableRequest (Evaluate ((`elem` opts) .||. opts')) c in Response (Just er) . respond er
          _ -> fail "Last (editable) request was not an evaluation request."
      ) <|> (do
        kwd ["again", "try again"]; eof
        case prevs of
          [] -> fail "There is no repeatable request."
          er : _ -> return $ Response Nothing . respond er
      ) <|> (do
        cs <- EditCmds.commandsP; eof
        case prevs of
          [] -> fail "There is no previous editable request."
          EditableRequest kind h : _ -> case EditCmds.execute cs h of
            Left e -> fail e
            Right h' | length_ge 1000 h' -> fail "Request would become too large."
            Right h' -> return $ let e' = EditableRequest kind h' in Response (Just e') . respond e'
      ) <|> (do
        opts <- evalOptParser; i <- getInput
        return $ let er = EditableRequest (Evaluate (`elem` opts)) i in Response (Just er) . respond er)
  let b = replace no_break_space ' ' r
  either (return . Response Nothing . ("error: " ++) . EditCmds.showParseError "request" b True) id (parse p "" b)
