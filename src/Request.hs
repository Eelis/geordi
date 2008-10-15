module Request (is_addressed_request, is_short_request, evaluator, Context(..), Response(..)) where

import qualified EvalCxx
import qualified CxxParse as Cxx
import qualified MakeType
import qualified Text.ParserCombinators.Parsec as PS
import qualified EditCmds

import Control.Exception ()
import Data.Maybe (listToMaybe)
import Data.Char (isPrint, isAlpha, isDigit)
import Control.Monad.Error ()
import Data.Function (fix)
import Text.ParserCombinators.Parsec (getInput, (<|>), oneOf, lookAhead, spaces, satisfy, eof, CharParser, many1, string)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt)

import Prelude hiding (catch, (.))
import Util

data Opt = CompileOnly | Terse | Help | Version | NoWarn | CompileFlags | MakeType deriving Eq

optsDesc :: [OptDescr Opt]
optsDesc =
  [ Option "c" ["compile-only"] (NoArg CompileOnly) undefined
  , Option "w" ["no-warn"] (NoArg NoWarn) undefined
  , Option "t" ["terse"] (NoArg Terse) undefined
  , Option "h" ["help"] (NoArg Help) undefined
  , Option "v" ["version"] (NoArg Version) undefined
  , Option "" ["show-compile-flags"] (NoArg CompileFlags) undefined
  , Option "" ["make-type"] (NoArg MakeType) undefined
  ]

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
  either (const Nothing) Just (PS.parse (spaces >> lookAhead (string "{" <|> string "<<") >> getInput) "" txt)

is_addressed_request :: String -> Maybe (Nick, String)
is_addressed_request txt = either (const Nothing) Just (PS.parse p "" txt)
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

data Request
  = EvalRequest { er_code :: String, er_also_run, er_nowarn, er_terse :: Bool }
  | ShowCompileFlags
  | MakeTypeReq String
  | HelpRequest
  | CompilerVersionRequest
  | EditRequest [EditCmds.Command]
  | DiffRequest
  | ShowRequest

shortcut_syntaxes :: Cxx.Code -> String
shortcut_syntaxes (Cxx.Code (Cxx.Curlies c : b)) = show (Cxx.Code b) ++ "\n" ++ wrapStmts (show c)
shortcut_syntaxes (Cxx.Code (Cxx.Plain ('<':'<':x) : y)) =
    let (a, b) = splitSemicolon $ Cxx.Code $ Cxx.Plain x : y in
    show b ++ "\n" ++ wrapPrint (show a)
shortcut_syntaxes c = show c

parse :: (Functor m, Monad m) => String -> m Request
parse req = do
  (opts, rest) <- case getOpt RequireOrder optsDesc (words req) of
    (_, _, (err:_)) -> fail err
    (opts, non_opts, []) -> return (opts, concat $ takeBack (length non_opts) $ wordsWithWhite req)
      -- We can't use non_opts' contents, because whitespace between tokens has been lost.
  let opt = (`elem` opts)
  if opt Help then return HelpRequest else do
  if opt MakeType then return $ MakeTypeReq rest else do
  if opt CompileFlags then return ShowCompileFlags else do
  if opt Version then return CompilerVersionRequest else do
  if rest == "show" then return ShowRequest else do
  if rest `elem` ["diff", "diffs", "differences", "change", "changes"] then return DiffRequest else do
  either (fail . showParseError False) return $ PS.parse ((EditRequest . EditCmds.commandsP << eof) <|> return (EvalRequest rest (not $ opt CompileOnly) (opt NoWarn) (opt Terse))) "" rest

data Context = Context { request_history :: [String] }

data Response = Response
  { response_output :: String
  , response_add_history :: Maybe String }

evaluator :: IO (String -> Context -> IO Response)
evaluator = do
  (ev, compile_cfg) <- EvalCxx.evaluator
  let evf r = filter (isPrint .||. (== '\n')) . show . ev r
    -- Filtering using isPrint works properly because (1) the EvalCxx evaluator returns proper Unicode Strings, not mere byte blobs; and (2) to print filtered strings we will use System.IO.UTF8's hPutStrLn which properly UTF-8-encodes the filtered String.
    -- Possible problem: terminals which have not been (properly) UTF-8 configured might interpret bytes that are part of UTF-8 encoded characters as control characters.
  let prel = "#include \"prelude.hpp\"\n"
  return $ fix $ \f r (Context prevs) -> case parse r of
    Left e -> return $ Response ("error: " ++ e) Nothing
    Right HelpRequest -> do
      ou <- evf $ EvalCxx.Request (prel ++ wrapPrint "help") True False
      return $ Response ou Nothing
    Right CompilerVersionRequest -> do
      ou <- evf $ EvalCxx.Request (prel ++ wrapPrint "\"g++ (GCC) \" << __VERSION__") True False
      return $ Response ou Nothing
    Right ShowCompileFlags -> return $ Response (unwords $ EvalCxx.compileFlags compile_cfg) Nothing
    Right (MakeTypeReq d) -> return $ Response (either ("error: " ++) show $ MakeType.makeType d) Nothing
    Right ShowRequest -> return $ Response (listToMaybe prevs `orElse` "<none>") Nothing
    Right (EditRequest editcmd) -> case prevs of
      [] -> return $ Response "There is no previous request to modify." Nothing
      (h:_) -> case EditCmds.execute editcmd h of
        Left e -> return $ Response e Nothing
        Right h' | length_ge 1000 h' -> return $ Response "Request would become too large." Nothing
        Right h' -> f h' (Context prevs)
    Right DiffRequest -> return $ Response (case prevs of
        y : x : _ -> either id show $ EditCmds.diff x y
        _ -> "Need at least two editable requests to compare.") Nothing
    Right (EvalRequest code also_run nowarn terse) -> do
      case PS.parse (Cxx.code << eof) "" code of
        Right sc -> do
          ou <- evf $ EvalCxx.Request (prel ++ (if terse then "#include \"terse.hpp\"\n" else "") ++ shortcut_syntaxes (line_breaks sc)) also_run nowarn
          return $ Response ou (Just r)
        Left e -> return $ Response (showParseError True e) (Just r)
