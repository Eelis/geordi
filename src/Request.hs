module Request (is_addressed_request, is_short_request, evaluator, parse, Request(..), is_editable) where

import qualified EvalCxx
import qualified CxxParse as Cxx
import qualified MakeType
import qualified Text.ParserCombinators.Parsec as PS

import Control.Exception ()
import Data.Char (isPrint, isAlpha, isDigit)
import Control.Monad.Error ()
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

newlines ::Cxx.Code -> Cxx.Code
newlines = Cxx.map_chunks $ Cxx.map_plain $ map $ \c -> if c == '\\' then '\n' else c

data Request
  = EvalRequest { er_code :: String, er_also_run, er_nowarn, er_terse :: Bool }
  | ShowCompileFlags
  | MakeTypeReq String
  | HelpRequest
  | CompilerVersionRequest

is_editable :: Request -> Bool
is_editable (EvalRequest _ _ _ _) = True
is_editable (MakeTypeReq _) = True
is_editable _ = False

parse :: (Functor m, Monad m) => String -> m Request
parse req = do
  (opts, rest) <- case getOpt RequireOrder optsDesc (words req) of
    (_, _, (err:_)) -> fail err
    (opts, non_opts, []) -> return (opts, concat $ takeBack (length non_opts) $ wordsWithWhite req)
      -- We can't use non_opts' contents, because whitespace between tokens has been lost.
  reqCode <- newlines . parseOrFail (Cxx.code << eof) rest
  let
    opt = (`elem` opts)
    er code = EvalRequest code (not $ opt CompileOnly) (opt NoWarn) (opt Terse)
  return $ case () of
    ()| opt Help -> HelpRequest
    ()| opt MakeType -> MakeTypeReq rest
    ()| opt CompileFlags -> ShowCompileFlags
    ()| opt Version -> CompilerVersionRequest
    ()| Cxx.Code (Cxx.Curlies c : b) <- reqCode -> er $ show (Cxx.Code b) ++ "\n" ++ wrapStmts (show c)
    ()| Cxx.Code (Cxx.Plain ('<':'<':x) : y) <- reqCode ->
      let (a, b) = splitSemicolon $ Cxx.Code $ Cxx.Plain x : y in
      er $ show b ++ "\n" ++ wrapPrint (show a)
    ()| otherwise -> er $ show reqCode

evaluator :: IO (Request -> IO String)
evaluator = do
  (ev, compile_cfg) <- EvalCxx.evaluator
  let evf r = filter (isPrint .||. (== '\n')) . show . ev r
    -- Filtering using isPrint works properly because (1) the EvalCxx evaluator returns proper Unicode Strings, not mere byte blobs; and (2) to print filtered strings we will use System.IO.UTF8's hPutStrLn which properly UTF-8-encodes the filtered String.
    -- Possible problem: terminals which have not been (properly) UTF-8 configured might interpret bytes that are part of UTF-8 encoded characters as control characters.
  let prel = "#include \"prelude.hpp\"\n"
  return $ \r -> case r of
    HelpRequest -> evf $ EvalCxx.Request (prel ++ wrapPrint "help") True False
    CompilerVersionRequest -> evf $ EvalCxx.Request (prel ++ wrapPrint "\"g++ (GCC) \" << __VERSION__") True False
    ShowCompileFlags -> return $ unwords $ EvalCxx.compileFlags compile_cfg
    MakeTypeReq d -> return $ either ("error: " ++) show $ MakeType.makeType d
    EvalRequest code also_run nowarn terse ->
      evf $ EvalCxx.Request (prel ++ (if terse then "#include \"terse.hpp\"\n" else "") ++ code) also_run nowarn
