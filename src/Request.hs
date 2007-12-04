module Request (parse_request, is_request, prepare_evaluator) where

import qualified EvalCxx

import Control.Exception ()
import Data.Char (isLetter, isPrint)
import Control.Monad.Error ()
import Text.ParserCombinators.Parsec (parse, getInput, spaces, satisfy, (<|>), oneOf, try, string)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt)
import Control.Applicative ((<*>))

import Prelude hiding (catch, (.))
import Util

data RequestOpt = RO_compileOnly | RO_terse | RO_help | RO_version deriving Eq

requestOptsDesc :: [OptDescr RequestOpt]
requestOptsDesc =
  [ Option "c" ["compile-only"] (NoArg RO_compileOnly) undefined
  , Option "t" ["terse"] (NoArg RO_terse) undefined
  , Option "h" ["help"] (NoArg RO_help) undefined
  , Option "v" ["version"] (NoArg RO_version) undefined
  ]

wrapPrePost :: String -> String -> String
wrapPrePost t c = "GEORDI_" ++ t ++ "_PRE " ++ c ++ "\nGEORDI_" ++ t ++ "_POST"

wrapPrint, wrapStmts :: String -> String
wrapPrint = wrapPrePost "PRINT"
wrapStmts = wrapPrePost "STATEMENTS"

is_request :: String -> String -> String -> Maybe String
is_request botnick botaltnick txt = either (const Nothing) Just (parse p "" txt)
  where
   p = do
    foldr1 (<|>) $ try . string . sortByProperty length
      [botnick, capitalize botnick, botaltnick, capitalize botaltnick]
    (oneOf ":," >> getInput) <|> ((:) . (spaces >> satisfy (not . (isLetter .||. (`elem` "\'/*")))) <*> getInput)

parse_request :: Monad m => String -> m (String {- code -}, Bool {- also run -})
parse_request req = do
  (opts, rest) <- case getOpt RequireOrder requestOptsDesc (words req) of
    (_, _, (err:_)) -> fail err
    (opts, non_opts, []) -> return (opts, concat $ takeBack (length non_opts) $ wordsWithWhite req)
      -- We can't use non_opts' contents, because whitespace between tokens has been lost.
  let
    opt = (`elem` opts)
    code = unlines $
      ["#include \"prelude.h\""] ++
      (if opt RO_terse then ["#include \"terse.hpp\""] else []) ++
      case () of
        _ | opt RO_help -> [wrapPrint "help"]
        _ | opt RO_version -> [wrapPrint $ "\"g++ (GCC) \" << __VERSION__"]
        _ | '{':_ <- rest -> [wrapStmts rest]
        _ | '<':'<':x <- rest -> [wrapPrint x]
        _ -> [rest]
    also_run = opt RO_help || opt RO_version || not (opt RO_compileOnly)
  return (code, also_run)

prepare_evaluator :: IO (String -> IO String)
prepare_evaluator = do
  EvalCxx.cap_fds
  gxx : flags <- words . (full_evaluate =<< readFile "compile-config")
    -- readFile would fail after the chroot, hence full_evaluate.
  return $ either return ((filter (isPrint .||. (== '\n')) . show .) . uncurry (EvalCxx.evaluate gxx (["prelude.a", "-lmcheck"] ++ flags))) . Request.parse_request
    -- filtering using isPrint works properly because (1) EvalCxx.evaluate returns a proper Unicode String, not a load of bytes; and (2) to print filtered strings we will use System.IO.UTF8's hPutStrLn which properly UTF-8 encodes the filtered String.
    -- Possible problem: terminals which have not been (properly) UTF-8 configured might interpret bytes that are part of UTF-8 encoded characters as control characters.
