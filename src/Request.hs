module Request (is_request, prepare_evaluator) where

import qualified EvalCxx
import qualified System.Directory

import Control.Exception ()
import Control.Applicative ((<*>))
import Data.Char (isPrint, toUpper, toLower)
import Control.Monad.Error ()
import Text.ParserCombinators.Parsec (parse, getInput, (<|>), oneOf, try, string, lookAhead, choice, spaces, satisfy, sourceColumn, eof, GenParser)
import Text.ParserCombinators.Parsec.Error (errorMessages, Message(..), errorPos, showErrorMessages)
import System.Console.GetOpt (OptDescr(..), ArgDescr(..), ArgOrder(..), getOpt)
import System.Posix.User
  (getGroupEntryForName, getUserEntryForName, setGroupID, setUserID, groupID, userID)
import Sys (chroot)

import qualified CxxParse as Cxx

import Prelude hiding (catch, (.))
import Util

data Opt = CompileOnly | Terse | Help | Version deriving Eq

optsDesc :: [OptDescr Opt]
optsDesc =
  [ Option "c" ["compile-only"] (NoArg CompileOnly) undefined
  , Option "t" ["terse"] (NoArg Terse) undefined
  , Option "h" ["help"] (NoArg Help) undefined
  , Option "v" ["version"] (NoArg Version) undefined
  ]

wrapPrePost :: String -> String -> String
wrapPrePost t c = "GEORDI_" ++ t ++ "_PRE " ++ c ++ "\nGEORDI_" ++ t ++ "_POST"

wrapPrint, wrapStmts :: String -> String
wrapPrint = wrapPrePost "PRINT"
wrapStmts = wrapPrePost "STATEMENTS"

is_request :: [String] -> String -> Maybe String
is_request nicks txt = either (const Nothing) Just (parse p "" txt)
  where
   p = do
    choice $ ((\(x:xs) -> try $ oneOf [toLower x, toUpper x] >> string xs) .) $
      reverse $ sortByProperty length nicks
    lookAhead $ satisfy (/= '-')
    oneOf ":," <|> (spaces >> lookAhead (oneOf "<{-"))
    getInput

splitSemicolon :: Cxx.Code -> (Cxx.Code, Cxx.Code)
splitSemicolon (Cxx.Code []) = (Cxx.Code [], Cxx.Code [])
splitSemicolon (Cxx.Code (Cxx.Plain s : r)) | last s == ';' = (Cxx.Code [Cxx.Plain s], Cxx.Code r)
splitSemicolon (Cxx.Code (a : r)) = (Cxx.Code $ a : x, y)
  where (Cxx.Code x,y) = splitSemicolon (Cxx.Code r)

parseOrFail :: Monad m => GenParser tok () a -> [tok] -> m a
parseOrFail p t = either (fail . showParseError) return $ parse p "" t
 where
  showParseError e =
    "column " ++ show (sourceColumn $ errorPos e) ++ ": " ++
    concatMap (++ ". ") (tail $ lines $ showErrorMessages "or" undefined undefined "unexpected" "end of request" $ filter isUnexpMsg $ errorMessages e)
  isUnexpMsg (SysUnExpect _) = True
  isUnexpMsg (UnExpect _) = True
  isUnexpMsg _ = False

parse_request :: (Functor m, Monad m) => String -> m (String {- code -}, Bool {- also run -})
parse_request req = do
  (opts, rest) <- case getOpt RequireOrder optsDesc (words req) of
    (_, _, (err:_)) -> fail err
    (opts, non_opts, []) -> return (opts, concat $ takeBack (length non_opts) $ wordsWithWhite req)
      -- We can't use non_opts' contents, because whitespace between tokens has been lost.
  let
    opt = (`elem` opts)
    pre = ["#include \"prelude.hpp\""] ++ if opt Terse then ["#include \"terse.hpp\""] else []
    also_run = opt Help || opt Version || not (opt CompileOnly)
  code <- unlines . (pre ++) . case () of
    ()| opt Help -> return [wrapPrint "help"]
    ()| opt Version -> return [wrapPrint $ "\"g++ (GCC) \" << __VERSION__"]
    ()| '{':_ <- rest -> do
      Cxx.Code (Cxx.Curlies c : b) <- parseOrFail (Cxx.code << eof) rest
      return [show (Cxx.Code b), wrapStmts (show c)]
    ()| '<':'<':x <- rest -> do
      (a, b) <- splitSemicolon . parseOrFail (Cxx.code << eof) x
      return [show b, wrapPrint (show a)]
    ()| otherwise -> return [rest]
  return (code, also_run)

data JailConfig = JailConfig { user, group :: String, path :: FilePath } deriving Read

jail :: IO ()
jail = do
  cfg <- readTypedFile "jail-config"
  gid <- groupID . getGroupEntryForName (group cfg)
  uid <- userID . getUserEntryForName (user cfg)
  chroot $ path cfg
  System.Directory.setCurrentDirectory "/"
  setGroupID gid
  setUserID uid

prepare_evaluator :: IO (String -> IO String)
prepare_evaluator = do
  EvalCxx.cap_fds
  gxx : flags <- (words .) $ (++) . readFileNow "compile-config" <*> readFileNow "link-config"
  jail
  return $ either return ((filter (isPrint .||. (== '\n')) . show .) . uncurry (EvalCxx.evaluate gxx flags)) . parse_request
    -- filtering using isPrint works properly because (1) EvalCxx.evaluate returns a proper Unicode String, not a load of bytes; and (2) to print filtered strings we will use System.IO.UTF8's hPutStrLn which properly UTF-8-encodes the filtered String.
    -- Possible problem: terminals which have not been (properly) UTF-8 configured might interpret bytes that are part of UTF-8 encoded characters as control characters.
