module RequestEval (evaluator) where

import qualified Data.Set as Set
import qualified EvalCxx
import qualified Editing.Parse
import qualified Editing.Diff
import qualified Editing.Execute
import qualified Editing.Basics
import qualified Editing.EditsPreparation
import qualified Cxx.Parse
import qualified Cxx.Operations
import qualified Cxx.Show
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.NonEmptyList as NeList

import Control.Monad.Error ()
import Control.Monad (join)
import Data.Char (isPrint, isSpace)
import Data.Either (lefts)
import Editing.Basics (FinalCommand(..))
import Parsers ((<|>), eof, optParser, option, spaces, getInput, kwd, kwds, Parser, run_parser, ParseResult(..), optional, parseOrFail, commit)
import Util ((.), (<<), (.||.), commas_and, capitalize, length_ge, replace, show_long_opt, strip, convert)
import Request (Context(..), EvalOpt(..), Response(..), HistoryModification(..), EditableRequest(..), EditableRequestKind(..), EphemeralOpt(..))
import Prelude hiding (catch, (.))

show_EditableRequest :: Cxx.Show.Highlighter -> EditableRequest -> String
show_EditableRequest h (EditableRequest (Evaluate f) s) | Set.null f = Cxx.Parse.highlight h s
show_EditableRequest _ (EditableRequest k s) = show k ++ (if null s then "" else ' ' : s)

instance Show EditableRequest where
  show = show_EditableRequest Cxx.Show.noHighlighting

no_break_space :: Char
no_break_space = '\x00A0'

diff :: EditableRequest -> EditableRequest -> String
diff (EditableRequest MakeType y) (EditableRequest MakeType x) = pretty $ show . Editing.Diff.diff x y
diff (EditableRequest Precedence y) (EditableRequest Precedence x) = pretty $ show . Editing.Diff.diff x y
diff (EditableRequest (Evaluate flags) y) (EditableRequest (Evaluate flags') x) =
  pretty $ f "removed" flags' flags ++ f "added" flags flags' ++ show . Editing.Diff.diff x y
    where f n fl fl' = maybe [] (\l -> [n ++ " " ++ concat (List.intersperse " and " $ map show_long_opt $ NeList.to_plain l)]) (NeList.from_plain $ Set.elems $ (Set.\\) fl fl')
diff _ _ = "Requests differ in kind."

pretty :: [String] -> String -- Todo: This is awkward.
pretty [] = "Requests are identical."
pretty l = capitalize (commas_and l) ++ "."

evaluator :: Cxx.Show.Highlighter -> IO (String -> Context -> IO Response)
evaluator h = do
  (ev, compile_cfg) <- EvalCxx.evaluator
  let
    evf :: EvalCxx.Request -> IO String
    evf r = filter (isPrint .||. (== '\n')) . show . ev r
    -- Filtering using isPrint works properly because (1) the EvalCxx evaluator returns proper Unicode Strings, not mere byte blobs; and (2) to print filtered strings we will use System.IO.UTF8's hPutStrLn which properly UTF-8-encodes the filtered String.
    -- Possible problem: terminals which have not been (properly) UTF-8 configured might interpret bytes that are part of UTF-8 encoded characters as control characters.
    prel = "#include \"prelude.hpp\"\n"

    respond :: EditableRequest -> Either String (IO String)
    respond (EditableRequest MakeType d) = return . Cxx.Show.show_simple . Cxx.Parse.makeType d
    respond (EditableRequest Precedence t) = return . Cxx.Parse.precedence t
    respond (EditableRequest (Evaluate opts) code) = do
      sc <- parseOrFail (Cxx.Parse.code << eof) (dropWhile isSpace code) "request"
      return $ evf $ EvalCxx.Request (prel ++ (if Set.member Terse opts then "#include \"terse.hpp\"\n" else "") ++ show (Cxx.Operations.expand $ Cxx.Operations.shortcut_syntaxes $ Cxx.Operations.line_breaks sc)) (not $ Set.member CompileOnly opts) (Set.member NoWarn opts)

    respond_and_remember :: EditableRequest -> Either String (IO Response)
    respond_and_remember er = (Response (Just $ AddLast er) .) . respond er

  return $ \r (Context prevs) -> do
  let
    help_response = Response Nothing . evf (EvalCxx.Request (prel ++ "int main() { cout << help; }") True False)
    version_response = Response Nothing . evf (EvalCxx.Request (prel ++ "int main() { cout << \"g++ (GCC) \" << __VERSION__; }") True False)
    uname_response = Response Nothing . evf (EvalCxx.Request (prel ++ "int main() { cout << geordi::uname(); }") True False)

    editcmd :: Maybe EditableRequest -> Parser Char (Either String (EditableRequest, IO String))
    editcmd mprev = do
      oe <- Editing.Parse.commandsP; commit $ (eof >>) $ return $ do
      prev <- maybe (fail "There is no prior request.") return mprev
      (cs, mfcmd) <- oe
      edited <- Editing.Execute.execute cs prev
      if length_ge 1000 (editable_body edited) then fail "Request would become too large." else do
      (,) edited . case mfcmd of
        Just fcmd -> return . final_cmd fcmd (Just edited)
        Nothing -> respond edited

    final_cmd :: FinalCommand -> Maybe EditableRequest -> Either String String
    final_cmd _ Nothing = fail "There is no previous request."
    final_cmd (Show Nothing) (Just er) = return $ show_EditableRequest h er
    final_cmd (Show (Just substrs)) (Just (EditableRequest (Evaluate _) c)) = do
      l <- NeList.to_plain . Editing.EditsPreparation.findInStr c substrs
      return $ commas_and (map (\x -> '`' : strip (Editing.Basics.selectRange (convert x) c) ++ "`") l) ++ "."
    final_cmd (Show (Just _)) (Just _) = fail "Last (editable) request was not an evaluation request."
    final_cmd (Identify substrs) (Just (EditableRequest (Evaluate _) c)) = do
      tree <- Cxx.Parse.parseRequest c
      l <- NeList.to_plain . Editing.EditsPreparation.findInStr c substrs
      return $ concat $ List.intersperse ", " $ map (concat . List.intersperse " -> " . Cxx.Operations.namedPathTo tree . convert) l
    final_cmd Parse (Just (EditableRequest (Evaluate _) c)) =
      Cxx.Parse.parseRequest c >> return "Looks fine to me."
    final_cmd _ (Just _) = fail "Last (editable) request was not an evaluation request."

    p :: Parser Char (Either String (IO Response))
    p = (spaces >>) $ do
        fcmd_or_error <- Editing.Parse.finalCommandP; commit $ (eof >>) $ return $ do
        fcmd <- fcmd_or_error
        return . Response Nothing . final_cmd fcmd (Maybe.listToMaybe prevs)
      <|> do
        kwds ["undo", "revert"]; commit $ case prevs of
          _:prev:_ -> kwd "and" >> (do
              fcmd_or_error <- Editing.Parse.finalCommandP; commit $ (eof >>) $ return $ do
              fcmd <- fcmd_or_error
              return . Response (Just DropLast) . final_cmd fcmd (Just prev)
            <|> do
              y <- editcmd $ Just prev; return $ do
              (edited, output) <- y
              return $ Response (Just $ ReplaceLast edited) . output)
          _ -> return $ fail "There is no prior request."
      <|> do
        kwds ["--precedence", "precedence"]
        return . respond_and_remember . EditableRequest Precedence =<< getInput
      <|> do
        kwds ["--make-type", "make type"]
        return . respond_and_remember . EditableRequest MakeType =<< getInput
      <|> do kwds ["help"]; return $ return help_response
      <|> do kwds ["version"]; return $ return version_response
      <|> do kwds ["uname"]; return $ return uname_response
      <|> do
        kwd "--show-compile-flags"
        return $ return $ return $ Response Nothing $ unwords $ EvalCxx.compileFlags compile_cfg
          -- Here we can nicely summarize the three monad levels we're in. The first return indicates a successfully parsed command. The second indicates there were no errors executing the command. The third indicates a pure result in IO.
      <|> do
        kwds ["diff", "diffs", "differences", "changes"]; commit $ (eof >>) $ return $ case prevs of
          x : y : _ -> return $ return $ Response Nothing $ diff x y
          _ -> fail "I have not yet seen two comparable requests."
      <|> do
        optional (kwd "try"); kwd "again"; commit $ (eof >>) $ return $ case prevs of
          [] -> fail "There is no repeatable request."
          x : _ -> (Response Nothing .) . respond x
      <|> do
        y <- editcmd $ Maybe.listToMaybe prevs; return $ do
        (edited, output) <- y
        return $ Response (Just $ AddLast edited) . output
      <|> do
        mopts <- option (return []) optParser; spaces
        case mopts of
          Left e -> return $ fail e
          Right opts -> do
            let evalopts = Set.fromList $ lefts opts
            case () of { ()
              | Right Help `elem` opts -> return $ return help_response
              | Right Version `elem` opts -> return $ return version_response
              | Right Resume `elem` opts -> case prevs of
                [] -> return $ fail "There is no previous resumable request."
                EditableRequest (Evaluate oldopts) oldcodeblob : _ -> case run_parser (Cxx.Parse.code << eof) (dropWhile isSpace oldcodeblob) of
                  ParseSuccess oldcode _ _ _ -> do
                    code <- Cxx.Parse.code; eof; return $ respond_and_remember $ EditableRequest (Evaluate $ Set.union evalopts oldopts) $ show $ Cxx.Operations.blob $ Cxx.Operations.resume (Cxx.Operations.shortcut_syntaxes oldcode) (Cxx.Operations.shortcut_syntaxes code)
                  ParseFailure _ _ _ -> return $ fail "Previous request too malformed to resume."
                _ -> return $ fail "Last (editable) request was not resumable."
              | otherwise -> return . respond_and_remember =<< EditableRequest (Evaluate evalopts) . getInput }

  either (return . Response Nothing . ("error: " ++)) id $
    join (parseOrFail p (replace no_break_space ' ' r) "request")
