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
import qualified Data.NonEmptyList as NeList
import Data.NonEmptyList (NeList(..))

import Control.Monad.Error ()
import Control.Monad (join)
import Data.Char (isPrint, isSpace)
import Data.Either (lefts)
import Editing.Basics (FinalCommand(..))
import Parsers ((<|>), eof, optParser, option, spaces, getInput, kwd, kwds, Parser, run_parser, ParseResult(..), optional, parseOrFail, commit)
import Util ((.), (<<), (.||.), commas_and, capitalize, length_ge, replace, show_long_opt, strip, convert, maybeLast, orElse, E)
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

ellipsis_options :: [(String, Bool)] -> NeList [String]
ellipsis_options [] = NeList.one []
ellipsis_options ((y, _) : ys) = work ((y, False) : ys)
  where
    dummy = " -> ..."
    work [] = NeList.one []
    work [(x, _)] = NeList.one [x]
    work ((x, False) : xs) = fmap (x:) (work xs)
    work ((x, True) : xs) =
      NeList.concatMap (\o -> if dummy `elem` o
        then (NeList.one $ if head o == dummy then o else dummy : o)
        else NeList (dummy : o) [x : o]) (work xs)

nicer_namedPathTo :: [String] -> String
nicer_namedPathTo l = drop 4 $ concat $ maybeLast (takeWhile ((<= 140) . length . concat) $ NeList.to_plain n) `orElse` h
  where n@(NeList h _) = ellipsis_options $ map (\s -> (" -> " ++ s, "expr" `List.isSuffixOf` s)) l
    -- Todo: Also don't abbreviate when there's enough space.

evaluator :: Cxx.Show.Highlighter -> IO (String -> Context -> IO Response)
evaluator h = do
  (ev, compile_cfg) <- EvalCxx.evaluator
  let
    evf :: EvalCxx.Request -> IO String
    evf r = filter (isPrint .||. (== '\n')) . show . ev r
    -- Filtering using isPrint works properly because (1) the EvalCxx evaluator returns proper Unicode Strings, not mere byte blobs; and (2) to print filtered strings we will use System.IO.UTF8's hPutStrLn which properly UTF-8-encodes the filtered String.
    -- Possible problem: terminals which have not been (properly) UTF-8 configured might interpret bytes that are part of UTF-8 encoded characters as control characters.
    prel = "#include \"prelude.hpp\"\n"

    respond :: EditableRequest -> E (IO String)
    respond (EditableRequest MakeType d) = return . Cxx.Show.show_simple . Cxx.Parse.makeType d
    respond (EditableRequest Precedence t) = return . Cxx.Parse.precedence t
    respond (EditableRequest (Evaluate opts) code) = do
      sc <- parseOrFail (Cxx.Parse.code << eof) (dropWhile isSpace code) "request"
      return $ evf $ EvalCxx.Request (prel ++ (if Set.member Terse opts then "#include \"terse.hpp\"\n" else "") ++ show (Cxx.Operations.expand $ Cxx.Operations.shortcut_syntaxes $ Cxx.Operations.line_breaks sc)) (not $ Set.member CompileOnly opts) (Set.member NoWarn opts)

    respond_and_remember :: EditableRequest -> E (IO Response)
    respond_and_remember er = (Response (Just $ AddLast er) .) . respond er

    help_response = Response Nothing . evf (EvalCxx.Request (prel ++ "int main() { cout << help; }") True False)
    version_response = Response Nothing . evf (EvalCxx.Request (prel ++ "int main() { cout << \"g++ (GCC) \" << __VERSION__; }") True False)
    uname_response = Response Nothing . evf (EvalCxx.Request (prel ++ "int main() { cout << geordi::uname(); }") True False)

    final_cmd :: FinalCommand -> [EditableRequest] -> E String
    final_cmd _ [] = fail "There is no previous request."
    final_cmd (Show Nothing) (er:_) = return $ show_EditableRequest h er
    final_cmd (Show (Just substrs)) (EditableRequest (Evaluate _) c : _) = do
      l <- ((\(Editing.EditsPreparation.Found _ x) -> x) .) . NeList.to_plain . Editing.EditsPreparation.findInStr c (flip (,) return . Cxx.Parse.parseRequest c) substrs
      return $ commas_and (map (\x -> '`' : strip (Editing.Basics.selectRange (convert $ Editing.Basics.replace_range x) c) ++ "`") l) ++ "."
    final_cmd (Show (Just _)) (_:_) = fail "Last (editable) request was not an evaluation request."
    final_cmd (Identify substrs) (EditableRequest (Evaluate _) c : _) = do
      tree <- Cxx.Parse.parseRequest c
      l <- ((\(Editing.EditsPreparation.Found _ x) -> x) .) . NeList.to_plain . Editing.EditsPreparation.findInStr c (Right (tree, return)) substrs
      return $ concat $ List.intersperse ", " $ map (nicer_namedPathTo . Cxx.Operations.namedPathTo tree . convert . Editing.Basics.replace_range) l
    final_cmd Parse (EditableRequest (Evaluate _) c : _) =
      Cxx.Parse.parseRequest c >> return "Looks fine to me."
    final_cmd Diff (x : y : _) = return $ diff x y
    final_cmd Diff [_] = fail "History exhausted."
    final_cmd _ (_:_) = fail "Last (editable) request was not an evaluation request."

    editcmd :: [EditableRequest] -> Parser Char (E (EditableRequest, IO String))
    editcmd prevs = do
      oe <- Editing.Parse.commandsP; commit $ (eof >>) $ return $ do
      case prevs of
        [] -> fail "There is no prior request."
        prev : _ -> do
          (cs, mfcmd) <- oe
          edited <- Editing.Execute.execute cs prev
          if length_ge 1000 (editable_body edited) then fail "Request would become too large." else do
          (,) edited . case mfcmd of
            Just fcmd -> return . final_cmd fcmd (edited : prevs)
            Nothing -> case respond edited of
              Left e -> Right $ return $ "error: " ++ e
              Right x -> Right x

  return $ \r (Context prevs) -> do
  let
    p :: Parser Char (E (IO Response))
    p = (spaces >>) $ do
        fcmd_or_error <- Editing.Parse.finalCommandP; commit $ (eof >>) $ return $ do
        fcmd <- fcmd_or_error
        return . Response Nothing . final_cmd fcmd prevs
      <|> do
        kwds ["undo", "revert"]; commit $ case prevs of
          _ : old -> kwd "and" >> (do
              fcmd_or_error <- Editing.Parse.finalCommandP; commit $ (eof >>) $ return $ do
              fcmd <- fcmd_or_error
              return . Response (Just DropLast) . final_cmd fcmd old
            <|> do
              y <- editcmd old; return $ do
              (edited, output) <- y
              return $ Response (Just $ ReplaceLast edited) . output)
          _ -> return $ fail "History exhausted."
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
        optional (kwd "try"); kwd "again"; commit $ (eof >>) $ return $ case prevs of
          [] -> fail "There is no repeatable request."
          x : _ -> (Response Nothing .) . respond x
      <|> do
        y <- editcmd prevs; return $ do
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
