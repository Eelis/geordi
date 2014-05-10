{-# LANGUAGE UnicodeSyntax, CPP, ViewPatterns, RecordWildCards, PatternGuards #-}

module RequestEval (evaluator) where

import qualified Data.Set as Set
import qualified EvalCxx
import qualified Editing.Parse
import qualified Editing.Diff
import qualified Editing.Execute
import qualified Editing.Basics
import qualified Editing.Commands
import qualified Editing.EditsPreparation
import qualified Parsers as P
import qualified Cxx.Parse
import qualified Cxx.Operations
import qualified Cxx.Show
import qualified Data.List as List
import qualified Data.List.NonEmpty as NeList
import qualified Gcc

import Control.Monad.Error (throwError)
import Control.Monad (join, when)
import Control.Arrow (first)
import Cxx.Show (Highlighter)
import EvalCxx (WithEvaluation, noEvaluation)
import Data.Char (isPrint, isSpace)
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.Pointed (Pointed(..))
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.Set (Set)
import Editing.Commands (FinalCommand(..))
import Parsers ((<|>), eof, option, spaces, getInput, kwd, kwds, Parser, run_parser, ParseResult(..), parseOrFail, commit, peek, parseSuccess)
import Util ((.), (‥), (<<), (.∨.), commas_and, capitalize, length_ge, replace, replaceWithMany, show_long_opt, strip, convert, maybeLast, orElse, E, NeList, propagateE, splitBy)
import Request (Context(..), EvalOpt(..), Response(..), HistoryModification(..), EditableRequest(..), EditableRequestKind(..), EphemeralOpt(..), popContext)
import Data.SetOps
import Prelude hiding ((.))
import Prelude.Unicode hiding ((∈), (∉))

show_EditableRequest :: Highlighter → EditableRequest → String
show_EditableRequest h (EditableRequest (Evaluate f) s) | Set.null f = Cxx.Parse.highlight h s
show_EditableRequest _ (EditableRequest k s) = show k ++ (if null s then "" else ' ' : s)

instance Show EditableRequest where
  show = show_EditableRequest Cxx.Show.noHighlighting

no_break_space :: Char
no_break_space = '\x00A0'

diff :: EditableRequest → EditableRequest → String
diff (EditableRequest MakeType y) (EditableRequest MakeType x) = pretty $ show . Editing.Diff.diff x y
diff (EditableRequest Precedence y) (EditableRequest Precedence x) = pretty $ show . Editing.Diff.diff x y
diff (EditableRequest (Evaluate flags) y) (EditableRequest (Evaluate flags') x) =
  pretty $ f "removed" flags' flags ++ f "added" flags flags' ++ show . Editing.Diff.diff x y
    where f n fl fl' = maybe [] (\l → [n ++ " " ++ concat (List.intersperse " and " $ map show_long_opt $ toList l)]) (nonEmpty $ Set.elems $ (Set.\\) fl fl')
diff _ _ = "Requests differ in kind."

pretty :: [String] → String -- Todo: This is awkward.
pretty [] = "Requests are identical."
pretty l = capitalize (commas_and l) ++ "."

ellipsis_options :: [(String, Bool)] → NeList [String]
ellipsis_options [] = return []
ellipsis_options ((y, _) : ys) = work ((y, False) : ys)
  where
    dummy = " → …"
    work [] = return []
    work [(x, _)] = return [x]
    work ((x, False) : xs) = fmap (x:) (work xs)
    work ((x, True) : xs) = work xs >>= \o → if dummy ∈ o
        then (return $ if head o == dummy then o else dummy : o)
        else (dummy : o) :| [x : o]

nicer_namedPathTo :: [String] → String
nicer_namedPathTo l = drop 3 $ concat $ maybeLast (takeWhile ((≤ 140) . length . concat) $ toList n) `orElse` NeList.head n
  where n = ellipsis_options $ map (\s → (" → " ++ s, "expr" `List.isSuffixOf` s)) l
    -- Todo: Also don't abbreviate when there's enough space.

-- The following aliases for 'return' and 'id' serve only to make subsequent monad-heavy code more readable.

noErrors :: a → E a
noErrors = return

inE :: E a → E a
inE = id

continueParsing :: Parser t a → Parser t a
continueParsing = id

tagError :: Pointed p ⇒ E (p String) → p String
tagError = either (point . ("error: " ++)) id

optParser :: Parser Char (E (Set EvalOpt, [EphemeralOpt]))
optParser = first Set.fromList ‥ partitionEithers ‥ option (return []) P.optParser

makeEvalCxxRequest :: Set EvalOpt → Cxx.Parse.Code → EvalCxx.Request
makeEvalCxxRequest opts sc = EvalCxx.Request{..}
 where
  code :: String
  code = show (Cxx.Operations.expand $ Cxx.Operations.shortcut_syntaxes $ Cxx.Operations.line_breaks sc)
  pre = "#include \"prelude.hpp\"\n"
    ++ (if NoUsingStd ∈ opts || PreprocessOnly ∈ opts then "" else "using namespace std;\n")
  units :: [String]
  units = (pre ++) . unlines . splitBy ((== "#") . strip) (lines code)
  stageOfInterest
    | CompileOnly ∈ opts = Gcc.Compile
    | PreprocessOnly ∈ opts = Gcc.Preprocess
    | otherwise = Gcc.Run
  no_warn = NoWarn ∈ opts

execEditableRequest :: EditableRequest → E (WithEvaluation String)
execEditableRequest (EditableRequest kind (dropWhile isSpace → body)) = case kind of
  MakeType → noEvaluation . Cxx.Show.show_simple . Cxx.Parse.makeType body
  Precedence → noEvaluation . Cxx.Parse.precedence body
  Evaluate opts → evaluate . makeEvalCxxRequest opts . parseOrFail (Cxx.Parse.code << eof) body "request"

respond_and_remember :: EditableRequest → WithEvaluation Response
respond_and_remember er = Response (Just $ AddLast er) . tagError (execEditableRequest er)

execFinalCommand :: Context → FinalCommand → E (WithEvaluation String)
execFinalCommand context@Context{..} fc = case fc of
  Show Nothing → noEvaluation . show_EditableRequest highlighter . fst . popContext context
  Show (Just substrs) → do
    c ← evalRequestBody
    l ← (\(Editing.EditsPreparation.Found _ x) → x) ‥ toList . Editing.EditsPreparation.findInStr c (flip (,) return . Cxx.Parse.parseRequest c) substrs
    return $ noEvaluation $ commas_and (map (\x → '`' : strip (Editing.Basics.selectRange (convert $ Editing.Commands.replace_range x) c) ++ "`") l) ++ "."
  Identify substrs → do
    c ← evalRequestBody
    tree ← Cxx.Parse.parseRequest c
    l ← (\(Editing.EditsPreparation.Found _ x) → x) ‥ toList . Editing.EditsPreparation.findInStr c (Right (tree, return)) substrs
    return $ noEvaluation $ concat $ List.intersperse ", " $ map (nicer_namedPathTo . Cxx.Operations.namedPathTo tree . convert . Editing.Commands.replace_range) l
  Parse → evalRequestBody >>= Cxx.Parse.parseRequest >> return (noEvaluation "Looks fine to me.")
  Diff → do (x, context') ← popContext context; noEvaluation . diff x . fst . popContext context'
  Run → fst . popContext context >>= execEditableRequest
 where
  evalRequestBody :: E String
  evalRequestBody = do
    EditableRequest kind body ← fst . popContext context
    case kind of Evaluate _ → return body; _ → throwError "Last (editable) request was not an evaluation request."

execEditCommand :: Context → ([Editing.Commands.Command], Maybe FinalCommand) → E (EditableRequest, WithEvaluation String)
execEditCommand context@Context{..} (cs, mfcmd) = do
  edited ← fst . popContext context >>= Editing.Execute.execute cs
  when (length_ge 1000 (editable_body edited)) $ throwError "Request would become too large."
  (,) edited . case mfcmd of
    Just fcmd → execFinalCommand context{previousRequests = edited : previousRequests} fcmd
    Nothing → execEditableRequest edited

cout :: String → Parser Char (E (WithEvaluation Response))
cout s = parseSuccess $ Response Nothing ‥ execEditableRequest (EditableRequest (Evaluate (∅)) ("<< " ++ s))

p :: EvalCxx.CompileConfig → Context → Parser Char (E (WithEvaluation Response))
p compile_cfg context@Context{..} = (spaces >>) $ do
    (Response Nothing .) ‥ (>>=  execFinalCommand context) . (Editing.Parse.finalCommandP << commit eof)
  <|> do
    kwds ["undo", "revert"]; commit $ propagateE (snd . popContext context) $ \context' → do
    kwd "and"
    (Response (Just DropLast) .) ‥ (>>= execFinalCommand context') . (Editing.Parse.finalCommandP << commit eof)
     <|> (\(edited, output) → Response (Just $ ReplaceLast edited) . output) ‥ (>>= execEditCommand context') . (Editing.Parse.commandsP << commit eof)
  <|> do
    kwds ["--precedence", "precedence"]
    noErrors . respond_and_remember . EditableRequest Precedence . getInput
  <|> do
    kwds ["--make-type", "make type"]
    noErrors . respond_and_remember . EditableRequest MakeType . getInput
  <|> do kwds ["uname"]; cout "geordi::uname()"
  <|> do
    kwd "--show-compile-flags"
    parseSuccess $ noErrors $ noEvaluation $ Response Nothing $ unwords $ EvalCxx.compileFlags compile_cfg
  <|> (\(edited, output) → Response (Just $ AddLast edited) . output) ‥ (>>= execEditCommand context) . (Editing.Parse.commandsP << commit eof)
  <|> do
    mopts ← optParser; spaces
    propagateE mopts $ \(evalopts, eph_opts) → continueParsing $ do
    s ← peek
    case () of { ()
      | Help ∈ eph_opts || s == "help" → cout "help"
      | Version ∈ eph_opts || s == "version" → cout "\"g++ (GCC) \" << __VERSION__"
      | Resume ∈ eph_opts → flip fmap (Cxx.Parse.code << eof) $ \code → case previousRequests of
        [] → throwError "There is no previous resumable request."
        EditableRequest (Evaluate oldopts) (dropWhile isSpace → oldcodeblob) : _ → do
          case run_parser (Cxx.Parse.code << eof) oldcodeblob of
            ParseSuccess oldcode _ _ _ → noErrors $ respond_and_remember $
              EditableRequest (Evaluate $ evalopts ∪ oldopts) $ show $ Cxx.Operations.blob $ Cxx.Operations.resume (Cxx.Operations.shortcut_syntaxes oldcode) (Cxx.Operations.shortcut_syntaxes code)
            ParseFailure{} → throwError "Previous request too malformed to resume."
        _ → throwError "Last (editable) request was not resumable."
      | otherwise → parseSuccess . noErrors . respond_and_remember =<< EditableRequest (Evaluate evalopts) . getInput }

evaluate :: EvalCxx.Request → WithEvaluation String
evaluate r = filter (isPrint .∨. (== '\n')) . replaceWithMany '\a' "*BEEP*" . show . EvalCxx.withEvaluation r

evaluator :: IO (String → Context → IO Response)
evaluator = do
  (ev, cfg) ← EvalCxx.evaluator
  return $ \r context → either (return . Response Nothing . ("error: " ++)) ev $
    join (parseOrFail (p cfg context) (replace no_break_space ' ' r) "request")
