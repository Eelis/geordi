{-# LANGUAGE
  UnicodeSyntax,
  CPP,
  ViewPatterns,
  RecordWildCards,
  PatternGuards,
  ScopedTypeVariables,
  FlexibleInstances,
  TupleSections #-}

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

import Control.Monad.Error (Error(..), throwError)
import Control.Monad (join, when)
import Control.Arrow (first, second)
import Cxx.Basics (Code)
import Cxx.Show (Highlighter)
import EvalCxx (WithEvaluation, noEvaluation, EvaluationResult(..), Line, Column)
import Data.Char (isPrint, isSpace, showLitChar)
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.Pointed (Pointed(..))
import Data.List.NonEmpty (NonEmpty((:|)), nonEmpty)
import Data.Set (Set)
import Editing.Basics (TextEdit(..), Range(..), Pos(..))
import Editing.Commands (FinalCommand(..))
import Parsers ((<|>), eof, option, spaces, getInput, kwd, kwds, Parser, run_parser, ParseResult(..), parseOrFail, commit, peek, parseSuccess)
import Util ((.), (‥), (<<), commas_and, capitalize, length_ge, replace, show_long_opt, strip, convert, maybeLast, orElse, E, NeList, propagateE, splitBy, mapHead)
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

tagError :: (Pointed p, Error a) ⇒ E (p a) → p a
tagError = either (point . strMsg . ("error: " ++)) id

instance Error (String, Maybe (TextEdit Char)) where
  noMsg = ("", Nothing)
  strMsg = noFixit . strMsg

optParser :: Parser Char (E (Set EvalOpt, [EphemeralOpt]))
optParser = first Set.fromList ‥ partitionEithers ‥ option (return []) P.optParser

preamble :: Set EvalOpt → [String]
preamble opts =
  [ "#if !defined(GEORDI_PRELUDE) && !defined(__clang__)"
  , "#define GEORDI_PRELUDE"
  , "#include \"prelude.hpp\""
  , "#endif" ] ++
  ["using namespace std;" | NoUsingStd ∉ opts, PreprocessOnly ∉ opts]

type LocationMap = (Int {- TU -}, (Line, Column)) → Int
  -- To map locations in generated TUs back to positions in the request body.

generated :: String → TString
generated = map (, 0)

tlines :: TString → [TString]
tlines (break ((== '\n') . fst) → (x, y)) =
  x : case y of
    [] -> []
    _:z -> tlines z

type TString = [(Char, Int)]

translationUnits :: Set EvalOpt → Code → [[TString]]
translationUnits opts requestChunks =
    mapHead (++ maybe [] tlines main)
     $ (map generated (preamble opts) ++) . splitBy null (tlines code)
  where
    (main, code) = Cxx.Operations.expand $ Cxx.Operations.line_breaks requestChunks

fix_as_edit :: LocationMap → (EvalCxx.Fix → TextEdit Char)
fix_as_edit f (EvalCxx.Fix file begin e repl) =
    RangeReplaceEdit (Range (Pos begin') $ f (file, e) - begin') repl
  where begin' = f (file, begin)

locationMap :: [[TString]] → LocationMap
locationMap us (u, (l, c))
  | u < length us
  , the_u <- us !! u
  , l <= length the_u
  , the_l@(_:_) <- the_u !! (l - 1) =
      if c <= length the_l
        then snd (the_l !! (c - 1))
        else snd (last the_l) + 1 -- needed for fix-its that append at the end of lines
  | otherwise = 0

un_t :: TString → String
un_t = map fst

execEditableRequest :: Bool → EditableRequest → E (WithEvaluation (String, Maybe (TextEdit Char)))
execEditableRequest clangByDefault (EditableRequest kind (dropWhile isSpace → body)) = case kind of
  MakeType → noEvaluation . noFixit . Cxx.Show.show_simple . Cxx.Parse.makeType body
  Precedence → noEvaluation . noFixit . Cxx.Parse.precedence body
  Evaluate opts → do
    chunks :: Code ← parseOrFail (Cxx.Parse.code << eof) (dropWhile isSpace body) "request"
    let
      tunits = translationUnits opts chunks
      stageOfInterest
        | CompileOnly ∈ opts = Gcc.Compile
        | PreprocessOnly ∈ opts = Gcc.Preprocess
        | otherwise = Gcc.Run
      no_warn = NoWarn ∈ opts
      clang
        | clangByDefault = Gcc ∉ opts
        | otherwise = Clang ∈ opts
      units :: [String]
      units = map (unlines . map un_t) tunits
    return $ second (fix_as_edit (locationMap tunits) .) . evaluate EvalCxx.Request{..}

respond_and_remember :: Bool → EditableRequest → WithEvaluation Response
respond_and_remember clangByDefault er = fmap f (tagError (execEditableRequest clangByDefault er))
  where f (ou, edit) = Response (Just $ AddLast (er, edit)) ou

noFixit :: String -> (String, Maybe (TextEdit Char))
noFixit = flip (,) Nothing

execFinalCommand :: Context → FinalCommand → E (WithEvaluation (String, Maybe (TextEdit Char)))
execFinalCommand context@Context{..} fc = case fc of
  Show Nothing → noEvaluation . noFixit . show_EditableRequest highlighter . fst . fst . popContext context
  Show (Just substrs) → do
    c ← evalRequestBody
    l ← (\(Editing.EditsPreparation.Found _ x) → x) ‥ toList . Editing.EditsPreparation.findInStr c Nothing (flip (,) return . Cxx.Parse.parseRequest c) substrs
    return $ noEvaluation $ noFixit $ commas_and (map (\x → '`' : strip (Editing.Basics.selectRange (convert $ Editing.Commands.replace_range x) c) ++ "`") l) ++ "."
  Identify substrs → do
    c ← evalRequestBody
    tree ← Cxx.Parse.parseRequest c
    l ← (\(Editing.EditsPreparation.Found _ x) → x) ‥ toList . Editing.EditsPreparation.findInStr c Nothing (Right (tree, return)) substrs
    return $ noEvaluation $ noFixit $ concat $ List.intersperse ", " $ map (nicer_namedPathTo . Cxx.Operations.namedPathTo tree . convert . Editing.Commands.replace_range) l
  Parse → evalRequestBody >>= Cxx.Parse.parseRequest >> return (noEvaluation $ noFixit "Looks fine to me.")
  Diff → do ((x, _), context') ← popContext context; noEvaluation . noFixit . diff x . fst . fst . popContext context'
  Run → fst . fst . popContext context >>= execEditableRequest clangByDefault
 where
  evalRequestBody :: E String
  evalRequestBody = do
    EditableRequest kind body ← fst . fst . popContext context
    case kind of Evaluate _ → return body; _ → throwError "Last (editable) request was not an evaluation request."

execEditCommand :: Context → ([Editing.Commands.Command], Maybe FinalCommand)
  → E (EditableRequest, WithEvaluation (String, Maybe (TextEdit Char)))
execEditCommand context@Context{..} (cs, mfcmd) = do
  (r, maybeFixit) <- fst . popContext context
  edited ← Editing.Execute.execute maybeFixit cs r
  when (length_ge 1000 (editable_body edited)) $ throwError "Request would become too large."
  (,) edited . case mfcmd of
    Just fcmd → execFinalCommand context{previousRequests = (edited, Nothing) : previousRequests} fcmd
    Nothing → execEditableRequest clangByDefault edited

cout :: Context → Set EvalOpt → String → Parser Char (E (WithEvaluation Response))
cout Context{..} opts s = parseSuccess $
  Response Nothing ‥ fst ‥ execEditableRequest clangByDefault (EditableRequest (Evaluate opts) ("<< " ++ s))

p :: EvalCxx.CompileConfig → Context → Parser Char (E (WithEvaluation Response))
p compile_cfg context@Context{..} = (spaces >>) $ do
    (Response Nothing .) ‥ (>>= (fst ‥) . execFinalCommand context) . (Editing.Parse.finalCommandP << commit eof)
  <|> do
   kwds ["undo", "revert"]; commit $ propagateE (snd . popContext context) $ \context' → do
    kwd "and"
    (Response (Just DropLast) .) ‥ (>>= (fst ‥) . execFinalCommand context') . (Editing.Parse.finalCommandP << commit eof)
     <|> (\(edited, we) -> (\(output, _) -> Response (Just $ ReplaceLast (edited, Nothing)) output) . we) ‥ (>>= execEditCommand context') . (Editing.Parse.commandsP << commit eof)
  <|> do
    kwds ["--precedence", "precedence"]
    noErrors . respond_and_remember False . EditableRequest Precedence . getInput
  <|> do
    kwds ["--make-type", "make type"]
    noErrors . respond_and_remember False . EditableRequest MakeType . getInput
  <|> do kwds ["uname"]; cout context (∅) "geordi::uname()"
  <|> do
    kwd "--show-compile-flags"
    parseSuccess $ noErrors $ noEvaluation $ Response Nothing $ unwords $ EvalCxx.gccCompileFlags compile_cfg
  <|>
    (\(edited, we) → (\(s, e) → Response (Just $ AddLast (edited, e)) s) . we) ‥ (>>= execEditCommand context) . (Editing.Parse.commandsP << commit eof)
  <|> do
   mopts ← optParser; spaces
   propagateE mopts $ \(evalopts, eph_opts) → continueParsing $ do
    s ← peek
    case () of { ()
      | Help ∈ eph_opts || s == "help" → cout context (∅) "help"
      | Version ∈ eph_opts || s == "version" → cout context evalopts "geordi::compiler_description"
      | Resume ∈ eph_opts → flip fmap (Cxx.Parse.code << eof) $ \code → case previousRequests of
        [] → throwError "There is no previous resumable request."
        (EditableRequest (Evaluate oldopts) (dropWhile isSpace → oldcodeblob), _) : _ → do
          case run_parser (Cxx.Parse.code << eof) oldcodeblob of
            ParseSuccess oldcode _ _ _ → noErrors $ respond_and_remember clangByDefault $
              EditableRequest (Evaluate $ evalopts ∪ oldopts) $ Cxx.Operations.requestBody $ Cxx.Operations.resume (Cxx.Operations.parseAbbrMain oldcode) (Cxx.Operations.parseAbbrMain code)
            ParseFailure{} → throwError "Previous request too malformed to resume."
        _ → throwError "Last (editable) request was not resumable."
      | otherwise → parseSuccess . noErrors . respond_and_remember clangByDefault
          =<< EditableRequest (Evaluate evalopts) . getInput }

evaluate :: EvalCxx.Request → WithEvaluation (String, Maybe EvalCxx.Fix)
evaluate = (g .) . EvalCxx.withEvaluation
  where
    g :: EvaluationResult -> (String, Maybe EvalCxx.Fix)
    g er = (concatMap f $ show er, returnedFix er)
    f :: Char -> String
    f '\a' = "*BEEP*"
    f '\n' = "\n"
    f c | isPrint c = [c]
    f c = '[' : showLitChar c "]"

evaluator :: IO (String → Context → [(String, String)] → IO Response)
evaluator = do
  (ev, cfg) ← EvalCxx.evaluator
  return $ \r context extra_env → either (return . Response Nothing . ("error: " ++)) (ev extra_env) $
    join (parseOrFail (p cfg context) (replace no_break_space ' ' r) "request")
