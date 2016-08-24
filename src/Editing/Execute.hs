{-# LANGUAGE UnicodeSyntax, PatternGuards, ScopedTypeVariables, TypeSynonymInstances, ViewPatterns, FlexibleContexts, RecordWildCards, MultiParamTypeClasses #-}

module Editing.Execute (execute) where

import qualified Data.Set as Set
import qualified Cxx.Parse

import Editing.EditsPreparation (FindResult(..), FoundIn(..), findInStr)

import Control.Monad (foldM)
import Data.Monoid (Monoid(..))
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad.Except (MonadError(..))
import Request (EditableRequest(..), EditableRequestKind(..), RequestEdit(..), addEvalOpt)
import Cxx.Basics (GeordiRequest)
import Data.SetOps
import Util ((.), E, MaybeApply(..), Apply(..))

import Prelude hiding ((.))
import Editing.Basics
import Editing.Commands

editRequestBody :: (String → String) → (EditableRequest → EditableRequest)
editRequestBody f (EditableRequest k s) = EditableRequest k (f s)

instance MaybeApply RequestEdit EditableRequest where
  mapply e er@EditableRequest{..} = case e of
    TextEdit te → return $ editRequestBody (apply te) er
    RemoveOptions opts
      | Evaluate f ← kind → return er{ kind = Evaluate $ (Set.\\) f $ Set.fromList opts }
      | otherwise → throwError $ "Cannot remove evaluation options from \"" ++ show kind ++ "\" request."
    AddOptions opts
      | Evaluate f ← kind → return er{ kind = Evaluate $ foldr addEvalOpt f opts }
      | otherwise → throwError $ "Cannot use evaluation options for \"" ++ show kind ++ "\" request."

data FoldState = FoldState
  { adjust_since_start :: Adjuster Char
  , current_request :: EditableRequest
  , milepost :: E WellFormedMilepost }

data WellFormedMilepost = WellFormedMilepost
  { tree :: GeordiRequest
  , adjust_to_wf :: Adjuster Char
  , adjust_since_wf :: Adjuster Char }
      -- The earliest well-formed AST of the request body, its String version, an adjuster adjusting anchors in the original request to anchors in the well-formed request, and an adjuster adjusting edits in the well-formed request to edits in the current request.

fold_edit :: RequestEdit → FoldState → E FoldState
  -- The edit must be relative to the current request in the fold state (sequence_edit's job).
fold_edit e fs = do
  r ← mapply e $ current_request fs
  let
    f req = WellFormedMilepost req (adjust_since_start new) mempty
    a = case e of TextEdit te → adjuster (editable_body $ current_request fs) te; _ → mempty
    new = FoldState
      (adjust_since_start fs `mappend` a)
      r
      (((\mp → mp { adjust_since_wf = adjust_since_wf mp `mappend` a }) . milepost fs) <|>
        f . Cxx.Parse.parseRequest (editable_body r))
  return new

sequence_edit :: FoldState → FindResult RequestEdit → E FoldState
sequence_edit fs (Found f e) = do
  a :: Adjuster Char ← case f of
    InGiven → return $ adjust_since_start fs
    InWf → adjust_since_wf . milepost fs
  case e of
    TextEdit e' → do
      t ← (TextEdit .) . editAdjuster a e'
      maybe return fold_edit t fs
    _ → fold_edit e fs

exec_cmd :: Maybe (TextEdit Char) -> String → FoldState → Command → E FoldState
exec_cmd fixit s fs = (>>= foldM sequence_edit fs) .
  findInStr s fixit ((tree &&& anchorAdjuster . adjust_to_wf) . milepost fs)

execute :: Maybe (TextEdit Char) -> [Command] → EditableRequest → E EditableRequest
execute fixit l r@(EditableRequest _ s) = current_request . foldM (exec_cmd fixit s) fs l
  where
    f t = WellFormedMilepost t mempty mempty
    fs = (FoldState mempty r $ f . Cxx.Parse.parseRequest s)
