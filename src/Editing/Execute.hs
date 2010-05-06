{-# LANGUAGE PatternGuards, ScopedTypeVariables, TypeSynonymInstances, ViewPatterns #-}

module Editing.Execute (execute) where

import qualified Data.Set as Set
import qualified Cxx.Parse
import qualified Editing.Show

import Editing.EditsPreparation (FindResult(..), FoundIn(..), findInStr)

import Control.Monad (foldM)
import Data.Monoid (Monoid(..))
import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Request (EditableRequest(..), EditableRequestKind(..))
import Cxx.Basics (GeordiRequest)
import Data.SetOps
import Util ((.), E)

import Prelude hiding ((.))
import Prelude.Unicode
import Editing.Basics

overlap :: Range a → Range a → Int
overlap (Range x s) (Range x' s') = max 0 $ min (x + s) (x' + s') - max x x'

splitRange :: Range Char → String → (String, String, String)
splitRange (Range st si) s = (x, y, z)
  where (x, s') = splitAt st s; (y, z) = splitAt si s'

adjustAnchor :: Edit → Anchor → Maybe Anchor
adjustAnchor (InsertEdit (Anchor _ p) s) a@(Anchor ba' p') =
  Just $ if (ba' == After ∧ p == p') ∨ p < p' then Anchor ba' (p' + length s) else a
adjustAnchor (RangeReplaceEdit (Range st si) "") (Anchor ba p)
  | ba == After, p == st = Just $ Anchor Before p
  | ba == Before, p == st + si = Just $ Anchor After st
adjustAnchor (RangeReplaceEdit (Range st si) repl) a@(Anchor ba p)
  | st + si ≤ p = Just $ Anchor ba (p - si + length repl)
  | p ≤ st = Just a
  | otherwise = Nothing
adjustAnchor (MoveEdit ba p r@(Range st si)) a@(Anchor ba' p')
  | st < p', p' < st + si = Just $ Anchor ba' (p' + p)
  | p < 0 = adjustAnchor (RangeReplaceEdit r "") a >>=
    adjustAnchor (InsertEdit (Anchor ba (p + st)) (replicate si 'x'))
  | otherwise = adjustAnchor (InsertEdit (Anchor ba (p + st + si)) (replicate si 'x')) a >>=
    adjustAnchor (RangeReplaceEdit r "")
adjustAnchor (RemoveOptions _) a = Just a
adjustAnchor (AddOptions _) a = Just a

adjustRange :: Edit → Range Char → Maybe (Range Char)
adjustRange (RangeReplaceEdit (Range st si) repl) r'@(Range st' si')
  | st + si ≤ st' = Just $ Range (st' - si + length repl) si'
  | st' + si' ≤ st = Just r'
  | otherwise = Nothing
adjustRange (InsertEdit (Anchor _ p) s) r = adjustRange (RangeReplaceEdit (Range p 0) s) r
adjustRange (MoveEdit ba p r@(Range st si)) r'@(Range st' si')
  | r' `contained_in` r = Just $ Range (p + st') si'
  | p < 0 = adjustRange (RangeReplaceEdit r "") r' >>=
    adjustRange (InsertEdit (Anchor ba (p + st)) (replicate si 'x'))
  | otherwise = adjustRange (InsertEdit (Anchor ba (p + st + si)) (replicate si 'x')) r' >>=
    adjustRange (RangeReplaceEdit r "")
adjustRange (RemoveOptions _) r = Just r
adjustRange (AddOptions _) r = Just r

adjustEraseRange :: Edit → Range Char → Maybe (Range Char)
adjustEraseRange e@(RangeReplaceEdit r@(Range st si) "") r'@(Range st' si') =
  return $ case adjustRange e r' of
    Just r'' → r''
    Nothing | st ≤ st' → Range st (max 0 $ (st' + si') - (st + si))
    Nothing → Range st' (si' - overlap r r')
adjustEraseRange (MoveEdit ba p r@(Range st si)) r'@(Range st' si')
  | r' `contained_in` r = Just $ Range (p + st') si'
  | p < 0 = adjustEraseRange (RangeReplaceEdit r "") r' >>=
    adjustEraseRange (InsertEdit (Anchor ba (st + p)) (replicate si 'x'))
  | otherwise = adjustEraseRange (InsertEdit (Anchor ba (st + si + p)) (replicate si 'x')) r' >>=
    adjustEraseRange (RangeReplaceEdit r "")
adjustEraseRange e r = adjustRange e r

adjustEdit :: Edit → Edit → Maybe Edit
  -- Returns an adjusted Edit, or Nothing if the edits conflict. Second Edit is the one to be adjusted.
adjustEdit (RemoveOptions _) e = Just e
adjustEdit _ e@(RemoveOptions _) = Just e
adjustEdit (AddOptions _) e = Just e
adjustEdit _ e@(AddOptions _) = Just e
adjustEdit e (InsertEdit a s) = flip InsertEdit s . adjustAnchor e a
adjustEdit e (MoveEdit ba p r@(Range st si)) = do
  r' ← adjustEraseRange e r
  a ← adjustAnchor e $ Anchor ba $ if p < 0 then st + p else st + si + p
  makeMoveEdit a r'
adjustEdit e (RangeReplaceEdit r s) =
  flip RangeReplaceEdit s . (if null s then adjustEraseRange else adjustRange) e r

-- Adjusters:

data Adjuster = Adjuster
  { editAdjuster :: Edit → E (Maybe Edit)
  , anchorAdjuster :: Anchor → E Anchor }

instance Monoid Adjuster where
  mempty = Adjuster { editAdjuster = return . return, anchorAdjuster = return }
  mappend x y = Adjuster
    { editAdjuster = (>>= maybe (return Nothing) (editAdjuster y)) . editAdjuster x
    , anchorAdjuster = \a → anchorAdjuster x a >>= anchorAdjuster y }

adjuster :: String → Edit → Adjuster
adjuster s add = Adjuster
  { editAdjuster = \e → if add == e then return Nothing else case adjustEdit add e of
      Just e' → return $ Just e'
      _ → fail $ "Overlapping edits: " ++ Editing.Show.showEdit s add ++ " and " ++ Editing.Show.showEdit s e ++ "."
  , anchorAdjuster = nothingAsError msg . adjustAnchor add }
  where msg = "Could not adjust anchor in original snippet to anchor in well formed snippet."

exec_edit :: Monad m ⇒ Edit → EditableRequest → m EditableRequest
exec_edit e (EditableRequest k s) = case e of
  RangeReplaceEdit r repl →
    let (x, _, b) = splitRange r s in return $ EditableRequest k $ x ++ repl ++ b
  InsertEdit (Anchor _ p) repl →
    let (x, y) = splitAt p s in return $ EditableRequest k $ x ++ repl ++ y
  MoveEdit _ p r@(Range st si)
    | p < 0 → do
      let (x, y) = splitAt (st + p) s; (a, _, c) = splitRange (Range (-p) si) y
      return $ EditableRequest k $ x ++ selectRange r s ++ a ++ c
    | otherwise → do
      let (x, y) = splitAt (st + si + p) s; (a, _, c) = splitRange r x
      return $ EditableRequest k $ a ++ c ++ selectRange r s ++ y
  RemoveOptions opts
    | Evaluate f ← k → return $ EditableRequest (Evaluate $ (Set.\\) f $ Set.fromList opts) s
    | otherwise → fail $ "Cannot remove evaluation options from \"" ++ show k ++ "\" request."
  AddOptions opts
    | Evaluate f ← k → return $ EditableRequest (Evaluate $ f ∪ Set.fromList opts) s
    | otherwise → fail $ "Cannot use evaluation options for \"" ++ show k ++ "\" request."

nothingAsError :: String → Maybe a → E a
nothingAsError s = maybe (fail s) return

data FoldState = FoldState
  { adjust_since_start :: Adjuster
  , current_request :: EditableRequest
  , milepost :: E WellFormedMilepost }

data WellFormedMilepost = WellFormedMilepost
  { tree :: GeordiRequest
  , adjust_to_wf :: Adjuster
  , adjust_since_wf :: Adjuster }
      -- The earliest well-formed AST of the request body, its String version, an adjuster adjusting anchors in the original request to anchors in the well-formed request, and an adjuster adjusting edits in the well-formed request to edits in the current request.

fold_edit :: Edit → FoldState → E FoldState
  -- The edit must be relative to the current request in the fold state (sequence_edit's job).
fold_edit e fs = do
  r ← exec_edit e $ current_request fs
  let
    f req = WellFormedMilepost req (adjust_since_start new) mempty
    a = adjuster (editable_body $ current_request fs) e
    new = FoldState
      (adjust_since_start fs `mappend` a)
      r
      (((\mp → mp { adjust_since_wf = adjust_since_wf mp `mappend` a }) . milepost fs) <|>
        f . Cxx.Parse.parseRequest (editable_body r))
  return new

sequence_edit :: FoldState → FindResult Edit → E FoldState
sequence_edit fs (Found f e) = do
  a :: Adjuster ← case f of
    InGiven → return $ adjust_since_start fs
    InWf → adjust_since_wf . milepost fs
  t ← editAdjuster a e
  maybe return fold_edit t fs

exec_cmd :: String → FoldState → Command → E FoldState
exec_cmd s fs = (>>= foldM sequence_edit fs) .
  findInStr s ((tree &&& anchorAdjuster . adjust_to_wf) . milepost fs)

execute :: [Command] → EditableRequest → E EditableRequest
execute l r@(EditableRequest _ s) = current_request . foldM (exec_cmd s) fs l
  where
    f t = WellFormedMilepost t mempty mempty
    fs = (FoldState mempty r $ f . Cxx.Parse.parseRequest s)
