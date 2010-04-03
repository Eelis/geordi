{-# LANGUAGE PatternGuards #-}

module Editing.Execute (execute) where

import qualified Data.Set as Set
import qualified Cxx.Parse
import qualified Editing.Show

import Editing.EditsPreparation (FindResult(..), FoundIn(..), findInStr)

import Control.Monad (foldM)
import Request (EditableRequest(..), EditableRequestKind(..))
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

adjustByEdits :: String → Edit → [Edit] → E (Maybe Edit) -- Returns Nothing if edit would be redundant.
adjustByEdits _ e [] = return $ Just e
adjustByEdits s e (h : t)
  | h == e = return Nothing
  | Just e' ← adjustEdit h e = adjustByEdits s e' t
  | otherwise = fail $ "Overlapping edits: " ++ Editing.Show.showEdit s h ++ " and " ++ Editing.Show.showEdit s e ++ "."

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
      | Evaluate f ← k → return $ EditableRequest (Evaluate $ Set.union f $ Set.fromList opts) s
      | otherwise → fail $ "Cannot use evaluation options for \"" ++ show k ++ "\" request."

execute :: [Command] → EditableRequest → Either String EditableRequest
execute cmds r@(EditableRequest _ str) = do
  (_, r'', _) ← foldM (\(i, r'@(EditableRequest _ str'), m) cmd → do
    let m' = case m of Left _ → (\req → (req, str', i, [])) . Cxx.Parse.parseRequest str'; _ → m
    es ← findInStr str ((\(x, _, z, _) → (x, (\v → case foldM (flip adjustAnchor) v z of Nothing → fail "Could not adjust anchor in original snippet to anchor in well formed snippet."; Just g → return g))) . m') cmd >>=
      foldM (\j (Found f e) → (j++) . maybe [] (:[]) . case f of
          InGiven → adjustByEdits str e $ i ++ j
          InWf → case m' of Left _ → fail "oops"; Right (_, s, _, k) → adjustByEdits s e $ k ++ j) []
    r'' ← foldM (flip exec_edit) r' es
    return (i ++ es, r'', (\(a, b, c, d) → (a, b, c, d ++ es)) . m')
    ) ([], r, fail "oops") cmds
  return r''
