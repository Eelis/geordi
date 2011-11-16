{-# LANGUAGE MultiParamTypeClasses, PatternGuards, FlexibleInstances, TypeSynonymInstances, OverlappingInstances, ViewPatterns, FlexibleContexts, RecordWildCards #-}

module Editing.Basics where

import qualified Data.List as List
import qualified Data.List.NonEmpty as NeList
import Control.Arrow ((&&&))
import Control.Monad.Error (MonadError(..))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Monoid (Monoid(..))
import Data.Ord (comparing)
import Util ((.), NeList, neElim, E, nothingAsError, Apply(..))

import Prelude.Unicode
import Prelude hiding ((.))

-- Positions, ranges, and anchors.

type Pos a = Int
  -- The 'a' phantom parameter denotes the element type for the position. This prevents accidental mix-ups of different kinds of positions.
data Range a = Range { start :: Pos a, size :: Int } deriving Eq

data BefAft = Before | After deriving (Eq, Ord)

data Anchor = Anchor { anchor_befAft :: BefAft, anchor_pos :: Pos Char } deriving Eq

type ARange = BefAft → Anchor

data DualARange = DualARange { full_range, replace_range :: ARange }
  -- In "void f(int i, double d);", the command "replace first parameter-declaration with char c" should produce "void f(char c, double d);", while the command "erase first parameter-declaration" should produce "void f(double d);". Hence, in the former, the clause "first parameter-declaration" should match "char c", while in the latter, it should match "char c, ". To accomodate this, our substring resolution functions return DualARanges containing both of these ranges.

end :: Range a → Pos a
end (Range x y) = x + y

selectRange :: Range a → [a] → [a]
selectRange (Range st si) = take si . drop st

replaceRange :: Range a → [a] → [a] → [a]
replaceRange (Range p l) r s = take p s ++ r ++ drop (p + l) s

wide_range :: Range Char → ARange
wide_range (Range st si) = arange (Anchor Before st) (Anchor After (st+si))

contained_in :: Range a → Range a → Bool
contained_in (Range st si) (Range st' si') = st' ≤ st ∧ st + si ≤ st' + si'

touch :: Range a → Range a → Bool
touch x y = end x ≥ start y ∧ end y ≥ start x

findWithRest :: (a → Maybe b) → [a] → Maybe (b, [a])
findWithRest p = work []
  where
    work _ [] = Nothing
    work old (h:t)
      | Just b ← p h = Just (b, old ++ t)
      | otherwise = work (h:old) t

two_contiguous :: ARange → ARange → Maybe ARange
two_contiguous x y
  | touch (unanchor_range x) (unanchor_range y) = Just $ arange (min (x Before) (y Before)) (max (x After) (y After))
  | otherwise = Nothing

contiguous :: NeList ARange → Maybe ARange
contiguous l = case NeList.tail l of
  [] → Just $ NeList.head l
  t → findWithRest (two_contiguous $ NeList.head l) t >>= contiguous . uncurry (:|)

merge_contiguous :: NeList ARange → NeList ARange
merge_contiguous l = case neElim l of
  (_, []) → l
  (h, t@(x:xs)) → case findWithRest (two_contiguous h) t of
    Nothing → NeList.cons h (merge_contiguous $ x :| xs)
    Just (h', t') → merge_contiguous $ h' :| t'

class Offsettable a where offset :: Int → a → a

instance Offsettable (Range Char) where offset x (Range y z) = Range (y + x) z
instance Offsettable Anchor where offset x (Anchor y z) = Anchor y (z + x)
instance Offsettable ARange where offset = (.) . offset
instance (Offsettable a, Offsettable b) ⇒ Offsettable (a, b) where offset i (x, y) = (offset i x, offset i y)
instance (Offsettable a, Functor f) ⇒ Offsettable (f a) where offset = fmap . offset
instance Offsettable Int where offset = (+)
instance Offsettable TextEdit where
  offset i (RangeReplaceEdit r s) = RangeReplaceEdit (offset i r) s
  offset i (MoveEdit ba j r) = MoveEdit ba (offset i j) (offset i r)
  offset i (InsertEdit a s) = InsertEdit (offset i a) s

find_occs :: Eq a ⇒ [a] → [a] → [Pos a]
find_occs x = map fst . filter (List.isPrefixOf x . snd) . zip [0..] . List.tails

arange :: Anchor → Anchor → ARange
arange x _ Before = x
arange _ x After = x

anchor_range :: Range a → ARange
anchor_range (Range x y) = arange (Anchor After x) (Anchor Before (x + y))

unanchor_range :: ARange → Range a
unanchor_range r | Anchor _ x ← r Before, Anchor _ y ← r After = Range x (y - x)

-- Edits

instance Ord Anchor where compare = comparing (anchor_pos &&& anchor_befAft)

data TextEdit
  = RangeReplaceEdit (Range Char) String
  | InsertEdit Anchor String
  | MoveEdit BefAft Int (Range Char)
    deriving Eq
    -- The Int is an offset. If it is a nonnegative number n, the insert position is n characters beyond the end of the source range. If it is a negative number -n, the insert position is n characters before the start of the source range. We use this instead of a normal Anchor because it ensures that silly "move into self"-edits are not representable. This constructor must not be used by anyone but the makeMoveEdit smart constructor, which detects such edits.
    -- We don't just use a RangeReplaceEdit with range length 0 for insertions, because it is not expressive enough. For instance, given "xy", insertions at the positions "after x" and "before y" would both designate position 1, but a prior "add z after x" edit should increment the latter position but not the former. InsertEdit's BefAft argument expresses this difference.
    -- The BefAft will probably need to be generalized to Before|After|Both for "insert x between 3 and 4".

makeMoveEdit :: MonadError String m ⇒ Anchor → Range Char → m TextEdit
makeMoveEdit (Anchor ba p) r@(Range st si)
  | p ≤ st = return $ MoveEdit ba (p - st) r
  | st + si ≤ p = return $ MoveEdit ba (p - st - si) r
  | otherwise = throwError "Move destination lies in source range."

splitRange :: Range Char → String → (String, String, String)
splitRange Range{..} (splitAt start → (x, splitAt size → (y, z))) = (x, y, z)

instance Apply TextEdit String String where
  apply e s = case e of
    RangeReplaceEdit r repl →
      let (x, _, b) = splitRange r s
      in x ++ repl ++ b
    InsertEdit (Anchor _ p) repl →
      let (x, y) = splitAt p s
      in x ++ repl ++ y
    MoveEdit _ p r@(Range st si)
      | p < 0 →
        let (x, y) = splitAt (st + p) s; (a, _, c) = splitRange (Range (-p) si) y
        in x ++ selectRange r s ++ a ++ c
      | otherwise →
        let (x, y) = splitAt (st + si + p) s; (a, _, c) = splitRange r x
        in a ++ c ++ selectRange r s ++ y

overlap :: Range a → Range a → Int
overlap (Range x s) (Range x' s') = max 0 $ min (x + s) (x' + s') - max x x'

adjustAnchor :: TextEdit → Anchor → Maybe Anchor
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

adjustRange :: TextEdit → Range Char → Maybe (Range Char)
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

adjustEraseRange :: TextEdit → Range Char → Maybe (Range Char)
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

adjustEdit :: TextEdit → TextEdit → Maybe TextEdit
  -- Returns an adjusted Edit, or Nothing if the edits conflict. Second Edit is the one to be adjusted.
adjustEdit e (InsertEdit a s) = flip InsertEdit s . adjustAnchor e a
adjustEdit e (MoveEdit ba p r@(Range st si)) = do
  r' ← adjustEraseRange e r
  a ← adjustAnchor e $ Anchor ba $ if p < 0 then st + p else st + si + p
  makeMoveEdit a r'
adjustEdit e (RangeReplaceEdit r s) =
  flip RangeReplaceEdit s . (if null s then adjustEraseRange else adjustRange) e r

showTextEdit :: String → TextEdit → String
showTextEdit _ (RangeReplaceEdit (Range 0 0) r) = "prepend " ++ show r
showTextEdit s (RangeReplaceEdit (Range t _) r) | t == length s = "append " ++ show r
showTextEdit _ (RangeReplaceEdit (Range _ 0) r) = "insert " ++ show r
showTextEdit _ (InsertEdit _ r) = "insert " ++ show r
showTextEdit s (RangeReplaceEdit r "") = "erase " ++ show (selectRange r s)
showTextEdit s (RangeReplaceEdit r s') = "replace " ++ show (selectRange r s) ++ " with " ++ show s'
showTextEdit s (MoveEdit _ _ r) = "move " ++ show (selectRange r s)

-- Adjusters:

data Adjuster = Adjuster
  { editAdjuster :: TextEdit → E (Maybe TextEdit)
  , anchorAdjuster :: Anchor → E Anchor }

instance Monoid Adjuster where
  mempty = Adjuster { editAdjuster = return . return, anchorAdjuster = return }
  mappend x y = Adjuster
    { editAdjuster = (>>= maybe (return Nothing) (editAdjuster y)) . editAdjuster x
    , anchorAdjuster = \a → anchorAdjuster x a >>= anchorAdjuster y }

adjuster :: String → TextEdit → Adjuster
adjuster s add = Adjuster
  { editAdjuster = \e → if add == e then return Nothing else case adjustEdit add e of
      Just e' → return $ Just e'
      _ → Left $ "Overlapping edits: " ++ showTextEdit s add ++ " and " ++ showTextEdit s e ++ "."
  , anchorAdjuster = nothingAsError msg . adjustAnchor add }
  where msg = "Could not adjust anchor in original snippet to anchor in well formed snippet."
