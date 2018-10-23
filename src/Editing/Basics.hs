{-# LANGUAGE UnicodeSyntax, MultiParamTypeClasses, PatternGuards, FlexibleInstances, TypeSynonymInstances, OverlappingInstances, ViewPatterns, FlexibleContexts, RecordWildCards, NamedFieldPuns, GeneralizedNewtypeDeriving #-}

module Editing.Basics (Pos(Pos, pos), positionIn, Anchor(..), Side(..), StickyRange, Range(..), TextEdit(..), tightRange, unanchor_range, selectRange, Offsettable(..), showEditOperand, showTextEdit, find_occs, wideRange, stickyRange, end, contained_in, merge_contiguous, makeMoveEdit, replaceRange, Adjuster(..), adjuster, rangeFromTo, frontPos, frontAnchor, fullRange) where

import qualified Data.List as List
import qualified Data.Char as Char
import qualified Data.List.NonEmpty as NeList
import Control.Arrow ((&&&))
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Semigroup (Semigroup(..))
import Data.Monoid (Monoid(..))
import Data.Ord (comparing)
import Util ((.), NeList, neElim, E, nothingAsError, Apply(..), isIdChar, none, MyMonadError(..))

import Prelude.Unicode
import Prelude hiding ((.))

-- Offset

class Offsettable a where offset :: Int → a → a

instance (Offsettable a, Offsettable b) ⇒ Offsettable (a, b) where offset i (x, y) = (offset i x, offset i y)
instance Offsettable a ⇒ Offsettable [a] where offset = fmap . offset
instance Offsettable Int where offset = (+)
instance Offsettable (Pos a) where offset x (Pos p) = Pos (p + x)
instance Offsettable (Range a) where offset x (Range y z) = Range (offset x y) z
instance Offsettable (Anchor a) where offset x (Anchor y z) = Anchor y (offset x z)
instance Offsettable (StickyRange a) where offset = (.) . offset
instance Offsettable (TextEdit a) where
  offset i (RangeReplaceEdit r s) = RangeReplaceEdit (offset i r) s
  offset i (MoveEdit ba j r) = MoveEdit ba (offset i j) (offset i r)
  offset i (InsertEdit a s) = InsertEdit (offset i a) s

-- Positions

newtype Pos a = Pos {pos :: Int}
  deriving (Eq, Ord, Enum)
    -- The 'a' phantom parameter denotes the element type for the position. This prevents accidental mix-ups of different kinds of positions. The Pos constructor should be used as little as possible.

positionIn :: [a] → Int → Pos a
positionIn _ = Pos

frontPos :: Pos a
frontPos = Pos 0

backPos :: [a] → Pos a
backPos l = Pos (length l)

find_occs :: Eq a ⇒ [a] → [a] → [Pos a]
find_occs x = map (Pos . fst) . filter (List.isPrefixOf x . snd) . zip [0..] . List.tails

splitAtPos :: Pos a → [a] → ([a], [a])
splitAtPos = splitAt . pos

instance Show (Pos a) where show = show . pos

-- Ranges

data Range a = Range { start :: Pos a, size :: Int } deriving Eq

rangeFromTo :: Pos a → Pos a → Range a
rangeFromTo from to = Range from (pos to - pos from)

fullRange :: [a] → Range a
fullRange l = rangeFromTo frontPos (backPos l)

end :: Range a → Pos a
end Range{..} = offset size start

splitRange :: Range a → [a] → ([a], [a], [a])
splitRange Range{..} (splitAt (pos start) → (x, splitAt size → (y, z))) = (x, y, z)

selectRange :: Range a → [a] → [a]
selectRange Range{..} = take size . drop (pos start)

replaceRange :: Range a → [a] → [a] → [a]
replaceRange r replacement (splitRange r → (pre, _, post)) = pre ++ replacement ++ post

contained_in :: Range a → Range a → Bool
contained_in x y = start y ≤ start x ∧ end x ≤ end y

overlap :: Range a → Range a → Int
overlap (Range x s) (Range x' s') = max 0 $ min (pos x + s) (pos x' + s') - max (pos x) (pos x')

touch :: Range a → Range a → Bool
touch x y = end x ≥ start y ∧ end y ≥ start x

-- Sticky positions and ranges

data Side = Before | After deriving (Eq, Ord)

data Anchor a = Anchor { anchor_side :: Side, anchor_pos :: Pos a } deriving Eq

instance Ord (Anchor a) where compare = comparing (anchor_pos &&& anchor_side)

frontAnchor :: Anchor a
frontAnchor = Anchor Before frontPos

type StickyRange a = Side → Anchor a

stickyRange :: Anchor a → Anchor a → StickyRange a
stickyRange x _ Before = x
stickyRange _ x After = x

unanchor_range :: StickyRange a → Range a
unanchor_range r | Anchor _ x ← r Before, Anchor _ y ← r After = Range x (pos y - pos x)

wideRange, tightRange :: Range a → StickyRange a
wideRange r = stickyRange (Anchor Before $ start r) (Anchor After $ end r)
tightRange r = stickyRange (Anchor After $ start r) (Anchor Before $ end r)

two_contiguous :: StickyRange a → StickyRange a → Maybe (StickyRange a)
two_contiguous x y
  | touch (unanchor_range x) (unanchor_range y) = Just $ stickyRange (min (x Before) (y Before)) (max (x After) (y After))
  | otherwise = Nothing

findWithRest :: (a → Maybe b) → [a] → Maybe (b, [a])
findWithRest p = work []
  where
    work _ [] = Nothing
    work old (h:t)
      | Just b ← p h = Just (b, old ++ t)
      | otherwise = work (h:old) t

contiguous :: NeList (StickyRange a) → Maybe (StickyRange a)
contiguous l = case NeList.tail l of
  [] → Just $ NeList.head l
  t → findWithRest (two_contiguous $ NeList.head l) t >>= contiguous . uncurry (:|)

merge_contiguous :: NeList (StickyRange a) → NeList (StickyRange a)
merge_contiguous l = case neElim l of
  (_, []) → l
  (h, t@(x:xs)) → case findWithRest (two_contiguous h) t of
    Nothing → NeList.cons h (merge_contiguous $ x :| xs)
    Just (h', t') → merge_contiguous $ h' :| t'

-- Edits

data TextEdit a
  = RangeReplaceEdit (Range a) [a]
  | InsertEdit (Anchor a) [a]
    -- We don't just use a RangeReplaceEdit with empty range for insertions, because it is not expressive enough. For instance, given "xy", insertions at the positions "after x" and "before y" would both designate position 1, but a prior "add z after x" edit should increment the latter position but not the former. The Anchor's Side expresses this difference.
  | MoveEdit Side Int (Range a)
    -- MoveEdit's Int is an offset. If it is a nonnegative number n, the insert position is n characters beyond the end of the source range. If it is a negative number -n, the insert position is n characters before the start of the source range. We use this instead of a normal Anchor because it ensures that silly "move into self"-edits are not representable. This constructor must not be used by anyone but the makeMoveEdit smart constructor, which detects such edits.
  deriving Eq
    -- The source ranges for move and replace are implicitly narrow-sticky (i.e. right-sticky begin & left-sticky end).

makeMoveEdit :: MyMonadError String m ⇒ Anchor a → Range a → m (TextEdit a)
makeMoveEdit (Anchor ba p) r
  | p ≤ start r = return $ MoveEdit ba (pos p - pos (start r)) r
  | end r ≤ p = return $ MoveEdit ba (pos p - pos (start r) - size r) r
  | otherwise = throwError "Move destination lies in source range."

instance Apply (TextEdit a) [a] [a] where
  apply e s = case e of
    RangeReplaceEdit r repl → replaceRange r repl s
    InsertEdit (Anchor _ p) repl → let (x, y) = splitAtPos p s in x ++ repl ++ y
    MoveEdit _ p r
      | p < 0 → let (splitAtPos (offset p (start r)) → (x, y), z, w) = splitRange r s in x ++ z ++ y ++ w
      | otherwise → let (x, y, splitAt p → (z, u)) = splitRange r s in x ++ z ++ y ++ u

-- More regular edit representation used during adjustment below.

data Replacement a = Replacement { repl_what :: Range a, repl_length :: Int }

insertion :: Pos a → Int → Replacement a
insertion p s = Replacement (Range p 0) s

erasure :: Range a → Replacement a
erasure = flip Replacement 0

-- Adjustment

adjust_with_insertion :: (Pos a, Int) → Anchor a → Anchor a
  -- How does the insertion at the given place of a given number of things change an Anchor?
adjust_with_insertion (p, s) a@Anchor{..}
  | (anchor_side == After ∧ p == anchor_pos) ∨ p < anchor_pos = offset s a
  | otherwise = a

class Adjust a b where adjust :: a → b → Maybe b

instance Adjust (TextEdit a) (Anchor a) where
  adjust (InsertEdit (Anchor _ p) s) a@Anchor{..} = Just $ adjust_with_insertion (p, length s) a
  adjust (RangeReplaceEdit r []) a
    | a == Anchor After (start r) = Just $ Anchor Before (start r)
    | a == Anchor Before (end r) = Just $ Anchor After (start r)
  adjust (RangeReplaceEdit r repl) a@Anchor{anchor_pos}
    | end r ≤ anchor_pos = Just $ offset (length repl - size r) a
    | anchor_pos ≤ start r = Just a
    | otherwise = Nothing
  adjust (MoveEdit _ p r) a@Anchor{..}
    | start r < anchor_pos, anchor_pos < end r = Just $ offset p a
    | p < 0 = adjust_with_insertion (offset p (start r), size r) . adjust (RangeReplaceEdit r []) a
    | otherwise = adjust (RangeReplaceEdit r []) (adjust_with_insertion (offset p (end r), size r) a)

instance Adjust (Replacement a) (Replacement a) where
  adjust (Replacement r l) re@(Replacement r' l')
    | end r ≤ start r' = Just $ re{repl_what=offset (l - size r) r'} -- This reflects that we treat (begin r') as left-sticky.
    | end r' ≤ start r = Just re -- This reflects that we treat (end r') as left-sticky.
    | l == 0, l' == 0 = Just re{repl_what = if start r ≤ start r'
        then Range (start r) (max 0 $ pos (end r') - pos (end r))
        else Range (start r') (size r' - overlap r r')}
    | otherwise = Nothing

instance Adjust (TextEdit a) (Replacement a) where
  adjust (RangeReplaceEdit r repl) r' = adjust (Replacement r (length repl)) r'
  adjust (InsertEdit (Anchor _ p) s) r = adjust (insertion p (length s)) r
  adjust (MoveEdit _ p r) r'
    | repl_what r' `contained_in` r = Just $ r'{repl_what = offset p (repl_what r')}
    | p < 0 = adjust (erasure r) r' >>= adjust (insertion (offset p (start r)) (size r))
    | otherwise = adjust (insertion (offset p (end r)) (size r)) r' >>= adjust (erasure r)

instance Adjust (TextEdit a) (TextEdit a) where
  adjust e (InsertEdit a s) = flip InsertEdit s . adjust e a
  adjust e (MoveEdit ba p r) = do
    r' ← repl_what . adjust e (erasure r)
    a ← adjust e $ Anchor ba $ offset p ((if p < 0 then start else end) r)
    makeMoveEdit a r'
  adjust e (RangeReplaceEdit r s) = flip RangeReplaceEdit s . repl_what . adjust e (Replacement r (length s))

data Adjuster a = Adjuster
  { editAdjuster :: TextEdit a → E (Maybe (TextEdit a))
  , anchorAdjuster :: Anchor a → E (Anchor a) }

instance Semigroup (Adjuster a) where
  x <> y = Adjuster
    { editAdjuster = (>>= maybe (return Nothing) (editAdjuster y)) . editAdjuster x
    , anchorAdjuster = \a → anchorAdjuster x a >>= anchorAdjuster y }

instance Monoid (Adjuster a) where
  mempty = Adjuster { editAdjuster = return . return, anchorAdjuster = return }

adjuster :: String → TextEdit Char → Adjuster Char
adjuster s add = Adjuster
  { editAdjuster = \e → if add == e then return Nothing else case adjust add e of
      Just e' → return $ Just e'
      _ → Left $ "Overlapping edits: " ++ showTextEdit s add ++ " and " ++ showTextEdit s e ++ "."
  , anchorAdjuster = nothingAsError msg . adjust add }
  where msg = "Could not adjust anchor in original snippet to anchor in well formed snippet."

-- Display

showEditOperand :: String → String
showEditOperand " " = "space"
showEditOperand "," = "comma"
showEditOperand ":" = "colon"
showEditOperand ";" = "semicolon"
showEditOperand s
  | all Char.isSpace s = "spaces"
  | all isIdChar s = s
  | none (`elem` " ,;") s, length s < 10 = s
  | otherwise = '`' : s ++ "`"

showTextEdit :: String → TextEdit Char → String
showTextEdit _ (RangeReplaceEdit (Range (Pos 0) 0) r) = "prepend " ++ showEditOperand r
showTextEdit s (RangeReplaceEdit (Range (Pos t) _) r) | t == length s = "append " ++ showEditOperand r
showTextEdit _ (RangeReplaceEdit (Range _ 0) r) = "insert " ++ showEditOperand r
showTextEdit _ (InsertEdit _ r) = "insert " ++ showEditOperand r
showTextEdit s (RangeReplaceEdit r "") = "erase " ++ showEditOperand (selectRange r s)
showTextEdit s (RangeReplaceEdit r s') = "replace " ++ showEditOperand (selectRange r s) ++ " with " ++ showEditOperand s'
showTextEdit s (MoveEdit _ _ r) = "move " ++ showEditOperand (selectRange r s)
