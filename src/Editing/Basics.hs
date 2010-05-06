{-# LANGUAGE MultiParamTypeClasses, PatternGuards, FlexibleInstances, TypeSynonymInstances, OverlappingInstances, ViewPatterns #-}

module Editing.Basics where

import qualified Cxx.Basics
import qualified Request
import qualified Data.List as List
import qualified Data.List.NonEmpty as NeList
import qualified Data.Char as Char
import Data.Function (on)
import Data.Foldable (toList)
import Data.Ord (comparing)
import Control.Arrow ((&&&))
import Prelude.Unicode

import Cxx.Basics (DeclaratorId, Findable)

import Data.Monoid (Monoid(..))
import Util (Convert(..), Invertible(..), Ordinal(..), (.), findMaybe, take_atleast, isIdChar, NeList, neElim)
import Data.List.NonEmpty ((|:), (.:))
import Data.Semigroup (Semigroup(..))

import Prelude hiding ((.))

-- AndLists

newtype AndList a = AndList { andList :: NeList a }

instance Functor AndList where fmap f (AndList l) = AndList (fmap f l)

and_one :: a → AndList a
and_one = AndList . return

instance Semigroup (AndList a) where
  AndList x .++. AndList y = AndList $ x .++. y

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
contiguous l = case NeList.neTail l of 
  [] → Just $ NeList.neHead l
  t → findWithRest (two_contiguous $ NeList.neHead l) t >>= contiguous . uncurry (|:)

merge_contiguous :: NeList ARange → NeList ARange
merge_contiguous l = case neElim l of
  (_, []) → l
  (h, t@(x:xs)) → case findWithRest (two_contiguous h) t of
    Nothing → h .: (merge_contiguous $ x |: xs)
    Just (h', t') → merge_contiguous $ h' |: t'

class Offsettable a where offset :: Int → a → a

instance Offsettable (Range Char) where offset x (Range y z) = Range (y + x) z
instance Offsettable Anchor where offset x (Anchor y z) = Anchor y (z + x)
instance Offsettable ARange where offset = (.) . offset
instance (Offsettable a, Offsettable b) ⇒ Offsettable (a, b) where offset i (x, y) = (offset i x, offset i y)
instance (Offsettable a, Functor f) ⇒ Offsettable (f a) where offset = fmap . offset
instance Offsettable Int where offset = (+)
instance Offsettable Edit where
  offset i (RangeReplaceEdit r s) = RangeReplaceEdit (offset i r) s
  offset i (MoveEdit ba j r) = MoveEdit ba (offset i j) (offset i r)
  offset i (InsertEdit a s) = InsertEdit (offset i a) s
  offset _ e = e

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

  -- This BefAft will probably need to be generalized to Before|After|Both for "insert x between 3 and 4".
data Edit
  = RangeReplaceEdit (Range Char) String
  | InsertEdit Anchor String
  | MoveEdit BefAft Int (Range Char)
    -- The Int is an offset. If it is a nonnegative number n, the insert position is n characters beyond the end of the source range. If it is a negative number -n, the insert position is n characters before the start of the source range. We use this instead of a normal Anchor because it ensures that silly "move into self"-edits are not representable. This constructor must not be used by anyone but the makeMoveEdit smart constructor, which detects such edits.
  | AddOptions [Request.EvalOpt]
  | RemoveOptions [Request.EvalOpt]
    deriving Eq
  -- We don't just use a RangeReplaceEdit with range length 0 for insertions, because it is not expressive enough. For instance, given "xy", insertions at the positions "after x" and "before y" would both designate position 1, but a prior "add z after x" edit should increment the latter position but not the former. InsertEdit's BefAft argument expresses this difference.

makeMoveEdit :: Monad m ⇒ Anchor → Range Char → m Edit
makeMoveEdit (Anchor ba p) r@(Range st si)
  | p ≤ st = return $ MoveEdit ba (p - st) r
  | st + si ≤ p = return $ MoveEdit ba (p - st - si) r
  | otherwise = fail "Move destination lies in source range."

-- Command grammar

data EverythingOr a = Everything | NotEverything a
data Ranked a = Ranked Ordinal a | Sole a
newtype OccurrencesClause = OccurrencesClause (NeList Ordinal)
data Rankeds a = Rankeds (AndList OccurrencesClause) a | Sole' a | All a | AllBut (AndList OccurrencesClause) a
data Bound = Bound (Maybe BefAft) Substr
data RelativeBound = Front | Back | RelativeBound (Maybe BefAft) (Relative Substr)
data Relative a = Absolute a | Relative a (AndList BefAft) (Ranked (Either Findable String)) | Between a Betw | FromTill Bound RelativeBound
  -- FromTill is not the same as (Between Everything), because in the former, the second bound is interpreted relative to the first, whereas in the latter, both bounds are absolute.
  -- Strictly speaking, Absolute can be (and was at some point) encoded by (Between blabla). However, it isn't worth the trouble of the necessary special cases in Show, Parse, etc.
data In a = In a (Maybe InClause)
data PositionsClause = PositionsClause (AndList BefAft) Substrs
data InClause = InClause (AndList (In (Relative (Rankeds (Either Findable ImplicitBodyOf)))))
data AppendPositionsClause = AppendIn InClause | NonAppendPositionsClause PositionsClause
data PrependPositionsClause = PrependIn InClause | NonPrependPositionsClause PositionsClause
type Substr = EverythingOr (Ranked (Either Findable String))
newtype Substrs = Substrs (AndList (In (Relative (EverythingOr (Rankeds (Either Findable String))))))
newtype MakeSubject = MakeSubject (AndList (In (Relative (Rankeds (Either Findable ImplicitDeclarationOf)))))
data Position = Position BefAft (In (Relative Substr))
data Replacer = Replacer Substrs String | ReplaceOptions [Request.EvalOpt] [Request.EvalOpt]
data Changer = Changer Substrs String | ChangeOptions [Request.EvalOpt] [Request.EvalOpt]
data Eraser = EraseText Substrs | EraseOptions [Request.EvalOpt] | EraseAround Wrapping (Around Substrs)
data Mover = Mover Substrs Position
data Around a = Around a
data Betw = Betw Bound RelativeBound
data Wrapping = Wrapping String String
data UsePattern = UsePattern String
data UseClause = UseString (In (Relative UsePattern)) | UseOptions [Request.EvalOpt]
newtype ImplicitDeclarationOf = ImplicitDeclarationOf DeclaratorId
newtype ImplicitBodyOf = ImplicitBodyOf DeclaratorId
data Insertee = SimpleInsert String | WrapInsert Wrapping

data Command
  = Insert Insertee (AndList AppendPositionsClause)
  | Append String (Maybe (AndList AppendPositionsClause))
  | Prepend String (Maybe (AndList PrependPositionsClause))
  | Replace (AndList Replacer)
  | Change (AndList Changer)
  | Erase (AndList Eraser)
  | Move (AndList Mover)
  | Swap Substrs (Maybe Substrs)
  | Make MakeSubject Cxx.Basics.MakeDeclaration
  | Use (AndList UseClause)

data FinalCommand
  = Show (Maybe Substrs)
  | Identify Substrs
  | Parse
  | Diff

newtype Identifier = Identifier { identifier_string :: String }

data MakeClause = MakeClause (AndList DeclaratorId) Cxx.Basics.MakeDeclaration

flatten_MakeClauses :: AndList MakeClause → [(Cxx.Basics.MakeDeclaration, DeclaratorId)]
flatten_MakeClauses = concatMap (\(MakeClause (AndList l) d) → map ((,) d) (toList l)) . toList . andList

-- Convenience constructors

front, back :: Bound
front = Bound (Just Before) Everything
back = Bound (Just After) Everything

-- Convert/Invertible/Functor Instances

instance Functor EverythingOr where
  fmap _ Everything = Everything
  fmap f (NotEverything x) = NotEverything (f x)

instance Functor Ranked where
  fmap f (Ranked o x) = Ranked o (f x)
  fmap f (Sole x) = Sole (f x)

instance Functor Rankeds where
  fmap f (Rankeds o x) = Rankeds o $ f x
  fmap f (Sole' x) = Sole' $ f x
  fmap f (All x) = All $ f x
  fmap f (AllBut o x) = AllBut o $ f x

instance Functor Relative where
  fmap f (Absolute x) = Absolute $ f x
  fmap f (Relative x ba r) = Relative (f x) ba r
  fmap f (Between x b) = Between (f x) b
  fmap _ (FromTill a b) = FromTill a b

instance Convert (Ranked a) (Rankeds a) where
  convert (Ranked o x) = Rankeds (and_one $ OccurrencesClause $ return o) x
  convert (Sole x) = Sole' x

instance Convert (Range a) (Range a) where convert = id
instance Convert (Range a) [ARange] where convert = (:[]) . anchor_range
instance Convert (Range a) ARange where convert = anchor_range
instance Convert ARange (Range a) where convert = unanchor_range
instance Convert Anchor (Pos a) where convert = anchor_pos
instance Convert ARange DualARange where convert x = DualARange x x

instance Invertible BefAft where invert Before = After; invert After = Before

instance Invertible (Ranked a) where
  invert (Ranked r s) = Ranked (invert r) s
  invert x = x

instance Invertible OccurrencesClause where
  invert (OccurrencesClause l) = OccurrencesClause $ invert . l

instance Invertible (Rankeds a) where
  invert (Rankeds (AndList r) s) = Rankeds (AndList $ invert . r) s
  invert x = x

-- Misc operations

unrelative :: Relative a → Maybe a
unrelative (Absolute x) = Just x
unrelative (Relative x _ _) = Just x
unrelative (Between x _) = Just x
unrelative _ = Nothing
  -- Hm, this is slightly weird. You'd expect all "Relative a"s to contain an a.

merge_commands :: [Command] → [Command]
merge_commands [] = []
merge_commands (Erase l : Erase l' : r) = merge_commands $ Erase (l .++. l') : r
merge_commands
  (Replace (AndList (neElim → (Replacer (Substrs x) [], []))) :
  Replace (AndList (neElim → (Replacer (Substrs y) [], []))) : r) =
    merge_commands $ Replace (and_one $ Replacer (Substrs $ x .++. y) []) : r
merge_commands (h:t) = h : merge_commands t

describe_position_after :: Pos Char → String → Position
describe_position_after n s
  | n == 0 = Position Before $ In (Absolute Everything) Nothing
  | n == length s = Position After $ In (Absolute Everything) Nothing
  | otherwise = Position After $ In (Absolute $ NotEverything $ Sole $ Right $ concat $ reverse $ take_atleast 7 length $ reverse $ edit_tokens isIdChar $ take n s) Nothing

-- Tokenization:

edit_tokens :: (Char → Bool) → String → [String]
  -- First parameter is a predicate for identifier characters; sometimes we want "foo_bar" to be ["foo", "_", "bar"], but sometimes we want it to be ["foo_bar"].
  -- Literals are not parsed as single tokens. A sequence of spaces is parsed as a single token.
edit_tokens p = work
  where
    work [] = []
    work s | Just (o, s') ← findMaybe (\q → (\x → (q, x)) . List.stripPrefix q s) Cxx.Basics.long_ops = o : work s'
    work (' ':s) = let (x, s') = span Char.isSpace s in (' ':x) : work s'
    work (h:s) | Char.isDigit h = let (x, s') = span Char.isDigit s in (h:x) : work s'
    work (h:s) | p h = let (x, s') = span p s in (h:x) : work s'
    work (h:s) = [h] : work s

data Token = Token { tok_text, tok_white :: String }

instance Eq Token where (==) = (==) `on` tok_text

tok_len :: Token → Int
tok_len (Token x y) = length x + length y

toks_len :: [Token] → Int
toks_len = sum . (tok_len .)

instance Monoid Token where
  mappend (Token x y) (Token "" y') = Token x (y ++ y')
  mappend (Token x y) (Token x' y') = Token (x ++ y ++ x') y'
  mempty = Token "" ""

toks_text :: [Token] → String
toks_text = tok_text . mconcat
