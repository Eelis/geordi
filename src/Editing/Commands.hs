{-# LANGUAGE MultiParamTypeClasses, PatternGuards, FlexibleInstances, TypeSynonymInstances, OverlappingInstances, ViewPatterns, FlexibleContexts #-}

module Editing.Commands where

import qualified Cxx.Basics
import qualified Request
import qualified Data.List as List
import qualified Data.Char as Char
import Data.Function (on)
import Data.Foldable (toList)
import Editing.Basics (Pos, Anchor(..), BefAft(..), DualARange(..), ARange, Range(..), anchor_range, unanchor_range)
import Cxx.Basics (DeclaratorId, Findable)

import Data.Monoid (Monoid(..))
import Util (Convert(..), Invertible(..), Ordinal(..), (.), findMaybe, take_atleast, isIdChar, NeList, neElim)
import Data.Semigroup (Semigroup(..))

import Prelude hiding ((.))

-- AndLists

newtype AndList a = AndList { andList :: NeList a }

instance Functor AndList where fmap f (AndList l) = AndList (fmap f l)

and_one :: a → AndList a
and_one = AndList . return

instance Semigroup (AndList a) where
  AndList x <> AndList y = AndList $ x <> y

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
  | Run

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
merge_commands (Erase l : Erase l' : r) = merge_commands $ Erase (l <> l') : r
merge_commands
  (Replace (AndList (neElim → (Replacer (Substrs x) [], []))) :
  Replace (AndList (neElim → (Replacer (Substrs y) [], []))) : r) =
    merge_commands $ Replace (and_one $ Replacer (Substrs $ x <> y) []) : r
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
