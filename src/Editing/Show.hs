{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, PatternGuards #-}

module Editing.Show (showEdit, Show(..)) where

import Cxx.Show ()
import qualified Data.List as List
import Data.Char as Char
import Util (isVowel, show_long_opts, capitalize, commas_and, Ordinal, none, isIdChar)
import Cxx.Basics (DeclaratorId, Findable)
import Data.Foldable (toList)
import Editing.Basics
import qualified Prelude
import Prelude hiding (Show(..))

showEdit :: String → Edit → String
showEdit _ (RemoveOptions opts) = "remove " ++ show_long_opts opts
showEdit _ (AddOptions opts) = "use " ++ show_long_opts opts
showEdit _ (RangeReplaceEdit (Range 0 0) r) = "prepend " ++ show r
showEdit s (RangeReplaceEdit (Range t _) r) | t == length s = "append " ++ show r
showEdit _ (RangeReplaceEdit (Range _ 0) r) = "insert " ++ show r
showEdit _ (InsertEdit _ r) = "insert " ++ show r
showEdit s (RangeReplaceEdit r "") = "erase " ++ show (selectRange r s)
showEdit s (RangeReplaceEdit r s') = "replace " ++ show (selectRange r s) ++ " with " ++ show s'
showEdit s (MoveEdit _ _ r) = "move " ++ show (selectRange r s)

class Show a where show :: a → String
  -- To let us define our own instances for things like Either and String.

instance Show Ordinal where show = Prelude.show
instance Show DeclaratorId where show = Prelude.show

instance Show Wrapping where
  show (Wrapping "<" ">") = "angle brackets"
  show (Wrapping "{" "}") = "curly brackets"
  show (Wrapping "[" "]") = "square brackets"
  show (Wrapping "(" ")") = "parentheses"
  show (Wrapping "'" "'") = "single quotes"
  show (Wrapping "\"" "\"") = "double quotes"
  show (Wrapping x y) = x ++ " and " ++ y

instance Show a ⇒ Show (EverythingOr a) where
  show Everything = "everything"
  show (NotEverything x) = show x

instance (Show a, Show b) ⇒ Show (Either a b) where
  show (Left x) = show x; show (Right x) = show x

instance Show BefAft where show Before = "before"; show After = "after"

instance Show a ⇒ Show (Ranked a) where
  show (Sole s) = show s
  show (Ranked r s) = show r ++ " " ++ show s

instance Show Findable where show = Prelude.show

instance Show Position where
  show (Position Before (In (Absolute Everything) Nothing)) = "at start"
  show (Position After (In (Absolute Everything) Nothing)) = "at end"
  show (Position a x) = show a ++ " " ++ show x

instance Show a ⇒ Show (AndList a) where
  show (AndList l) = concat $ List.intersperse " and " $ map show $ toList l

instance Show Substrs where show (Substrs l) = show l

instance Show PositionsClause where show (PositionsClause ba s) = show ba ++ " " ++ show s

instance Show AppendPositionsClause where
  show (NonAppendPositionsClause pc) = show pc
  show (AppendIn d) = show d

instance Show PrependPositionsClause where
  show (NonPrependPositionsClause pc) = show pc
  show (PrependIn d) = show d

instance Show Bound where
  show (Bound mba x) = maybe "" ((++ " ") . show) mba ++ show x

instance Show Betw where show (Betw x y) = "between " ++ show x ++ " and " ++ show y

instance Show a ⇒ Show (Relative a) where
  show (Absolute x) = show x
  show (Between x y) = show x ++ " " ++ show y
  show (Relative x y z) = show x ++ " " ++ show y ++ " " ++ show z
  show (FromTill b c) = "from " ++ show b ++ " till " ++ show c

instance Show a ⇒ Show (In a) where show (In x incl) = show x ++ maybe "" show incl

instance Show ImplicitBodyOf where show (ImplicitBodyOf x) = show x
instance Show ImplicitDeclarationOf where show (ImplicitDeclarationOf x) = show x

instance Show InClause where show (InClause x) = "in " ++ show x

instance Show RelativeBound where
  show Front = "front"; show Back = "back"
  show (RelativeBound mba x) = maybe "" ((++ " ") . show) mba ++ show x

instance Show OccurrencesClause where show (OccurrencesClause l) = show (AndList l)

instance Show a ⇒ Show (Rankeds a) where
  show (Sole' x) = show x
  show (All x) = "all " ++ show x
  show (AllBut x y) = "all but " ++ show x ++ " " ++ show y
  show (Rankeds l x) = show l ++ " " ++ show x

instance Show Replacer where
  show (Replacer x y) = show x ++ " with " ++ show y
  show (ReplaceOptions o o') = show_long_opts o ++ " with " ++ show_long_opts o'
instance Show Changer where
  show (Changer x y) = show x ++ " to " ++ show y
  show (ChangeOptions o o') = show_long_opts o ++ " to " ++ show_long_opts o'
instance Show Eraser where
  show (EraseText l) = show l
  show (EraseOptions o) = show_long_opts o
  show (EraseAround w l) = show w ++ " " ++ show l

instance Show UsePattern where show (UsePattern p) = p
instance Show UseClause where show (UseString s) = show s; show (UseOptions o) = show_long_opts o
instance Show Mover where show (Mover x y) = show x ++ " to " ++ show y

instance Show String where
  show " " = "space"
  show "," = "comma"
  show ":" = "colon"
  show ";" = "semicolon"
  show s
    | all Char.isSpace s = "spaces"
    | all isIdChar s = s
    | none (`elem` " ,;") s, length s < 10 = s
    | otherwise = '`' : s ++ "`"

instance Show a ⇒ Show (Around a) where show (Around a) = "around " ++ show a

data Tense = Present | Past

past :: String → String
past "wrap" = "wrapped"
past "swap" = "swapped"
past s | isVowel (List.last s) = s ++ "d"
past s = s ++ "ed"

tense :: Tense → String → String
tense Present s = s
tense Past s = past s

instance Show Insertee where
  show (SimpleInsert s) = show s
  show (WrapInsert w) = show w

show_command :: Tense → Command → String
show_command t (Insert s p) = tense t "insert" ++ " " ++ show s ++ " " ++ show p
show_command t (Erase l) = tense t "erase" ++ " " ++ show l
show_command t (Replace l) = tense t "replace" ++ " " ++ show l
show_command t (Change l) = tense t "change" ++ " " ++ show l
show_command t (Use l) = tense t "use" ++ " " ++ show l
show_command t (Prepend s mp) = tense t "prepend" ++ " " ++ show s ++ maybe "" ((" " ++) . show) mp
show_command t (Append s mp) = tense t "append" ++ " " ++ show s ++ maybe "" ((" " ++) . show) mp
show_command t (Move l) = tense t "move" ++ " " ++ show l
show_command t (Swap l Nothing) = tense t "swap" ++ " " ++ show l
show_command t (Swap l (Just y)) = tense t "swap" ++ " " ++ show l ++ " with " ++ show y
show_command _ _ = "<command>"

instance Prelude.Show Command where
  show = show_command Past
  showList l r = capitalize (commas_and $ map Prelude.show l) ++ "." ++ r

instance Prelude.Show Position where show = Editing.Show.show
