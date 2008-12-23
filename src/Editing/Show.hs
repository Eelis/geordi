{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Editing.Show (showEdit) where

import Cxx.Show ()
import qualified Data.List as List
import Data.Char (isSpace)
import Util (unne, isVowel, show_long_opts, capitalize, commas_and, Ordinal)
import Cxx.Basics (DeclaratorId)
import Editing.Basics
import qualified Prelude
import Prelude hiding (Show(..))

showEdit :: String -> Edit -> String
showEdit _ (RemoveOptions opts) = "remove " ++ show_long_opts opts
showEdit _ (AddOptions opts) = "use " ++ show_long_opts opts
showEdit _ (RangeReplaceEdit (Range 0 0) r) = "prepend " ++ show r
showEdit s (RangeReplaceEdit (Range t _) r) | t == length s = "append " ++ show r
showEdit _ (RangeReplaceEdit (Range _ 0) r) = "insert " ++ show r
showEdit _ (InsertEdit _ r) = "insert " ++ show r
showEdit s (RangeReplaceEdit r "") = "erase " ++ show (selectRange r s)
showEdit s (RangeReplaceEdit r s') = "replace " ++ show (selectRange r s) ++ " with " ++ show s'
showEdit s (MoveEdit _ _ r) = "move " ++ show (selectRange r s)

class Show a where show :: a -> String
  -- To let us define our own instances for things like Either.

instance Show Ordinal where show = Prelude.show
instance Show DeclaratorId where show = Prelude.show
instance Show String where show = Prelude.show

instance Show Wrapping where
  show (Wrapping "<" ">") = "angle brackets"
  show (Wrapping "{" "}") = "curly brackets"
  show (Wrapping "[" "]") = "square brackets"
  show (Wrapping "(" ")") = "parentheses"
  show (Wrapping "'" "'") = "single quotes"
  show (Wrapping "\"" "\"") = "double quotes"
  show (Wrapping x y) = x ++ " and " ++ y

instance Show a => Show (EverythingOr a) where
  show Everything = "everything"
  show (NotEverything x) = show x

instance (Show a, Show b) => Show (Either a b) where
  show (Left x) = show x; show (Right x) = show x

instance Show BefAft where show Before = "before"; show After = "after"

instance RawShow a => Show (Ranked a) where
  show (Sole s) = raw_show s
  show (Ranked r s) = show r ++ " " ++ raw_show s

instance Show Position where
  show (Position Before (Right Everything)) = "at start"
  show (Position After (Right Everything)) = "at end"
  show (Position a x) = show a ++ " " ++ show x

instance Show a => Show (AndList a) where
  show (AndList l) = concat $ List.intersperse " and " $ map show $ unne l

instance Show PositionsClause where
  show (PositionsClause ba l) = show ba ++ " " ++ show l

instance Show Bound where
  show (Bound mba x) = maybe "" ((++ " ") . show) mba ++ show x

instance Show a => Show (Relative a) where
  show (Between x (Betw (Bound (Just Before) Everything) Back)) = show x
  show (Between x (Betw y z)) = show x ++ " between " ++ show y ++ " and " ++ show z
  show (Relative x y z) = show x ++ " " ++ show y ++ " " ++ show z
  show (FromTill b c) = "from " ++ show b ++ " till " ++ show c

instance Show RelativeBound where
  show Front = "front"; show Back = "back"
  show (RelativeBound mba x) = maybe "" ((++ " ") . show) mba ++ show x

instance RawShow a => Show (Rankeds a) where
  show (Sole' x) = raw_show x
  show (All x) = "all " ++ raw_show x
  show (AllBut x y) = "all but " ++ show x ++ " " ++ raw_show y
  show (Rankeds l x) = show l ++ " " ++ raw_show x

instance Show Replacer where
  show (Replacer x y) = show x ++ " with " ++ raw_show y
  show (ReplaceOptions o o') = show_long_opts o ++ " with " ++ show_long_opts o'
instance Show Eraser where show (EraseText l) = show l; show (EraseOptions o) = show_long_opts o
instance Show UseClause where show (UseString s) = s; show (UseOptions o) = show_long_opts o
instance Show Mover where show (Mover x y) = show x ++ " to " ++ show y

class RawShow a where raw_show :: a -> String

instance RawShow String where
  raw_show " " = "space"
  raw_show s | List.all isSpace s = "spaces"
  raw_show "," = "comma"
  raw_show x = show x

instance Show Around where show (Around a) = show a

data Tense = Present | Past

past :: String -> String
past "wrap" = "wrapped"
past s | isVowel (List.last s) = s ++ "d"
past s = s ++ "ed"

tense :: Tense -> String -> String
tense Present s = s
tense Past s = past s

show_command :: Tense -> Command -> String
show_command t (Insert s p) = tense t "insert" ++ " " ++ raw_show s ++ " " ++ show p
show_command t (Erase l) = tense t "erase" ++ " " ++ show l
show_command t (Replace l) = tense t "replace" ++ " " ++ show l
show_command t (Use l) = tense t "use" ++ " " ++ show l
show_command t (Prepend s mp) = tense t "prepend" ++ " " ++ raw_show s ++ maybe "" ((" " ++) . show) mp
show_command t (Append s mp) = tense t "append" ++ " " ++ raw_show s ++ maybe "" ((" " ++) . show) mp
show_command t (Move l) = tense t "move" ++ " " ++ show l
show_command t (WrapIn l w) = tense t "wrap" ++ " " ++ show l ++ " in " ++ show w
show_command t (WrapAround w l) = tense t "wrap" ++ " " ++ show w ++ " around " ++ show l

instance Prelude.Show Command where
  show = show_command Past
  showList l r = capitalize (commas_and $ map Prelude.show l) ++ "." ++ r

instance Prelude.Show Position where show = Editing.Show.show
