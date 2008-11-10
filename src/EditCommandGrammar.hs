module EditCommandGrammar where

import Request (EvalOpt)
import Util (NElist(..), Convert(..))

newtype Ordinal = Ordinal { ordinal_carrier :: Int } -- 0 = first, 1 = second, -1 = last, -2 = second last, etc

newtype AndList a = AndList { andList :: NElist a }

instance Functor AndList where fmap f (AndList l) = AndList (fmap f l)

and_one :: a -> AndList a
and_one x = AndList (NElist x [])

data EverythingOr a = Everything | NotEverything a
data Ranked a = Ranked Ordinal a | Sole a
data Rankeds a = Rankeds (AndList Ordinal) a | Sole' a | All a | AllBut (AndList Ordinal) a
data Bound = Bound (Maybe BefAft) (EverythingOr (Ranked String))
data RelativeBound = Front | Back | RelativeBound (Maybe BefAft) (Relative (EverythingOr (Ranked String)))
data Relative a = Relative a BefAft (Ranked String) | Between a Betw | FromTill Bound RelativeBound
  -- FromTill is not the same as (Between Everything), because in the former, the second bound is interpreted relative to the first, whereas in the latter, both bounds are absolute.
data PositionsClause = PositionsClause BefAft (AndList (Relative (EverythingOr (Rankeds String))))
data Position = Position BefAft (EverythingOr (Ranked String))
type Positions = AndList PositionsClause
data Replacer = Replacer (AndList (Relative (EverythingOr (Rankeds String)))) String | ReplaceOptions [EvalOpt] [EvalOpt]
data Eraser = EraseText (Relative (EverythingOr (Rankeds String))) | EraseOptions [EvalOpt]

data Mover = Mover (Relative (EverythingOr (Ranked String))) Position
data BefAft = Before | After deriving Eq
data Around = Around (AndList (Relative (EverythingOr (Rankeds String))))
data Betw = Betw Bound RelativeBound
data Wrapping = Wrapping String String
data UseClause = UseString String | UseOptions [EvalOpt]

data Command
  = Insert String Positions
  | Append String (Maybe Positions)
  | Prepend String (Maybe Positions)
  | Replace (AndList Replacer)
  | Erase (AndList Eraser)
  | Move (AndList Mover)
  | WrapAround Wrapping (AndList Around)
  | WrapIn (AndList (Relative (EverythingOr (Rankeds String)))) Wrapping
  | Use (AndList UseClause)

front, back :: Bound
front = Bound (Just Before) Everything
back = Bound (Just After) Everything

absolute :: a -> Relative a
absolute x = Between x $ Betw front Back

instance Functor EverythingOr where
  fmap _ Everything = Everything
  fmap f (NotEverything x) = NotEverything (f x)

instance Convert (Ranked a) (Rankeds a) where
  convert (Ranked o x) = Rankeds (and_one o) x
  convert (Sole x) = Sole' x

and_and :: AndList a -> AndList a -> AndList a
and_and (AndList (NElist x y)) (AndList (NElist a b)) = AndList $ NElist x (y ++ a : b)

merge_commands :: [Command] -> [Command]
merge_commands [] = []
merge_commands (Erase l : Erase l' : r) = merge_commands $ Erase (l `and_and` l') : r
merge_commands (Replace (AndList (NElist (Replacer x []) [])) : Replace (AndList (NElist (Replacer y []) [])) : r) =
  merge_commands $ Replace (and_one $ Replacer (x `and_and` y) []) : r
merge_commands (h:t) = h : merge_commands t
