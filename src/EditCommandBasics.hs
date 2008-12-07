module EditCommandBasics (describe_position_after, execute, findInStr, replaceRange, selectRange, Range(..), toks_text, Edit(..), toks_len, tok_len, Token(..), ops, edit_tokens) where

import Control.Monad.Error ()
import Data.Monoid (Monoid(..))
import Data.Char (isSpace, isAlpha, isDigit, isAlphaNum)
import qualified Prelude
import Prelude hiding (last, (.), all, (!!))
import qualified Data.List as List
import Request (EvalOpt, EditableRequest(..), EditableRequestKind(..))
import Util (findMaybe, take_atleast, (.), isIdChar, Convert(..), Op(..), ops_cost, unne, erase_indexed, levenshtein, replaceAllInfix, approx_match, (!!), Cost, once_twice_thrice, Invertible(..), (.||.), (.&&.))

import EditCommandGrammar

-- Positions/ranges:

type Pos a = Int
  -- The 'a' phantom parameter denotes the element type for the position. This prevents accidental mix-ups of different kinds of positions.
data Range a = Range { start :: Pos a, size :: Int } deriving Eq

end :: Range a -> Pos a
end (Range x y) = x + y

selectRange :: Range a -> [a] -> [a]
selectRange (Range st si) = take si . drop st

replaceRange :: Range a -> [a] -> [a] -> [a]
replaceRange (Range p l) r s = take p s ++ r ++ drop (p + l) s

splitRange :: Range Char -> String -> (String, String, String)
splitRange (Range st si) s = (x, y, z)
  where (x, s') = splitAt st s; (y, z) = splitAt si s'

contained_in :: Range a -> Range a -> Bool
contained_in (Range st si) (Range st' si') = st' <= st && st + si <= st' + si'

overlap :: Range a -> Range a -> Int
overlap (Range x s) (Range x' s') = max 0 $ min (x + s) (x' + s') - max x x'

find_occs :: Eq a => [a] -> [a] -> [Pos a]
find_occs _ [] = []
find_occs x y | Just z <- List.stripPrefix x y = 0 : (+ length x) . find_occs x z
find_occs x (_:ys) = (+ 1) . find_occs x ys

nth :: (Monad m, Show a, Eq a) => Int -> [a] -> [a] -> m (Range a)
nth _ x y | [] <- find_occs x y = fail $ "String " ++ show x ++ " does not occur."
nth n x y | l <- find_occs x y, (- length l) <= n, n < length l = return $ Range (l !! n) (length x)
nth n x _ = fail $ "String " ++ show x ++ " does not occur " ++ once_twice_thrice (if n < 0 then -n else (n+1)) ++ "."

-- Edits:

data Anchor = Anchor { anchor_befAft :: BefAft, anchor_pos :: Pos Char } deriving Eq
  -- This BefAft will probably need to be generalized to Before|After|Both for "insert x between 3 and 4".
data Edit
  = RangeReplaceEdit (Range Char) String
  | InsertEdit Anchor String
  | MoveEdit BefAft Int (Range Char)
    -- The Int is an offset. If it is a nonnegative number n, the insert position is n characters beyond the end of the source range. If it is a negative number -n, the insert position is n characters before the start of the source range. We use this instead of a normal Anchor because it ensures that silly "move into self"-edits are not representable. This constructor must not be used by anyone but the makeMoveEdit smart constructor, which detects such edits.
  | AddOptions [EvalOpt]
  | RemoveOptions [EvalOpt]
    deriving Eq
  -- We don't just use a RangeReplaceEdit with range length 0 for insertions, because it is not expressive enough. For instance, given "xy", insertions at the positions "after x" and "before y" would both designate position 1, but a prior "add z after x" edit should increment the latter position but not the former. InsertEdit's BefAft argument expresses this difference.

makeMoveEdit :: Monad m => Anchor -> Range Char -> m Edit
makeMoveEdit (Anchor ba p) r@(Range st si)
  | p < st = return $ MoveEdit ba (p - st) r
  | st + si < p = return $ MoveEdit ba (p - st - si) r
  | otherwise = fail "Move destination lies in source range."

showEdit :: String -> Edit -> String
showEdit _ (RemoveOptions opts) = "remove " ++ show opts
showEdit _ (AddOptions opts) = "use " ++ show opts
showEdit _ (RangeReplaceEdit (Range 0 0) r) = "prepend " ++ show r
showEdit s (RangeReplaceEdit (Range t _) r) | t == length s = "append " ++ show r
showEdit _ (RangeReplaceEdit (Range _ 0) r) = "insert " ++ show r
showEdit _ (InsertEdit _ r) = "insert " ++ show r
showEdit s (RangeReplaceEdit r "") = "erase " ++ show (selectRange r s)
showEdit s (RangeReplaceEdit r s') = "replace " ++ show (selectRange r s) ++ " with " ++ show s'
showEdit s (MoveEdit _ _ r) = "move " ++ show (selectRange r s)

adjustAnchor :: Edit -> Anchor -> Maybe Anchor
adjustAnchor (InsertEdit (Anchor _ p) s) a@(Anchor ba' p') =
  Just $ if (ba' == After && p == p') || p < p' then Anchor ba' (p' + length s) else a
adjustAnchor (RangeReplaceEdit (Range st si) "") (Anchor ba p)
  | ba == After, p == st = Just $ Anchor Before p
  | ba == Before, p == st + si = Just $ Anchor After st
adjustAnchor (RangeReplaceEdit (Range st si) repl) a@(Anchor ba p)
  | st + si <= p = Just $ Anchor ba (p - si + length repl)
  | p <= st = Just a
  | otherwise = Nothing
adjustAnchor (MoveEdit ba p r@(Range st si)) a@(Anchor ba' p')
  | st < p', p' < st + si = Just $ Anchor ba' (p' + p)
  | p < 0 = adjustAnchor (RangeReplaceEdit r "") a >>=
    adjustAnchor (InsertEdit (Anchor ba (p + st)) (replicate si 'x'))
  | otherwise = adjustAnchor (InsertEdit (Anchor ba (p + st + si)) (replicate si 'x')) a >>=
    adjustAnchor (RangeReplaceEdit r "")
adjustAnchor (RemoveOptions _) a = Just a
adjustAnchor (AddOptions _) a = Just a

adjustRange :: Edit -> Range Char -> Maybe (Range Char)
adjustRange (RangeReplaceEdit (Range st si) repl) r'@(Range st' si')
  | st + si <= st' = Just $ Range (st' - si + length repl) si'
  | st' + si' <= st = Just r'
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

adjustEraseRange :: Edit -> Range Char -> Maybe (Range Char)
adjustEraseRange e@(RangeReplaceEdit r@(Range st si) "") r'@(Range st' si') =
  return $ case adjustRange e r' of
    Just r'' -> r''
    Nothing | st <= st' -> Range st (max 0 $ (st' + si') - (st + si))
    Nothing -> Range st' (si' - overlap r r')
adjustEraseRange (MoveEdit ba p r@(Range st si)) r'@(Range st' si')
  | r' `contained_in` r = Just $ Range (p + st') si'
  | p < 0 = adjustEraseRange (RangeReplaceEdit r "") r' >>=
    adjustEraseRange (InsertEdit (Anchor ba (st + p)) (replicate si 'x'))
  | otherwise = adjustEraseRange (InsertEdit (Anchor ba (st + si + p)) (replicate si 'x')) r' >>=
    adjustEraseRange (RangeReplaceEdit r "")
adjustEraseRange e r = adjustRange e r

adjustEdit :: Edit -> Edit -> Maybe Edit
  -- Returns an adjusted Edit, or Nothing if the edits conflict. Second Edit is the one to be adjusted.
adjustEdit e e' | e == e' = Just e
adjustEdit (RemoveOptions _) e = Just e
adjustEdit _ e@(RemoveOptions _) = Just e
adjustEdit (AddOptions _) e = Just e
adjustEdit _ e@(AddOptions _) = Just e
adjustEdit e (InsertEdit a s) = flip InsertEdit s . adjustAnchor e a
adjustEdit e (MoveEdit ba p r@(Range st si)) = do
  r' <- adjustEraseRange e r
  a <- adjustAnchor e $ Anchor ba $ if p < 0 then st + p else st + si + p
  makeMoveEdit a r'
adjustEdit e (RangeReplaceEdit r s) =
  flip RangeReplaceEdit s . (if null s then adjustEraseRange else adjustRange) e r

adjustEdits :: Edit -> [Edit] -> Either Edit [Edit]
  -- Returns either a conflicting Edit or the list of adjusted Edits.
adjustEdits _ [] = Right []
adjustEdits e (e' : t)
  | Just r <- adjustEdit e e' = (r :) . adjustEdits e t
  | otherwise = Left e'

exec_edits :: Monad m => [Edit] -> EditableRequest -> m EditableRequest
exec_edits [] r = return r
exec_edits (e : t) (EditableRequest k s) = case adjustEdits e t of
  Left e' -> fail $ "Overlapping edits: " ++ showEdit s e ++ " and " ++ showEdit s e' ++ "."
  Right t' -> case e of
    RangeReplaceEdit r repl ->
      let (x, _, b) = splitRange r s in exec_edits t' $ EditableRequest k $ x ++ repl ++ b
    InsertEdit (Anchor _ p) repl ->
      let (x, y) = splitAt p s in exec_edits t' $ EditableRequest k $ x ++ repl ++ y
    MoveEdit _ p r@(Range st si)
      | p < 0 -> do
        let (x, y) = splitAt (st + p) s; (a, b, c) = splitRange (Range (-p) si) y
        exec_edits t' $ EditableRequest k $ x ++ selectRange r s ++ a ++ c
      | otherwise -> do
        let (x, y) = splitAt (st + si + p) s; (a, b, c) = splitRange r x
        exec_edits t' $ EditableRequest k $ a ++ c ++ selectRange r s ++ y
    RemoveOptions opts
      | Evaluate f <- k -> exec_edits t' $ EditableRequest (Evaluate (f .&&. (not . (`elem` opts)))) s
      | otherwise -> fail $ "Cannot remove evaluation options from \"" ++ show k ++ "\" request."
    AddOptions opts
      | Evaluate f <- k -> exec_edits t' $ EditableRequest (Evaluate (f .||. (`elem` opts))) s
      | otherwise -> fail $ "Cannot use evaluation options for \"" ++ show k ++ "\" request."

-- Resolving positions/occurrences/edits in the subject string:

type ARange = BefAft -> Anchor

arange :: Anchor -> Anchor -> ARange
arange x _ Before = x
arange _ x After = x

anchor_range :: Range a -> ARange
anchor_range (Range x y) = arange (Anchor After x) (Anchor Before (x + y))

unanchor_range :: ARange -> Range a
unanchor_range r | Anchor _ x <- r Before, Anchor _ y <- r After = Range x (y - x)

class Offsettable a where offset :: Int -> a -> a

instance Offsettable (Range Char) where offset x (Range y z) = Range (y + x) z
instance Offsettable Anchor where offset x (Anchor y z) = Anchor y (z + x)
instance Offsettable ARange where offset x r = offset x . r
instance Offsettable a => Offsettable [a] where offset x = (offset x .)

class FindInStr a b | a -> b where findInStr :: (Functor m, Monad m) => String -> a -> m b

instance FindInStr Around [ARange] where findInStr s (Around x) = concat . findInStr s x

instance FindInStr (Ranked String) ARange where
  findInStr t (Ranked (Ordinal o) s) = anchor_range . nth o s t
  findInStr y (Sole x) = case find_occs x y of
    [z] -> return $ anchor_range $ Range z (length x)
    [] -> fail $ "String " ++ show x ++ " does not occur."
    _ -> fail $ "String " ++ show x ++ " occurs multiple times."

instance Invertible BefAft where invert Before = After; invert After = Before

instance Invertible Ordinal where
  invert (Ordinal r) | r >= 0 = Ordinal (-r - 1)
  invert o = o

instance Invertible (Ranked String) where
  invert (Ranked r s) = Ranked (invert r) s
  invert x = x

instance Invertible (Rankeds String) where
  invert (Rankeds (AndList r) s) = Rankeds (AndList $ invert . r) s
  invert x = x

instance Convert (Range Char) [ARange] where convert = (:[]) . anchor_range
instance Convert (Range Char) ARange where convert = anchor_range

instance Convert ARange (Range Char) where convert = unanchor_range
instance Convert Anchor (Pos Char) where convert = anchor_pos

instance (Offsettable b, Invertible a, FindInStr a b, Convert (Range Char) b) => FindInStr (Relative a) b where
  findInStr s (Relative o ba w) = do
    Range st si <- unanchor_range . findInStr s w
    case ba of
      Before -> findInStr (take st s) (invert o)
      After -> offset (st + si) . findInStr (drop (st + si) s) o
  findInStr s (Between o (Betw b e)) = do
    x <- convert . findInStr s b
    y <- convert . findInStr s e
    let (p, q) = if either start id x <= either start id (y :: Either (Range Char) (Pos Char))  then (x, y) else (y, x)
    let p' = either end id p; q' = either start id q
    offset p' . findInStr (take (q' - p') $ drop p' s) o
  findInStr s (FromTill b e) = do
    x <- convert . findInStr s b
    let p = either start id (x :: Either (Range Char) (Pos Char))
    y <- convert . findInStr (drop p s) e
    return $ convert $ (Range p (either end id (y :: Either (Range Char) (Pos Char))) :: Range Char)

everything_arange :: String -> ARange
everything_arange _ Before = Anchor Before 0
everything_arange s After = Anchor After (length s)

instance FindInStr (EverythingOr (Ranked String)) ARange where
  findInStr s Everything = return $ everything_arange s
  findInStr s (NotEverything x) = findInStr s x

instance FindInStr (EverythingOr (Rankeds String)) [ARange] where
  findInStr s Everything = return [everything_arange s]
  findInStr s (NotEverything x) = findInStr s x

instance FindInStr (Rankeds String) [ARange] where
  findInStr y (All x) = case find_occs x y of
    [] -> fail $ "String " ++ show x ++ " does not occur."
    l -> return $ (anchor_range . flip Range (length x)) . l
  findInStr y (Sole' x) = case find_occs x y of
    [z] -> return [anchor_range $ Range z (length x)]
    [] -> fail $ "String " ++ show x ++ " does not occur."
    _ -> fail $ "String " ++ show x ++ " occurs multiple times."
  findInStr x (Rankeds (AndList rs) s) = sequence $ (\r -> findInStr x (Ranked r s)) . unne rs
  findInStr x (AllBut (AndList rs) s) =
    return $ (anchor_range . flip Range (length s)) . erase_indexed (ordinal_carrier . unne rs) (find_occs s x)

instance FindInStr PositionsClause [Anchor] where
  findInStr s (PositionsClause Before (AndList o)) = (($ Before) .) . concat . sequence (findInStr s . unne o)
  findInStr s (PositionsClause After (AndList o)) = (($ After) .) . concat . sequence (findInStr s . unne o)

instance FindInStr a b => FindInStr (AndList a) [b] where
  findInStr s (AndList l) = sequence (findInStr s . unne l)

instance FindInStr Replacer [Edit] where
  findInStr s (Replacer p r) = (flip RangeReplaceEdit r .) . concat . ((unanchor_range .) .) . findInStr s p
  findInStr _ (ReplaceOptions o o') = return [RemoveOptions o, AddOptions o']

instance FindInStr Eraser [Edit] where
  findInStr s (EraseText p) = ((flip RangeReplaceEdit "" . unanchor_range) .) . findInStr s p
  findInStr _ (EraseOptions o) = return $ [RemoveOptions o]

instance FindInStr Bound (Either ARange Anchor) where
  findInStr s (Bound Nothing Everything) = return $ Left $ everything_arange s
  findInStr _ (Bound (Just Before) Everything) = return $ Right $ Anchor Before 0
  findInStr s (Bound (Just After) Everything) = return $ Right $ Anchor After (length s)
  findInStr s (Bound mba p) = maybe Left (\ba -> Right . ($ ba)) mba . findInStr s p

instance FindInStr RelativeBound (Either ARange Anchor) where
  findInStr _ Front = return $ Right $ Anchor Before 0
  findInStr s Back = return $ Right $ Anchor After (length s)
  findInStr s (RelativeBound mba p) = maybe Left (\ba -> Right . ($ ba)) mba . findInStr s p

instance FindInStr Mover Edit where
  findInStr s (Mover o p) = do
    a <- findInStr s p
    r <- unanchor_range . findInStr s o
    makeMoveEdit a r

instance FindInStr Position Anchor where
  findInStr _ (Position Before Everything) = return $ Anchor Before 0
  findInStr s (Position After Everything) = return $ Anchor After (length s)
  findInStr s (Position ba (NotEverything p)) = ($ ba) . findInStr s p

instance FindInStr UseClause Edit where
  findInStr _ (UseOptions o) = return $ AddOptions o
  findInStr s (UseString z) = do
    if y == 0 || ops_cost owc > fromIntegral (length z) / 1.5 then fail "No match."
      else return $ RangeReplaceEdit (Range x y) z
   where
    text_tokens = edit_tokens isAlphaNum s
    pattern_tokens = edit_tokens isAlphaNum z
    (x, y) = (sum $ length . take stt text_tokens, sum $ length . take siz (drop stt text_tokens))
    (owc, stt, siz) = head $ approx_match token_edit_cost pattern_tokens (replaceAllInfix pattern_tokens (replicate (length pattern_tokens) (replicate 100 'X')) text_tokens)

token_edit_cost :: Op String -> Cost
token_edit_cost (SkipOp (' ':_)) = 0
token_edit_cost (SkipOp x) | x `elem` keywords = -2.4
token_edit_cost (SkipOp (h:t)) | isAlphaNum h = -2.2 - fromIntegral (length t) * 0.2
token_edit_cost (SkipOp _) = -2
token_edit_cost (EraseOp (' ':_)) = 0.02
token_edit_cost (EraseOp x) = token_edit_cost (InsertOp x)
token_edit_cost (InsertOp t) | t `elem` keywords = 2
token_edit_cost (InsertOp (' ':_)) = -0.02
token_edit_cost (InsertOp x@(y:_)) | isAlpha y = fromIntegral (length x) * 0.7
token_edit_cost (InsertOp (x:y)) | isDigit x = 1 + fromIntegral (length y) * 0.3
token_edit_cost (InsertOp _) = 1
token_edit_cost (ReplaceOp x y)
  | or $ (\c -> List.all (`elem` c) [x, y]) . [classKeys, accessSpecifiers, relational_ops, arithmetic_ops] = 0.4
token_edit_cost (ReplaceOp (c:_) (d:_)) | not (isAlphaNum c || isAlphaNum d) = 1.1
token_edit_cost (ReplaceOp x@(c:_) y@(d:_)) | isAlpha c && isAlpha d =
  if null (List.intersect x y) then 10 else levenshtein x y * 0.4
token_edit_cost (ReplaceOp x@(c:_) y@(d:_)) | isAlphaNum c && isAlphaNum d = levenshtein x y * 0.8
token_edit_cost (ReplaceOp _ _) = 10
  -- The precise values of these costs are fine-tuned to make the tests pass, and that is their only justification. We're trying to approximate the human intuition for what substring should be replaced, as codified in the tests.

instance FindInStr Command [Edit] where
  findInStr s (Use (AndList l)) = sequence $ findInStr s . unne l
  findInStr s (Append x Nothing) = return [InsertEdit (Anchor After (length s)) x]
  findInStr _ (Prepend x Nothing) = return [InsertEdit (Anchor Before 0) x]
  findInStr s (Append x (Just y)) = findInStr s (Insert x y)
  findInStr s (Prepend x (Just y)) = findInStr s (Insert x y)
  findInStr s (Erase (AndList l)) = concat . sequence (findInStr s . unne l)
  findInStr s (Replace (AndList l)) = concat . sequence (findInStr s . unne l)
  findInStr s (Insert r p) = (flip InsertEdit r .) . concat . findInStr s p
  findInStr s (Move (AndList movers)) = sequence (findInStr s . unne movers)
  findInStr s (WrapAround (Wrapping x y) z) = concat . ((\r -> [InsertEdit (r Before) x, InsertEdit (r After) y]) .) . concat . findInStr s z
  findInStr s (WrapIn z (Wrapping x y)) = findInStr s $ WrapAround (Wrapping x y) $ and_one $ Around z

-- Known tokens:

relational_ops, accessSpecifiers, classKeys, types, casts, keywords, arithmetic_ops, ops, long_ops :: [String]
relational_ops = words "< > <= >= == !="
arithmetic_ops = concatMap (\x -> [x, x ++ "="]) $ words "+ - * / << >> | & ^"
ops = relational_ops ++ arithmetic_ops ++ words "++ -- -> .* :: && || ! = ~ [ ] ( ) { } :"
long_ops = filter ((>1) . length) ops

accessSpecifiers = words "public private protected"
classKeys = words "class struct union"
types = words "short auto bool double int signed unsigned void char wchar_t char32_t float long char16_t"
casts = words "reinterpret_cast dynamic_cast static_cast const_cast"
keywords = accessSpecifiers ++ classKeys ++ types ++ casts ++ words "alignas continue friend typedef alignof decltype goto return typeid asm default if typename delete inline sizeof break do static_assert using case mutable virtual catch else namespace static enum new volatile explicit nullptr switch export operator template while extern this const false throw constexpr true for register try define elif include defined"

-- Tokenization:

edit_tokens :: (Char -> Bool) -> String -> [String]
  -- First parameter is a predicate for identifier characters; sometimes we want "foo_bar" to be ["foo", "_", "bar"], but sometimes we want it to be ["foo_bar"].
  -- Literals are not parsed as single tokens. A sequence of spaces is parsed as a single token.
edit_tokens p = work
  where
    work [] = []
    work s | Just (o, s') <- findMaybe (\q -> (\x -> (q, x)) . List.stripPrefix q s) long_ops = o : work s'
    work (' ':s) = let (x, s') = span isSpace s in (' ':x) : work s'
    work (h:s) | isDigit h = let (x, s') = span isDigit s in (h:x) : work s'
    work (h:s) | p h = let (x, s') = span p s in (h:x) : work s'
    work (h:s) = [h] : work s

data Token = Token { tok_text, tok_white :: String }

instance Eq Token where t == t' = tok_text t == tok_text t'

tok_len :: Token -> Int
tok_len (Token x y) = length x + length y

toks_len :: [Token] -> Int
toks_len = sum . (tok_len .)

instance Monoid Token where
  mappend (Token x y) (Token "" y') = Token x (y ++ y')
  mappend (Token x y) (Token x' y') = Token (x ++ y ++ x') y'
  mempty = Token "" ""

toks_text :: [Token] -> String
toks_text = tok_text . mconcat

describe_position_after :: Pos Char -> String -> Position
describe_position_after n s
  | n == 0 = Position Before Everything
  | n == length s = Position After Everything
  | otherwise = Position After $ NotEverything $ Sole $ concat $ reverse $ take_atleast 7 length $ reverse $ edit_tokens isIdChar $ take n s

execute :: (Functor m, Monad m) => [Command] -> EditableRequest -> m EditableRequest
execute cmds r@(EditableRequest _ str) = do
  edits <- concat . sequence (findInStr str . cmds)
  exec_edits edits r
