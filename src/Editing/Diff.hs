{-# LANGUAGE PatternGuards #-}

-- Express the difference between two strings as a list of edit commands.

module Editing.Diff (diff, diff_as_Edits) where

import qualified Cxx.Basics
import qualified Data.Char as Char
import Data.Algorithm.Diff (DI(..), getDiff)
import Data.Maybe (listToMaybe)
import Data.Monoid (Monoid(..))
import Util (orElse, take_atleast, (.), isIdChar, invert)
import Editing.Basics
import Prelude hiding ((.))
import Prelude.Unicode

diff_as_Edits :: String → String → [Edit]
diff_as_Edits pre post =
  map simpleEdit_to_Edit $
  merge_nearby_edits pre $
  diffsAsSimpleEdits $ getDiff pre post

diff :: String → String → [Command]
diff pre post =
  let pre_toks = diff_tokenize pre in
  merge_commands $
  map (`describe_simpleEdit` pre_toks) $
  merge_nearby_edits pre_toks $
  diffsAsSimpleEdits $ getDiff pre_toks (diff_tokenize post)

data SimpleEdit a = SimpleEdit { se_range :: Range a, _se_repl :: [a] }

simpleEdit_to_Edit :: SimpleEdit Char → Edit
simpleEdit_to_Edit (SimpleEdit (Range p 0) s) = InsertEdit (Anchor Before p) s
simpleEdit_to_Edit (SimpleEdit r s) = RangeReplaceEdit r s
  -- This "Before" seems arbitrary.

describe_simpleEdit :: SimpleEdit Token → [Token] → Command
describe_simpleEdit (SimpleEdit (Range p 0) s) t | p == length t = Append (toks_text s) Nothing
describe_simpleEdit (SimpleEdit (Range 0 0) s) _ = Prepend (toks_text s) Nothing
describe_simpleEdit (SimpleEdit r@(Range pos siz) s) t
  | null s = Erase $ and_one $ EraseText $ Substrs $ and_one $ In describe_range Nothing
  | siz == 0 = case () of -- insert
    ()| repl_elem ["{", "("] ∨ alpha After → ins Before
    ()| repl_elem ["}", ")", ";"] ∨ alpha Before → ins After
    ()| toks_len s ≤ 4, size (se_range expanded_edit) > siz → describe_simpleEdit expanded_edit t
    () → ins $ if toks_len (context After) > toks_len (context Before) then Before else After
  | all ((∈ Cxx.Basics.ops) . tok_text) sr, s /= [], not (source_elem ["{", "}", "(", ")"]), size (se_range expanded_edit) > siz =
    describe_simpleEdit expanded_edit t
  | otherwise = Replace $ and_one $ Replacer (Substrs $ and_one $ In describe_range Nothing) (toks_text s)
  where
    repl_elem x = case s of [Token u _] → elem u x; _ → False
    source_elem = (sr' ∈)
    sr' = tok_text (mconcat sr)
    expanded_edit = SimpleEdit (Range (pos - length (context Before)) (siz + length (context Before) + length (context After))) (inorder_context Before ++ s ++ context After)
    toks Before = reverse $ take pos t
    toks After = drop (pos + siz) t
    context = take_atleast 7 tok_len . takeWhile ((/= ";") . tok_text) . toks
    inorder_context After = context After
    inorder_context Before = reverse $ context Before
    furthest Before = listToMaybe; furthest After = listToMaybe . reverse
    alpha ba = (Char.isAlpha . (tok_text . listToMaybe (toks ba) >>= furthest (invert ba))) `orElse` False
    ins ba = Insert (SimpleInsert $ toks_text s) $ and_one $ NonAppendPositionsClause $ PositionsClause (and_one ba) $ Substrs $ and_one $ In (Absolute $ NotEverything $ Sole' $ Right $ toks_text $ inorder_context $ invert ba) Nothing
    sr = selectRange r t
    sole = NotEverything $ Sole' $ Right sr'
    rel ba = Relative sole (and_one ba) $ Sole $ Right $ toks_text $ inorder_context $ invert ba
    describe_range :: Relative (EverythingOr (Rankeds (Either Cxx.Basics.Findable String)))
    describe_range
      | source_elem ["{", "(", "friend"] ∨ (toks_len sr ≤ 4 ∧ alpha After) = rel Before
      | source_elem ["}", ")", ";"] ∨ (toks_len sr ≤ 4 ∧ alpha Before) = rel After
      | otherwise = Absolute sole

diffsAsSimpleEdits :: [(DI, a)] → [SimpleEdit a]
diffsAsSimpleEdits = work 0 where
  work _ [] = []
  work n ((F, _) : r) = SimpleEdit (Range n 1) [] : work (n + 1) r
  work n ((S, x) : r) = SimpleEdit (Range n 0) [x] : work n r
  work n ((B, _) : r) = work (n + 1) r

merge_nearby_edits :: [a] → [SimpleEdit a] → [SimpleEdit a]
  -- Also produces replace edits from insert+erase edits.
merge_nearby_edits _ [] = []
merge_nearby_edits _ [x] = [x]
merge_nearby_edits s (SimpleEdit (Range b e) r : SimpleEdit (Range b' e') r' : m) | b' - e - b ≤ 1 =
  merge_nearby_edits s $ SimpleEdit (Range b (b' - b + e')) (r ++ take (max 0 $ b' - e - b) (drop (b + e) s) ++ r') : m
merge_nearby_edits s (e : m) = e : merge_nearby_edits s m

diff_tokenize :: String → [Token]
diff_tokenize = g . f . edit_tokens isIdChar
  where
    separator = (∈ words ", { } ( ) ; = friend <<")
    f :: [String] → [Token]
    f [] = []
    f (s : t@(' ':_) : m) = Token s t : f m
    f (s : m) = Token s "" : f m
    g :: [Token] → [Token]
    g [] = []
    g (t@(Token "<<" _) : r) = case g r of [] → [t]; (u : m) → mappend t u : m
    g (t : r) | separator (tok_text t) = t : g r
    g a =
      let (b, c) = break (separator . tok_text) a in
      let (d, e) = splitAt 5 b in
      mconcat d : g (e ++ c)
