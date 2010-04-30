{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances, CPP, PatternGuards, ViewPatterns #-}

module Editing.EditsPreparation (use_tests, findInStr, FindResult(..), FoundIn(..)) where

import qualified Cxx.Basics
import qualified Cxx.Show
import qualified Cxx.Operations
import qualified Editing.Diff
import qualified Editing.Show
import qualified Data.List as List
import qualified Data.Char as Char
import Data.Foldable (toList)
import Data.Traversable (forM, mapM, sequence)
import Data.List.NonEmpty ((|:), toNonEmpty, neHead, neTail)
import Control.Monad (liftM2, join)
import Control.Monad.Error ()
import Data.SetOps
import Util ((.), Convert(..), Op(..), ops_cost, erase_indexed, levenshtein, replaceAllInfix, approx_match, Cost, Invertible(..), Ordinal(..), test_cmp, multiplicative_numeral, E, or_fail, pairs, NeList, neElim, neHomogenize, safeNth)

-- One property that might be suitable for formal verification is that finders only return anchor/ranges/edits contained in the range they received, and that no anchor/range/edit ever goes out of bounds.

#define case_of \case_of_detail → case case_of_detail of
  -- Todo: Move to utility header..

import Prelude hiding (last, (.), all, (!!), sequence, mapM)
import Prelude.Unicode hiding ((∈))
import Editing.Basics

import Control.Monad.Reader (ReaderT(..), local, ask)

{- The present module concerns the translation of commands into Edits. The translation mostly follows the grammatical structure of commands, and so we get translators for various kinds of clauses, most of which will actually yield (lists of) positions and/or ranges rather than Edits, with only the topmost translators yielding actual Edits.

Since most edit clauses refer to parts of a subject snippet, the translation from commands to edits is always performed in the context of such a subject. In addition, a context consists of several more things: -}

data ResolutionContext = ResolutionContext
  { context_suffix :: String
  , _given :: String
  , search_range :: Range Char -- Todo: Should this not be an ARange?
  , well_formed :: E (Cxx.Basics.GeordiRequest, Anchor → E Anchor)
  }

-- We will explain each of these fields in more detail, but first introduce a type class for translators, which are just functions run in a reader monad providing the above context, returning types in a certain class.

type Resolver = ReaderT ResolutionContext E

class InGiven_to_InWf b ⇒ Find a b | a → b where find :: a → Resolver b

-- We will describe the InGiven_to_InWf class in a moment. Some fairly obvious Find instances are:

instance (Find x a, Find y a) ⇒ Find (Either x y) a where find = either find find
instance Find a b ⇒ Find (AndList a) (NeList b) where find = sequence . (find .) . andList

-- The _given and search_range fields in ResolutionContext simply specify a string and subrange of that string for the finder to search in. The context_suffix field describes the context (e.g. "after third statement in body of f"). Its only purpose is to make for nicer error messages: when Find instances fail, context_suffix lets us produce error messages like "Could not find `beh` after third statement in body of f."

fail_with_context :: String → Resolver a
fail_with_context s = (s ++) . context_suffix . ask >>= fail

-- Find instances for things like Relative typically invoke Find instances for constituent clauses on subranges of the range they received themselves. For this we define |narrow|, which simultaneously modifies the search_range and extends the context_suffix:

narrow :: String → Range Char → Resolver a → Resolver a
narrow x y = local $ \(ResolutionContext z v _ w) → ResolutionContext (" " ++ x ++ z) v y w

{- To motivate the well_formed field in ResolutionContext and the InGiven_to_InWf class, we must first describe some general edit command properties we desire.

Consider the perfectly reasonable composite command "erase first x and move second x to end" executed on the snippet "xyxy". Clearly, we want the result to be "yyx". This means we cannot just execute the two commands in sequence in isolation, because once the first 'x' has been erased, there no longer /is/ a second 'x' to move. We conclude that whenever possible, all commands in a composite command should be translated to edits in the context of the same original snippet. The edits from the different commands should then be merged intelligently (this is done by the Execute module).

Now consider the composite command "add ) after ( and erase declaration of i" executed on the snippet "void f(; int i;". Following the principle above to the letter, we should look for a declaration of i in the original snippet. However, since the original snippet does not parse, this would result in an error. This is really unfortunate, because the whole point of the "add ) after (" edit was to make the snippet well-formed. Hence, what we really want is for non-semantic things to be resolved in the original snippet as usual, but for semantic things to be looked up in the original snippet with the fewest number of preceding edits needed to make it well formed applied to it, so that in the above example, the declaration of i is looked for in the snippet as it appears after the "add ) after (" command has been applied to it.

Following this idea, a Find instance that needs to do a semantic look-up should not just try to parse _given, but should have access to this notion of "the original snippet with the fewest number of preceding edits needed to make it well formed applied to it" (let us call this "the well-formed snippet"), and that is exactly what the well_formed field is. The 'E' monad is there because there simply may not be a sequence of preceding edits that make the snippet well-formed. The Anchor transformer translates an Anchor in _given to an Anchor in well-formed snippet, encapsulating the actual edits that turn the former into the latter. In particular, the Anchor transformer may be applied to search_range to give the range to search in the well-formed snippet.

The anchor/ranges/edits a Find instance might find in well_formed are obviously relative to the well-formed snippet, not to _given (unless they are the same--more about that later). To inform the caller of what a returned anchor/range/edit is relative to, these are wrapped in FindResults: -}

data FoundIn = InGiven | InWf deriving Eq
data FindResult a = Found FoundIn a

instance Functor FindResult where fmap f (Found x y) = Found x (f y)

-- Before we explain the InGiven_to_InWf constraint and some of the finer _given vs well_formed points, let us look at some actual Find instances, starting with the one for verbatim strings.

instance Find String (NeList (FindResult DualARange)) where
  find x = do
    ResolutionContext _ s r _ ← ask
    case toNonEmpty $ find_occs x $ selectRange r s of
      Nothing → fail_with_context $ "String `" ++ x ++ "` does not occur"
      Just l → return $ (Found InGiven . convert . (\o → arange (Anchor After $ start r + o) (Anchor Before $ start r + o + length x))) . l

{- Since no semantic lookup is needed, Find String only looks in _given, of which it informs its caller by returning values marked Found InGiven. DualARange and Anchor sidedness is described in Editing.Basics.

For our next example, we consider the Find instance for "in"-clauses: -}

instance (Find a (NeList b)) ⇒ Find (In a) (NeList b) where
  find (In o Nothing) = find o
  find (In o (Just incl)) = ((full_range .) .) . find incl >>= (join .) . mapM (\(Found a x) →
    (case a of InGiven → id; InWf → inwf) $ narrow (Editing.Show.show incl) (convert x) $ find o)

-- For the nontrivial case, we first simply search for incl, which yields a number of DualARanges, which we map to their full_range components. Then, for each ARange x that was found, we distinguish between two cases. If x is relative to the current _given, we just use |narrow| to focus our attention on x, and try to find |o| there. If x is relative to the well-formed snippet, then we should find |o| in there, too. So in this case, we want to force the Find instance for |o| to search in the well-formed snippet. We do this by first changing _given to the well-formed snippet and setting the Anchor transformer in well_formed to |return|, and then proceeding with |narrow| as before. We realize this with the following utility function:

inwf :: InGiven_to_InWf a ⇒ Resolver a → Resolver a
inwf re = ReaderT $ \(ResolutionContext w _ r wf) → do
  (tree, anchor_trans) ← or_fail wf
  Anchor _ a ← anchor_trans $ Anchor Before $ start r
  Anchor _ b ← anchor_trans $ Anchor Before $ end r
  (inGiven_to_inWf .) $ runReaderT re $ ResolutionContext w
    (Cxx.Show.show_simple tree) (Range a (b - a)) (Right (tree, return))

-- Results returned by the re-contexted resolver may be marked as Found InGiven, but since we changed _given to the well-formed snippet, these are really Found InWf, so inwf should adjust them, and that's where the InGiven_to_InWf class comes in.

class InGiven_to_InWf a where inGiven_to_inWf :: a → a

instance InGiven_to_InWf (FindResult a) where inGiven_to_inWf (Found _ x) = Found InWf x
instance InGiven_to_InWf (Range Char) where inGiven_to_inWf = id
instance InGiven_to_InWf a ⇒ InGiven_to_InWf (NeList a) where inGiven_to_inWf = fmap inGiven_to_inWf
instance InGiven_to_InWf a ⇒ InGiven_to_InWf [a] where inGiven_to_inWf = fmap inGiven_to_inWf

-- Next, we look at a Find instance for a typically semantic thing:

instance Find Cxx.Basics.Findable (NeList (FindResult DualARange)) where
  find d = inwf $ do
    (tree, _) ← well_formed . ask >>= or_fail
    r ← search_range . ask
    case toNonEmpty $ filter ((`contained_in` r) . fst) $ Cxx.Operations.find d tree of
      Nothing → fail_with_context $ "Could not find " ++ show d
      Just l → return $ fmap (\(q, r'@(Range u h)) →
        let m = length $ takeWhile (==' ') $ reverse $ selectRange r' (Cxx.Show.show_simple tree) in
          Found InWf $ DualARange (convert q) (convert $ Range u (h-m))) l

{- Here, we immediately go into wf and do all the work there.

In several other places we can see given-vs.-wf considerations: -}

instance (Invertible a, Find a b, Convert (FindResult ARange) b) ⇒ Find (Relative a) (NeList b) where
  find (Absolute x) = return . find x
  find (Relative o (AndList bas) w) = do
    Found c (Range st si) ← ((unanchor_range . full_range) .) . find w
    (case c of InGiven → id; InWf → inwf) $ do
    Range a b ← search_range . ask
    forM bas $ \ba → do
      let h = Editing.Show.show ba ++ " " ++ Editing.Show.show w
      case ba of
        Before → narrow h (Range a (st - a)) $ find (invert o)
        After → narrow h (Range (st + si) ((a + b) - (st + si))) $ find o
  find (FromTill b e) = do
    Found c p'@(Anchor _ p) ← (either ($ Before) id .) . find b
    (case c of InGiven → id; InWf → inwf) $ do
    Range st si ← search_range . ask
    narrow ("after " ++ Editing.Show.show b) (Range p (st + si - p)) $ do
    Found d y ← (either ($ After) id .) . find e
    return . convert . Found d . flip arange y . (case d of InGiven → return; InWf → toWf) p'
  find (Between o be@(Betw b e)) = do
    Found c x ← find b
    Found d y ← find e
    x' ← (if (c, d) == (InGiven, InGiven) ∨ c == InWf then return else toWf) x
    y' ← (if (c, d) == (InGiven, InGiven) ∨ d == InWf then return else toWf) y
    (if (c, d) == (InGiven, InGiven) then id else inwf) $ do
    let (p, q) = if either ($ Before) id x' ≤ either ($ Before) id y' then (x', y') else (y', x')
    narrow (Editing.Show.show be) (convert $ arange (either ($ After) id p) (either ($ Before) id q)) $ return . find o

-- More documentation some other time!

findInStr :: Find a b ⇒ String → (E (Cxx.Basics.GeordiRequest, Anchor → E Anchor)) → a → E b
findInStr s e x = runReaderT (find x) $ ResolutionContext "." s (Range 0 $ length s) e

instance Find (Around Substrs) (NeList (FindResult DualARange)) where find (Around x) = find x

instance Convert (FindResult ARange) (NeList (FindResult DualARange)) where
  convert (Found c x) = return $ Found c $ convert x

instance Find Substrs (NeList (FindResult DualARange)) where
  find (Substrs l) = join . join . find l

instance Find MakeSubject (NeList (FindResult DualARange)) where
  find (MakeSubject l) = join . join . find l

class OccurrenceError a where
  doesNotOccur_n_times :: a → Int → String
  multipleOccur :: a → String

instance OccurrenceError String where
  doesNotOccur_n_times s n = "String `" ++ s ++ "` does not occur " ++ multiplicative_numeral (if n < 0 then -n else n+1)
  multipleOccur s = "String `" ++ s ++ "` occurs multiple times"

instance OccurrenceError Cxx.Basics.Findable where
  doesNotOccur_n_times s n = "Could not find a " ++ show (Ordinal n) ++ " " ++ Editing.Show.show s
  multipleOccur s = "Multiple " ++ Cxx.Show.show_plural s ++ " occur"

instance (OccurrenceError a, OccurrenceError b) ⇒ OccurrenceError (Either a b) where
  doesNotOccur_n_times = either doesNotOccur_n_times doesNotOccur_n_times
  multipleOccur = either multipleOccur multipleOccur

instance Editing.Show.Show a ⇒ OccurrenceError a where
  doesNotOccur_n_times s n = Editing.Show.show s ++ " does not occur " ++ multiplicative_numeral (if n < 0 then -n else n+1)
  multipleOccur s = Editing.Show.show s ++ " occurs multiple times"

instance (OccurrenceError a, Find a (NeList (FindResult DualARange))) ⇒ Find (Ranked a) (FindResult DualARange) where
  find (Sole x) = find x >>= \l → if null (neTail l) then return $ neHead l else fail_with_context $ multipleOccur x
  find (Ranked (Ordinal n) s) = safeNth n . toList . find s >>= maybe (fail_with_context $ doesNotOccur_n_times s n) return

instance (OccurrenceError a, Find a (NeList (FindResult DualARange))) ⇒ Find (Rankeds a) (NeList (FindResult DualARange)) where
  find (All x) = find x
  find (Sole' x) =
    find x >>= \l → if null (neTail l) then return l else fail_with_context $ multipleOccur x
  find (Rankeds rs s) = sequence ((\r → find (Ranked r s)) . flatten_occ_clauses rs)
  find (AllBut rs s) =
    erase_indexed (ordinal_carrier . toList (flatten_occ_clauses rs)) . toList . find s >>= case_of
      [] → fail "All occurrences excluded." -- Todo: Better error.
      x:y → return $ x |: y

flatten_occ_clauses :: AndList OccurrencesClause → NeList Ordinal
flatten_occ_clauses (AndList rs) = join $ (\(OccurrencesClause l) → l) . rs

findResult_as_either :: FindResult a → Either a a
findResult_as_either (Found c a) = (case c of InGiven → Left; InWf → Right) a

merge_contiguous_FindResult_ARanges :: NeList (FindResult ARange) → Resolver (FindResult (NeList ARange))
merge_contiguous_FindResult_ARanges l =
  neHomogenize toWf (findResult_as_either . l) >>= case_of
    Left xs → return $ Found InGiven $ merge_contiguous xs
    Right xs → return $ Found InWf $ merge_contiguous xs
  -- This is not optimal, because wf-ness of one contiguous range should not imply wf-ness of all ranges.

instance Find Substr (FindResult DualARange) where
  find Everything = Found InGiven . convert . wide_range . search_range . ask
  find (NotEverything x) = find x

instance Find (EverythingOr (Rankeds (Either Cxx.Basics.Findable String))) (NeList (FindResult DualARange)) where
  find Everything = return . Found InGiven . convert . wide_range . search_range . ask
  find (NotEverything x) = find x

instance Find (EverythingOr (Rankeds (Either Cxx.Basics.Findable ImplicitDeclarationOf))) (NeList (FindResult DualARange)) where
  find Everything = return . Found InGiven . convert . wide_range . search_range . ask
  find (NotEverything x) = find x

instance Find ImplicitBodyOf (NeList (FindResult DualARange)) where
  find (ImplicitBodyOf x) = find $ Cxx.Basics.BodyOf x

instance Find ImplicitDeclarationOf (NeList (FindResult DualARange)) where
  find (ImplicitDeclarationOf x) = find $ Cxx.Basics.DeclarationOf x

instance Find InClause (NeList (FindResult DualARange)) where find (InClause x) = join . join . find x

instance Find AppendPositionsClause (NeList (FindResult Anchor)) where
  find (NonAppendPositionsClause pc) = find pc
  find (AppendIn incl) = (((($ After) . full_range) .) .) . find incl

instance Find PrependPositionsClause (NeList (FindResult Anchor)) where
  find (NonPrependPositionsClause pc) = find pc
  find (PrependIn incl) = (((($ Before) . full_range) .) .) . find incl

instance Find PositionsClause (NeList (FindResult Anchor)) where
  find (PositionsClause (AndList bas) x) = do
    Found w l ← ((full_range .) .) . find x >>= merge_contiguous_FindResult_ARanges
    return $ l >>= (\e → (\ba → Found w $ e ba) . bas)

instance Find Replacer (NeList (FindResult Edit)) where
  find (Replacer p r) = do
    Found c v ← ((replace_range .) .) . find p >>= merge_contiguous_FindResult_ARanges
    return $ ((flip RangeReplaceEdit r . unanchor_range) .) . Found c . v
  find (ReplaceOptions o o') = return $ fmap (Found InGiven) $ RemoveOptions o |: [AddOptions o']

instance Find Changer (NeList (FindResult Edit)) where
  find (Changer p r) = find (Replacer p r)
  find (ChangeOptions o o') = find (ReplaceOptions o o')

instance Find Eraser [FindResult Edit] where
  find (EraseText x) = (((flip RangeReplaceEdit "" . unanchor_range . full_range) .) .) . toList . find x
  find (EraseOptions o) = return [Found InGiven $ RemoveOptions o]
  find (EraseAround (Wrapping x y) (Around z)) = do
    l ← (((unanchor_range . full_range) .) .) . toList . find z
    (concat .) $ forM l $ \(Found v (Range p q)) →
      (case v of InGiven → id; InWf → inwf) $ do
      Range a b ← search_range . ask
      (concat .) $ forM [(Before, x, -1, Range a (p-a)), (After, y, 0, Range (p+q) (a+b-(p+q)))] $ \(ba, xy, i, r) →
        narrow (Editing.Show.show ba ++ " " ++ Editing.Show.show z) r $
          find $ EraseText $ Substrs $ and_one $ flip In Nothing $ Absolute $ NotEverything $ Rankeds (and_one $ OccurrencesClause $ return $ Ordinal i) (Right xy)

instance Find Bound (FindResult (Either ARange Anchor)) where
  find (Bound Nothing Everything) = Found InGiven . Left . arange (Anchor Before 0) . Anchor After . size . search_range . ask
  find (Bound (Just Before) Everything) = return $ Found InGiven $ Right $ Anchor Before 0
  find (Bound (Just After) Everything) = Found InGiven . Right . Anchor After . size . search_range . ask
  find (Bound mba p) = ((maybe Left (\ba → Right . ($ ba)) mba . full_range) .) . find p

instance Find RelativeBound (FindResult (Either ARange Anchor)) where
  find Front = Found InGiven . Right . Anchor Before . start . search_range . ask
  find Back = Found InGiven . Right . Anchor After . end . search_range . ask
  find (RelativeBound mba p) = find p >>= \l → if null (neTail l)
    then return $ maybe Left (\ba → Right . ($ ba)) mba . full_range . neHead l
    else fail "Relative bound must be singular."

class ToWf a where toWf :: a → Resolver a

instance ToWf Anchor where toWf a = (((($ a) . snd) .) . well_formed . ask >>= or_fail) >>= or_fail
instance ToWf ARange where
  toWf a = do
    f ← (snd .) . well_formed . ask >>= or_fail
    liftM2 arange (or_fail (f $ a Before)) (or_fail (f $ a After))

instance (ToWf a, ToWf b) ⇒ ToWf (Either a b) where
  toWf (Left x) = Left . toWf x
  toWf (Right x) = Right . toWf x

makeMoveEdit' :: FindResult Anchor → FindResult ARange → Resolver (FindResult Edit)
makeMoveEdit' (Found InGiven a) (Found InGiven r) = Found InGiven . makeMoveEdit a (unanchor_range r)
makeMoveEdit' (Found InWf a) (Found c x) = do
  r ← (case c of InGiven → toWf; InWf → return) x
  Found InWf . makeMoveEdit a (unanchor_range r)
makeMoveEdit' (Found c x) (Found InWf r) = do
  a' ← (case c of InGiven → toWf; InWf → return)  x
  Found InWf . makeMoveEdit a' (unanchor_range r)

makeSwapEdit :: FindResult ARange → FindResult ARange → Resolver [FindResult Edit]
makeSwapEdit a b = do
  some ← makeMoveEdit' (($ Before) . b) a
  more ← makeMoveEdit' (($ Before) . a) b
  return [some, more]

instance Find Mover [FindResult Edit] where
  find (Mover o p) = do
    a ← find p
    toList . find o >>= mapM (makeMoveEdit' a . (full_range .)) . reverse

instance Find Position (FindResult Anchor) where
  find (Position ba x) = find x >>= \l → if null (neTail l)
    then return $ flip full_range ba . (neHead l)
    else fail "Anchor position must be singular."

instance Find UsePattern (FindResult (Range Char)) where
  find (UsePattern z) = do
    ResolutionContext _ s r _ ← ask
    let
      text_tokens = edit_tokens Char.isAlphaNum $ selectRange r s
      pattern_tokens = edit_tokens Char.isAlphaNum z
      (x, y) = (sum $ length . take stt text_tokens, sum $ length . take siz (drop stt text_tokens))
      (owc, stt, siz) = head $ approx_match token_edit_cost pattern_tokens (replaceAllInfix pattern_tokens (replicate (length pattern_tokens) (replicate 100 'X')) text_tokens)
    if y == 0 ∨ ops_cost owc > fromIntegral (length z) / 1.5 then fail_with_context $ "No non-exact match for " ++ z else return $ Found InGiven $ offset (start r) $ Range x y

instance Invertible UsePattern where invert = id

instance Convert (FindResult (Range Char)) (FindResult (Range Char)) where convert = id

instance Find UseClause (NeList (FindResult Edit)) where
  find (UseOptions o) = return $ return $ Found InGiven $ AddOptions o
  find (UseString ru@(In b _)) = case unrelative b of
    Nothing → fail "Nonsensical use-command."
    Just (UsePattern v) → (((flip RangeReplaceEdit v) .) .) . find ru

token_edit_cost :: Op String → Cost
token_edit_cost (SkipOp (' ':_)) = 0
token_edit_cost (SkipOp x) | x ∈ Cxx.Basics.keywords = -2.4
token_edit_cost (SkipOp (h:t)) | Char.isAlphaNum h = -2.2 - fromIntegral (length t) * 0.2
token_edit_cost (SkipOp _) = -2
token_edit_cost (EraseOp (' ':_)) = 0.02
token_edit_cost (EraseOp x) = token_edit_cost (InsertOp x)
token_edit_cost (InsertOp t) | t ∈ Cxx.Basics.keywords = 2
token_edit_cost (InsertOp (' ':_)) = -0.02
token_edit_cost (InsertOp x@(y:_)) | Char.isAlpha y = fromIntegral (length x) * 0.7
token_edit_cost (InsertOp (x:y)) | Char.isDigit x = 1 + fromIntegral (length y) * 0.3
token_edit_cost (InsertOp _) = 1
token_edit_cost (ReplaceOp x y)
  | or $ (\c → List.all (∈ c) [x, y]) . [Cxx.Basics.classKeys, Cxx.Basics.accessSpecifiers, Cxx.Basics.relational_ops] = 0.4
token_edit_cost (ReplaceOp (c:_) (d:_)) | not $ Char.isAlphaNum c ∨ Char.isAlphaNum d = 1.1
token_edit_cost (ReplaceOp x@(c:_) y@(d:_)) | Char.isAlpha c, Char.isAlpha d =
  if null (x ∩ y) then 10 else levenshtein x y * 0.4
token_edit_cost (ReplaceOp x@(c:_) y@(d:_)) | Char.isAlphaNum c, Char.isAlphaNum d = levenshtein x y * 0.8
token_edit_cost (ReplaceOp _ _) = 10
  -- The precise values of these costs are fine-tuned to make the tests pass, and that is their only justification. We're trying to approximate the human intuition for what substring should be replaced, as codified in the tests.

instance Find Command [FindResult Edit] where
  find (Use l) = toList . join . find l
  find (Append x Nothing) = do
    r ← search_range . ask
    return [Found InGiven $ InsertEdit (Anchor After (size r)) x]
  find (Prepend x Nothing) = return [Found InGiven $ InsertEdit (Anchor Before 0) x]
  find (Append r (Just p)) = toList . ((flip InsertEdit r .) .) . join . find p
  find (Prepend r (Just p)) = toList . ((flip InsertEdit r .) .) . join . find p
  find (Erase (AndList l)) = concat . sequence (find . toList l)
  find (Replace (AndList l)) = concat . sequence ((toList .) . find . toList l)
  find (Change (AndList l)) = concat . sequence ((toList .) . find . toList l)
  find (Insert (SimpleInsert r) p) = toList . ((flip InsertEdit r .) .) . join . find p
  find (Insert (WrapInsert (Wrapping x y)) (AndList z)) =
    concatMap (\(Found v a, Found w b) → [Found v $ InsertEdit a x, Found w $ InsertEdit b y]) . pairs . concat . map toList . sequence (map find $ toList z)
  find (Move (AndList movers)) = concat . sequence (find . toList movers)
  find (Swap substrs Nothing) = toList . ((replace_range .) .) . find substrs >>= f
    where
      f [] = return []
      f (a:b:c) = liftM2 (++) (makeSwapEdit a b) (f c)
      f _ = fail "Cannot swap uneven number of operands."
  find (Swap substrs (Just substrs')) = do
    Found v x ← ((full_range .) .) . find substrs >>= merge_contiguous_FindResult_ARanges
    Found w y ← ((full_range .) .) . find substrs' >>= merge_contiguous_FindResult_ARanges
    let a = Found v . x; b = Found w . y
    if null (neTail a) && null (neTail b) then makeSwapEdit (neHead a) (neHead b)
     else fail "Swap operands must be contiguous ranges."
  find (Make s b) = inwf $ do
    (tree, _) ← well_formed . ask >>= or_fail
    l ← (fmap (\(Found _ x) → replace_range x)) . find s
    (Found InGiven .) . concat . toList . forM l (\x →
      Cxx.Operations.make_edits (convert x) b 0 tree)

use_tests :: IO ()
use_tests = do
  t "ETYPE_DESC" "ETPYE" "Replaced `<< ETPYE` with `<< ETYPE_DESC`." "Replaced `<< ETYPE_DESC` with `<< ETPYE`."
  t "kip(a.~T)" "a.~T" "Replaced a.~T with kip(a.~T)." "Replaced kip(a.~T) with a.~T."
  -- t "cos(a.~T)" "a.~T" -- Fails, but can probably be made to work by rewarding successive skips.
  t "size_type" "size_t" "Replaced `string::size_t- siz` with `string::size_type- siz`." "Replaced `string::size_type- siz` with `string::size_t- siz`."
  t "size = 9" "siz = 2" "Replaced `string::size_t- siz = 2` with `string::size_t- size = 9`." "Replaced `string::size_t- size = 9` with `string::size_t- siz = 2`."
  t "ETYPE" "ETPYE" "Replaced `<< ETPYE` with `<< ETYPE`." "Replaced `<< ETYPE` with `<< ETPYE`."
  t "std::string" "string" "Replaced `string::size_t- siz` with `std::string::size_t- siz`." "Replaced `std::string::size_t- siz` with `string::size_t- siz`."
  t "; float x" "; int x" "Replaced `int x` with `float x`." "Replaced `float x` with `int x`."
  t "x-" "x -" "Replaced `x - size` with `x- size`." "Replaced `x- size` with `x - size`."
  t ") cin <<" ") cout <<" "Replaced cout with cin." "Replaced cin with cout."
  t "x = 4" "x = 3" "Replaced 3 with 4." "Replaced 4 with 3."
  t "x - 8);" "x - size);" "Replaced `x - size` with `x - 8`." "Replaced `x - 8` with `x - size`."
  t "(!i)" "(i == 0)" "Replaced `i == 0` with !i."  "Replaced !i with `i == 0`."
  t "seekp" "seek" "Replaced a.seek with a.seekp." "Replaced a.seekp with a.seek."
  t "<char>" "<unsigned char>" "Replaced `vector<unsigned char> & r` with `vector<char> & r`." "Replaced `vector<char> & r` with `vector<unsigned char> & r`."
  t "<const fish>" "<fish>" "Replaced `reinterpret_cat<fish>` with `reinterpret_cat<const fish>`." "Replaced `reinterpret_cat<const fish>` with `reinterpret_cat<fish>`."
  t "&); };" "&) };" "Inserted semicolon after `C const &)`." "Erased semicolon after `C const &)`."
  t "> * r = v" "> & r = v" "Replaced `& r` with `* r`." "Replaced `* r` with `& r`."
  t "v.cbegin()" "v.begin()" "Replaced v.begin with v.cbegin." "Replaced v.cbegin with v.begin."
  -- Todo: "void foo" should match "voidfoo".
  t "x - sizeof(y))" "x - size)" "Replaced `x - size` with `x - sizeof(y)`." "Replaced `x - sizeof(y))` with `x - size)`."
  t "int a(2);" "int a;" "Inserted (2) after `{ int a`." "Erased (2) after `{ int a`."
  t "int const * w" "int * w" "Replaced `int * w` with `int const * w`." "Replaced `int const * w` with `int * w`."
  t "main(int argc) {" "main() {" "Inserted `int argc` after `void main(`." "Erased `int argc`."
  t "_cast" "_cat" "Replaced `reinterpret_cat<fish>` with `reinterpret_cast<fish>`." "Replaced `reinterpret_cast<fish>` with `reinterpret_cat<fish>`."
  t "(++a)" "(a++)" "Replaced a++ with ++a." "Replaced ++a with a++."
  t "list<int>" "vector<int>" "Replaced `vector<int> v` with `list<int> v`." "Replaced `list<int> v` with `vector<int> v`."
  t "a->seekp" "a.seek" "Replaced a.seek with a->seekp." "Replaced a->seekp with a.seek."
  t "vector<int>::iterator i" "vector<int> i" "Replaced `vector<int> i` with `vector<int>::iterator i`." "Replaced `vector<int>::iterator i` with `vector<int> i`."
  t "runtime_error(" "runtime_exception(" "Replaced `throw runtime_exception` with `throw runtime_error`." "Replaced `throw runtime_error` with `throw runtime_exception`."
  t "~T();" "~T;" "Inserted () after `) { a.~T`." "Erased () after `) { a.~T`." -- Todo: ugly.
  t "int const * w" "int * w" "Replaced `int * w` with `int const * w`." "Replaced `int const * w` with `int * w`."
  t "(T & a)" "(T a)" "Replaced `T a` with `T & a`." "Replaced `T & a` with `T a`."
  t "& r(v);" "& r = v;" "Replaced `= v` after `vector<unsigned char> & r` with (v)." "Replaced (v) after `vector<unsigned char> & r` with `= v`."
  t "ios_base::end_t" "ios::end" "Replaced ios::end with `ios_base::end_t`.""Replaced `ios_base::end_t` with ios::end."
  t "95" "94" "Replaced 94 with 95." "Replaced 95 with 94."
  t "vector<int> const v { 3, 2 };" "vector<int> v; v = { 3, 2 };" "Replaced `vector<int> v; v =` with `vector<int> const v`." "Replaced `vector<int> const v` with `vector<int> v; v =`."
  t "class C" "struct C" "Replaced `struct C` with `class C`." "Replaced `class C` with `struct C`."
  t "B z{p};" "B z = B{p};" "Erased `= B` after `B z`." "Inserted `= B` after `B z`."
  t "friend C & operator+" "C & operator+" "Inserted friend before `C & operator+`." "Erased friend before `C & operator+`."
  t "char const(&here)[N]" "char(const&here)[N]" "Replaced `char(const&here` with `char const(&here`." "Replaced `char const(&here` with `char(const&here`."
  t "z = shared_ptr<B>{new p}" "z = B{p}" "Replaced B{p with `shared_ptr<B>{new p`." "Replaced `shared_ptr<B>{new p` with B{p." -- Todo: ugly.
  t "(X(y));" "X(y);" "Inserted ( before X(y) and inserted ) after `} X(y)`." "Erased ( before X(y)) and ) after `} (X(y)`." -- Todo: ugly.
  t "2000" "1800" "Replaced 1800 with 2000." "Replaced 2000 with 1800."
  t "8000100808" "10000000000" "Replaced 10000000000 with 8000100808." "Replaced 8000100808 with 10000000000."
  t "> 7" ">= 7" "Replaced `x >= 7` with `x > 7`." "Replaced `x > 7` with `x >= 7`."
  t "private: fstream" "public: fstream" "Replaced `public: fstream p` with `private: fstream p`." "Replaced `private: fstream p` with `public: fstream p`." -- Todo: "replaced public: with private: before fstream p".
  t "int main" "void main" "Replaced `void main` with `int main`." "Replaced `int main` with `void main`." -- Todo: One day this should say: "Made main return int."
  t "<char>" "<unsigned char>" "Replaced `vector<unsigned char> & r` with `vector<char> & r`." "Replaced `vector<char> & r` with `vector<unsigned char> & r`."
  t "int const u =" "int x =" "Replaced `int x` with `int const u`." "Replaced `int const u` with `int x`."
  t "u - -j" "u--j" "Replaced &u--j with `&u - -j`." "Replaced `&u - -j` with &u--j."
  t "struct C{" "struct C(){" "Erased () after `struct C`." "Inserted () after `struct C`."
  --t "&ETPYE" "ETPYE" "Replaced ETPYE with &ETPYE." "Replaced &ETPYE with ETPYE."
  putStrLn "All use tests passed."
 where
  u :: String → String → String → String → String → IO ()
  u txt pattern match d rd =
    case runReaderT (find (UseString $ flip In Nothing $ Absolute $ UsePattern pattern)) (ResolutionContext "." txt (Range 0 (length txt)) (fail "-")) of
      Left e → fail e
      Right (neElim → (Found _ (RangeReplaceEdit rng _), [])) → do
        test_cmp pattern match (selectRange rng txt)
        let r = replaceRange rng pattern txt
        test_cmp pattern d $ show $ Editing.Diff.diff txt r
        test_cmp (pattern ++ " (reverse)") rd $ show $ Editing.Diff.diff r txt
      _ → error "should not happen"
  t :: String → String → String → String → IO ()
  t = u "{ string::size_t- siz = 2; int x = 3; if(i == 0) cout << ETPYE(x - size); vector<int> v; v = { 3, 2 }; vector<int> i = reinterpret_cat<fish>(10000000000, v.begin()); } X(y); using tracked::B; B z = B{p}; int const u = 94; int * w = &u--j; !B && !D; vector<unsigned char> & r = v; struct C(){ C & operator+(ostream &, char(const&here)[N], C const &) }; template<typename T> voidfoo(T a) { a.~T; } void main() { int a; a.seek(1800, ios::end); foo(a++); if(x >= 7) throw runtime_exception(y); } class Qbla { public: fstream p; };"
