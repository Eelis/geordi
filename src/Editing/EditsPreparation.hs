{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances, PatternGuards #-}
{-# OPTIONS -cpp #-}

module Editing.EditsPreparation (use_tests, findInStr) where

import qualified Cxx.Basics
import qualified Cxx.Parse
import qualified Cxx.Show
import qualified Cxx.Operations
import qualified Editing.Diff
import qualified Editing.Show
import qualified Data.List as List
import qualified Data.NonEmptyList as NeList
import qualified Data.Char as Char
import qualified Editing.Show
import Data.Maybe (mapMaybe)
import Control.Monad (liftM2)
import Control.Monad.Error ()
import Data.NonEmptyList (NeList (..))
import Util ((.), Convert(..), Op(..), ops_cost, erase_indexed, levenshtein, replaceAllInfix, approx_match, Cost, Invertible(..), Ordinal(..), test_cmp, multiplicative_numeral)

#define case_of \case_of_detail -> case case_of_detail of

import Prelude hiding (last, (.), all, (!!))
import Editing.Basics

import Control.Monad.Reader (ReaderT(..), local, ask)

class Offsettable a where offset :: Int -> a -> a

instance Offsettable (Range Char) where offset x (Range y z) = Range (y + x) z
instance Offsettable Anchor where offset x (Anchor y z) = Anchor y (z + x)
instance Offsettable ARange where offset x r = offset x . r
instance (Offsettable a, Functor f) => Offsettable (f a) where offset = fmap . offset

instance Convert (Range a) [ARange] where convert = (:[]) . anchor_range
instance Convert (Range a) ARange where convert = anchor_range
instance Convert ARange (Range a) where convert = unanchor_range

data ResolutionContext = ResolutionContext { context_suffix :: String, _original :: String, search_range :: Range Char }
type Resolver = ReaderT ResolutionContext

findInStr :: (Find a b, Functor m, Monad m) => String -> a -> m b
findInStr s x = runReaderT (find x) $ ResolutionContext "." s $ Range 0 $ length s

class Find a b | a -> b where find :: (Functor m, Monad m) => a -> Resolver m b

fail_with_context :: Monad m => String -> Resolver m a
fail_with_context s = do
  w <- context_suffix . ask
  fail $ s ++ w

narrow :: Monad m => String -> Range Char -> Resolver m a -> Resolver m a
narrow w' r' = local $ \(ResolutionContext w s _) -> ResolutionContext (" " ++ w' ++ w) s r'

-- We pass the whole original string because it may need to be parsed.

instance Find (Around Substrs) (NeList ARange) where find (Around x) = find x
instance (Find x a, Find y a) => Find (Either x y) a where find = either find find
instance Find a b => Find (AndList a) (NeList b) where find (AndList l) = NeList.sequence $ find . l

instance Convert (Range Char) (NeList ARange) where convert = NeList.one . anchor_range

instance Find Substrs (NeList ARange) where find (Substrs l) = NeList.concat . NeList.concat . find l

class OccurrenceError a where
  doesNotOccur_n_times :: a -> Int -> String
  multipleOccur :: a -> String

instance OccurrenceError String where
  doesNotOccur_n_times s n = "String `" ++ s ++ "` does not occur " ++ multiplicative_numeral (if n < 0 then -n else n+1)
  multipleOccur s = "String `" ++ s ++ "` occurs multiple times"

instance OccurrenceError Cxx.Basics.Findable where
  doesNotOccur_n_times s n = "Could not find a " ++ show (Ordinal n) ++ " " ++ Editing.Show.show s
  multipleOccur s = "Multiple " ++ Cxx.Show.show_plural s ++ " occur"

instance (OccurrenceError a, OccurrenceError b) => OccurrenceError (Either a b) where
  doesNotOccur_n_times = either doesNotOccur_n_times doesNotOccur_n_times
  multipleOccur = either multipleOccur multipleOccur

instance Editing.Show.Show a => OccurrenceError a where
  doesNotOccur_n_times s n = Editing.Show.show s ++ " does not occur " ++ multiplicative_numeral (if n < 0 then -n else n+1)
  multipleOccur s = Editing.Show.show s ++ " occurs multiple times"

instance Find String (NeList ARange) where
  find x = do
    ResolutionContext _ s r <- ask
    case map (\o -> Range o (length x)) $ find_occs x $ selectRange r s of
      [] -> fail_with_context $ "String `" ++ x ++ "` does not occur"
      h:t -> return $ fmap anchor_range $ NeList h t

instance (OccurrenceError a, Find a (NeList ARange)) => Find (Ranked a) ARange where
  find (Sole x) = find x >>= case_of NeList z [] -> return z; _ -> fail_with_context $ multipleOccur x
  find (Ranked (Ordinal n) s) = NeList.nth n . find s >>= maybe (fail_with_context $ doesNotOccur_n_times s n) return

instance (OccurrenceError a, Find a (NeList ARange)) => Find (Rankeds a) (NeList ARange) where
  find (All x) = find x
  find (Sole' x) =
    find x >>= case_of l@(NeList _ []) -> return l; _ -> fail_with_context $ multipleOccur x
  find (Rankeds rs s) = NeList.sequence ((\r -> find (Ranked r s)) . flatten_occ_clauses rs)
  find (AllBut rs s) = do
    erase_indexed (ordinal_carrier . NeList.to_plain (flatten_occ_clauses rs)) . NeList.to_plain . find s >>= case_of
      [] -> fail "All occurrences excluded." -- Todo: Better error.
      x:y -> return $ NeList x y

flatten_occ_clauses :: AndList OccurrencesClause -> NeList Ordinal
flatten_occ_clauses (AndList rs) = NeList.concat $ (\(OccurrencesClause l) -> l) . rs

instance (Offsettable b, Invertible a, Find a b, Convert (Range Char) b) => Find (Relative a) (NeList b) where
  find (Absolute x) = NeList.one . find x
  find (Relative o ba w) = do
    Range a b <- search_range . ask
    Range st si <- unanchor_range . find w
    let h = Editing.Show.show ba ++ " " ++ Editing.Show.show w
    NeList.one . case ba of
      Before -> narrow h (Range a st) $ find (invert o)
      After -> narrow h (Range (a + st + si) (b - st - si)) $ offset (st + si) . find o
  find (Between o be@(Betw b e)) = do
    r <- search_range . ask
    x <- convert . find b
    y <- convert . find e
    let (p, q) = if either start id x <= either start id (y :: Either (Range Char) (Pos Char)) then (x, y) else (y, x)
    let p' = either end id p; q' = either start id q
    narrow (Editing.Show.show be) (Range (start r + p') (q' - p')) $ NeList.one . offset p' . find o
  find (FromTill b e) = do
    Range st si <- search_range . ask
    x <- convert . find b
    let p = either start id (x :: Either (Range Char) (Pos Char))
    narrow ("after " ++ Editing.Show.show b) (Range (st+p) (si-p)) $ do
    y <- convert . find e
    return $ NeList.one $ convert (Range p (either end id (y :: Either (Range Char) (Pos Char))) :: Range Char)

instance (Offsettable (NeList b), Find a (NeList b)) => Find (In a) (NeList b) where
  find (In o Nothing) = find o
  find (In o (Just incl)) = do
    r <- search_range . ask
    find incl >>= (NeList.concat .) . NeList.mapM (\a ->
      narrow (Editing.Show.show incl) (offset (start r) (unanchor_range a)) $
        offset (convert $ a Before) . find o)

instance Find Substr ARange where
  find Everything = arange (Anchor Before 0) . Anchor After . size . search_range . ask
  find (NotEverything x) = find x

instance Find (EverythingOr (Rankeds (Either Cxx.Basics.Findable String))) (NeList ARange) where
  find Everything = NeList.one . arange (Anchor Before 0) . Anchor After . size . search_range . ask
  find (NotEverything x) = find x

instance Find Cxx.Basics.Findable (NeList ARange) where
  find d = do
    ResolutionContext _ s (Range st si) <- ask
    let
      f :: Range Char -> Maybe (Range Char)
      f (Range x y)
        | st <= x, x + y <= st + si = Just $ Range (x - st) y
        | otherwise = Nothing
    case Cxx.Parse.parseRequest s of
      Left e -> fail $ "Could not parse code in previous request. " ++ e
      Right r -> case NeList.from_plain $ mapMaybe f $ Cxx.Operations.find d r of
          Nothing -> fail_with_context $ "Could not find " ++ show d
          Just l -> return $ fmap convert l

instance Find Cxx.Basics.DeclaratorId (NeList ARange) where find = find . Cxx.Basics.BodyOf

instance Find InClause (NeList ARange) where find (InClause x) = NeList.concat . NeList.concat . find x

instance Find AppendPositionsClause (NeList Anchor) where
  find (NonAppendPositionsClause pc) = find pc
  find (AppendIn incl) = (($ After) .) . find incl

instance Find PrependPositionsClause (NeList Anchor) where
  find (NonPrependPositionsClause pc) = find pc
  find (PrependIn incl) = (($ Before) .) . find incl

instance Find PositionsClause (NeList Anchor) where find (PositionsClause ba x) = (($ ba) .) . find x

instance Find Replacer (NeList Edit) where
  find (Replacer p r) = (flip RangeReplaceEdit r .) . (unanchor_range .) . merge_contiguous . find p
  find (ReplaceOptions o o') = return $ NeList (RemoveOptions o) [AddOptions o']

instance Find Changer (NeList Edit) where
  find (Changer p r) = (flip RangeReplaceEdit r .) . (unanchor_range .) . find p -- shouldn't we do merge_contiguous here, too?
  find (ChangeOptions o o') = return $ NeList (RemoveOptions o) [AddOptions o']

instance Find Eraser [Edit] where
  find (EraseText x) = ((flip RangeReplaceEdit "" . unanchor_range) .) . NeList.to_plain . find x
  find (EraseOptions o) = return [RemoveOptions o]
  find (EraseAround (Wrapping x y) (Around z)) = liftM2 (++) (f Before) (f After)
    where
      w Before = x; w After = y
      f ba = find $ EraseText $ Substrs $ and_one $ flip In Nothing $ Relative (NotEverything $ Rankeds (and_one $ OccurrencesClause $ NeList.one $ Ordinal 0) (Right $ w ba)) ba z

instance Find Bound (Either ARange Anchor) where
  find (Bound Nothing Everything) = Left . arange (Anchor Before 0) . Anchor After . size . search_range . ask
  find (Bound (Just Before) Everything) = return $ Right $ Anchor Before 0
  find (Bound (Just After) Everything) = Right . Anchor After . size . search_range . ask
  find (Bound mba p) = maybe Left (\ba -> Right . ($ ba)) mba . find p

instance Find RelativeBound (Either ARange Anchor) where
  find Front = return $ Right $ Anchor Before 0
  find Back = Right . Anchor After . size . search_range . ask
  find (RelativeBound mba p) = find p >>= case_of
    NeList x [] -> return $ maybe Left (\ba -> Right . ($ ba)) mba $ x
    _ -> fail "Relative bound must be singular."

instance Find Mover [Edit] where
  find (Mover o p) = do
    a <- find p
    NeList.to_plain . find o >>= mapM (makeMoveEdit a . unanchor_range) . reverse

instance Find Position Anchor where
  find (Position ba x) = find x >>= case_of
    NeList y [] -> return $ y ba
    _ -> fail "Anchor position must be singular."

instance Find UsePattern (Range Char) where
  find (UsePattern z) = do
    ResolutionContext _ s r <- ask
    let
      text_tokens = edit_tokens Char.isAlphaNum $ selectRange r s
      pattern_tokens = edit_tokens Char.isAlphaNum z
      (x, y) = (sum $ length . take stt text_tokens, sum $ length . take siz (drop stt text_tokens))
      (owc, stt, siz) = head $ approx_match token_edit_cost pattern_tokens (replaceAllInfix pattern_tokens (replicate (length pattern_tokens) (replicate 100 'X')) text_tokens)
    if y == 0 || ops_cost owc > fromIntegral (length z) / 1.5 then fail_with_context $ "No non-exact match for " ++ z else return (Range x y)

instance Invertible UsePattern where invert = id

instance Find UseClause (NeList Edit) where
  find (UseOptions o) = return $ NeList.one $ AddOptions o
  find (UseString ru@(In b _)) = case unrelative b of
    Nothing -> fail "Nonsensical use-command."
    Just (UsePattern v) -> (flip RangeReplaceEdit v .) . find ru

token_edit_cost :: Op String -> Cost
token_edit_cost (SkipOp (' ':_)) = 0
token_edit_cost (SkipOp x) | x `elem` Cxx.Basics.keywords = -2.4
token_edit_cost (SkipOp (h:t)) | Char.isAlphaNum h = -2.2 - fromIntegral (length t) * 0.2
token_edit_cost (SkipOp _) = -2
token_edit_cost (EraseOp (' ':_)) = 0.02
token_edit_cost (EraseOp x) = token_edit_cost (InsertOp x)
token_edit_cost (InsertOp t) | t `elem` Cxx.Basics.keywords = 2
token_edit_cost (InsertOp (' ':_)) = -0.02
token_edit_cost (InsertOp x@(y:_)) | Char.isAlpha y = fromIntegral (length x) * 0.7
token_edit_cost (InsertOp (x:y)) | Char.isDigit x = 1 + fromIntegral (length y) * 0.3
token_edit_cost (InsertOp _) = 1
token_edit_cost (ReplaceOp x y)
  | or $ (\c -> List.all (`elem` c) [x, y]) . [Cxx.Basics.classKeys, Cxx.Basics.accessSpecifiers, Cxx.Basics.relational_ops] = 0.4
token_edit_cost (ReplaceOp (c:_) (d:_)) | not (Char.isAlphaNum c || Char.isAlphaNum d) = 1.1
token_edit_cost (ReplaceOp x@(c:_) y@(d:_)) | Char.isAlpha c && Char.isAlpha d =
  if null (List.intersect x y) then 10 else levenshtein x y * 0.4
token_edit_cost (ReplaceOp x@(c:_) y@(d:_)) | Char.isAlphaNum c && Char.isAlphaNum d = levenshtein x y * 0.8
token_edit_cost (ReplaceOp _ _) = 10
  -- The precise values of these costs are fine-tuned to make the tests pass, and that is their only justification. We're trying to approximate the human intuition for what substring should be replaced, as codified in the tests.

instance Find Command [Edit] where
  find (Use l) = NeList.to_plain . NeList.concat . find l
  find (Append x Nothing) = do
    r <- search_range . ask
    return [InsertEdit (Anchor After (size r)) x]
  find (Prepend x Nothing) = return [InsertEdit (Anchor Before 0) x]
  find (Append r (Just p)) = NeList.to_plain . (flip InsertEdit r .) . NeList.concat . find p
  find (Prepend r (Just p)) = NeList.to_plain . (flip InsertEdit r .) . NeList.concat . find p
  find (Erase (AndList l)) = concat . sequence (find . NeList.to_plain l)
  find (Replace (AndList l)) = concat . sequence ((NeList.to_plain .) . find . NeList.to_plain l)
  find (Change (AndList l)) = concat . sequence ((NeList.to_plain .) . find . NeList.to_plain l)
  find (Insert r p) = NeList.to_plain . (flip InsertEdit r .) . NeList.concat . find p
  find (Move (AndList movers)) = concat . sequence (find . NeList.to_plain movers)
  find (Swap substrs Nothing) = NeList.to_plain . find substrs >>= f
    where
      f [] = return []
      f (a:b:c) = do
        edits <- f c
        return $ makeMoveEdit (b Before) (unanchor_range a) ++ makeMoveEdit (a Before) (unanchor_range b) ++ edits
      f _ = fail "Cannot swap uneven number of operands."
  find (Swap substrs (Just substrs')) = do
    nl <- find substrs
    nl' <- find substrs'
    case (contiguous nl, contiguous nl') of
      (Just a, Just b) -> return $ makeMoveEdit (b Before) (unanchor_range a) ++ makeMoveEdit (a Before) (unanchor_range b)
      _ -> fail "Swap operands must be contiguous regions."
  find (WrapAround (Wrapping x y) (AndList z)) =
    fmap concat $ sequence $ NeList.to_plain $ flip fmap z $ \z' ->
      concat . ((\r -> [InsertEdit (r Before) x, InsertEdit (r After) y]) .) . NeList.to_plain . merge_contiguous . find z'
  find (WrapIn z (Wrapping x y)) = find $ WrapAround (Wrapping x y) $ and_one $ Around z

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
  t "&ETPYE" "ETPYE" "Replaced ETPYE with &ETPYE." "Replaced &ETPYE with ETPYE."
  putStrLn "All use tests passed."
 where
  u :: String -> String -> String -> String -> String -> IO ()
  u txt pattern match d rd = do
    NeList (RangeReplaceEdit rng _) [] <- runReaderT (find (UseString $ flip In Nothing $ Absolute $ UsePattern pattern)) (ResolutionContext "." txt (Range 0 (length txt)))
    test_cmp pattern match (selectRange rng txt)
    let r = replaceRange rng pattern txt
    test_cmp pattern d $ show $ Editing.Diff.diff txt r
    test_cmp (pattern ++ " (reverse)") rd $ show $ Editing.Diff.diff r txt
  t :: String -> String -> String -> String -> IO ()
  t = u "{ string::size_t- siz = 2; int x = 3; if(i == 0) cout << ETPYE(x - size); vector<int> v; v = { 3, 2 }; vector<int> i = reinterpret_cat<fish>(10000000000, v.begin()); } X(y); using tracked::B; B z = B{p}; int const u = 94; int * w = &u--j; !B && !D; vector<unsigned char> & r = v; struct C(){ C & operator+(ostream &, char(const&here)[N], C const &) }; template<typename T> voidfoo(T a) { a.~T; } void main() { int a; a.seek(1800, ios::end); foo(a++); if(x >= 7) throw runtime_exception(y); } class Qbla { public: fstream p; };"
