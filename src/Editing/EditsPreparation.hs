{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances, TypeSynonymInstances, FlexibleContexts, UndecidableInstances, OverlappingInstances, PatternGuards #-}

module Editing.EditsPreparation (prepareEdits, use_tests) where

import qualified Cxx.Basics
import qualified Cxx.Parse
import qualified Editing.Diff
import qualified Data.List as List
import qualified Data.Char as Char
import Editing.Show ()
import Cxx.Operations (findDeclaration)
import Control.Monad.Error ()

import Util ((.), Convert(..), Op(..), ops_cost, unne, erase_indexed, levenshtein, replaceAllInfix, approx_match, Cost, Invertible(..), Ordinal(..), test_cmp, strip)

import Prelude hiding (last, (.), all, (!!))
import Editing.Basics

-- Positions/ranges:

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

instance Convert (Range Char) [ARange] where convert = (:[]) . anchor_range
instance Convert (Range Char) ARange where convert = anchor_range

instance Convert Anchor (Pos Char) where convert = anchor_pos

instance FindInStr Around [ARange] where findInStr s (Around x) = concat . findInStr s x

instance FindInStr (Ranked String) ARange where
  findInStr t (Ranked (Ordinal o) s) = anchor_range . nth o s t
  findInStr y (Sole x) = case find_occs x y of
    [z] -> return $ anchor_range $ Range z (length x)
    [] -> fail $ "String " ++ show x ++ " does not occur."
    _ -> fail $ "String " ++ show x ++ " occurs multiple times."

instance Convert ARange (Range Char) where convert = unanchor_range

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

instance FindInStr Cxx.Basics.DeclaratorId ARange where
  findInStr s did = case Cxx.Parse.parseRequest s of
    Left e -> fail $ "Could not parse code in previous request. " ++ e
    Right r -> case findDeclaration did r of
      Nothing -> fail $ "Could not find " ++ strip (show did) ++ "."
      Just (Range x y) -> return $ \ba -> Anchor (invert ba) (case ba of Before -> x; After -> (x + y))

instance FindInStr (Either Cxx.Basics.DeclaratorId (Relative (EverythingOr (Ranked String)))) ARange where
  findInStr s (Right t) = findInStr s t
  findInStr s (Left did) = findInStr s did

instance FindInStr (Either Cxx.Basics.DeclaratorId (Relative (EverythingOr (Rankeds String)))) [ARange] where
  findInStr s (Right r) = findInStr s r
  findInStr s (Left did) = (:[]) . findInStr s did

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
  findInStr _ (Position Before (Right Everything)) = return $ Anchor Before 0
  findInStr s (Position After (Right Everything)) = return $ Anchor After (length s)
  findInStr s (Position ba (Right (NotEverything p))) = ($ ba) . findInStr s p
  findInStr s (Position ba (Left did)) = ($ ba) . findInStr s did

instance FindInStr UseClause Edit where
  findInStr _ (UseOptions o) = return $ AddOptions o
  findInStr s (UseString z) = do
    if y == 0 || ops_cost owc > fromIntegral (length z) / 1.5 then fail "No match."
      else return $ RangeReplaceEdit (Range x y) z
   where
    text_tokens = edit_tokens Char.isAlphaNum s
    pattern_tokens = edit_tokens Char.isAlphaNum z
    (x, y) = (sum $ length . take stt text_tokens, sum $ length . take siz (drop stt text_tokens))
    (owc, stt, siz) = head $ approx_match token_edit_cost pattern_tokens (replaceAllInfix pattern_tokens (replicate (length pattern_tokens) (replicate 100 'X')) text_tokens)

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
  | or $ (\c -> List.all (`elem` c) [x, y]) . [Cxx.Basics.classKeys, Cxx.Basics.accessSpecifiers, Cxx.Basics.relational_ops, Cxx.Basics.arithmetic_ops] = 0.4
token_edit_cost (ReplaceOp (c:_) (d:_)) | not (Char.isAlphaNum c || Char.isAlphaNum d) = 1.1
token_edit_cost (ReplaceOp x@(c:_) y@(d:_)) | Char.isAlpha c && Char.isAlpha d =
  if null (List.intersect x y) then 10 else levenshtein x y * 0.4
token_edit_cost (ReplaceOp x@(c:_) y@(d:_)) | Char.isAlphaNum c && Char.isAlphaNum d = levenshtein x y * 0.8
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

prepareEdits :: (Functor m, Monad m) => String -> Command -> m [Edit]
prepareEdits = findInStr

use_tests :: IO ()
use_tests = do
  t "ETYPE_DESC" "ETPYE" "Replaced \"<< ETPYE\" with \"<< ETYPE_DESC\"." "Replaced \"<< ETYPE_DESC\" with \"<< ETPYE\"."
  t "kip(a.~T)" "a.~T" "Replaced \"a.~T\" with \"kip(a.~T)\"." "Replaced \"kip(a.~T)\" with \"a.~T\"."
  -- t "cos(a.~T)" "a.~T" -- Fails, but can probably be made to work by rewarding successive skips.
  t "size_type" "size_t" "Replaced \"string::size_t- siz\" with \"string::size_type- siz\"." "Replaced \"string::size_type- siz\" with \"string::size_t- siz\"."
  t "size = 9" "siz = 2" "Replaced \"string::size_t- siz = 2\" with \"string::size_t- size = 9\"." "Replaced \"string::size_t- size = 9\" with \"string::size_t- siz = 2\"."
  t "ETYPE" "ETPYE" "Replaced \"<< ETPYE\" with \"<< ETYPE\"." "Replaced \"<< ETYPE\" with \"<< ETPYE\"."
  t "std::string" "string" "Replaced \"string::size_t- siz\" with \"std::string::size_t- siz\"." "Replaced \"std::string::size_t- siz\" with \"string::size_t- siz\"."
  t "; float x" "; int x" "Replaced \"int x\" with \"float x\"." "Replaced \"float x\" with \"int x\"."
  t "x-" "x -" "Replaced \"x - size\" with \"x- size\"." "Replaced \"x- size\" with \"x - size\"."
  t ") cin <<" ") cout <<" "Replaced \"cout\" with \"cin\"." "Replaced \"cin\" with \"cout\"."
  t "x = 4" "x = 3" "Replaced \"3\" with \"4\"." "Replaced \"4\" with \"3\"."
  t "x - 8);" "x - size);" "Replaced \"x - size\" with \"x - 8\"." "Replaced \"x - 8\" with \"x - size\"."
  t "(!i)" "(i == 0)" "Replaced \"i == 0\" with \"!i\"."  "Replaced \"!i\" with \"i == 0\"."
  t "seekp" "seek" "Replaced \"a.seek\" with \"a.seekp\"." "Replaced \"a.seekp\" with \"a.seek\"."
  t "<char>" "<unsigned char>" "Replaced \"vector<unsigned char> & r\" with \"vector<char> & r\"." "Replaced \"vector<char> & r\" with \"vector<unsigned char> & r\"."
  t "<const fish>" "<fish>" "Replaced \"reinterpret_cat<fish>\" with \"reinterpret_cat<const fish>\"." "Replaced \"reinterpret_cat<const fish>\" with \"reinterpret_cat<fish>\"."
  t "&); };" "&) };" "Inserted \";\" after \"C const &)\"." "Erased \";\" after \"C const &)\"."
  t "> * r = v" "> & r = v" "Replaced \"& r\" with \"* r\"." "Replaced \"* r\" with \"& r\"."
  t "v.cbegin()" "v.begin()" "Replaced \"v.begin\" with \"v.cbegin\"." "Replaced \"v.cbegin\" with \"v.begin\"."
  -- Todo: "void foo" should match "voidfoo".
  t "x - sizeof(y))" "x - size)" "Replaced \"x - size\" with \"x - sizeof(y)\"." "Replaced \"x - sizeof(y))\" with \"x - size)\"."
  t "int a(2);" "int a;" "Inserted \"(2)\" after \"{ int a\"." "Erased \"(2)\" after \"{ int a\"."
  t "int const * w" "int * w" "Replaced \"int * w\" with \"int const * w\"." "Replaced \"int const * w\" with \"int * w\"."
  t "main(int argc) {" "main() {" "Inserted \"int argc\" after \"void main(\"." "Erased \"int argc\"."
  t "operator-" "operator+" "Replaced \"C & operator+\" with \"C & operator-\"." "Replaced \"C & operator-\" with \"C & operator+\"."
  t "_cast" "_cat" "Replaced \"reinterpret_cat<fish>\" with \"reinterpret_cast<fish>\"." "Replaced \"reinterpret_cast<fish>\" with \"reinterpret_cat<fish>\"."
  t "(++a)" "(a++)" "Replaced \"a++\" with \"++a\"." "Replaced \"++a\" with \"a++\"."
  t "list<int>" "vector<int>" "Replaced \"vector<int> v\" with \"list<int> v\"." "Replaced \"list<int> v\" with \"vector<int> v\"."
  t "a->seekp" "a.seek" "Replaced \"a.seek\" with \"a->seekp\"." "Replaced \"a->seekp\" with \"a.seek\"."
  t "vector<int>::iterator i" "vector<int> i" "Replaced \"vector<int> i\" with \"vector<int>::iterator i\"." "Replaced \"vector<int>::iterator i\" with \"vector<int> i\"."
  t "runtime_error(" "runtime_exception(" "Replaced \"throw runtime_exception\" with \"throw runtime_error\"." "Replaced \"throw runtime_error\" with \"throw runtime_exception\"."
  t "~T();" "~T;" "Inserted \"()\" after \") { a.~T\"." "Erased \"()\" after \") { a.~T\"." -- Todo: ugly.
  t "int const * w" "int * w" "Replaced \"int * w\" with \"int const * w\"." "Replaced \"int const * w\" with \"int * w\"."
  t "(T & a)" "(T a)" "Replaced \"T a\" with \"T & a\"." "Replaced \"T & a\" with \"T a\"."
  t "& r(v);" "& r = v;" "Replaced \"= v\" after \"vector<unsigned char> & r\" with \"(v)\"." "Replaced \"(v)\" after \"vector<unsigned char> & r\" with \"= v\"."
  t "ios_base::end_t" "ios::end" "Replaced \"ios::end\" with \"ios_base::end_t\".""Replaced \"ios_base::end_t\" with \"ios::end\"."
  t "95" "94" "Replaced \"94\" with \"95\"." "Replaced \"95\" with \"94\"."
  t "vector<int> const v { 3, 2 };" "vector<int> v; v = { 3, 2 };" "Replaced \"vector<int> v; v =\" with \"vector<int> const v\"." "Replaced \"vector<int> const v\" with \"vector<int> v; v =\"."
  t "class C" "struct C" "Replaced \"struct C\" with \"class C\"." "Replaced \"class C\" with \"struct C\"."
  t "B z{p};" "B z = B{p};" "Erased \"= B\" after \"B z\"." "Inserted \"= B\" after \"B z\"."
  t "friend C & operator+" "C & operator+" "Inserted \"friend\" before \"C & operator+\"." "Erased \"friend\" before \"C & operator+\"."
  t "char const(&here)[N]" "char(const&here)[N]" "Replaced \"char(const&here\" with \"char const(&here\"." "Replaced \"char const(&here\" with \"char(const&here\"."
  t "z = shared_ptr<B>{new p}" "z = B{p}" "Replaced \"B{p\" with \"shared_ptr<B>{new p\"." "Replaced \"shared_ptr<B>{new p\" with \"B{p\"." -- Todo: ugly.
  t "(X(y));" "X(y);" "Inserted \"(\" before \"X(y)\", and inserted \")\" after \"} X(y)\"." "Erased \"(\" before \"X(y))\" and \")\" after \"} (X(y)\"." -- Todo: ugly.
  t "2000" "1800" "Replaced \"1800\" with \"2000\"." "Replaced \"2000\" with \"1800\"."
  t "8000100808" "10000000000" "Replaced \"10000000000\" with \"8000100808\"." "Replaced \"8000100808\" with \"10000000000\"."
  t "> 7" ">= 7" "Replaced \"x >= 7\" with \"x > 7\"." "Replaced \"x > 7\" with \"x >= 7\"."
  t "private: fstream" "public: fstream" "Replaced \"public: fstream p\" with \"private: fstream p\"." "Replaced \"private: fstream p\" with \"public: fstream p\"." -- Todo: "replaced public: with private: before fstream p".
  t "int main" "void main" "Replaced \"void main\" with \"int main\"." "Replaced \"int main\" with \"void main\"."
  t "<char>" "<unsigned char>" "Replaced \"vector<unsigned char> & r\" with \"vector<char> & r\"." "Replaced \"vector<char> & r\" with \"vector<unsigned char> & r\"."
  t "int const u =" "int x =" "Replaced \"int x\" with \"int const u\"." "Replaced \"int const u\" with \"int x\"."
  t "u - -j" "u--j" "Replaced \"&u--j\" with \"&u - -j\"." "Replaced \"&u - -j\" with \"&u--j\"."
  t "struct C{" "struct C(){" "Erased \"()\" after \"struct C\"." "Inserted \"()\" after \"struct C\"."
  putStrLn "All use tests passed."
 where
  txt = "{ string::size_t- siz = 2; int x = 3; if(i == 0) cout << ETPYE(x - size); vector<int> v; v = { 3, 2 }; vector<int> i = reinterpret_cat<fish>(10000000000, v.begin()); } X(y); using tracked::B; B z = B{p}; int const u = 94; int * w = &u--j; !B && !D; vector<unsigned char> & r = v; struct C(){ C & operator+(ostream &, char(const&here)[N], C const &) }; template<typename T> voidfoo(T a) { a.~T; } void main() { int a; a.seek(1800, ios::end); foo(a++); if(x >= 7) throw runtime_exception(y); } class Qbla { public: fstream p; };"
  t :: String -> String -> String -> String -> IO ()
  t pattern match d rd = do
    RangeReplaceEdit rng _ <- findInStr txt (UseString pattern)
    test_cmp pattern match (selectRange rng txt)
    let r = replaceRange rng pattern txt
    test_cmp pattern d $ show $ Editing.Diff.diff txt r
    test_cmp (pattern ++ " (reverse)") rd $ show $ Editing.Diff.diff r txt
