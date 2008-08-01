module EditCmds (exec, commands) where

import Control.Monad.Error

import Data.Char (isSpace, isAlpha, isDigit, isAlphaNum)
import Text.ParserCombinators.Parsec
  (choice, CharParser, char, string, try, (<?>), (<|>), eof, anyChar, errorPos, sourceColumn, lookAhead, unexpected)
import qualified Text.ParserCombinators.Parsec as PS
import qualified Text.ParserCombinators.Parsec.Error as PSE

import qualified Prelude
import Prelude hiding (last, (.), all, (!!))
import qualified Data.List as List
import Util

-- Positions/ranges:

type Pos = Int
data Range = Range { start :: Pos, size :: Int } deriving Eq

end :: Range -> Pos
end (Range x y) = x + y

selectRange :: Range -> String -> String
selectRange (Range st si) = take si . drop st

overlap :: Range -> Range -> Int
overlap (Range x s) (Range x' s') = max 0 $ min (x + s) (x' + s') - max x x'

find_occs :: Eq a => [a] -> [a] -> [Pos]
find_occs _ [] = []
find_occs x y | Just z <- stripPrefix x y = 0 : (+ length x) . find_occs x z
find_occs x (_:ys) = (+ 1) . find_occs x ys

nth :: (Monad m, Show a, Eq a) => Int -> [a] -> [a] -> m Range
nth _ x y | [] <- find_occs x y = fail $ "String " ++ show x ++ " does not occur."
nth n x y | l <- find_occs x y, (- length l) <= n, n < length l = return $ Range (l !! n) (length x)
nth n x _ = fail $ "String " ++ show x ++ " does not occur " ++ once_twice_thrice (if n < 0 then -n else (n+1)) ++ "."

-- Edits:

data Anchor = Anchor BefAft Pos deriving Eq
  -- This BefAft will probably need to be generalized to Before|After|Both for "insert x between 3 and 4".
data Edit = RangeReplaceEdit Range String | InsertEdit Anchor String deriving Eq
  -- We don't just use a RangeReplaceEdit with range length 0 for insertions, because it is not expressive enough. For instance, given "xy", insertions at the positions "after x" and "before y" would both designate position 1, but a prior "add z after x" edit should increment the latter position but not the former. InsertEdit's BefAft argument expresses this difference.

showEdit :: String -> Edit -> String
showEdit _ (RangeReplaceEdit (Range 0 0) r) = "prepend " ++ show r
showEdit s (RangeReplaceEdit (Range t _) r) | t == length s = "append " ++ show r
showEdit _ (RangeReplaceEdit (Range _ 0) r) = "insert " ++ show r
showEdit _ (InsertEdit _ r) = "insert " ++ show r
showEdit s (RangeReplaceEdit r "") = "erase " ++ show (selectRange r s)
showEdit s (RangeReplaceEdit r s') = "replace " ++ show (selectRange r s) ++ " with " ++ show s'

adjustEdit :: Edit -> Edit -> Maybe Edit
  -- Returns an adjusted Edit, or Nothing if the edits conflict.
adjustEdit (InsertEdit (Anchor _ p) s) e@(RangeReplaceEdit _ _) =
  adjustEdit (RangeReplaceEdit (Range p 0) s) e
adjustEdit (RangeReplaceEdit (Range st si) repl) e@(InsertEdit (Anchor ba p) s) =
  case () of
    ()| st + si <= p -> Just $ InsertEdit (Anchor ba (p - si + length repl)) s
    ()| p <= st -> Just e
    ()| otherwise -> Nothing
adjustEdit (InsertEdit (Anchor _ p) s) e@(InsertEdit (Anchor ba p') s') =
  Just $ if (ba == After && p == p') || p < p' then InsertEdit (Anchor ba (p' + length s)) s' else e
adjustEdit e@(RangeReplaceEdit r@(Range st si) repl) e'@(RangeReplaceEdit r'@(Range st' si') repl') =
  case () of
    ()| st + si <= st' -> Just $ RangeReplaceEdit (Range (st' - si + length repl) si') repl'
    ()| st' + si' <= st || e == e' -> Just e'
    ()| null repl && null repl' -> -- Overlapping erase-edits do not conflict.
      if st <= st' then Just $ RangeReplaceEdit (Range st (max 0 $ (st' + si') - (st + si))) repl'
      else Just $ RangeReplaceEdit (Range st' (si' - overlap r r')) repl'
    ()| otherwise -> Nothing

adjustEdits :: Edit -> [Edit] -> Either Edit [Edit]
  -- Returns either a conflicting Edit or the list of adjusted Edits.
adjustEdits _ [] = Right []
adjustEdits e (e' : t) =
  case adjustEdit e e' of
    Just r -> (r :) . adjustEdits e t
    Nothing -> Left e'

exec_edits :: Monad m => [Edit] -> String -> m String
exec_edits [] s = return s
exec_edits (e : t) s =
  case adjustEdits e t of
    Left e' -> fail $ "Overlapping edits: " ++ showEdit s e ++ " and " ++ showEdit s e' ++ "."
    Right t' -> case e of
      RangeReplaceEdit (Range st si) repl -> do
        let (x, y) = splitAt st s
        let (_, b) = splitAt si y
        exec_edits t' $ x ++ repl ++ b
      InsertEdit (Anchor _ p) repl -> do
        let (x, y) = splitAt p s
        exec_edits t' $ x ++ repl ++ y

-- English:

once_twice_thrice :: Int -> String
once_twice_thrice i = case i of
  1 -> "once"; 2 -> "twice"; 3 -> "thrice"
  n -> show n ++ " times"

newtype Ordinal = Ordinal { ordinal_carrier :: Int } -- 0 = first, 1 = second, -1 = last, -2 = second last, etc

instance Show Ordinal where
  show (Ordinal n) = case n of
    0 -> "zeroth"; 1 -> "first"; 2 -> "second"; 3 -> "third"; 4 -> "fourth"; 5 -> "fifth"
    6 -> "sixth"; 7 -> "seventh"; 8 -> "eighth"; 9 -> "ninth"; 10 -> "tenth"
    _ -> "<other ordinal>"

-- Edit command structure:

data EverythingOr a = Everything | NotEverything a
newtype AndList a = AndList { andList :: NElist a }

data Ranked a = Ranked Ordinal a | Sole a
data Rankeds a = Rankeds (AndList Ordinal) a | Sole' a | All a | AllBut (AndList Ordinal) a
data Bound = Bound (Maybe BefAft) (EverythingOr (Ranked String))
data RelativeBound = Front | Back | RelativeBound (Maybe BefAft) (Relative (EverythingOr (Ranked String)))
data Relative a = Relative a BefAft (Ranked String) | Between a Bound RelativeBound | FromTill Bound RelativeBound
  -- FromTill is not the same as (Between Everything), because in the former, the second bound is interpreted relative to the first, whereas in the latter, both bounds are absolute.
data PositionsClause = PositionsClause BefAft (AndList (Relative (EverythingOr (Rankeds String))))
data Position = Position BefAft (EverythingOr (Ranked String))
type Positions = AndList PositionsClause
data Replacer = Replacer (AndList (Relative (EverythingOr (Rankeds String)))) String
data Mover = Mover (Relative (EverythingOr (Ranked String))) Position
data BefAft = Before | After deriving Eq
data Around = Around (AndList (Relative (EverythingOr (Rankeds String))))
data Wrapping = Wrapping String String
newtype UseClause = UseClause String

data Command
  = Insert String Positions
  | Append String (Maybe Positions)
  | Prepend String (Maybe Positions)
  | Replace (AndList Replacer)
  | Move (AndList Mover)
  | WrapAround Wrapping (AndList Around)
  | WrapIn (AndList (Relative (EverythingOr (Rankeds String)))) Wrapping
  | Use (AndList UseClause)

front, back :: Bound
front = Bound (Just Before) Everything
back = Bound (Just After) Everything

-- Parsers:

notFollowedBy :: Show a => CharParser st a -> CharParser st ()
notFollowedBy p = ((try p >>= return . unexpected . show) <|> return (return ())) >>= id

kwd :: [String] -> CharParser st String
kwd s = choice $ try . string . s

kwd' :: [String] -> Bool -> CharParser st String
kwd' s b = choice (try . string . s) << eof_or_space b

select_act :: [([String], a)] -> CharParser st a
select_act = choice . map (\(s, r) -> choice (try . string . s) >> return r)

befAftP :: CharParser st BefAft
befAftP = select_act [(["before "], Before), (["after "], After)]

data Terminators = Terminators { term_eof :: Bool, term_keywords :: [String] }
type AndCont = String

add_terms :: [String] -> Terminators -> Terminators
add_terms s t = t { term_keywords = s ++ term_keywords t }

eof_or_space :: Bool -> CharParser st ()
eof_or_space b = (if b then (eof <|>) else id) (char ' ' >> lookAhead anyChar >> return ())

everything :: Terminators -> CharParser st ()
everything t = try $ string "everything" >> eof_or_space (term_eof t)

till :: [String]
till = ["till ", "until "]

verbatim :: Terminators -> CharParser st String
verbatim t = try (select_act cs << x) <|> fst . many1Till' anyChar (try x) <?> "verbatim string"
 where
  x = (if term_eof t then (eof <|>) else id) $ lookAhead (choice (try . string . (" " ++) . term_keywords t)) >> char ' ' >> return ()
  cs = [(["comma"], ","), (["space"], " "), (["colon"], ":"), (["semicolon"], ";"), (["ampersand"], "&"), (["tilde"], "~")]

class Parse t where parse :: Terminators -> [AndCont] -> CharParser st t

instance Parse (Ranked String) where
  parse k a = liftM2 Ranked (parse k a) (verbatim k) <|> Sole . verbatim k

instance Parse (Maybe BefAft) where
  parse _ _ = Just . befAftP <|> return Nothing

instance Parse RelativeBound where
  parse t a =
    (try (kwd' ["end", "back"] (term_eof t)) >> return Back) <|>
    (try (kwd' ["begin", "front"] (term_eof t)) >> return Front) <|>
    liftM2 RelativeBound (parse t a) (parse t a)

instance Parse Ordinal where
  parse _ _ = (Ordinal .) $ (<?> "ordinal") $ (try (string "last ") >> return (-1)) <|> do
    n <- select_act ((\n -> ([show (Ordinal $ n+1)], n)) . [0..9]) << char ' '
    if n /= 0 then (try (string "last ") >> return (- n - 1)) <|> return n else return n

instance Parse (EverythingOr (Ranked String)) where
  parse t a = (everything t >> return Everything)
    <|> NotEverything . parse (add_terms ("before " : "after " : till) t) a

relative :: a -> Terminators -> [AndCont] -> CharParser st (Relative a)
relative x t a =
  liftM2 (Relative x) befAftP (parse t a)
  <|> uncurry (Between x) . betweenP t a
  <|> return (Between x front Back)

instance Parse Bound where
  parse t a = (kwd ["begin ", "front "] >> return front)
    <|> (kwd ["end ", "back "] >> return back)
    <|> liftM2 Bound (parse t a) (parse t a)

betweenP :: Terminators -> [AndCont] -> CharParser st (Bound, RelativeBound)
betweenP t a = do
  try (string "between ")
  do
    r <- parse t a
    do
      try $ string "and "
      rank' <- parse t a
      s <- verbatim t
      return (Bound Nothing (NotEverything $ Ranked r s), RelativeBound Nothing (Between (NotEverything (Ranked rank' s)) front Back))
     <|> liftM2 (,) (Bound Nothing . NotEverything . Ranked r . verbatim (Terminators False ["and"])) (string "and " >> parse t a)
   <|> liftM2 (,) (parse (Terminators False ["and"]) a) (string "and " >> parse t a)

relative_everything_or :: Terminators -> [AndCont] -> CharParser st (Relative (EverythingOr a))
relative_everything_or t a =
    (kwd till >> Between Everything front . parse t a)
  <|> (try (string "from ") >> liftM2 (FromTill) (parse (add_terms till t) a) ((kwd till >> parse t a) <|> return Back))
  <|> do
    everything t
    (try (string "from ") >> liftM2 (Between Everything) (parse (add_terms till t) a) (kwd till >> parse t a))
      <|> (kwd till >> Between Everything front . parse t a)
      <|> relative Everything t a
  <|> (try (string "begin ") >> kwd till >> Between Everything front . parse t a)
  <|> do
    try (string "before ")
    ranked <- parse (add_terms till t) a
    uncurry (Between Everything) . ((kwd till >> (,) (Bound (Just Before) $ NotEverything ranked) . parse t a) <|> return (front, RelativeBound (Just Before) $ Between (NotEverything ranked) front Back))
  <|> uncurry (Between Everything) . betweenP t a
  <|> (try (string "after ") >> liftM2 (Between Everything)
    (Bound (Just After) . parse (add_terms till t) a)
    ((kwd till >> parse t a) <|> return Back))

instance Parse (Relative (EverythingOr (Rankeds String))) where
  parse t a = relative_everything_or t a <|> do
    x <- parse (add_terms ("before " : "after " : "between" : till) t) a
    case x of
      Rankeds (AndList (NElist r [])) s -> (kwd till >> Between Everything (Bound (Just Before) $ NotEverything $ Ranked r s) . parse t a) <|> relative (NotEverything x) t a
      Sole' s -> (kwd till >> Between Everything (Bound (Just Before) $ NotEverything $ Sole s) . parse t a) <|> relative (NotEverything x) t a
      _ -> relative (NotEverything x) t a

instance Parse (Relative (EverythingOr (Ranked String))) where
  parse t a = relative_everything_or t a <|> do
    x <- parse (add_terms ("before " : "after " : "between" : till) t) a
    case x of
      Ranked r s -> (kwd till >> Between Everything (Bound (Just Before) $ NotEverything $ Ranked r s) . parse t a) <|> relative (NotEverything x) t a
      Sole s -> (kwd till >> Between Everything (Bound (Just Before) $ NotEverything $ Sole s) . parse t a) <|> relative (NotEverything x) t a

instance Parse Position where
  parse t a = (flip Position Everything . select_act [(["begin", "front"], Before), (["end", "back"], After)] << eof_or_space True) <|> liftM2 Position befAftP (parse t a)

instance Parse (Rankeds String) where
  parse k _ = do
      try (string "all ")
      (kwd ["except ", "but "] >> liftM2 AllBut (parse k []) (verbatim $ add_terms till k)) <|> All . verbatim k
    <|> (kwd ["any ", "every ", "each "] >> All . verbatim k)
    <|> liftM2 Rankeds (parse k []) (verbatim $ add_terms till k)
    <|> Sole' . verbatim k

instance Parse PositionsClause where
  parse t a =
    (try (string "at ") >> (\ba -> PositionsClause ba (AndList (NElist (Between Everything front Back) []))) . (select_act [(["begin", "front"], Before), (["end", "back"], After)] << eof_or_space True))
    <|> liftM2 PositionsClause befAftP (parse t ("before " : "after " : "at " : a))

instance Parse Replacer where
  parse t _ = liftM2 Replacer (parse (Terminators False wb) []) (kwd wb >> verbatim t)
    where wb = ["with ", "by "]

instance Parse a => Parse (AndList a) where
  parse t a = AndList . (parse (add_terms ["and "] t) a `sepBy1'` try (string "and " >> notFollowedBy (kwd a)))

instance Parse Mover where
  parse t a = liftM2 Mover (parse (Terminators False ["to "]) a) (string "to " >> parse t a)

instance Parse Around where parse t a = try (string "around ") >> Around . parse t ("around ": a)

instance Parse UseClause where parse t _ = UseClause. verbatim t

instance Parse Wrapping where
  parse t@(Terminators b _) _ =
    (kwd' ["curlies", "braces", "curly brackets"] b >> return (Wrapping "{" "}"))
    <|> (kwd' ["parentheses", "parens", "round brackets"] b >> return (Wrapping "(" ")"))
    <|> (kwd' ["square brackets"] b >> return (Wrapping "[" "]"))
    <|> (kwd' ["angle brackets"] b >> return (Wrapping "<" ">"))
    <|> (kwd' ["single quotes"] b >> return (Wrapping "'" "'"))
    <|> (kwd' ["double quotes"] b >> return (Wrapping "\"" "\""))
    <|> liftM2 Wrapping (verbatim (Terminators False ["and "]) << string "and ") (verbatim t)

instance Parse Command where
  parse t a = select_act l >>= id
   where
    l =
      [ (["insert ", "add "], liftM2 Insert (verbatim (Terminators False ["after ", "before ", "at "])) (parse t a'))
      , (["append "], liftM2 Append (verbatim (add_terms ["before ", "after "] t)) (Just . parse t a' <|> return Nothing))
      , (["prepend "], liftM2 Prepend (verbatim (add_terms ["before ", "after "] t)) (Just . parse t a' <|> return Nothing))
      , (["erase ", "remove ", "kill ", "cut ", "omit ", "delete "], do
          s <- parse t a'; return $ Replace $ AndList (NElist (Replacer s "") []))
      , (["replace "], Replace . parse t a')
      , (["use "], Use . parse t a')
      , (["move "], Move . parse t a')
      , (,) ["wrap "] $ do
        b <- snd . (lookAhead $ many1Till' anyChar $ (try (string " around ") >> return False) <|> (try (string " in ") >> return True))
        if b
          then liftM2 WrapIn
            (parse (add_terms ["in "] t) a')
            (string "in " >> parse t a')
          else liftM2 WrapAround
            (parse (Terminators False ["around "]) a')
            (parse t a')
      ]
    a' = concat (fst . l) ++ a

commandsP :: CharParser st [Command]
commandsP = unne . andList . parse (Terminators True []) []

-- Resolving positions/occurrences/edits in the subject string:

type ARange = BefAft -> Anchor

arange :: Anchor -> Anchor -> ARange
arange x _ Before = x
arange _ x After = x

anchor_range :: Range -> ARange
anchor_range (Range x y) = arange (Anchor After x) (Anchor Before (x + y))

unanchor_range :: ARange -> Range
unanchor_range r | Anchor _ x <- r Before, Anchor _ y <- r After = Range x (y - x)

class Offsettable a where offset :: Int -> a -> a

instance Offsettable Range where offset x (Range y z) = Range (y + x) z
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

class Invertible a where invert :: a -> a

instance Invertible Ordinal where
  invert (Ordinal r) | r >= 0 = Ordinal (-r - 1)
  invert o = o

instance Invertible a => Invertible (EverythingOr a) where
  invert Everything = Everything
  invert (NotEverything x) = NotEverything (invert x)

instance Invertible (Ranked String) where
  invert (Ranked r s) = Ranked (invert r) s
  invert x = x

instance Invertible (Rankeds String) where
  invert (Rankeds (AndList r) s) = Rankeds (AndList $ invert . r) s
  invert x = x

instance Convert Range [ARange] where convert a = [anchor_range a]
instance Convert Range ARange where convert = anchor_range

instance Convert ARange Range where convert = unanchor_range
instance Convert Anchor Pos where convert (Anchor _ p) = p

instance (Offsettable b, Invertible a, FindInStr a b, Convert Range b) => FindInStr (Relative a) b where
  findInStr s (Relative o ba w) = do
    Range st si <- unanchor_range . findInStr s w
    case ba of
      Before -> findInStr (take st s) (invert o)
      After -> offset (st + si) . findInStr (drop (st + si) s) o
  findInStr s (Between o b e) = do
    x <- convert . findInStr s b
    y <- convert . findInStr s e
    let (p, q) = if either start id x <= either start id y then (x, y) else (y, x)
    let p' = either end id p; q' = either start id q
    offset p' . findInStr (take (q' - p') $ drop p' s) o
  findInStr s (FromTill b e) = do
    x <- convert . findInStr s b
    let p = either start id x
    y <- convert . findInStr (drop p s) e
    return $ convert $ Range p (either end id y)

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

instance FindInStr Bound (Either ARange Anchor) where
  findInStr s (Bound ba Everything) =
    return $ case ba of
      Nothing -> Left $ everything_arange s
      Just Before -> Right $ Anchor Before 0
      Just After -> Right $ Anchor After (length s)
  findInStr s (Bound mba p) = maybe Left (\ba -> Right . ($ ba)) mba . findInStr s p

instance FindInStr RelativeBound (Either ARange Anchor) where
  findInStr _ Front = return $ Right $ Anchor Before 0
  findInStr s Back = return $ Right $ Anchor After (length s)
  findInStr s (RelativeBound mba p) = maybe Left (\ba -> Right . ($ ba)) mba . findInStr s p

instance FindInStr Mover [Edit] where
  findInStr s (Mover o p) = do
    r <- unanchor_range . findInStr s o
    a <- findInStr s p
    return [RangeReplaceEdit r "", InsertEdit a (selectRange r s)]

instance FindInStr Position Anchor where
  findInStr _ (Position Before Everything) = return $ Anchor Before 0
  findInStr s (Position After Everything) = return $ Anchor After (length s)
  findInStr s (Position ba (NotEverything p)) = ($ ba) . findInStr s p

instance FindInStr UseClause Edit where
  findInStr s (UseClause z) = do
    if y == 0 || cost > fromIntegral (length z) / 1.5 then fail "No match."
      else return $ RangeReplaceEdit (Range x y) z
   where
    (x, y) = (sum $ length . take stt txt_toks, sum $ length . take siz (drop stt txt_toks))
    txt_toks = tokenize s
    (cost, stt, siz) = approx_match token_skip_cost token_insert_cost token_erase_cost token_replace_cost (tokenize z) txt_toks

token_replace_cost :: String -> String -> Cost
token_replace_cost (c:_) (d:_) | not (isAlphaNum c || isAlphaNum d) = 1.1
token_replace_cost x@(c:_) y@(d:_) | isAlpha c && isAlpha d = levenshtein x y * 0.4
token_replace_cost x@(c:_) y@(d:_) | isAlphaNum c && isAlphaNum d = levenshtein x y * 0.8
token_replace_cost _ _ = 10
token_skip_cost, token_insert_cost, token_erase_cost :: String -> Cost
token_skip_cost (' ':_) = 0
token_skip_cost (h:t) | isAlpha h = -2.5 - fromIntegral (length t) * 0.2
token_skip_cost _ = -2.5
token_insert_cost t | t `elem` keywords = 2
token_insert_cost (' ':_) = -0.02
token_insert_cost x@(y:_) | isAlpha y = fromIntegral (length x) * 0.7
token_insert_cost (x:y) | isDigit x = 1 + fromIntegral (length y) * 0.3
token_insert_cost _ = 1
token_erase_cost (' ':_) = 0.02
token_erase_cost x = token_insert_cost x

keywords, long_tokens :: [String]
keywords = words "alignas continue friend reinterpret_cast typedef alignof decltype goto return typeid asm default if short typename auto delete inline signed union bool double int sizeof unsigned break do long static_assert using case dynamic_cast mutable static_cast virtual catch else namespace static void char enum new struct volatile char16_t explicit nullptr switch wchar_t char32_t export operator template while class extern private this const false protected throw constexpr float public true const_cast for register try define elif include defined"
long_tokens = keywords ++ words "<<= >>= &&= ||= ++ -- -> .* += *= /= -= :: == << >> && ||"

tokenize :: String -> [String]
tokenize [] = []
tokenize s | Just (o, s') <- findMaybe (\p -> (\x -> (p, x)). stripPrefix p s) long_tokens = o : tokenize s'
tokenize (' ':s) = let (x, s') = span isSpace s in (' ':x) : tokenize s'
tokenize (h:s) | isAlpha h = let (x, s') = span isAlpha s in (h:x) : tokenize s'
tokenize (h:s) | isDigit h = let (x, s') = span isDigit s in (h:x) : tokenize s'
tokenize (h:s) = [h] : tokenize s

instance FindInStr Command [Edit] where
  findInStr s (Use (AndList l)) = sequence $ findInStr s . unne l
  findInStr s (Append x Nothing) = return [InsertEdit (Anchor After (length s)) x]
  findInStr _ (Prepend x Nothing) = return [InsertEdit (Anchor Before 0) x]
  findInStr s (Append x (Just y)) = findInStr s (Insert x y)
  findInStr s (Prepend x (Just y)) = findInStr s (Insert x y)
  findInStr s (Replace (AndList l)) = concat . sequence (findInStr s . unne l)
  findInStr s (Insert r p) = (flip InsertEdit r .) . concat . findInStr s p
  findInStr s (Move (AndList movers)) = concat . sequence (findInStr s . unne movers)
  findInStr s (WrapAround (Wrapping x y) z) = concat . ((\r -> [InsertEdit (r Before) x, InsertEdit (r After) y]) .) . concat . findInStr s z
  findInStr s (WrapIn z (Wrapping x y)) = findInStr s (WrapAround (Wrapping x y) (AndList (NElist (Around z) [])))

-- Main:

commands :: [String]
commands = words "append prepend erase remove cut omit kill delete replace remove add insert move wrap use"

exec :: (Functor m, Monad m) => String -> String -> m String
exec cmd_str str = do
  cmds <- case PS.parse (commandsP << eof) "" cmd_str of
    Left e -> fail $ "column " ++ show (sourceColumn $ errorPos e) ++ ": " ++
      (concatMap (++ ". ") $ filter (not . List.all isSpace) $ lines $ PSE.showErrorMessages "or" "unknown parse error" "expected:" "unexpected" "end of command" $ PSE.errorMessages e)
    Right x -> return x
  edits <- concat . sequence (findInStr str . cmds)
  exec_edits edits str

-- Testing:

itest :: IO ()
itest = do
  l <- getLine
  case (exec l "1 2 3 2 3 4 5" :: Either String String) of
    Left x -> putStr "syntax error: " >> putStrLn x
    Right x -> putStrLn x

test :: IO ()
test = do
  t "erase all 2 and insert x before second 3 and prepend y" $ Right "y1  3  x3 4 5"
  t "erase everything before last 2 and replace everything after 4 with x" $ Right "2 3 4x"
  t "insert x after all 2 and append y" $ Right "1 2x 3 2x 3 4 5y"
  t "erase first 2 and replace second 2 with x and omit last  " $ Right "1  3 x 3 45"
  t "erase first and last 2 and all 3 and 4 and everything before first 2 and everything after 4" $ Right "    "
  t "erase third last space and kill everything after second last  " $ Right "1 2 3 23 "
  t "insert x after 1 2 and before all 3 and erase 5 and prepend y" $ Right "y1 2x x3 2 x3 4 "
  t "erase everything before 4 and everything after 5" $ Right "4 5"
  t "insert x before 3 4 and 5" $ Right "1 2 3 2 x3 4 x5"
  t "insert x after 1 and all 3 and before 5 and erase 4" $ Right "1x 2 3x 2 3x  x5"
  t "erase second space before 4" $ Right "1 2 3 23 4 5"
  t "erase begin until second 3" $ Right "3 4 5"
  t "erase first 2 3 and 3 2 and 4 5 and last  " $ Right "1  3 "
  t "move 1 till second 3 to before 5" $ Right "3 4 1 2 3 2 5"
  t "move 1 till after second 3 to before 5" $ Right " 4 1 2 3 2 35"
  t "replace 2 after second space with x" $ Right "1 2 3 x 3 4 5"
  t "move everything till 4  to end and erase 4 " $ Right "51 2 3 2 3 "
  t "erase everything" $ Right ""
  t "insert x after second space before 4" $ Right "1 2 3 2 x3 4 5"
  t "wrap < and > around second 3" $ Right "1 2 3 2 <3> 4 5"
  t "wrap braces around everything" $ Right "{1 2 3 2 3 4 5}"
  t "move everything after second 3 to begin" $ Right " 4 51 2 3 2 3"
  t "move second 3 to end and 5 to begin" $ Right "51 2 3 2  4 3"
  t "erase first space until second space before 4" $ Right "1 3 4 5"
  t "erase second last space before 4" $ Right "1 2 3 23 4 5"
  t "erase after second 2" $ Right "1 2 3 2"
  t "erase after second 2 until before 5" $ Right "1 2 3 25"
  t "erase before second last space" $ Right " 4 5"
  t "move 4 5 to before 1 and erase everything after second 2" $ Right "4 51 2 3 2"
  t "erase everything between last and first 2" $ Right "1 22 3 4 5"
  t "erase everything between second 2 and begin" $ Right "2 3 4 5"
  t "erase all space between second 2 and 4" $ Right "1 2 3 234 5"
  t "erase from first 3 until after everything" $ Right "1 2 "
  t "move before second 3 until 4 to begin" $ Right "3 1 2 3 2 4 5"
  t "erase everything between begin and end" $ Right ""
  t "move everything between first 3 and 4 to begin" $ Right " 2 3 1 2 34 5"
  t "erase before second 3 until 4" $ Right "1 2 3 2 4 5"
  t "insert x before second 3 and at end" $ Right "1 2 3 2 x3 4 5x"
  t "erase until after second 3" $ Right " 4 5"
  t "erase from before second 3" $ Right "1 2 3 2 "
  t "replace all but first and second last space with x" $ Right "1 2x3x2x3 4x5"
  t "erase between second and fourth space and 1" $ Right " 2  3 4 5"
  t "erase between first and third space and prepend x" $ Right "x1  2 3 4 5"
  t "wrap parentheses around every space between first 2 and 4 and around 5 and erase second last 3" $ Right "1 2( )( )2( )3( )4 (5)"
  t "move from first 3 until 4 to begin" $ Right "3 2 3 41 2  5"
  t "erase everything from before everything until second 3" $ Right "3 4 5"
  t "wrap parentheses around first 2 and 5" $ Right "1 (2) 3 2 3 4 (5)"
  t "erase everything between first space and last space" $ Right "1  5"
  t "erase all 3 and all 2 between begin and end" $ Right "1     4 5"
  t "erase everything between second and first 2 " $ Right "1 2 2 3 4 5"
  t "erase from second 2 till last space" $ Right "1 2 3 5"
  t "erase from second 2 until after 3 and add x before 4" $ Right "1 2 3  x4 5"
  t "erase between 1 and second 2 and between 4 and 5" $ Right "12 3 45"
  t "erase from before 4 until end" $ Right "1 2 3 2 3 "
  t "use 5x and y4" $ Right "1 2 3 2 3 y4 5x"
  t "erase everything from after 1 until second last space" $ Right "1 4 5"
  t "wrap parentheses around everything between 1 and second space before 4" $ Right "1( 2 3 2) 3 4 5"
  t "wrap all 3 and second 2 in + and - and prepend x" $ Right "x1 2 +3- +2- +3- 4 5"
  t "move 4 till end to front" $ Right "4 51 2 3 2 3 "
  t "erase all space after first 2" $ Right "1 232345"
  t "add x before first 3 after second 2" $ Right "1 2 3 2 x3 4 5"
  t "add x before second space after second 3" $ Right "1 2 3 2 3 4x 5"
  t "add x after all space before first 3" $ Right "1 x2 x3 2 3 4 5"
  t "erase all space before last 3" $ Right "12323 4 5"
  t "erase second 2 till end" $ Right "1 2 3 "
  t "append x before 5 and prepend y before all 2 between first 3 and 5" $ Right "1 2 3 y2 3 4 x5"
  t "move everything after second 3 to begin" $ Right " 4 51 2 3 2 3"
  t "replace first 2 with x and replace all 2 with x" $ Right "1 x 3 x 3 4 5"
  t "erase everything until 4 and 5" $ Right "4 "
  t "erase 3 2 till 5" $ Right "1 2 5"
  t "move second 2 to back and prepend x" $ Right "x1 2 3  3 4 52"
  t "cut everything before first 2 and first and second 3 and everything after 4 and prepend x" $ Right "x2  2  4"
  t "replace all 2 with 3 and erase all 3 and add x after second 2" $ Right "1 3  3x  4 5"
  t "insert spacer before 4 and insert semicolon after 1 and erase last space" $ Right "1; 2 3 2 3 spacer45"
  t "erase first and second last  " $ Right "12 3 2 34 5"
  t "replace 1 with x and all 2 with y and erase second 3" $ Right "x y 3 y  4 5"
  -- Order-sensitive edits:
  t "wrap parentheses around everything and append x" $ Right "(1 2 3 2 3 4 5)x"
  t "append x and wrap parentheses around everything" $ Right "(1 2 3 2 3 4 5x)"
  t "append x after everything before 4 and add y before 4" $ Right "1 2 3 2 3 xy4 5"
  t "insert y before 4 and insert z after second 3 " $ Right "1 2 3 2 3 zy4 5"
  t "prepend x and move 5 to begin and insert y before 1 and insert z before everything" $ Right "z5xy1 2 3 2 3 4 "
  t "wrap parentheses around everything and prepend x" $ Right "x(1 2 3 2 3 4 5)"
  t "prepend x and wrap parentheses around everything" $ Right "(x1 2 3 2 3 4 5)"
  t "prepend x before everything after 4 and add y after 4" $ Right "1 2 3 2 3 4yx 5"
  t "add y after 4 and prepend x before everything after 4" $ Right "1 2 3 2 3 4xy 5"
  -- Edit errors:
  t "move second 2 to x" $ Left "column 18: unexpected \"x\". expected: \"begin\", \"front\", \"end\", \"back\", \"before \" or \"after \". "
  t "replace alligators with chickens" $ Left "String \"alligators\" does not occur."
  t "use banana" $ Left "No match."
  t "use 5426" $ Left "No match."
  t "erase 2" $ Left "String \"2\" occurs multiple times."
  t "replace 1 and erase with 4" $ Left "String \"erase\" does not occur."
  t "replace tenth last 2 by x" $ Left "String \"2\" does not occur 10 times."
  t "erase second 9" $ Left "String \"9\" does not occur."
  t "replace all 2 with 3 and replace second 2 with x" $ Left "Overlapping edits: replace \"2\" with \"3\" and replace \"2\" with \"x\"."
  t "erase everything before first 3 and replace first 2 with x" $ Left "Overlapping edits: erase \"1 2 \" and replace \"2\" with \"x\"."
  -- Syntax errors:
  t "move 4 to back " $ Left "column 16: unexpected end of command. "
  t "isnert 3 before 4" $ Left "column 1: unexpected \"s\". expected: \"insert \", \"add \", \"append \", \"prepend \", \"erase \", \"remove \", \"kill \", \"cut \", \"omit \", \"delete \", \"replace \", \"use \", \"move \" or \"wrap \". "
  t "insert " $ Left "column 8: unexpected end of command. expected: verbatim string. "
  t "erase first and " $ Left "column 17: unexpected end of command. expected: ordinal. "
  t "erase between second " $ Left "column 22: unexpected end of command. expected: \"last \", \"and \" or verbatim string. "
  t "insert kung fu" $ Left "column 15: unexpected end of command. expected: \" after \", \" before \" or \" at \". "
  t "move " $ Left "column 6: unexpected end of command. expected: \"till \", \"until \", \"from \", \"everything\", \"begin \", \"before \", \"between \", \"after \", ordinal or verbatim string. "
  t "move x " $ Left "column 8: unexpected end of command. expected: \" before \", \" after \", \" between\", \" till \", \" until \" or \" to \". "
  t "move x to "$ Left "column 11: unexpected end of command. expected: \"begin\", \"front\", \"end\", \"back\", \"before \" or \"after \". "
  t "wrap x and y" $ Left $ "column 13: unexpected end of command. expected: \" around \" or \" in \". "
  t "append x and erase first " $ Left "column 26: unexpected end of command. expected: \"and \" or verbatim string. "
  t "erase all 2 and " $ Left "column 17: unexpected end of command. expected: \"insert \", \"add \", \"append \", \"prepend \", \"erase \", \"remove \", \"kill \", \"cut \", \"omit \", \"delete \", \"replace \", \"use \", \"move \", \"wrap \", \"till \", \"until \", \"from \", \"everything\", \"begin \", \"before \", \"between \", \"after \", \"all \", \"any \", \"every \", \"each \", ordinal or verbatim string. "
  -- "use" tests:
  ut "size_type" "size_t"
  ut "size = 9" "siz = 2"
  ut "ETYPE" "ETPYE"
  ut "std::string" "string"
  ut "; float x" "; int x"
  ut "x-" "x -"
  ut ") cin <<" ") cout <<"
  ut "x = 4" "x = 3"
  ut "x - 8);" "x - size);"
  ut "(!i)" "(i == 0)"
  ut "seekp" "seek"
  ut "<char>" "<unsigned char>"
  ut "<const fish>" "<fish>"
  ut "&); };" "&) };"
  ut "> * r = v" "> & r = v"
  ut "v.cbegin()" "v.begin()"
  ut "void foo" "voidfoo"
  ut "char foo(T a)" "voidfoo(T a)"
  ut "x - sizeof(y))" "x - size)"
  ut "int a(2);" "int a;"
  ut "int const * w" "int * w"
  ut "int main(int argc) {" "int main() {"
  ut "operator-(" "operator+("
    -- Note: While at first sight it seems reasonable to expect this to work without the (, this is only so because we humans have special knowledge about "operator". The situation is equivalent to searching for "bla()" in "bla;*x", which we don't want to yield "bla;*" either. Hence, inserting operators is cheaper than replacing them.
  ut "_cast" "_cat"
  ut "(++a)" "(a++)"
  ut "list<int>" "vector<int>"
  ut "a->seekp" "a.seek"
  ut "vector<int>::iterator i" "vector<int> i"
  ut "runtime_error(" "runtime_exception(" -- Without the (, this fails, but that should be fixable by adding a bias against insertions causing concatenated alpha tokens.
  ut "~T();" "~T;"
  ut "int const * w" "int * w"
  ut "(T & a)" "(T a)"
  ut "& r(v);" "& r = v;"
  ut "ios_base::end_t" "ios::end"
  ut "95" "94"
  ut "vector<int> const v { 3, 2 };" "vector<int> v; v = { 3, 2 };"
  ut "; class C" "; struct C" -- Without the semicolon, this matches " C". Todo: This should be fixed by making "struct" -> "class" cheaper than inserting "class".
  ut "struct C{" "struct C(){"
  ut "B z{p};" "B z = B{p};"
  ut "friend C & operator+" "C & operator+"
  ut "char const(&here)[N]" "char(const&here)[N]"
  ut "z = shared_ptr<B>{new p}" "z = B{p}"
  ut "(X(y));" "X(y);"
  ut "2000" "1800"
  ut "8000100808" "10000000000"

  putStrLn "No test failures."
 where
  t :: String -> Either String String -> IO ()
  t c o = let o' = exec c "1 2 3 2 3 4 5" in when (o' /= o) $ fail $ "test failed: " ++ show (c, o, o')
  ut :: String -> String -> IO ()
  ut pattern match = do
    let txt = "{ string::size_t- siz = 2; int x = 3; if(i == 0) cout << ETPYE(x - size); vector<int> v; v = { 3, 2 }; vector<int> i = reinterpret_cat<fish>(10000000000, v.begin()); } X(y); using tracked::B; B z = B{p}; int const u = 94; int * w = &u; vector<unsigned char> & r = v; struct C(){ C & operator+(ostream &, char(const&here)[N], C const &) }; template<typename T> voidfoo(T a) { a.~T; } int main() { int a; a.seek(1800, ios::end); foo(a++); if(x) throw runtime_exception(y); }"
    RangeReplaceEdit rng _ <- findInStr txt (UseClause pattern)
    let s = selectRange rng txt
    when (s /= match) $ fail $ "\"use\" test \"" ++ pattern ++ "\" failed, got \"" ++ s ++ "\" instead of \"" ++ match ++ "\"."

{- Command grammar:

  command = (insert | append | prepend | erase | replace | move | wrap | use)*

  insert = ("insert" | "add") ... positions*
  append = "append" ... [positions*]
  prepend = "prepend" ... [positions*]
  erase = ("erase" | "remove" | "delete" | "cut" | "omit" | "kill") substrs*
  replace = "replace" (substrs* ("with" | "by") ...)*
  move = "move" (substr "to" position)*
  wrap = "wrap" wrapping ("around" substrs*)* | "wrap" substrs* "in" wrapping
  use = "use" verbatim*

  wrapping = ... "and" ... | "parentheses" | "parens" | "braces" | "curlies"
    | ("square" | "angle" | "curly" | "round") "brackets" | ("single" | "double") "quotes"
  relative = between | befaft ranked
  substrs = range | ("everything" | rankeds) [relative]
  substr = range | ("everything" | ranked) [relative]
  position = limit | befaft ("everything" | ranked)
  befaft = "before" | "after"
  positions = "at" limit | befaft (("everything" | rankeds) [befaft ranked])*
  ordinal = "first" | ("second" | "third" | etc) ["last"] | "last"
  ranked = [ordinal] ...
  rankeds = "all" [("except" | "but") ordinal*] ... | ["each" | "every" | "any" | ordinal*] ...
  between = "between" (bound "and" relative-bound | ordinal "and" ordinal ...)
  range = ([["everything"] "from"] bound | "everything") ("till" | "until") relative-bound
  limit = "begin" | "front" | "end" | "back"
  bound = limit | [befaft] ("everything" | ranked)
  relative-bound = limit | [befaft] ("everything" | ranked) [relative]

  Ellipsis denote a verbatim string, and x* = x ["and" x*].

  This is an idealized grammar with LOTS of ambiguity. The parsers do their best to choose the most sensible interpretation of ambiguous commands.

Design notes:

  "use" is heavily biased toward whole-token edits, so users are encouraged to use those. This will not only produce better edits, it is also more readable.

  Giving moves a single target makes "move 4 to end and 5 to begin" work, because otherwise it would be parsed as a move with two targets, the second of which, 5, is not a valid target.

  Should "second last x before y" in "xxaxxy" designate the 'x' before or after 'a'?

    We choose the latter, because it seems more natural, despite the curious result that "first x before y" now means the same as "last x before y".

  Should "erase all x and y" mean "erase all x and all y" or "erase all x and the sole y"?

    We choose the latter, because:
    - it's easier to implement, because we don't need "and"-repetition nested under "all";
    - it's safe, because if y occurs multiple times, a clear "y occurs multiple times" error will be emitted, whereas the former solution could result in unintended edit results.

  Should "first and second last x" mean "(first and second) last x" or "first and (second last) x"?

    I have no strong feelings on the matter. Currently it is interpreted to mean the second.

  Should "from ..." / "until ..." bounds be inclusive or exclusive?

    We choose the former, for no particular reason. Note that this default can be overridden by by saying "from after ..." / "until before ...".

  Grammar guidelines:

    - Ranges are never relative (but their bounds may be).
    - Position specifications never mention ranges (this means that "everything" must not be a range).

  Semantic choices:

    - Ordinal specifications are always relative to the full string, so "erase from x until second b" in "bxabcb" produces "bxbcb", not "bxb".

-}
