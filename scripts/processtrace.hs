import Data.Char
import Data.List
import Data.Maybe

repl_o :: String -> [String] -> (String, [String])
repl_o y ("-o":x:r) = (x, "-o":y:r)
repl_o y (h:t) = let (x, r) = repl_o y t in (x, h : r)
repl_o _ [] = error "no -o in list"

replace :: Eq a => [a] -> a -> a -> [a]
replace [] _ _ = error "element not found"
replace (h:t) x y = if h == x then y:t else h:replace t x y

strip = dropWhile (/= '(') . reverse . dropWhile (/= ')') . reverse

main = interact $ \i ->
  let
    u = map (\l -> let (s, argv, _) = read (strip l) :: (String, [String], [String]) in (s, argv)) (lines i)
    f s = let t = fromJust $ find (isInfixOf s . fst) u in fst t : tail (snd t)
    (s_file, cc1plus) = repl_o "t.s" (f "cc1plus")
    (o_file, as) = repl_o "t.o" (replace (f "as") s_file "t.s")
    ld = replace (f "ld") o_file "t.o"
  in
    show (cc1plus, as, ld)
