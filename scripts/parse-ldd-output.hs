import Text.Regex
import Data.Maybe
import Data.List
main = interact $ concat . intersperse " " . nub . map head . catMaybes . map (matchRegex $ mkRegex "[[:blank:]](/[^[:blank:]]*)") . lines
