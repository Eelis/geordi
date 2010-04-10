{-# LANGUAGE PatternGuards #-}

module CompileConfig where

import qualified Data.Map as Map
import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import Control.Applicative ((<*>))
import Paths_geordi (getDataFileName)
import Prelude hiding ((.))
import Prelude.Unicode
import Util (readFileNow, (.))

data CompileConfig = CompileConfig { gxxPath :: FilePath, compileFlags, linkFlags :: [String] }

readCompileConfig :: IO CompileConfig
readCompileConfig = do
  l ← lines . (getDataFileName file >>= readFileNow)
  let
    m = Map.fromList $ Maybe.catMaybes $ (uncurry parseLine .) $ zip [1..] l
    var k = maybe (fail $ "Missing variable in " ++ file ++ ": " ++ k) return (Map.lookup k m)
  CompileConfig . var "GXX" <*> (words . var "COMPILE_FLAGS") <*> (words . var "LINK_FLAGS")
 where
  file = "compile-config"
  parseLine :: Int -> String -> Maybe (String, String)
  parseLine linenum line
    | s@(c:_) <- dropWhile Char.isSpace line, c ≠ '#' =
      case span (≠ '=') s of
        (key, _ : right) | [(value, _)] ← reads right -> Just (key, value)
        _ -> error $ "Syntax error on line " ++ show linenum ++ " in " ++ file ++ "."
    | otherwise = Nothing
