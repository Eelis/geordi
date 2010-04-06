{-# LANGUAGE UnicodeSyntax, ViewPatterns #-}

import System.Posix (createFile, createDirectory, closeFd,
  FileMode, unionFileModes, accessModes, nullFileMode,
  ownerReadMode, ownerWriteMode, ownerExecuteMode,
  groupReadMode, groupWriteMode, groupExecuteMode, 
  otherReadMode, otherWriteMode, otherExecuteMode,
  setFileCreationMask)
import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))
import System.Directory (createDirectoryIfMissing, copyFile, doesFileExist)
import System.FilePath (takeDirectory, (</>))
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)
import System.Environment (getEnv)
import Control.Monad (when, forM)
import Text.Regex (matchRegex, mkRegex)
import Data.Maybe (mapMaybe)
import Data.List (nub)
import Util (findM, (.))
import Prelude hiding ((.))
import Prelude.Unicode
import Paths_geordi (getDataFileName)
import CompileConfig

split_paths :: String → [FilePath]
split_paths [] = []
split_paths (span (/= ':') → (f, r)) = f : split_paths (drop 1 r)

which :: String → IO (Maybe FilePath)
which s = getEnv "PATH" >>= findM doesFileExist . (s:) . map (</> s) . filter (not . null) . split_paths

modes :: [FileMode] → FileMode
modes = foldl1 unionFileModes

readModes, writeModes, executeModes :: FileMode
readModes = modes [ownerReadMode, groupReadMode, otherReadMode]
writeModes = modes [ownerWriteMode, groupWriteMode, otherWriteMode]
executeModes = modes [ownerExecuteMode, groupExecuteMode, otherExecuteMode]

ldd :: FilePath → IO [FilePath]
ldd f = do
  (status, out, err) ← readProcessWithExitCode "ldd" [f] ""
  if status ≠ ExitSuccess then error err else do
  return $ map head $ mapMaybe (matchRegex $ mkRegex "[[:blank:]](/[^[:blank:]]*)") $ lines out

compiler_files :: IO [FilePath]
compiler_files = (nub .) $ do
  gxx ← gxxPath . readCompileConfig
  let
    query_gxx q = do
      (status, out, err) ← readProcessWithExitCode gxx [q] ""
      if status /= ExitSuccess then error err else do
      return $ head $ lines out
  fs ← (concat .) $ forM l $ \f → do
    out ← query_gxx $ "-print-file-name=" ++ f
    return [out | out ≠ f]
  fs' ← (concat .) $ forM ["cc1plus", "as", "ld"] $ \p → do
    mf ← query_gxx ("-print-prog-name=" ++ p) >>= which
    case mf of
      Nothing → error $ "could not find " ++ p
      Just f → (f:) . ldd f
  gxxlibs ← ldd gxx
  return $ gxx : gxxlibs ++ fs ++ fs'
 where l = words "crt1.o crti.o crtn.o crtbegin.o crtend.o libgcc.a libgcc_s.so libstdc++.so libstdc++.so.6 libmcheck.a libc.so libc_nonshared.a libm.so libm.so.6 libc.so.6 libgcc_s.so.1"

main :: IO ()
main = do
  setFileCreationMask $ modes [groupWriteMode, otherWriteMode]
  rt ← getDataFileName "rt"
  putStr $ "Setting up " ++ rt ++ " ..."
  hFlush stdout
  (compiler_files >>=) $ mapM_ $ \f → do
    let to = rt ++ "/" ++ f -- can't use </> here because f is absolute
    createDirectoryIfMissing True $ takeDirectory to
    copyFile f to
  setFileCreationMask nullFileMode
  createFile (rt </> "lock") readModes >>= closeFd
  createFile (rt </> "t") accessModes >>= closeFd
  forM ["t.cpp", "t.s", "t.o"] $ (>>= closeFd) . flip createFile (unionFileModes writeModes readModes) . (rt </>)
  putStrLn " done."
