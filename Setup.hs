#! /usr/bin/env runhaskell

import Distribution.Simple
import Distribution.Simple.Setup (ConfigFlags, InstallFlags)
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.InstallDirs (initialPathTemplateEnv, prefix, bindir, datadir, datasubdir, fromPathTemplate, installDirsTemplateEnv, packageTemplateEnv, substPathTemplate)
import System.Posix.Files (setFileMode, ownerModes)

main :: IO ()
main = defaultMainWithHooks $ autoconfUserHooks { postInst = myPostInst }

myPostInst :: Args -> InstallFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostInst args flags pkg_descr lbi = do
  (pre, _:post) <- fmap (span (/= "DATA=\"\"") . lines) $ readFile "scripts/compile-prelude"
  let
    idt = installDirTemplates lbi
    env = installDirsTemplateEnv idt
    idt' = fmap (fromPathTemplate
      . substPathTemplate env
      . substPathTemplate (packageTemplateEnv (package pkg_descr))) idt
      -- This makes little sense to me, but it works...
    to = bindir idt' ++ "/geordi-compile-prelude"
  writeFile to $ unlines $ pre ++ ["DATA=\"" ++ datadir idt' ++ "/" ++ datasubdir idt' ++ "/\""] ++ post
  setFileMode to ownerModes
  postInst autoconfUserHooks args flags pkg_descr lbi
