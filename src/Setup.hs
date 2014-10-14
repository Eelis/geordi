#! /usr/bin/env runhaskell

import Distribution.Simple
import Distribution.Simple.Setup (ConfigFlags, InstallFlags)
import Distribution.PackageDescription (PackageDescription(..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.InstallDirs (initialPathTemplateEnv, prefix, bindir, datadir, datasubdir, fromPathTemplate, installDirsTemplateEnv, packageTemplateEnv, substPathTemplate)
import System.Posix.Files (setFileMode, ownerModes)

main :: IO ()
main = defaultMain
