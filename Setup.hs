import Distribution.Simple
import Distribution.Simple.Setup (ConfigFlags)
import Distribution.PackageDescription (PackageDescription)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.InstallDirs (prefix, fromPathTemplate)

main :: IO ()
main = defaultMainWithHooks $ autoconfUserHooks { postConf = myPostConf }

myPostConf :: Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
myPostConf args flags pkg_descr lbi = do
  writeFile "prefix" $ fromPathTemplate $ prefix $ installDirTemplates lbi
  postConf autoconfUserHooks args flags pkg_descr lbi
