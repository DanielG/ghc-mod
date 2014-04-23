import Spec
import Dir

import Test.Hspec
import System.Process

main = do
  let sandboxes = [ "test/data", "test/data/check-packageid" ]
      genSandboxCfg dir = withDirectory dir $ \cwd -> do
         system ("sed 's|@CWD@|" ++ cwd ++ "|g' cabal.sandbox.config.in > cabal.sandbox.config")
      pkgDirs =
        [ "test/data/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"
        , "test/data/check-packageid/.cabal-sandbox/i386-osx-ghc-7.6.3-packages.conf.d"]
      genGhcPkgCache dir = system $ "ghc-pkg recache --force -f" ++ dir
  genSandboxCfg `mapM_` sandboxes
  genGhcPkgCache `mapM_` pkgDirs
  hspec spec
