import Spec
import Dir

import Test.Hspec
import System.Process

main = do
  let sandboxes = [ "test/data", "test/data/check-packageid" ]
      genSandboxCfg dir = withDirectory dir $ \cwd -> do
         system ("sed 's|@CWD@|" ++ cwd ++ "|g' cabal.sandbox.config.in > cabal.sandbox.config")
  genSandboxCfg `mapM` sandboxes
  hspec spec
