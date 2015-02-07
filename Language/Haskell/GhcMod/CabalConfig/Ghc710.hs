module Language.Haskell.GhcMod.CabalConfig.Ghc710 (
    configDependencies
  , configFlags
  , getConfig
  ) where

import Control.Monad
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo, externalPackageDeps)
import qualified Distribution.Simple.LocalBuildInfo as LBI
import Distribution.Simple.Configure (getConfigStateFile)
import Distribution.Simple.Setup (configConfigurationsFlags)
import Distribution.PackageDescription (FlagAssignment)

import MonadUtils (liftIO)

import Language.Haskell.GhcMod.Error
import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.PathsAndFiles
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Utils
import Language.Haskell.GhcMod.World


-- | Get contents of the file containing 'LocalBuildInfo' data. If it doesn't
-- exist run @cabal configure@ i.e. configure with default options like @cabal
-- build@ would do.
getConfig :: (IOish m, GmError m)
          => Cradle
          -> m LocalBuildInfo
getConfig cradle = liftIO (getCurrentWorld cradle) >>= \world -> do
    when (isSetupConfigOutOfDate world) configure
    liftIO (getConfigStateFile file) `tryFix` \_ ->
        configure `modifyError'` GMECabalConfigure
 where
   file = setupConfigFile cradle
   prjDir = cradleRootDir cradle

   configure :: (IOish m, GmError m) => m ()
   configure = withDirectory_ prjDir $ void $ readProcess' "cabal" ["configure"]

configDependencies :: a -> LocalBuildInfo -> [Package]
configDependencies _ lbi =
  [ fromInstalledPackageId instPkgId
  | (instPkgId, _) <- externalPackageDeps lbi ]


configFlags :: LocalBuildInfo -> Either String FlagAssignment
configFlags = Right . configConfigurationsFlags . LBI.configFlags
