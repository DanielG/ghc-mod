module GhcMod.World where

import GhcMod.GhcPkg
import GhcMod.PathsAndFiles
import GhcMod.Types
import GhcMod.Monad.Types
import GhcMod.Utils

import Control.Applicative
import Data.Maybe
import Data.Traversable hiding (mapM)
import System.FilePath ((</>))

import GHC.Paths (libdir)
import Prelude

data World = World {
    worldPackageCaches :: [TimedFile]
  , worldCabalFile     :: Maybe TimedFile
  , worldCabalConfig   :: Maybe TimedFile
  , worldCabalSandboxConfig :: Maybe TimedFile
  , worldMappedFiles   :: FileMappingMap
  } deriving (Eq)

timedPackageCaches :: IOish m => GhcModT m [TimedFile]
timedPackageCaches = do
    fs <- mapM (liftIO . mightExist) . map (</> packageCache)
            =<< getPackageCachePaths libdir
    (liftIO . timeFile) `mapM` catMaybes fs

getCurrentWorld :: IOish m => GhcModT m World
getCurrentWorld = do
    crdl <- cradle
    pkgCaches    <- timedPackageCaches
    mCabalFile   <- liftIO $ timeFile `traverse` cradleCabalFile crdl
    mCabalConfig <- liftIO $ timeMaybe (setupConfigFile crdl)
    mCabalSandboxConfig <- liftIO $ timeMaybe (sandboxConfigFile crdl)
    mFileMap     <- getMMappedFiles

    return World {
        worldPackageCaches = pkgCaches
      , worldCabalFile     = mCabalFile
      , worldCabalConfig   = mCabalConfig
      , worldCabalSandboxConfig = mCabalSandboxConfig
      , worldMappedFiles   = mFileMap
      }

didWorldChange :: IOish m => World -> GhcModT m Bool
didWorldChange world = do
    (world /=) <$> getCurrentWorld

isYoungerThanSetupConfig :: FilePath -> World -> IO Bool
isYoungerThanSetupConfig file World {..} = do
  tfile <- timeFile file
  return $ worldCabalConfig < Just tfile
