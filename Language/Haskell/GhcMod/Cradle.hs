module Language.Haskell.GhcMod.Cradle (
    findCradle
  , findCradle'
  , findSpecCradle
  , cleanupCradle
  ) where

import Language.Haskell.GhcMod.PathsAndFiles
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Utils

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Maybe
import System.Directory
import System.FilePath
import Prelude

----------------------------------------------------------------

-- | Finding 'Cradle'.
--   Find a cabal file by tracing ancestor directories.
--   Find a sandbox according to a cabal sandbox config
--   in a cabal directory.
findCradle :: IO Cradle
findCradle = findCradle' =<< getCurrentDirectory

findCradle' :: FilePath -> IO Cradle
findCradle' dir = run $ do
    (stackCradle dir `mplus` cabalCradle dir `mplus` sandboxCradle dir `mplus` plainCradle dir)
 where run a = fillTempDir =<< (fromJust <$> runMaybeT a)

findSpecCradle :: FilePath -> IO Cradle
findSpecCradle dir = do
    let cfs = [cabalCradle, sandboxCradle]
    cs <- catMaybes <$> mapM (runMaybeT . ($ dir)) cfs
    gcs <- filterM isNotGmCradle cs
    fillTempDir =<< case gcs of
                      [] -> fromJust <$> runMaybeT (plainCradle dir)
                      c:_ -> return c
 where
   isNotGmCradle :: Cradle -> IO Bool
   isNotGmCradle crdl = do
     not <$> doesFileExist (cradleRootDir crdl </> "ghc-mod.cabal")

cleanupCradle :: Cradle -> IO ()
cleanupCradle crdl = removeDirectoryRecursive $ cradleTempDir crdl

fillTempDir :: MonadIO m => Cradle -> m Cradle
fillTempDir crdl = do
  tmpDir <- liftIO $ newTempDir (cradleRootDir crdl)
  return crdl { cradleTempDir = tmpDir }

cabalCradle :: FilePath -> MaybeT IO Cradle
cabalCradle wdir = do
    cabalFile <- MaybeT $ findCabalFile wdir

    let cabalDir = takeDirectory cabalFile

    return Cradle {
        cradleProjectType = CabalProject
      , cradleCurrentDir = wdir
      , cradleRootDir    = cabalDir
      , cradleTempDir    = error "tmpDir"
      , cradleCabalFile  = Just cabalFile
      , cradleDistDir    = "dist"
      }

stackCradle :: FilePath -> MaybeT IO Cradle
stackCradle wdir = do
    cabalFile <- MaybeT $ findCabalFile wdir

    let cabalDir = takeDirectory cabalFile

    _stackConfigFile <- MaybeT $ findStackConfigFile cabalDir
    distDir <- liftIO $ findStackDistDir cabalDir

    return Cradle {
        cradleProjectType = StackProject
      , cradleCurrentDir = wdir
      , cradleRootDir    = cabalDir
      , cradleTempDir    = error "tmpDir"
      , cradleCabalFile  = Just cabalFile
      , cradleDistDir    = distDir
      }

sandboxCradle :: FilePath -> MaybeT IO Cradle
sandboxCradle wdir = do
    sbDir <- MaybeT $ findCabalSandboxDir wdir
    return Cradle {
        cradleProjectType = SandboxProject
      , cradleCurrentDir = wdir
      , cradleRootDir    = sbDir
      , cradleTempDir    = error "tmpDir"
      , cradleCabalFile  = Nothing
      , cradleDistDir    = "dist"
      }

plainCradle :: FilePath -> MaybeT IO Cradle
plainCradle wdir = do
    return $ Cradle {
        cradleProjectType = PlainProject
      , cradleCurrentDir = wdir
      , cradleRootDir    = wdir
      , cradleTempDir    = error "tmpDir"
      , cradleCabalFile  = Nothing
      , cradleDistDir    = "dist"
      }
