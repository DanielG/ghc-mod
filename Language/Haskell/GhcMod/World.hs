module Language.Haskell.GhcMod.World where

import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.PathsAndFiles
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Utils

import Control.Applicative
import Data.Maybe
import Data.Traversable
import System.FilePath ((</>))

import GHC.Paths (libdir)
import Prelude

data World = World {
    worldPackageCaches :: [TimedFile]
  , worldCabalFile     :: Maybe TimedFile
  , worldCabalConfig   :: Maybe TimedFile
  , worldSymbolCache   :: Maybe TimedFile
  } deriving (Eq, Show)

timedPackageCaches :: Cradle -> IO [TimedFile]
timedPackageCaches crdl = do
    fs <- mapM mightExist . map (</> packageCache)
            =<< getPackageCachePaths libdir crdl
    timeFile `mapM` catMaybes fs

getCurrentWorld :: Cradle -> IO World
getCurrentWorld crdl = do
    pkgCaches    <- timedPackageCaches crdl
    mCabalFile   <- timeFile `traverse` cradleCabalFile crdl
    mCabalConfig <- timeMaybe (setupConfigFile crdl)
    mSymbolCache <- timeMaybe (symbolCache crdl)

    return World {
        worldPackageCaches = pkgCaches
      , worldCabalFile     = mCabalFile
      , worldCabalConfig   = mCabalConfig
      , worldSymbolCache   = mSymbolCache
      }

didWorldChange :: World -> Cradle -> IO Bool
didWorldChange world crdl = do
    (world /=) <$> getCurrentWorld crdl

-- * Neither file exists -> should return False:
--   @Nothing < Nothing = False@
--   (since we don't need to @cabal configure@ when no cabal file exists.)
--
-- * Cabal file doesn't exist (unlikely case) -> should return False
--   @Just cc < Nothing = False@
--   TODO: should we delete dist/setup-config?
--
-- * dist/setup-config doesn't exist yet -> should return True:
--   @Nothing < Just cf = True@
--
-- * Both files exist
--   @Just cc < Just cf = cc < cf = cc `olderThan` cf@
isSetupConfigOutOfDate :: World -> Bool
isSetupConfigOutOfDate World {..} = do
  worldCabalConfig < worldCabalFile

isYoungerThanSetupConfig :: FilePath -> World -> IO Bool
isYoungerThanSetupConfig file World {..} = do
  tfile <- timeFile file
  return $ worldCabalConfig < Just tfile
