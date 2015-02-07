{-# LANGUAGE RecordWildCards, CPP #-}
module Language.Haskell.GhcMod.World where
{-(
  , World
  , getCurrentWorld
  , isWorldChanged
  ) where
-}

import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.PathsAndFiles
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Utils

import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad
import Data.Maybe
import Data.Traversable (traverse)
import System.Directory (getModificationTime)
import System.FilePath ((</>))

import GHC.Paths (libdir)

#if __GLASGOW_HASKELL__ <= 704
import System.Time (ClockTime)
#else
import Data.Time (UTCTime)
#endif


#if __GLASGOW_HASKELL__ <= 704
type ModTime = ClockTime
#else
type ModTime = UTCTime
#endif

data TimedFile = TimedFile FilePath ModTime deriving (Eq, Show)

instance Ord TimedFile where
    compare (TimedFile _ a) (TimedFile _ b) = compare a b

timeFile :: FilePath -> IO TimedFile
timeFile f = TimedFile <$> pure f <*> getModificationTime f

data World = World {
    worldPackageCaches :: [TimedFile]
  , worldCabalFile     :: Maybe TimedFile
  , worldCabalConfig   :: Maybe TimedFile
  , worldSymbolCache   :: Maybe TimedFile
  , worldPrettyCabalConfigCache :: Maybe TimedFile
  } deriving (Eq, Show)

timedPackageCache :: Cradle -> IO [TimedFile]
timedPackageCache crdl = do
    fs <- mapM mightExist . map (</> packageCache)
            =<< getPackageCachePaths libdir crdl
    timeFile `mapM` catMaybes fs

getCurrentWorld :: Cradle -> IO World
getCurrentWorld crdl = do
    pkgCaches    <- timedPackageCache crdl
    mCabalFile   <- timeFile `traverse` cradleCabalFile crdl
    mCabalConfig <- timeMaybe (setupConfigFile crdl)
    mSymbolCache <- timeMaybe (symbolCache crdl)
    mPrettyConfigCache <- timeMaybe prettyConfigCache

    return World {
        worldPackageCaches = pkgCaches
      , worldCabalFile     = mCabalFile
      , worldCabalConfig   = mCabalConfig
      , worldSymbolCache   = mSymbolCache
      , worldPrettyCabalConfigCache = mPrettyConfigCache
      }

 where
   timeMaybe :: FilePath -> IO (Maybe TimedFile)
   timeMaybe f = do
     join $ (timeFile `traverse`) <$> mightExist f

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
