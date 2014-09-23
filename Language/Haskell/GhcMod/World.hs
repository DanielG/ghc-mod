{-# LANGUAGE RecordWildCards, CPP #-}

module Language.Haskell.GhcMod.World (
    World
  , getCurrentWorld
  , isWorldChanged
  , isSetupConfigValid
  ) where

import Control.Applicative ((<$>))
import Data.Traversable (traverse)
import DynFlags (DynFlags(..))
import Language.Haskell.GhcMod.CabalConfig
import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.Types
import System.Directory (doesFileExist, getModificationTime)
import System.FilePath ((</>))

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

data World = World {
    worldCabalFile :: Maybe FilePath
  , worldCabalFileModificationTime :: Maybe ModTime
  , worldPackageCache :: FilePath
  , worldPackageCacheModificationTime :: ModTime
  , worldSetupConfig :: FilePath
  , worldSetupConfigModificationTime :: Maybe ModTime
  } deriving (Show, Eq)

getCurrentWorld :: Cradle -> DynFlags -> IO World
getCurrentWorld crdl dflags = do
    cachePath <- getPackageCachePath crdl dflags
    let mCabalFile = cradleCabalFile crdl
        pkgCache = cachePath </> packageCache
        setupFile = setupConfigFile crdl
    mCabalFileMTime <- getModificationTime `traverse` mCabalFile
    pkgCacheMTime <- getModificationTime pkgCache
    exist <- doesFileExist setupFile
    mSeetupMTime <- if exist then
                        Just <$> getModificationTime setupFile
                      else
                        return Nothing
    return $ World {
        worldCabalFile = mCabalFile
      , worldCabalFileModificationTime = mCabalFileMTime
      , worldPackageCache = pkgCache
      , worldPackageCacheModificationTime = pkgCacheMTime
      , worldSetupConfig = setupFile
      , worldSetupConfigModificationTime = mSeetupMTime
      }

isWorldChanged :: World -> Cradle -> DynFlags -> IO Bool
isWorldChanged world crdl dflags = do
    world' <- getCurrentWorld crdl dflags
    return (world /= world')

isSetupConfigValid :: World -> Bool
isSetupConfigValid World{ worldSetupConfigModificationTime = Nothing, ..} = False
isSetupConfigValid World{ worldSetupConfigModificationTime = Just mt, ..} =
    cond1 && cond2
  where
    cond1 = case worldCabalFileModificationTime of
        Nothing -> True
        Just mtime -> mtime <= mt
    cond2 = worldPackageCacheModificationTime <= mt
