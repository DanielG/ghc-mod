module Language.Haskell.GhcMod.Cradle (
    findCradle
  , findCradle'
  , findCradleWithoutSandbox
  , cleanupCradle
  ) where

import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.PathsAndFiles
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Utils

import Control.Exception.IOChoice ((||>))
import System.Directory (getCurrentDirectory, removeDirectoryRecursive)
import System.FilePath (takeDirectory)


----------------------------------------------------------------

-- | Finding 'Cradle'.
--   Find a cabal file by tracing ancestor directories.
--   Find a sandbox according to a cabal sandbox config
--   in a cabal directory.
findCradle :: IO Cradle
findCradle = findCradle' =<< getCurrentDirectory

findCradle' :: FilePath -> IO Cradle
findCradle' dir = cabalCradle dir ||> sandboxCradle dir ||> plainCradle dir

cleanupCradle :: Cradle -> IO ()
cleanupCradle crdl = removeDirectoryRecursive $ cradleTempDir crdl

cabalCradle :: FilePath -> IO Cradle
cabalCradle wdir = do
    Just cabalFile <- findCabalFile wdir
    let cabalDir = takeDirectory cabalFile
    pkgDbStack <- getPackageDbStack cabalDir
    tmpDir <- newTempDir cabalDir
    return Cradle {
        cradleCurrentDir = wdir
      , cradleRootDir    = cabalDir
      , cradleTempDir    = tmpDir
      , cradleCabalFile  = Just cabalFile
      , cradlePkgDbStack = pkgDbStack
      }

sandboxCradle :: FilePath -> IO Cradle
sandboxCradle wdir = do
    Just sbDir <- getSandboxDb wdir
    pkgDbStack <- getPackageDbStack wdir
    tmpDir <- newTempDir sbDir
    return Cradle {
        cradleCurrentDir = wdir
      , cradleRootDir    = sbDir
      , cradleTempDir    = tmpDir
      , cradleCabalFile  = Nothing
      , cradlePkgDbStack = pkgDbStack
      }

plainCradle :: FilePath -> IO Cradle
plainCradle wdir = do
    tmpDir <- newTempDir wdir
    return Cradle {
        cradleCurrentDir = wdir
      , cradleRootDir    = wdir
      , cradleTempDir    = tmpDir
      , cradleCabalFile  = Nothing
      , cradlePkgDbStack = [GlobalDb, UserDb]
      }

-- Just for testing
findCradleWithoutSandbox :: IO Cradle
findCradleWithoutSandbox = do
    cradle <- findCradle
    return cradle { cradlePkgDbStack = [GlobalDb]} -- FIXME
