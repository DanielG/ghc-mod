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
    (stackCradle dir `mplus` customCradle dir `mplus` cabalCradle dir `mplus`  sandboxCradle dir `mplus` plainCradle dir)
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

customCradle :: FilePath -> MaybeT IO Cradle
customCradle wdir = do
    cabalFile <- MaybeT $ findCabalFile wdir
    let cabalDir = takeDirectory cabalFile
    cradleFile <- MaybeT $ findCradleFile cabalDir
    pkgDbStack <- liftIO $ parseCradle cradleFile
    return Cradle {
        cradleCurrentDir = wdir
      , cradleRootDir    = cabalDir
      , cradleTempDir    = error "tmpDir"
      , cradleCabalFile  = Just cabalFile
      , cradlePkgDbStack = pkgDbStack
      }

stackCradle :: FilePath -> MaybeT IO Cradle
stackCradle wdir = do
    cabalFile <- MaybeT $ findCabalFile wdir

    let cabalDir = takeDirectory cabalFile
    pkgDbStack <- liftIO $ getStackPackageDbStack -- cabalDir

    return Cradle {
        cradleCurrentDir = wdir
      , cradleRootDir    = cabalDir
      , cradleTempDir    = error "tmpDir"
      , cradleCabalFile  = Just cabalFile
      , cradlePkgDbStack = pkgDbStack
      }

cabalCradle :: FilePath -> MaybeT IO Cradle
cabalCradle wdir = do
    cabalFile <- MaybeT $ findCabalFile wdir

    let cabalDir = takeDirectory cabalFile
    pkgDbStack <- liftIO $ getPackageDbStack cabalDir

    return Cradle {
        cradleCurrentDir = wdir
      , cradleRootDir    = cabalDir
      , cradleTempDir    = error "tmpDir"
      , cradleCabalFile  = Just cabalFile
      , cradlePkgDbStack = pkgDbStack
      }

sandboxCradle :: FilePath -> MaybeT IO Cradle
sandboxCradle wdir = do
    sbDir <- MaybeT $ findCabalSandboxDir wdir
    pkgDbStack <- liftIO $ getPackageDbStack sbDir
    return Cradle {
        cradleCurrentDir = wdir
      , cradleRootDir    = sbDir
      , cradleTempDir    = error "tmpDir"
      , cradleCabalFile  = Nothing
      , cradlePkgDbStack = pkgDbStack
      }

plainCradle :: FilePath -> MaybeT IO Cradle
plainCradle wdir = do
    return $ Cradle {
        cradleCurrentDir = wdir
      , cradleRootDir    = wdir
      , cradleTempDir    = error "tmpDir"
      , cradleCabalFile  = Nothing
      , cradlePkgDbStack = [GlobalDb, UserDb]
      }

getPackageDbStack :: FilePath -- ^ Project Directory (where the
                                 -- cabal.sandbox.config file would be if it
                                 -- exists)
                  -> IO [GhcPkgDb]
getPackageDbStack cdir =
    ([GlobalDb] ++) . maybe [UserDb] return <$> getSandboxDb cdir

getStackPackageDbStack :: IO [GhcPkgDb]
getStackPackageDbStack = do
  pl <- getStackDbList
  return (GlobalDb:pl)

parseCradle :: FilePath -> IO [GhcPkgDb]
parseCradle path = do
    source <- readFile path
    return $ parseCradle' source
  where
    parseCradle' source = map parsePkgDb $ filter (not . null) $ lines source

    parsePkgDb "global" = GlobalDb
    parsePkgDb "user" = UserDb
    parsePkgDb s = PackageDb s
