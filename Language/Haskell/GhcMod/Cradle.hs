{-# LANGUAGE CPP #-}
module Language.Haskell.GhcMod.Cradle
#ifndef SPEC
  (
    findCradle
  , findCradle'
  , findSpecCradle
  , cleanupCradle
  )
#endif
  where

import Language.Haskell.GhcMod.PathsAndFiles
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Utils
import Language.Haskell.GhcMod.Stack

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
findCradle :: (IOish m, GmOut m) => m Cradle
findCradle = findCradle' =<< liftIO getCurrentDirectory

findCradle' :: (IOish m, GmOut m) => FilePath -> m Cradle
findCradle' dir = run $
    msum [ stackCradle dir
         , cabalCradle dir
         , sandboxCradle dir
         , explicitCradle dir
         , plainCradle dir
         ]
 where run a = fillTempDir =<< (fromJust <$> runMaybeT a)

findSpecCradle :: (IOish m, GmOut m) => FilePath -> m Cradle
findSpecCradle dir = do
    let cfs = [stackCradleSpec, cabalCradle, sandboxCradle]
    cs <- catMaybes <$> mapM (runMaybeT . ($ dir)) cfs
    gcs <- filterM isNotGmCradle cs
    fillTempDir =<< case gcs of
                      [] -> fromJust <$> runMaybeT (plainCradle dir)
                      c:_ -> return c
 where
   isNotGmCradle crdl =
     liftIO $ not <$> doesFileExist (cradleRootDir crdl </> "ghc-mod.cabal")

cleanupCradle :: Cradle -> IO ()
cleanupCradle crdl = removeDirectoryRecursive $ cradleTempDir crdl

fillTempDir :: IOish m => Cradle -> m Cradle
fillTempDir crdl = do
  tmpDir <- liftIO $ newTempDir (cradleRootDir crdl)
  return crdl { cradleTempDir = tmpDir }

cabalCradle :: IOish m => FilePath -> MaybeT m Cradle
cabalCradle wdir = do
    cabalFile <- MaybeT $ liftIO $ findCabalFile wdir

    let cabalDir = takeDirectory cabalFile

    return Cradle {
        cradleProject    = CabalProject
      , cradleCurrentDir = wdir
      , cradleRootDir    = cabalDir
      , cradleTempDir    = error "tmpDir"
      , cradleCabalFile  = Just cabalFile
      , cradleDistDir    = "dist"
      }

stackCradle :: (IOish m, GmOut m) => FilePath -> MaybeT m Cradle
stackCradle wdir = do
    cabalFile <- MaybeT $ liftIO $ findCabalFile wdir

    let cabalDir = takeDirectory cabalFile

    _stackConfigFile <- MaybeT $ liftIO $ findStackConfigFile cabalDir

    -- If dist/setup-config already exists the user probably wants to use cabal
    -- rather than stack, or maybe that's just me ;)
    whenM (liftIO $ doesFileExist $ setupConfigPath "dist") $ mzero

    senv <- MaybeT $ getStackEnv cabalDir

    return Cradle {
        cradleProject    = StackProject senv
      , cradleCurrentDir = wdir
      , cradleRootDir    = cabalDir
      , cradleTempDir    = error "tmpDir"
      , cradleCabalFile  = Just cabalFile
      , cradleDistDir    = seDistDir senv
      }

stackCradleSpec :: (IOish m, GmOut m) => FilePath -> MaybeT m Cradle
stackCradleSpec wdir = do
  crdl <- stackCradle wdir
  case crdl of
    Cradle { cradleProject = StackProject StackEnv { seDistDir } } -> do
      b <- isGmDistDir seDistDir
      when b mzero
      return crdl
    _ -> error "stackCradleSpec"
 where
   isGmDistDir dir =
       liftIO $ not <$> doesFileExist (dir </> ".." </> "ghc-mod.cabal")

sandboxCradle :: IOish m => FilePath -> MaybeT m Cradle
sandboxCradle wdir = do
    sbDir <- MaybeT $ liftIO $ findCabalSandboxDir wdir
    return Cradle {
        cradleProject    = SandboxProject
      , cradleCurrentDir = wdir
      , cradleRootDir    = sbDir
      , cradleTempDir    = error "tmpDir"
      , cradleCabalFile  = Nothing
      , cradleDistDir    = "dist"
      }

plainCradle :: IOish m => FilePath -> MaybeT m Cradle
plainCradle wdir = do
    return $ Cradle {
        cradleProject    = PlainProject
      , cradleCurrentDir = wdir
      , cradleRootDir    = wdir
      , cradleTempDir    = error "tmpDir"
      , cradleCabalFile  = Nothing
      , cradleDistDir    = "dist"
      }

explicitCradle :: IOish m => FilePath -> MaybeT m Cradle
explicitCradle wdir = do
    optionsFile <- MaybeT $ liftIO $ findExplicitOptionsFile wdir
    return $ Cradle {
        cradleProject    = ExplicitProject
      , cradleCurrentDir = wdir
      , cradleRootDir    = takeDirectory optionsFile
      , cradleTempDir    = error "tmpDir"
      , cradleCabalFile  = Just optionsFile
      , cradleDistDir    = ""
      }
