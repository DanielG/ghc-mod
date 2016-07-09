{-# LANGUAGE CPP #-}
module Language.Haskell.GhcMod.Cradle
#ifndef SPEC
  (
    findCradle
  , findCradle'
  , findCradleNoLog
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
import Language.Haskell.GhcMod.Logging
import Language.Haskell.GhcMod.Error

import Safe
import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Maybe
import System.Directory
import System.FilePath
import Prelude
import Control.Monad.Trans.Journal (runJournalT)

----------------------------------------------------------------

-- | Finding 'Cradle'.
--   Find a cabal file by tracing ancestor directories.
--   Find a sandbox according to a cabal sandbox config
--   in a cabal directory.
findCradle :: (GmLog m, IOish m, GmOut m) => Programs -> m Cradle
findCradle progs = findCradle' progs =<< liftIO getCurrentDirectory

findCradleNoLog  :: forall m. (IOish m, GmOut m) => Programs -> m Cradle
findCradleNoLog progs =
    fst <$> (runJournalT (findCradle progs) :: m (Cradle, GhcModLog))

findCradle' :: (GmLog m, IOish m, GmOut m) => Programs -> FilePath -> m Cradle
findCradle' Programs { stackProgram, cabalProgram } dir = run $
    msum [ stackCradle stackProgram dir
         , cabalCradle cabalProgram dir
         , sandboxCradle dir
         , plainCradle dir
         ]
 where run a = fillTempDir =<< (fromJustNote "findCradle'" <$> runMaybeT a)

findSpecCradle ::
    (GmLog m, IOish m, GmOut m) => Programs -> FilePath -> m Cradle
findSpecCradle Programs { stackProgram, cabalProgram } dir = do
    let cfs = [ stackCradleSpec stackProgram
              , cabalCradle cabalProgram
              , sandboxCradle
              ]
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

cabalCradle ::
    (IOish m, GmLog m, GmOut m) => FilePath -> FilePath -> MaybeT m Cradle
cabalCradle cabalProg wdir = do
    -- If cabal doesn't exist the user probably wants to use something else
    whenM ((==Nothing) <$> liftIO (findExecutable cabalProg)) $ do
      gmLog GmInfo "" $ text "'dist/setup-config' exists but 'cabal' executable wasn't found"
      mzero

    cabalFile <- MaybeT $ liftIO $ findCabalFile wdir
    let cabalDir = takeDirectory cabalFile

    gmLog GmInfo "" $ text "found Cabal project at" <+>: text cabalDir
    return Cradle {
        cradleProject    = CabalProject
      , cradleCurrentDir = wdir
      , cradleRootDir    = cabalDir
      , cradleTempDir    = error "tmpDir"
      , cradleCabalFile  = Just cabalFile
      , cradleDistDir    = "dist"
      }

stackCradle ::
    (IOish m, GmLog m, GmOut m) => FilePath -> FilePath -> MaybeT m Cradle
stackCradle stackProg wdir = do
#if !MIN_VERSION_ghc(7,8,0)
    -- GHC < 7.8 is not supported by stack
    mzero
#endif

    -- If cabal doesn't exist the user probably wants to use something else
    whenM ((==Nothing) <$> liftIO (findExecutable stackProg)) $ do
      gmLog GmInfo "" $ text "'dist/setup-config' exists but 'cabal' executable wasn't found"
      mzero

    cabalFile <- MaybeT $ liftIO $ findCabalFile wdir

    let cabalDir = takeDirectory cabalFile

    _stackConfigFile <- MaybeT $ liftIO $ findStackConfigFile cabalDir

    -- If dist/setup-config already exists the user probably wants to use cabal
    -- rather than stack, or maybe that's just me ;)
    whenM (liftIO $ doesFileExist $ cabalDir </> setupConfigPath "dist") $ do
      gmLog GmWarning "" $ text "'dist/setup-config' exists, ignoring Stack and using cabal-install instead"
      mzero

    senv <- MaybeT $ getStackEnv cabalDir

    gmLog GmInfo "" $ text "found Stack project at" <+>: text cabalDir
    return Cradle {
        cradleProject    = StackProject senv
      , cradleCurrentDir = wdir
      , cradleRootDir    = cabalDir
      , cradleTempDir    = error "tmpDir"
      , cradleCabalFile  = Just cabalFile
      , cradleDistDir    = seDistDir senv
      }

stackCradleSpec ::
    (IOish m, GmLog m, GmOut m) => FilePath -> FilePath -> MaybeT m Cradle
stackCradleSpec stackProg wdir = do
  crdl <- stackCradle stackProg wdir
  case crdl of
    Cradle { cradleProject = StackProject StackEnv { seDistDir } } -> do
      b <- isGmDistDir seDistDir
      when b mzero
      return crdl
    _ -> error "stackCradleSpec"
 where
   isGmDistDir dir =
       liftIO $ not <$> doesFileExist (dir </> ".." </> "ghc-mod.cabal")

sandboxCradle :: (IOish m, GmLog m, GmOut m) => FilePath -> MaybeT m Cradle
sandboxCradle wdir = do
    sbDir <- MaybeT $ liftIO $ findCabalSandboxDir wdir
    gmLog GmInfo "" $ text "Found sandbox project at" <+>: text sbDir
    return Cradle {
        cradleProject    = SandboxProject
      , cradleCurrentDir = wdir
      , cradleRootDir    = sbDir
      , cradleTempDir    = error "tmpDir"
      , cradleCabalFile  = Nothing
      , cradleDistDir    = "dist"
      }

plainCradle :: (IOish m, GmLog m, GmOut m) => FilePath -> MaybeT m Cradle
plainCradle wdir = do
    gmLog GmInfo "" $ text "Found no other project type, falling back to plain GHC project"
    return $ Cradle {
        cradleProject    = PlainProject
      , cradleCurrentDir = wdir
      , cradleRootDir    = wdir
      , cradleTempDir    = error "tmpDir"
      , cradleCabalFile  = Nothing
      , cradleDistDir    = "dist"
      }
