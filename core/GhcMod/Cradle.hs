{-# LANGUAGE CPP #-}
module GhcMod.Cradle
  ( findCradle
  , findCradle'
  , findCradleNoLog
  , findSpecCradle
  , cleanupCradle

  -- * for @spec@
  , plainCradle
  ) where

import GhcMod.PathsAndFiles
import GhcMod.Monad.Types
import GhcMod.Types
import GhcMod.Utils
import GhcMod.Stack
import GhcMod.Logging
import GhcMod.Error

import Safe
import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Maybe
import System.Directory
import System.FilePath
import System.Environment
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
    cabalFile <- MaybeT $ liftIO $ findCabalFile wdir
    let cabalDir = takeDirectory cabalFile

    gmLog GmInfo "" $ text "Found Cabal project at" <+>: text cabalDir

    -- If cabal doesn't exist the user probably wants to use something else
    whenM ((==Nothing) <$> liftIO (findExecutable cabalProg)) $ do
      gmLog GmInfo "" $ text "'cabal' executable wasn't found, trying next project type"
      mzero

    gmLog GmInfo "" $ text "Using Cabal project at" <+>: text cabalDir
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
#if __GLASGOW_HASKELL__ < 708
    -- GHC < 7.8 is not supported by stack
    mzero
#endif

    cabalFile <- MaybeT $ liftIO $ findCabalFile wdir
    let cabalDir = takeDirectory cabalFile
    _stackConfigFile <- MaybeT $ liftIO $ findStackConfigFile cabalDir

    gmLog GmInfo "" $ text "Found Stack project at" <+>: text cabalDir

    stackExeSet    <- liftIO $ isJust <$> lookupEnv "STACK_EXE"
    stackExeExists <- liftIO $ isJust <$> findExecutable stackProg
    setupCfgExists <- liftIO $ doesFileExist $ cabalDir </> setupConfigPath "dist"

    case (stackExeExists, stackExeSet) of
      (False, True) -> do
        gmLog GmWarning "" $ text "'stack' executable wasn't found but STACK_EXE is set, trying next project type"
        mzero

      (False, False) -> do
        gmLog GmInfo "" $ text "'stack' executable wasn't found, trying next project type"
        mzero

      (True, True) -> do
        gmLog GmInfo "" $ text "STACK_EXE set, preferring Stack project"

      (True, False) | setupCfgExists -> do
        gmLog GmWarning "" $ text "'dist/setup-config' exists, ignoring Stack project"
        mzero

      (True, False) -> return ()

    senv <- MaybeT $ getStackEnv cabalDir stackProg

    gmLog GmInfo "" $ text "Using Stack project at" <+>: text cabalDir
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
    gmLog GmInfo "" $ text "Using sandbox project at" <+>: text sbDir
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
