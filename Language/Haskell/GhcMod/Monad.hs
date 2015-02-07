{-# LANGUAGE CPP, RecordWildCards #-}
module Language.Haskell.GhcMod.Monad (
  -- * Monad Types
    GhcModT
  , IOish
  -- ** Environment, state and logging
  , GhcModEnv(..)
  , newGhcModEnv
  , GhcModState(..)
  , defaultState
  , CompilerMode(..)
  , GhcModLog
  , GhcModError(..)
  -- * Monad utilities
  , runGhcModT
  , runGhcModT'
  , hoistGhcModT
  -- ** Accessing 'GhcModEnv', 'GhcModState' and 'GhcModLog'
  , gmsGet
  , gmsPut
  , gmLog
  , options
  , cradle
  , getCompilerMode
  , setCompilerMode
  , withOptions
  , withTempSession
  -- ** Re-exporting convenient stuff
  , liftIO
  , module Control.Monad.Reader.Class
  ) where

import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Logging
import Language.Haskell.GhcMod.Error
import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.DynFlags
import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.CabalApi
import Language.Haskell.GhcMod.CabalConfig
import qualified Language.Haskell.GhcMod.Gap as Gap

import GHC
import qualified GHC as G
import GHC.Paths (libdir)
import GhcMonad hiding (withTempSession)
#if __GLASGOW_HASKELL__ <= 702
import HscTypes
#endif

-- MonadUtils of GHC 7.6 or earlier defines its own MonadIO.
-- RWST does not automatically become an instance of MonadIO.
-- MonadUtils of GHC 7.8 or later imports MonadIO in Monad.Control.IO.Class.
-- So, RWST automatically becomes an instance of MonadIO.
import MonadUtils

import Control.Arrow (first)
import Control.Monad (void)
#if !MIN_VERSION_monad_control(1,0,0)
import Control.Monad (liftM)
#endif
import Control.Monad.Base (liftBase)

import Control.Monad.Reader.Class
import Control.Monad.State.Class (MonadState(..))

import Control.Monad.Error (runErrorT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (runStateT)
import Control.Monad.Trans.Journal (runJournalT)

import Data.Maybe (isJust)
import Data.IORef
import System.Directory (getCurrentDirectory)

----------------------------------------------------------------

-- | Initialize the 'DynFlags' relating to the compilation of a single
-- file or GHC session according to the 'Cradle' and 'Options'
-- provided.
initializeFlagsWithCradle :: (IOish m, GhcMonad m, GmError m, GmLog m)
        => Options
        -> Cradle
        -> CabalConfig
        -> m ()
initializeFlagsWithCradle opt c config
  | cabal     = withCabal
  | otherwise = withSandbox
  where
    mCabalFile = cradleCabalFile c

    cabal = isJust mCabalFile

    ghcopts = ghcUserOptions opt

    withCabal = do
        let Just cabalFile = mCabalFile
        pkgDesc <- parseCabalFile config cabalFile
        compOpts <- getCompilerOptions ghcopts c config pkgDesc
        initSession CabalPkg opt compOpts

    withSandbox = initSession SingleFile opt compOpts
      where
        importDirs = [".","..","../..","../../..","../../../..","../../../../.."]

        pkgOpts = ghcDbStackOpts $ cradlePkgDbStack c

        compOpts
          | null pkgOpts = CompilerOptions ghcopts importDirs []
          | otherwise    = CompilerOptions (ghcopts ++ pkgOpts) [wdir,rdir] []

        (wdir, rdir) = (cradleCurrentDir c, cradleRootDir c)

initSession :: GhcMonad m
            => Build
            -> Options
            -> CompilerOptions
            -> m ()
initSession build Options {..} CompilerOptions {..} = do
    df <- G.getSessionDynFlags
    void $ G.setSessionDynFlags =<< addCmdOpts ghcOptions
      ( setModeSimple
      $ Gap.setFlags
      $ setIncludeDirs includeDirs
      $ setBuildEnv build
      $ setEmptyLogger
      $ Gap.addPackageFlags depPackages df)

----------------------------------------------------------------

newGhcModEnv :: Options -> FilePath -> IO GhcModEnv
newGhcModEnv opt dir = do
      session <- newIORef (error "empty session")
      c <- findCradle' dir
      return GhcModEnv {
          gmGhcSession = session
        , gmOptions = opt
        , gmCradle = c
        }

cleanupGhcModEnv :: GhcModEnv -> IO ()
cleanupGhcModEnv env = cleanupCradle $ gmCradle env

-- | Run a @GhcModT m@ computation.
runGhcModT :: IOish m
           => Options
           -> GhcModT m a
           -> m (Either GhcModError a, GhcModLog)
runGhcModT opt action = gbracket newEnv delEnv $ \env -> do
    r <- first (fst <$>) <$> (runGhcModT' env defaultState $ do
        dflags <- getSessionDynFlags
        defaultCleanupHandler dflags $ do
            config <- cabalGetConfig =<< cradle
            initializeFlagsWithCradle opt (gmCradle env) config
            action )
    return r

 where
   newEnv = liftBase $ newGhcModEnv opt =<< getCurrentDirectory
   delEnv = liftBase . cleanupGhcModEnv

-- | @hoistGhcModT result@. Embed a GhcModT computation's result into a GhcModT
-- computation. Note that if the computation that returned @result@ modified the
-- state part of GhcModT this cannot be restored.
hoistGhcModT :: IOish m
             => (Either GhcModError a, GhcModLog)
             -> GhcModT m a
hoistGhcModT (r,l) = do
  gmJournal l >> case r of
    Left e -> throwError e
    Right a -> return a

-- | Run a computation inside @GhcModT@ providing the RWST environment and
-- initial state. This is a low level function, use it only if you know what to
-- do with 'GhcModEnv' and 'GhcModState'.
--
-- You should probably look at 'runGhcModT' instead.
runGhcModT' :: IOish m
           => GhcModEnv
           -> GhcModState
           -> GhcModT m a
           -> m (Either GhcModError (a, GhcModState), GhcModLog)
runGhcModT' r s a = do
  (res, w') <-
      flip runReaderT r $ runJournalT $ runErrorT $
        runStateT (unGhcModT $ initGhcMonad (Just libdir) >> a) s
  return (res, w')
----------------------------------------------------------------
-- | Make a copy of the 'gmGhcSession' IORef, run the action and restore the
-- original 'HscEnv'.
withTempSession :: IOish m => GhcModT m a -> GhcModT m a
withTempSession action = do
  session <- gmGhcSession <$> ask
  savedHscEnv <- liftIO $ readIORef session
  a <- action
  liftIO $ writeIORef session savedHscEnv
  return a

----------------------------------------------------------------

gmeAsk :: IOish m => GhcModT m GhcModEnv
gmeAsk = ask

gmsGet :: IOish m => GhcModT m GhcModState
gmsGet = GhcModT get

gmsPut :: IOish m => GhcModState -> GhcModT m ()
gmsPut = GhcModT . put

options :: IOish m => GhcModT m Options
options = gmOptions <$> gmeAsk

cradle :: IOish m => GhcModT m Cradle
cradle = gmCradle <$> gmeAsk

getCompilerMode :: IOish m => GhcModT m CompilerMode
getCompilerMode = gmCompilerMode <$> gmsGet

setCompilerMode :: IOish m => CompilerMode -> GhcModT m ()
setCompilerMode mode = (\s -> gmsPut s { gmCompilerMode = mode } ) =<< gmsGet

----------------------------------------------------------------

withOptions :: IOish m => (Options -> Options) -> GhcModT m a -> GhcModT m a
withOptions changeOpt action = local changeEnv action
  where
    changeEnv e = e { gmOptions = changeOpt opt }
      where
        opt = gmOptions e

----------------------------------------------------------------
