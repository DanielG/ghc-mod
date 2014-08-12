{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances, RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

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
  , withErrorHandler
  -- ** Conversion
  , toGhcModT
  -- ** Accessing 'GhcModEnv' and 'GhcModState'
  , options
  , cradle
  , getCompilerMode
  , setCompilerMode
  , withOptions
  -- ** Exporting convenient modules
  , module Control.Monad.Reader.Class
  , module Control.Monad.Writer.Class
  , module Control.Monad.State.Class
  , module Control.Monad.Journal.Class
  ) where

#if __GLASGOW_HASKELL__ < 708
-- 'CoreMonad.MonadIO' and 'Control.Monad.IO.Class.MonadIO' are different
-- classes before ghc 7.8
#define DIFFERENT_MONADIO 1

-- RWST doen't have a MonadIO instance before ghc 7.8
#define MONADIO_INSTANCES 1
#endif


import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.DynFlags
import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.CabalApi
import qualified Language.Haskell.GhcMod.Gap as Gap

import DynFlags
import Exception
import GHC
import qualified GHC as G
import GHC.Paths (libdir)
import GhcMonad
#if __GLASGOW_HASKELL__ <= 702
import HscTypes
#endif

-- MonadUtils of GHC 7.6 or earlier defines its own MonadIO.
-- RWST does not automatically become an instance of MonadIO.
-- MonadUtils of GHC 7.8 or later imports MonadIO in Monad.Control.IO.Class.
-- So, RWST automatically becomes an instance of MonadIO.
import MonadUtils

#if DIFFERENT_MONADIO
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.IO.Class
import Data.Monoid (Monoid)
#endif

import Control.Applicative (Alternative)
import Control.Arrow (first)
import Control.Monad (MonadPlus, void, liftM)
import Control.Monad.Base (MonadBase, liftBase)

-- Monad transformer stuff
import Control.Monad.Trans.Control (MonadBaseControl(..), StM, liftBaseWith,
  control, liftBaseOp, liftBaseOp_)

import Control.Monad.Trans.Class
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.State.Class

import Control.Monad.Error (Error(..), MonadError, ErrorT, runErrorT)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.State.Strict (StateT, runStateT)
import Control.Monad.Trans.Journal (JournalT, runJournalT)
#ifdef MONADIO_INSTANCES
import Control.Monad.Trans.Maybe (MaybeT)
#endif
import Control.Monad.Journal.Class

import Data.Maybe (fromJust, isJust)
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import System.Exit (exitSuccess)
import System.IO (hPutStr, hPrint, stderr)
import System.Directory (getCurrentDirectory)

----------------------------------------------------------------

data GhcModEnv = GhcModEnv {
      gmGhcSession :: !(IORef HscEnv)
    , gmOptions    :: Options
    , gmCradle     :: Cradle
    }

type GhcModLog = ()

data GhcModState = GhcModState {
      gmCompilerMode :: CompilerMode
    } deriving (Eq,Show,Read)

data CompilerMode = Simple | Intelligent deriving (Eq,Show,Read)

defaultState :: GhcModState
defaultState = GhcModState Simple

----------------------------------------------------------------

-- | A constraint alias (-XConstraintKinds) to make functions dealing with
-- 'GhcModT' somewhat cleaner.
--
-- Basicially an @IOish m => m@ is a 'Monad' supporting arbitrary 'IO' and
-- exception handling. Usually this will simply be 'IO' but we parametrise it in
-- the exported API so users have the option to use a custom inner monad.
type IOish m = (Functor m, MonadIO m, MonadBaseControl IO m)

-- | The GhcMod monad transformer data type. This is basically a newtype wrapper
-- around 'StateT', 'ErrorT', 'JournalT' and 'ReaderT' with custom instances for
-- 'GhcMonad' and it's constraints.
--
-- The inner monad should have instances for 'MonadIO' and 'MonadBaseControl'
-- 'IO'. Most @mtl@ monads already have 'MonadBaseControl' 'IO' instances, see
-- the @monad-control@ package.
newtype GhcModT m a = GhcModT {
      unGhcModT :: StateT GhcModState
                     (ErrorT GhcModError
                       (JournalT GhcModLog
                         (ReaderT GhcModEnv m) ) ) a
    } deriving ( Functor
               , Applicative
               , Alternative
               , Monad
               , MonadPlus
               , MonadIO
#if DIFFERENT_MONADIO
               , Control.Monad.IO.Class.MonadIO
#endif
               , MonadReader GhcModEnv
               , MonadWriter w
               , MonadState GhcModState
               , MonadError GhcModError
               )

instance MonadTrans GhcModT where
    lift = GhcModT . lift . lift . lift . lift

#if MONADIO_INSTANCES
instance MonadIO m => MonadIO (StateT s m) where
    liftIO = lift . liftIO

instance MonadIO m => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO

instance (Monoid w, MonadIO m) => MonadIO (JournalT w m) where
    liftIO = lift . liftIO

instance (Error e, MonadIO m) => MonadIO (ErrorT e m) where
    liftIO = lift . liftIO

instance MonadIO m => MonadIO (MaybeT m) where
    liftIO = lift . liftIO
#endif

----------------------------------------------------------------

-- | Initialize the 'DynFlags' relating to the compilation of a single
-- file or GHC session according to the 'Cradle' and 'Options'
-- provided.
initializeFlagsWithCradle :: GhcMonad m
        => Options
        -> Cradle
        -> m ()
initializeFlagsWithCradle opt c
  | cabal     = withCabal
  | otherwise = withSandbox
  where
    mCradleFile = cradleCabalFile c
    cabal = isJust mCradleFile
    ghcopts = ghcOpts opt
    withCabal = do
        pkgDesc <- liftIO $ parseCabalFile $ fromJust mCradleFile
        compOpts <- liftIO $ getCompilerOptions ghcopts c pkgDesc
        initSession CabalPkg opt compOpts
    withSandbox = initSession SingleFile opt compOpts
      where
        importDirs = [".","..","../..","../../..","../../../..","../../../../.."]
        pkgOpts = ghcDbStackOpts $ cradlePkgDbStack c
        compOpts
          | null pkgOpts = CompilerOptions ghcopts importDirs []
          | otherwise    = CompilerOptions (ghcopts ++ pkgOpts) [wdir,rdir] []
        wdir = cradleCurrentDir c
        rdir = cradleRootDir    c

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

-- | Run a @GhcModT m@ computation.
runGhcModT :: IOish m
           => Options
           -> GhcModT m a
           -> m (Either GhcModError a, GhcModLog)
runGhcModT opt action = do
    env <- liftBase $ newGhcModEnv opt =<< getCurrentDirectory
    first (fmap fst) <$> (runGhcModT' env defaultState $ do
        dflags <- getSessionDynFlags
        defaultCleanupHandler dflags $ do
            initializeFlagsWithCradle opt (gmCradle env)
            action)

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
      flip runReaderT r $ runJournalT $ runErrorT $ flip runStateT s
        $ (unGhcModT $ initGhcMonad (Just libdir) >> a)
  return $ (res, w')
----------------------------------------------------------------

withErrorHandler :: IOish m => String -> GhcModT m a -> GhcModT m a
withErrorHandler label = ghandle ignore
  where
    ignore :: IOish m => SomeException -> GhcModT m a
    ignore e = liftIO $ do
        hPutStr stderr $ label ++ ":0:0:Error:"
        hPrint stderr e
        exitSuccess

-- | This is only a transitional mechanism don't use it for new code.
toGhcModT :: IOish m => Ghc a -> GhcModT m a
toGhcModT a = do
    s <- gmGhcSession <$> ask
    liftIO $ unGhc a $ Session s

----------------------------------------------------------------

options :: IOish m => GhcModT m Options
options = gmOptions <$> ask

cradle :: IOish m => GhcModT m Cradle
cradle = gmCradle <$> ask

getCompilerMode :: (Functor m, MonadState GhcModState m) => m CompilerMode
getCompilerMode = gmCompilerMode <$> get

setCompilerMode :: MonadState GhcModState m => CompilerMode -> m ()
setCompilerMode mode = (\s -> put s { gmCompilerMode = mode } ) =<< get

----------------------------------------------------------------

withOptions :: IOish m => (Options -> Options) -> GhcModT m a -> GhcModT m a
withOptions changeOpt action = local changeEnv action
  where
    changeEnv e = e { gmOptions = changeOpt opt }
      where
        opt = gmOptions e

----------------------------------------------------------------

instance (MonadBaseControl IO m) => MonadBase IO (GhcModT m) where
    liftBase = GhcModT . liftBase

instance (MonadBaseControl IO m) => MonadBaseControl IO (GhcModT m) where
    newtype StM (GhcModT m) a = StGhcMod {
          unStGhcMod :: StM (StateT GhcModState
                              (ErrorT GhcModError
                                (JournalT GhcModLog
                                  (ReaderT GhcModEnv m) ) ) ) a }
    liftBaseWith f = GhcModT . liftBaseWith $ \runInBase ->
        f $ liftM StGhcMod . runInBase . unGhcModT

    restoreM = GhcModT . restoreM . unStGhcMod
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}

-- GHC cannot prove the following instances to be decidable automatically using
-- the FlexibleContexts extension as they violate the second Paterson Condition,
-- namely that: The assertion has fewer constructors and variables (taken
-- together and counting repetitions) than the head. Specifically the
-- @MonadBaseControl IO m@ constraint is causing this violation.
--
-- Proof of termination:
--
-- Assuming all constraints containing the variable `m' exist and are decidable
-- we show termination by manually replacing the current set of constraints with
-- their own set of constraints and show that this, after a finite number of
-- steps, results in the empty set, i.e. not having to check any more
-- constraints.
--
-- We start by setting the constraints to be those immediate constraints of the
-- instance declaration which cannot be proven decidable automatically for the
-- type under consideration.
--
-- @
-- { MonadBaseControl IO m }
-- @
--
-- Classes used:
--
-- * @class MonadBase b m => MonadBaseControl b m@
--
-- @
-- { MonadBase IO m }
-- @
--
-- Classes used:
--
-- * @class (Applicative b, Applicative m, Monad b, Monad m) => MonadBase b m@
--
-- @
-- { Applicative IO, Applicative m, Monad IO, Monad m }
-- @
--
-- Classes used:
--
-- * @class Monad m@
-- * @class Applicative f => Functor f@
--
-- @
-- { Functor m }
-- @
--
-- Classes used:
--
-- * @class Functor f@
--
-- @
-- { }
-- @
-- ∎

instance (Functor m, MonadIO m, MonadBaseControl IO m)
      => GhcMonad (GhcModT m) where
    getSession = (liftIO . readIORef) . gmGhcSession =<< ask
    setSession a = (liftIO . flip writeIORef a) . gmGhcSession =<< ask

#if __GLASGOW_HASKELL__ >= 706
instance (Functor m, MonadIO m, MonadBaseControl IO m)
      => HasDynFlags (GhcModT m) where
    getDynFlags = getSessionDynFlags
#endif

instance (MonadIO m, MonadBaseControl IO m)
      => ExceptionMonad (GhcModT m) where
    gcatch act handler = control $ \run ->
        run act `gcatch` (run . handler)

    gmask = liftBaseOp gmask . liftRestore
     where liftRestore f r = f $ liftBaseOp_ r
