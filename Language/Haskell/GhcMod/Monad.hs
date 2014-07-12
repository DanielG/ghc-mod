{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Haskell.GhcMod.Monad (
   GhcModT
 , IOish
 , GhcModEnv(..)
 , GhcModWriter
 , GhcModState(..)
 , runGhcModT'
 , runGhcModT
 , newGhcModEnv
 , withErrorHandler
 , toGhcMod
 , options
 , cradle
 , module Control.Monad.Reader.Class
 , module Control.Monad.Writer.Class
 , module Control.Monad.State.Class
 ) where

import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.DynFlags
import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.GHCChoice
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

#if __GLASGOW_HASKELL__ < 708
-- To make RWST an instance of MonadIO.
import Control.Monad.Trans.Class (lift)
import Data.Monoid (Monoid)
#endif

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus, liftM, void)
import Control.Monad.Base (MonadBase, liftBase)

import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control (MonadBaseControl(..), StM, liftBaseWith,
  control, liftBaseOp, liftBaseOp_)
import Control.Monad.Trans.RWS.Lazy (RWST(..), runRWST)
import Control.Monad.Writer.Class

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

data GhcModState = GhcModState

defaultState :: GhcModState
defaultState = GhcModState

type GhcModWriter = ()

----------------------------------------------------------------

type IOish m = (Functor m, MonadIO m, MonadBaseControl IO m)

newtype GhcModT m a = GhcModT {
      unGhcModT :: RWST GhcModEnv GhcModWriter GhcModState m a
    } deriving (Functor
               , Applicative
               , Alternative
               , Monad
               , MonadPlus
               , MonadIO
               , MonadReader GhcModEnv
               , MonadWriter GhcModWriter
               , MonadState GhcModState
               , MonadTrans
               )

#if __GLASGOW_HASKELL__ < 708
instance (Monoid w, MonadIO m) => MonadIO (RWST r w s m) where
--  liftIO :: MonadIO m => IO a -> m a
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
  | cabal     = withCabal |||> withSandbox
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
    void $ G.setSessionDynFlags =<< (addCmdOpts ghcOptions
      $ setLinkerOptions
      $ setIncludeDirs includeDirs
      $ setBuildEnv build
      $ setEmptyLogger
      $ Gap.addPackageFlags depPackages df)

----------------------------------------------------------------

runGhcModT' :: IOish m
           => GhcModEnv
           -> GhcModState
           -> GhcModT m a
           -> m (a,(GhcModState, GhcModWriter))
runGhcModT' r s a = do
  (a',s',w) <- runRWST (unGhcModT $ initGhcMonad (Just libdir) >> a) r s
  return (a',(s',w))

newGhcModEnv :: Options -> FilePath -> IO GhcModEnv
newGhcModEnv opt dir = do
      session <- newIORef (error "empty session")
      c <- findCradle' dir
      return GhcModEnv {
          gmGhcSession = session
        , gmOptions = opt
        , gmCradle = c
        }

runGhcModT :: IOish m => Options -> GhcModT m a -> m a
runGhcModT opt action = do
    env <- liftBase $ newGhcModEnv opt =<< getCurrentDirectory
    (a,(_,_)) <- runGhcModT' env defaultState $ do
        dflags <- getSessionDynFlags
        defaultCleanupHandler dflags $ do
            initializeFlagsWithCradle opt (gmCradle env)
            action
    return a
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
toGhcMod :: IOish m => Ghc a -> GhcModT m a
toGhcMod a = do
    s <- gmGhcSession <$> ask
    liftIO $ unGhc a $ Session s

----------------------------------------------------------------

options :: IOish m => GhcModT m Options
options = gmOptions <$> ask

cradle :: IOish m => GhcModT m Cradle
cradle = gmCradle <$> ask

instance (MonadBaseControl IO m) => MonadBase IO (GhcModT m) where
    liftBase = GhcModT . liftBase

instance (MonadBaseControl IO m) => MonadBaseControl IO (GhcModT m) where
    newtype StM (GhcModT m) a = StGhcMod {
          unStGhcMod :: StM (RWST GhcModEnv () GhcModState m) a }

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
