-- ghc-mod: Making Haskell development *more* fun
-- Copyright (C) 2015  Daniel Gröber <dxld ÄT darkboxed DOT org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances, RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables, BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Haskell.GhcMod.Monad.Types where


#if __GLASGOW_HASKELL__ < 708
-- 'CoreMonad.MonadIO' and 'Control.Monad.IO.Class.MonadIO' are different
-- classes before ghc 7.8
#define DIFFERENT_MONADIO 1

-- RWST doen't have a MonadIO instance before ghc 7.8
#define MONADIO_INSTANCES 1
#endif

import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Error

import GHC
import DynFlags
import GhcMonad hiding (withTempSession)
#if __GLASGOW_HASKELL__ <= 702
import HscTypes
#endif

-- MonadUtils of GHC 7.6 or earlier defines its own MonadIO.
-- RWST does not automatically become an instance of MonadIO.
-- MonadUtils of GHC 7.8 or later imports MonadIO in Monad.Control.IO.Class.
-- So, RWST automatically becomes an instance of MonadIO.
import MonadUtils

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Control.Monad.Error (ErrorT)
import Control.Monad.Reader (ReaderT)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Journal (JournalT)

import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Trans.Control (MonadBaseControl(..), StM, liftBaseWith,
  control, liftBaseOp, liftBaseOp_)

import Control.Monad.Trans.Class
import Control.Monad.Reader.Class
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Journal.Class (MonadJournal(..))

#ifdef MONADIO_INSTANCES
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Error (Error(..))
#endif

#if DIFFERENT_MONADIO
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.IO.Class
import Data.Monoid (Monoid)
#endif

#if !MIN_VERSION_monad_control(1,0,0)
import Control.Monad (liftM)
#endif

import Data.Monoid
import Data.IORef

data GhcModEnv = GhcModEnv {
      gmGhcSession :: !(IORef HscEnv)
    , gmOptions    :: Options
    , gmCradle     :: Cradle
    }

data GhcModLog = GhcModLog {
      gmLogMessages :: [String]
    } deriving (Eq, Show, Read)

instance Monoid GhcModLog where
    mempty = GhcModLog mempty
    GhcModLog a `mappend` GhcModLog b = GhcModLog (a `mappend` b)

data GhcModState = GhcModState {
      gmCompilerMode :: CompilerMode
    } deriving (Eq,Show,Read)

data CompilerMode = Simple | Intelligent deriving (Eq,Show,Read)

defaultState :: GhcModState
defaultState = GhcModState Simple

----------------------------------------------------------------

-- | This is basically a newtype wrapper around 'StateT', 'ErrorT', 'JournalT'
-- and 'ReaderT' with custom instances for 'GhcMonad' and it's constraints that
-- means you can run (almost) all functions from the GHC API on top of 'GhcModT'
-- transparently.
--
-- The inner monad @m@ should have instances for 'MonadIO' and
-- 'MonadBaseControl' 'IO', in the common case this is simply 'IO'. Most @mtl@
-- monads already have 'MonadBaseControl' 'IO' instances, see the
-- @monad-control@ package.
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
#if DIFFERENT_MONADIO
               , Control.Monad.IO.Class.MonadIO
#endif
               , MonadReader GhcModEnv -- TODO: make MonadReader instance
                                       -- pass-through like MonadState
               , MonadWriter w
               , MonadError GhcModError
               )

instance MonadIO m => MonadIO (GhcModT m) where
    liftIO action = do
      res <- GhcModT . liftIO . liftIO . liftIO . liftIO $ try action
      case res of
        Right a -> return a

        Left e | isIOError e ->
                   throwError $ GMEIOException (fromEx e :: IOError)
        Left e | isGhcModError e ->
                   throwError $ (fromEx e :: GhcModError)
        Left e -> throw e

     where
       fromEx :: Exception e => SomeException -> e
       fromEx se = let Just e = fromException se in e

       isIOError se =
           case fromException se of
             Just (_ :: IOError) -> True
             Nothing -> False

       isGhcModError se =
           case fromException se of
             Just (_ :: GhcModError) -> True
             Nothing -> False

instance (Monad m) => MonadJournal GhcModLog (GhcModT m) where
  journal !w = GhcModT $ lift $ lift $ (journal w)
  history    = GhcModT $ lift $ lift $ history
  clear      = GhcModT $ lift $ lift $ clear

instance MonadTrans GhcModT where
    lift = GhcModT . lift . lift . lift . lift

instance MonadState s m => MonadState s (GhcModT m) where
    get = GhcModT $ lift $ lift $ lift get
    put = GhcModT . lift . lift . lift . put
    state = GhcModT . lift . lift . lift . state

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


instance (MonadBaseControl IO m) => MonadBase IO (GhcModT m) where
    liftBase = GhcModT . liftBase

#if MIN_VERSION_monad_control(1,0,0)

instance (MonadBaseControl IO m) => MonadBaseControl IO (GhcModT m) where
    type StM (GhcModT m) a =
          StM (StateT GhcModState
                (ErrorT GhcModError
                  (JournalT GhcModLog
                    (ReaderT GhcModEnv m) ) ) ) a
    liftBaseWith f = GhcModT . liftBaseWith $ \runInBase ->
        f $ runInBase . unGhcModT

    restoreM = GhcModT . restoreM
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}

#else

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

#endif

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
