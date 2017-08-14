-- ghc-mod: Happy Haskell Hacking
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
{-# LANGUAGE TypeFamilies, UndecidableInstances, BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module GhcMod.Monad.Types (
  -- * Monad Types
    GhcModT
  , GmOutT(..)
  , GmT(..)
  , GmlT(..)
  , LightGhc(..)
  , GmGhc
  , IOish
  -- * Environment, state and logging
  , GhcModEnv(..)
  , GhcModState(..)
  , GhcModCaches(..)
  , defaultGhcModState
  , GmGhcSession(..)
  , GmComponent(..)
  -- * Accessing 'GhcModEnv', 'GhcModState' and 'GhcModLog'
  , GmLogLevel(..)
  , GhcModLog(..)
  , GhcModError(..)
  , Gm
  , GmEnv(..)
  , GmState(..)
  , GmLog(..)
  , GmOut(..)
  , cradle
  , options
  , outputOpts
  , withOptions
  , getMMappedFiles
  , setMMappedFiles
  , addMMappedFile
  , delMMappedFile
  , lookupMMappedFile
  , getMMappedFilePaths
  -- * Re-exporting convenient stuff
  , MonadIO
  , liftIO
  , gmlGetSession
  , gmlSetSession
  ) where

#include "Compat.hs_h"

import GhcMod.Types

import GhcMod.Monad.Env
import GhcMod.Monad.State
import GhcMod.Monad.Log
import GhcMod.Monad.Out
import GhcMod.Monad.Newtypes
import GhcMod.Monad.Orphans ()

import Safe

import GHC
import DynFlags
import Exception
import HscTypes

import Control.Applicative
import Control.Monad

import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State.Strict (StateT(..))
import Control.Monad.Trans.Journal (JournalT)
import Control.Monad.Trans.Maybe (MaybeT)

import Control.Monad.Trans.Control

import Control.Monad.Reader.Class

import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.IORef
import Prelude

type Gm m = (GmEnv m, GmState m, GmLog m, GmOut m)

--------------------------------------------------
-- GHC API instances -----------------------------

-- GHC cannot prove the following instances to be decidable automatically using
-- the FlexibleContexts extension as they violate the second Paterson Condition,
-- namely that: The assertion has fewer constructors and variables (taken
-- together and counting repetitions) than the head. Specifically the
-- @MonadBaseControl IO m@ constraint in 'IOish' is causing this violation.

type GmGhc m = (IOish m, GhcMonad m)

instance (MonadIO m, MonadBaseControl IO m) => GhcMonad (GmlT m) where
    getSession = gmlGetSession
    setSession = gmlSetSession

-- | Get the underlying GHC session
gmlGetSession :: (MonadIO m, MonadBaseControl IO m) => GmlT m HscEnv
gmlGetSession = do
        ref <- gmgsSession . fromJustNote "gmlGetSession" . gmGhcSession <$> gmsGet
        liftIO $ readIORef ref

-- | Set the underlying GHC session
gmlSetSession :: (MonadIO m, MonadBaseControl IO m) => HscEnv -> GmlT m ()
gmlSetSession a = do
        ref <- gmgsSession . fromJustNote "gmlSetSession" . gmGhcSession <$> gmsGet
        liftIO $ flip writeIORef a ref

instance GhcMonad LightGhc where
    getSession = (liftIO . readIORef) =<< LightGhc ask
    setSession a = (liftIO . flip writeIORef a) =<< LightGhc ask

#if __GLASGOW_HASKELL__ >= 706
instance (MonadIO m, MonadBaseControl IO m) => HasDynFlags (GmlT m) where
    getDynFlags = hsc_dflags <$> getSession

instance HasDynFlags LightGhc where
    getDynFlags = hsc_dflags <$> getSession
#endif

instance (MonadIO m, MonadBaseControl IO m) => ExceptionMonad (GmOutT m) where
    gcatch act handler = control $ \run ->
        run act `gcatch` (run . handler)

    gmask = liftBaseOp gmask . liftRestore
     where liftRestore f r = f $ liftBaseOp_ r

instance (MonadIO m, MonadBaseControl IO m) => ExceptionMonad (GmT m) where
    gcatch act handler = control $ \run ->
        run act `gcatch` (run . handler)

    gmask = liftBaseOp gmask . liftRestore
     where liftRestore f r = f $ liftBaseOp_ r

instance (MonadIO m, MonadBaseControl IO m) => ExceptionMonad (GmlT m) where
    gcatch act handler = control $ \run ->
        run act `gcatch` (run . handler)

    gmask = liftBaseOp gmask . liftRestore
     where liftRestore f r = f $ liftBaseOp_ r

instance ExceptionMonad LightGhc where
  gcatch act handl =
      LightGhc $ unLightGhc act `gcatch` \e -> unLightGhc (handl e)
  gmask f =
      LightGhc $ gmask $ \io_restore ->let
          g_restore (LightGhc m) = LightGhc $ io_restore m
      in
        unLightGhc (f g_restore)


instance (MonadIO m, MonadBaseControl IO m) => ExceptionMonad (StateT s m) where
    gcatch act handler = control $ \run ->
        run act `gcatch` (run . handler)

    gmask = liftBaseOp gmask . liftRestore
     where liftRestore f r = f $ liftBaseOp_ r

instance (MonadIO m, MonadBaseControl IO m) => ExceptionMonad (ReaderT s m) where
    gcatch act handler = control $ \run ->
        run act `gcatch` (run . handler)

    gmask = liftBaseOp gmask . liftRestore
     where liftRestore f r = f $ liftBaseOp_ r

instance (Monoid w, MonadIO m, MonadBaseControl IO m) => ExceptionMonad (JournalT w m) where
    gcatch act handler = control $ \run ->
        run act `gcatch` (run . handler)

    gmask = liftBaseOp gmask . liftRestore
     where liftRestore f r = f $ liftBaseOp_ r

instance (MonadIO m, MonadBaseControl IO m) => ExceptionMonad (MaybeT m) where
    gcatch act handler = control $ \run ->
        run act `gcatch` (run . handler)

    gmask = liftBaseOp gmask . liftRestore
     where liftRestore f r = f $ liftBaseOp_ r


----------------------------------------------------------------

options :: GmEnv m => m Options
options = gmOptions `liftM` gmeAsk

outputOpts :: GmOut m => m OutputOpts
outputOpts = gmoOptions `liftM` gmoAsk

cradle :: GmEnv m => m Cradle
cradle = gmCradle `liftM` gmeAsk

getMMappedFiles :: GmState m => m FileMappingMap
getMMappedFiles = gmMMappedFiles `liftM` gmsGet

setMMappedFiles :: GmState m => FileMappingMap -> m ()
setMMappedFiles mf = (\s -> gmsPut s { gmMMappedFiles = mf } ) =<< gmsGet

addMMappedFile  :: GmState m => FilePath -> FileMapping -> m ()
addMMappedFile t fm =
  getMMappedFiles >>= setMMappedFiles . M.insert t fm

delMMappedFile  :: GmState m => FilePath -> m ()
delMMappedFile t =
  getMMappedFiles >>= setMMappedFiles . M.delete t

lookupMMappedFile  :: GmState m => FilePath -> m (Maybe FileMapping)
lookupMMappedFile t =
  M.lookup t `liftM` getMMappedFiles

getMMappedFilePaths :: GmState m => m [FilePath]
getMMappedFilePaths = M.keys `liftM` getMMappedFiles

withOptions :: GmEnv m => (Options -> Options) -> m a -> m a
withOptions changeOpt action = gmeLocal changeEnv action
  where
    changeEnv e = e { gmOptions = changeOpt opt }
      where
        opt = gmOptions e
