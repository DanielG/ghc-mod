-- ghc-mod: Happy Haskell Hacking
-- Copyright (C) 2015,2016  Daniel Gröber <dxld ÄT darkboxed DOT org>
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

{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
{-# LANGUAGE RankNTypes, FlexibleInstances #-}

module GhcMod.Monad.Newtypes where

#include "Compat.hs_h"

import GhcMod.Types

import GHC

import Control.Applicative
import Control.Monad

import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Error (ErrorT(..), MonadError(..))
import Control.Monad.State.Strict (StateT(..))
import Control.Monad.Trans.Journal (JournalT)
import Control.Monad.Reader.Class
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Journal.Class (MonadJournal(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.Control
import Control.Monad.Base (MonadBase(..), liftBase)

import Data.IORef
import Prelude

type GhcModT m = GmT (GmOutT m)

newtype GmOutT m a = GmOutT {
      unGmOutT :: ReaderT GhcModOut m a
    } deriving ( Functor
               , Applicative
               , Alternative
               , Monad
               , MonadPlus
               , MonadTrans
               )

newtype GmT m a = GmT {
      unGmT :: StateT GhcModState
                 (ErrorT GhcModError
                   (JournalT GhcModLog
                     (ReaderT GhcModEnv m) ) ) a
    } deriving ( Functor
               , Applicative
               , Alternative
               , Monad
               , MonadPlus
               , MonadError GhcModError
               )

newtype GmlT m a = GmlT { unGmlT :: GhcModT m a }
    deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadPlus
             , MonadError GhcModError
             )

newtype LightGhc a = LightGhc { unLightGhc :: ReaderT (IORef HscEnv) IO a }
    deriving ( Functor
             , Applicative
             , Monad
             )

-- GmOutT ----------------------------------------
instance (MonadBaseControl IO m) => MonadBase IO (GmOutT m) where
    liftBase = GmOutT . liftBase

instance (MonadBaseControl IO m) => MonadBaseControl IO (GmOutT m) where
    type StM (GmOutT m) a = StM (ReaderT GhcModEnv m) a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}

instance MonadTransControl GmOutT where
    type StT GmOutT a = StT (ReaderT GhcModEnv) a
    liftWith = defaultLiftWith GmOutT unGmOutT
    restoreT = defaultRestoreT GmOutT


-- GmlT ------------------------------------------
instance (MonadBaseControl IO m) => MonadBase IO (GmlT m) where
    liftBase = GmlT . liftBase

instance (MonadBaseControl IO m) => MonadBaseControl IO (GmlT m) where
    type StM (GmlT m) a = StM (GmT m) a
    liftBaseWith = defaultLiftBaseWith
    restoreM = defaultRestoreM
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}

instance MonadTransControl GmlT where
    type StT GmlT a = StT GmT a
    liftWith f = GmlT $
      liftWith $ \runGm ->
        liftWith $ \runEnv ->
          f $ \ma -> runEnv $ runGm $ unGmlT ma
    restoreT = GmlT . restoreT . restoreT

instance MonadTrans GmlT where
    lift = GmlT . lift . lift

-- GmT ------------------------------------------

instance MonadReader r m => MonadReader r (GmT m) where
    local f ma = gmLiftWithInner (\run -> local f (run ma))
    ask = gmLiftInner ask

instance MonadState s m => MonadState s (GmT m) where
    get = GmT $ lift $ lift $ lift get
    put = GmT . lift . lift . lift . put
    state = GmT . lift . lift . lift . state

instance Monad m => MonadJournal GhcModLog (GmT m) where
  journal  w = GmT $ lift $ lift $ (journal w)
  history    = GmT $ lift $ lift $ history
  clear      = GmT $ lift $ lift $ clear

instance (MonadBaseControl IO m) => MonadBase IO (GmT m) where
    liftBase = GmT . liftBase

instance (MonadBaseControl IO m) => MonadBaseControl IO (GmT m) where
    type StM (GmT m) a =
          StM (StateT GhcModState
                (ErrorT GhcModError
                  (JournalT GhcModLog
                    (ReaderT GhcModEnv m) ) ) ) a
    liftBaseWith f = GmT (liftBaseWith $ \runInBase ->
        f $ runInBase . unGmT)
    restoreM = GmT . restoreM
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}

instance MonadTransControl GmT where
    type StT GmT a = (Either GhcModError (a, GhcModState), GhcModLog)
    liftWith f = GmT $
      liftWith $ \runS ->
        liftWith $ \runE ->
          liftWith $ \runJ ->
            liftWith $ \runR ->
              f $ \ma -> runR $ runJ $ runE $ runS $ unGmT ma
    restoreT = GmT . restoreT . restoreT . restoreT . restoreT
    {-# INLINE liftWith #-}
    {-# INLINE restoreT #-}

instance MonadTrans GmT where
    lift = GmT . lift . lift . lift . lift

gmLiftInner :: Monad m => m a -> GmT m a
gmLiftInner = GmT . lift . lift . lift . lift

gmLiftWithInner :: (MonadTransControl t, Monad m, Monad (t m))
                => (Run t -> m (StT t a)) -> t m a
gmLiftWithInner f = liftWith f >>= restoreT . return
