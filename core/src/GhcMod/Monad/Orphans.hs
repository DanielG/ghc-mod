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

{-# LANGUAGE CPP, UndecidableInstances, StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module GhcMod.Monad.Orphans where

#include "Compat.hs_h"

import GhcMod.Types
import GhcMod.Monad.Newtypes

#if DIFFERENT_MONADIO
import qualified MonadUtils as GHC (MonadIO(..))
#endif
import qualified Control.Monad.IO.Class as MTL

import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State.Strict (StateT(..))
import Control.Monad.Trans.Journal (JournalT)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Error (Error(..), ErrorT(..))

--------------------------------------------------
-- Miscellaneous instances

#if DIFFERENT_MONADIO
instance MTL.MonadIO m => GHC.MonadIO (ReaderT x m) where
    liftIO = MTL.liftIO
instance MTL.MonadIO m => GHC.MonadIO (StateT x m) where
    liftIO = MTL.liftIO
instance (Error e, MTL.MonadIO m) => GHC.MonadIO (ErrorT e m) where
    liftIO = MTL.liftIO
instance MTL.MonadIO m => GHC.MonadIO (JournalT x m) where
    liftIO = MTL.liftIO
instance MTL.MonadIO m => GHC.MonadIO (MaybeT m) where
    liftIO = MTL.liftIO
deriving instance MTL.MonadIO m => GHC.MonadIO (GmOutT m)
deriving instance MTL.MonadIO m => GHC.MonadIO (GmT m)
deriving instance MTL.MonadIO m => GHC.MonadIO (GmlT m)
deriving instance GHC.MonadIO LightGhc
#endif

deriving instance MTL.MonadIO m => MTL.MonadIO (GmOutT m)
deriving instance MTL.MonadIO m => MTL.MonadIO (GmT m)
deriving instance MTL.MonadIO m => MTL.MonadIO (GmlT m)
deriving instance MTL.MonadIO LightGhc

instance MonadIO IO where
    liftIO = id
instance MonadIO m => MonadIO (ReaderT x m) where
    liftIO = MTL.liftIO
instance MonadIO m => MonadIO (StateT x m) where
    liftIO = MTL.liftIO
instance (Error e, MonadIO m) => MonadIO (ErrorT e m) where
    liftIO = MTL.liftIO
instance MonadIO m => MonadIO (JournalT x m) where
    liftIO = MTL.liftIO
instance MonadIO m => MonadIO (MaybeT m) where
    liftIO = MTL.liftIO
instance MonadIOC m => MonadIO (GmOutT m) where
    liftIO = MTL.liftIO
instance MonadIOC m => MonadIO (GmT m) where
    liftIO = MTL.liftIO
instance MonadIOC m => MonadIO (GmlT m) where
    liftIO = MTL.liftIO
instance MonadIO LightGhc where
    liftIO = MTL.liftIO
