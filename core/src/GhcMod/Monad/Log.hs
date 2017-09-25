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

{-# LANGUAGE GeneralizedNewtypeDeriving, StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module GhcMod.Monad.Log where

import GhcMod.Types
import GhcMod.Monad.Newtypes

import Control.Monad
import Control.Monad.Trans.Journal (JournalT)
import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State.Strict (StateT(..))
import Control.Monad.Error (Error, ErrorT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Journal.Class (MonadJournal(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Prelude

class Monad m => GmLog m where
    gmlJournal :: GhcModLog -> m ()
    gmlHistory :: m GhcModLog
    gmlClear   :: m ()

instance Monad m => GmLog (JournalT GhcModLog m) where
    gmlJournal = journal
    gmlHistory = history
    gmlClear   = clear

instance Monad m => GmLog (GmT m) where
    gmlJournal = GmT . lift . lift . journal
    gmlHistory = GmT $ lift $ lift history
    gmlClear   = GmT $ lift $ lift clear

instance (Monad m, GmLog m) => GmLog (ReaderT r m) where
    gmlJournal = lift . gmlJournal
    gmlHistory = lift gmlHistory
    gmlClear = lift  gmlClear

instance (Monad m, GmLog m) => GmLog (StateT s m) where
    gmlJournal = lift . gmlJournal
    gmlHistory = lift gmlHistory
    gmlClear = lift gmlClear

instance (Monad m, GmLog m, Error e) => GmLog (ErrorT e m) where
    gmlJournal = lift . gmlJournal
    gmlHistory = lift gmlHistory
    gmlClear = lift gmlClear

instance (Monad m, GmLog m) => GmLog (MaybeT m) where
    gmlJournal = lift . gmlJournal
    gmlHistory = lift gmlHistory
    gmlClear = lift gmlClear

deriving instance GmLog m => GmLog (GmOutT m)
deriving instance (Monad m, GmLog (GhcModT m)) => GmLog (GmlT m)
