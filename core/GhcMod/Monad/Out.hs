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
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, UndecidableInstances #-}

module GhcMod.Monad.Out where

import GhcMod.Types
import GhcMod.Monad.Newtypes

import Control.Monad
import Control.Monad.State.Strict (StateT(..))
import Control.Monad.Trans.Journal (JournalT)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class (MonadTrans(..))
import Prelude

class Monad m => GmOut m where
    gmoAsk :: m GhcModOut

instance Monad m => GmOut (GmOutT m) where
    gmoAsk = GmOutT ask

instance Monad m => GmOut (GmlT m) where
    gmoAsk = GmlT $ lift $ GmOutT ask

instance GmOut m => GmOut (GmT m) where
    gmoAsk = lift gmoAsk

instance GmOut m => GmOut (StateT s m) where
    gmoAsk = lift gmoAsk

instance GmOut m => GmOut (JournalT w m) where
    gmoAsk = lift gmoAsk

instance GmOut m => GmOut (MaybeT m) where
    gmoAsk = lift gmoAsk
