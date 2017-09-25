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

module GhcMod.Monad.Env where

import GhcMod.Types
import GhcMod.Monad.Newtypes

import Control.Monad
import Control.Monad.Trans.Journal (JournalT)
import Control.Monad.State.Strict (StateT(..))
import Control.Monad.Error (ErrorT(..))
import Control.Monad.Reader.Class
import Control.Monad.Trans.Class (MonadTrans(..))
import Prelude

class Monad m => GmEnv m where
    gmeAsk :: m GhcModEnv
    gmeAsk = gmeReader id

    gmeReader :: (GhcModEnv -> a) -> m a
    gmeReader f = f `liftM` gmeAsk

    gmeLocal :: (GhcModEnv -> GhcModEnv) -> m a -> m a
    {-# MINIMAL (gmeAsk | gmeReader), gmeLocal #-}

instance Monad m => GmEnv (GmT m) where
    gmeAsk = GmT ask
    gmeReader = GmT . reader
    gmeLocal f a = GmT $ local f (unGmT a)

instance GmEnv m => GmEnv (GmOutT m) where
    gmeAsk = lift gmeAsk
    gmeReader = lift . gmeReader
    gmeLocal f ma = gmLiftWithInner (\run -> gmeLocal f (run ma))

instance GmEnv m => GmEnv (StateT s m) where
    gmeAsk = lift gmeAsk
    gmeReader = lift . gmeReader
    gmeLocal f ma = gmLiftWithInner (\run -> gmeLocal f (run ma))

instance GmEnv m => GmEnv (JournalT GhcModLog m) where
    gmeAsk = lift gmeAsk
    gmeReader = lift . gmeReader
    gmeLocal f ma = gmLiftWithInner (\run -> gmeLocal f (run ma))

instance GmEnv m => GmEnv (ErrorT GhcModError m) where
    gmeAsk = lift gmeAsk
    gmeReader = lift . gmeReader
    gmeLocal f ma = gmLiftWithInner (\run -> gmeLocal f (run ma))

deriving instance (Monad m, GmEnv (GhcModT m)) => GmEnv (GmlT m)
