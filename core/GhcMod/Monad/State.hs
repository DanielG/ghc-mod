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

module GhcMod.Monad.State where

import GhcMod.Types
import GhcMod.Monad.Newtypes

import Control.Monad
import Control.Monad.State.Strict (StateT(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Prelude

class Monad m => GmState m where
    gmsGet :: m GhcModState
    gmsGet = gmsState (\s -> (s, s))

    gmsPut :: GhcModState -> m ()
    gmsPut s = gmsState (\_ -> ((), s))

    gmsState :: (GhcModState -> (a, GhcModState)) -> m a
    gmsState f = do
      s <- gmsGet
      let ~(a, s') = f s
      gmsPut s'
      return a
    {-# MINIMAL gmsState | gmsGet, gmsPut #-}

instance GmState m => GmState (StateT s m) where
    gmsGet = lift gmsGet
    gmsPut = lift . gmsPut
    gmsState = lift . gmsState

instance Monad m => GmState (StateT GhcModState m) where
    gmsGet = get
    gmsPut = put
    gmsState = state

instance Monad m => GmState (GmT m) where
    gmsGet = GmT get
    gmsPut = GmT . put
    gmsState = GmT . state

instance GmState m => GmState (MaybeT m) where
    gmsGet = MaybeT $ Just `liftM` gmsGet
    gmsPut = MaybeT . (Just `liftM`) . gmsPut
    gmsState = MaybeT . (Just `liftM`) . gmsState

deriving instance (Monad m, GmState (GhcModT m)) => GmState (GmlT m)
