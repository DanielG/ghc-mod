module Language.Haskell.GhcMod.Logging where
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

import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad.Types

import Control.Monad.Journal.Class
import Control.Monad.Trans.Class
import System.IO

import MonadUtils

--gmSink :: IOish m => (GhcModLog -> IO ()) -> GhcModT m ()
--gmSink = GhcModT . (lift . lift . sink)

type GmLog m = MonadJournal GhcModLog m

gmJournal :: IOish m => GhcModLog -> GhcModT m ()
gmJournal = GhcModT . lift . lift . journal

gmLog :: (MonadIO m, MonadJournal GhcModLog m) => String -> m ()
gmLog str = liftIO (hPutStrLn stderr str) >> (journal $ GhcModLog [str])
