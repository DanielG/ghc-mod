module Language.Haskell.GhcMod.Logging where

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
