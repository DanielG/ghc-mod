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

{-# LANGUAGE CPP #-}
module GhcMod.Monad (
    runGmOutT
  , runGmOutT'
  , runGhcModT
  , runGhcModT'
  , hoistGhcModT
  , runGmlT
  , runGmlT'
  , runGmlTWith
  , runGmPkgGhc
  , withGhcModEnv
  , withGhcModEnv'
  , module GhcMod.Monad.Types
  ) where

import GhcMod.Types
import GhcMod.Monad.Types
import GhcMod.Error
import GhcMod.Logging
import GhcMod.Cradle
import GhcMod.Target
import GhcMod.Output

import Control.Arrow (first)
import Control.Applicative

import Control.Concurrent

import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (runStateT)
import Control.Monad.Trans.Journal (runJournalT)

import Exception

import System.Directory
import System.IO.Unsafe
import Prelude

withGhcModEnv :: (IOish m, GmOut m) => FilePath -> Options -> ((GhcModEnv, GhcModLog) -> m a) -> m a
withGhcModEnv dir opts f = withGhcModEnv' withCradle dir opts f
 where
   withCradle dir' =
       gbracket
         (runJournalT $ do
            gmSetLogLevel $ ooptLogLevel $ optOutput opts
            findCradle' (optPrograms opts) dir')
         (liftIO . cleanupCradle . fst)



cwdLock :: MVar ThreadId
cwdLock = unsafePerformIO $ newEmptyMVar
{-# NOINLINE cwdLock #-}

withGhcModEnv' :: (IOish m, GmOut m) => (FilePath -> ((Cradle, GhcModLog) -> m a) -> m a) -> FilePath -> Options -> ((GhcModEnv, GhcModLog) -> m a) -> m a
withGhcModEnv' withCradle dir opts f =
    withCradle dir $ \(crdl,lg) ->
      withCradleRootDir crdl $
        f (GhcModEnv opts crdl, lg)
 where
   swapCurrentDirectory ndir = do
     odir <- canonicalizePath =<< getCurrentDirectory
     setCurrentDirectory ndir
     return odir

   withCradleRootDir (cradleRootDir -> projdir) a = do
       success <- liftIO $ tryPutMVar cwdLock =<< myThreadId
       if not success
         then error "withGhcModEnv': using ghc-mod from multiple threads is not supported!"
         else gbracket setup teardown (const a)
    where
      setup = liftIO $ swapCurrentDirectory projdir

      teardown odir = liftIO $ do
        setCurrentDirectory odir
        void $ takeMVar cwdLock

runGmOutT :: IOish m => Options -> GmOutT m a -> m a
runGmOutT opts ma = do
    gmo@GhcModOut{..} <- GhcModOut (optOutput opts) <$> liftIO newChan
    let action = runGmOutT' gmo ma
    case ooptLinePrefix $ optOutput opts of
      Nothing -> action
      Just pfxs ->
        gbracket_ (liftIO $ forkIO $ stdoutGateway pfxs gmoChan)
                  (const $ liftIO $ flushStdoutGateway gmoChan)
                  action

runGmOutT' :: GhcModOut -> GmOutT m a -> m a
runGmOutT' gmo ma = flip runReaderT gmo $ unGmOutT ma

-- | Run a @GhcModT m@ computation.
runGhcModT :: IOish m
           => Options
           -> GhcModT m a
           -> m (Either GhcModError a, GhcModLog)
runGhcModT opt action = liftIO (getCurrentDirectory >>= canonicalizePath) >>= \dir' -> do
    runGmOutT opt $
      withGhcModEnv dir' opt $ \(env,lg) ->
        first (fst <$>) <$> runGhcModT' env defaultGhcModState (do
          gmSetLogLevel (ooptLogLevel $ optOutput opt)
          gmAppendLogQuiet lg
          action)

-- | @hoistGhcModT result@. Embed a GhcModT computation's result into a GhcModT
-- computation. Note that if the computation that returned @result@ modified the
-- state part of GhcModT this cannot be restored.
hoistGhcModT :: IOish m
             => (Either GhcModError a, GhcModLog)
             -> GhcModT m a
hoistGhcModT (r,l) = do
  gmlJournal l >> case r of
    Left e -> throwError e
    Right a -> return a


-- | Run a computation inside @GhcModT@ providing the RWST environment and
-- initial state. This is a low level function, use it only if you know what to
-- do with 'GhcModEnv' and 'GhcModState'.
--
-- You should probably look at 'runGhcModT' instead.
runGhcModT' :: IOish m
             => GhcModEnv
             -> GhcModState
             -> GhcModT m a
             -> GmOutT m (Either GhcModError (a, GhcModState), GhcModLog)
runGhcModT' r s a = do
  flip runReaderT r $ runJournalT $ runErrorT $ runStateT (unGmT a) s

gbracket_ :: ExceptionMonad m => m a -> (a -> m b) -> m c -> m c
gbracket_ ma mb mc = gbracket ma mb (const mc)
