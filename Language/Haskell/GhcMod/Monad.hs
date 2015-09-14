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

{-# LANGUAGE CPP #-}
module Language.Haskell.GhcMod.Monad (
    runGmOutT
  , runGhcModT
  , runGhcModT'
  , runGhcModT''
  , hoistGhcModT
  , runGmlT
  , runGmlT'
  , runGmlTWith
  , runGmPkgGhc
  , withGhcModEnv
  , module Language.Haskell.GhcMod.Monad.Types
  ) where

import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Error
import Language.Haskell.GhcMod.Logging
import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.Target
import Language.Haskell.GhcMod.Output

import Control.Arrow (first)
import Control.Applicative

import Control.Concurrent

import Control.Monad.Reader (runReaderT)
import Control.Monad.State.Strict (runStateT)
import Control.Monad.Trans.Journal (runJournalT)

import Exception (ExceptionMonad(..))

import System.Directory
import Prelude

withGhcModEnv :: (IOish m, GmOut m) => FilePath -> Options -> (GhcModEnv -> m a) -> m a
withGhcModEnv dir opts f =
    withStdoutGateway $
      withCradle $ \crdl ->
        withCradleRootDir crdl $
          f $ GhcModEnv opts crdl
 where
   withStdoutGateway a = do
       c <- gmoChan <$> gmoAsk
       gbracket_ (liftIO $ forkIO $ stdoutGateway c) (liftIO . killThread) a

   withCradle =
       gbracket (findCradle' dir) (liftIO . cleanupCradle)

   withCradleRootDir (cradleRootDir -> projdir) =
       gbracket_ (liftIO $ setCurrentDirectory projdir >> getCurrentDirectory)
                 (liftIO . setCurrentDirectory)

   gbracket_ ma mb mc = gbracket ma mb (const mc)

-- | Run a @GhcModT m@ computation.
runGhcModT :: IOish m
           => Options
           -> GhcModT m a
           -> m (Either GhcModError a, GhcModLog)
runGhcModT opt action = do
    dir <- liftIO getCurrentDirectory
    runGhcModT' dir opt action

runGhcModT' :: IOish m
            => FilePath
            -> Options
            -> GhcModT m a
            -> m (Either GhcModError a, GhcModLog)
runGhcModT' dir opt action = liftIO (canonicalizePath dir) >>= \dir' -> do
    gmo <- GhcModOut (optOutput opt) <$> liftIO newChan
    runGmOutT gmo $
      withGhcModEnv dir' opt $ \env ->
        first (fst <$>) <$> runGhcModT'' env defaultGhcModState
          (gmSetLogLevel (ooptLogLevel $ optOutput opt) >> action)

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
runGhcModT'' :: IOish m
             => GhcModEnv
             -> GhcModState
             -> GhcModT m a
             -> GmOutT m (Either GhcModError (a, GhcModState), GhcModLog)
runGhcModT'' r s a = do
  flip runReaderT r $ runJournalT $ runErrorT $ runStateT (unGmT a) s

runGmOutT :: IOish m => GhcModOut -> GmOutT m a -> m a
runGmOutT gmo ma = flip runReaderT gmo $ unGmOutT ma
