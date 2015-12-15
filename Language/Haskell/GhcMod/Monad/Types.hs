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

{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, RankNTypes #-}
{-# LANGUAGE TypeFamilies, UndecidableInstances, BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Haskell.GhcMod.Monad.Types (
  -- * Monad Types
    GhcModT
  , GmOutT(..)
  , GmT(..)
  , GmlT(..)
  , LightGhc(..)
  , GmGhc
  , IOish
  -- * Environment, state and logging
  , GhcModEnv(..)
  , GhcModState(..)
  , GhcModCaches(..)
  , defaultGhcModState
  , GmGhcSession(..)
  , GmComponent(..)
  -- * Accessing 'GhcModEnv', 'GhcModState' and 'GhcModLog'
  , GmLogLevel(..)
  , GhcModLog(..)
  , GhcModError(..)
  , Gm
  , GmEnv(..)
  , GmState(..)
  , GmLog(..)
  , GmOut(..)
  , cradle
  , options
  , outputOpts
  , withOptions
  , getMMappedFiles
  , setMMappedFiles
  , addMMappedFile
  , delMMappedFile
  , lookupMMappedFile
  , getMMappedFilePaths
  -- * Re-exporting convenient stuff
  , MonadIO
  , liftIO
  , gmlGetSession
  , gmlSetSession
  ) where

-- MonadUtils of GHC 7.6 or earlier defines its own MonadIO.
-- RWST does not automatically become an instance of MonadIO.
-- MonadUtils of GHC 7.8 or later imports MonadIO in Monad.Control.IO.Class.
-- So, RWST automatically becomes an instance of
#if __GLASGOW_HASKELL__ < 708
-- 'CoreMonad.MonadIO' and 'Control.Monad.IO.Class.MonadIO' are different
-- classes before ghc 7.8
#define DIFFERENT_MONADIO 1

-- RWST doen't have a MonadIO instance before ghc 7.8
#define MONADIO_INSTANCES 1
#endif

import Language.Haskell.GhcMod.Types

import GHC
import DynFlags
import Exception
import HscTypes

import Control.Applicative
import Control.Monad

import Control.Monad.Reader (ReaderT(..))
import Control.Monad.Error (ErrorT(..), MonadError(..))
import Control.Monad.State.Strict (StateT(..))
import Control.Monad.Trans.Journal (JournalT)
import Control.Monad.Trans.Maybe (MaybeT(..))

import Control.Monad.Base (MonadBase(..), liftBase)
import Control.Monad.Trans.Control

import Control.Monad.Reader.Class
import Control.Monad.Writer.Class
import Control.Monad.State.Class (MonadState(..))
import Control.Monad.Journal.Class (MonadJournal(..))
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Error (Error(..))
import qualified Control.Monad.IO.Class as MTL

#if DIFFERENT_MONADIO
import Data.Monoid (Monoid)
#endif

import qualified Data.Map as M
import Data.Maybe
import Data.Monoid
import Data.IORef
import Prelude

import qualified MonadUtils as GHC (MonadIO(..))

type GhcModT m = GmT (GmOutT m)

newtype GmOutT m a = GmOutT {
      unGmOutT :: ReaderT GhcModOut m a
    } deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadPlus
             , MonadTrans
             , MTL.MonadIO
#if DIFFERENT_MONADIO
             , GHC.MonadIO
#endif
             , GmLog
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
               , MTL.MonadIO
#if DIFFERENT_MONADIO
               , GHC.MonadIO
#endif
               , MonadError GhcModError
               )

newtype GmlT m a = GmlT { unGmlT :: GhcModT m a }
    deriving ( Functor
             , Applicative
             , Alternative
             , Monad
             , MonadPlus
             , MTL.MonadIO
#if DIFFERENT_MONADIO
             , GHC.MonadIO
#endif
             , MonadError GhcModError
             , GmEnv
             , GmState
             , GmLog
             )

newtype LightGhc a = LightGhc { unLightGhc :: ReaderT (IORef HscEnv) IO a }
    deriving ( Functor
             , Applicative
             , Monad
             , MTL.MonadIO
#if DIFFERENT_MONADIO
             , GHC.MonadIO
#endif
             )

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
#endif

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

instance MonadTrans GmT where
    lift = GmT . lift . lift . lift . lift
instance MonadTrans GmlT where
    lift = GmlT . lift . lift

--------------------------------------------------
-- Gm Classes

type Gm m = (GmEnv m, GmState m, GmLog m, GmOut m)

-- GmEnv -----------------------------------------
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

-- GmState ---------------------------------------
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

-- GmLog -----------------------------------------
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

instance (Monad m, GmLog m) => GmLog (MaybeT  m) where
    gmlJournal = lift . gmlJournal
    gmlHistory = lift gmlHistory
    gmlClear = lift gmlClear

-- GmOut -----------------------------------------
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

instance Monad m => MonadJournal GhcModLog (GmT m) where
  journal !w = GmT $ lift $ lift $ (journal w)
  history    = GmT $ lift $ lift $ history
  clear      = GmT $ lift $ lift $ clear

instance forall r m. MonadReader r m => MonadReader r (GmT m) where
    local f ma = gmLiftWithInner (\run -> local f (run ma))
    ask = gmLiftInner ask

instance (Monoid w, MonadWriter w m) => MonadWriter w (GmT m) where
    tell = gmLiftInner . tell
    listen ma =
      liftWith (\run -> listen (run ma)) >>= \(sta, w) ->
          flip (,) w `liftM` restoreT (return sta)

    pass maww = maww >>= gmLiftInner . pass . return

instance MonadState s m => MonadState s (GmT m) where
    get = GmT $ lift $ lift $ lift get
    put = GmT . lift . lift . lift . put
    state = GmT . lift . lift . lift . state


--------------------------------------------------
-- monad-control instances

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


-- GmT ------------------------------------------

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

gmLiftInner :: Monad m => m a -> GmT m a
gmLiftInner = GmT . lift . lift . lift . lift

gmLiftWithInner :: (MonadTransControl t, Monad m, Monad (t m))
                => (Run t -> m (StT t a)) -> t m a
gmLiftWithInner f = liftWith f >>= restoreT . return

--------------------------------------------------
-- GHC API instances -----------------------------

-- GHC cannot prove the following instances to be decidable automatically using
-- the FlexibleContexts extension as they violate the second Paterson Condition,
-- namely that: The assertion has fewer constructors and variables (taken
-- together and counting repetitions) than the head. Specifically the
-- @MonadBaseControl IO m@ constraint in 'IOish' is causing this violation.

type GmGhc m = (IOish m, GhcMonad m)

instance (MonadIO m, MonadBaseControl IO m) => GhcMonad (GmlT m) where
    getSession = gmlGetSession
    setSession = gmlSetSession

gmlGetSession :: (MonadIO m, MonadBaseControl IO m) => GmlT m HscEnv
gmlGetSession = do
        ref <- gmgsSession . fromJust . gmGhcSession <$> gmsGet
        GHC.liftIO $ readIORef ref

gmlSetSession :: (MonadIO m, MonadBaseControl IO m) => HscEnv -> GmlT m ()
gmlSetSession a = do
        ref <- gmgsSession . fromJust . gmGhcSession <$> gmsGet
        GHC.liftIO $ flip writeIORef a ref

instance GhcMonad LightGhc where
    getSession = (GHC.liftIO . readIORef) =<< LightGhc ask
    setSession a = (GHC.liftIO . flip writeIORef a) =<< LightGhc ask

#if __GLASGOW_HASKELL__ >= 706
instance (MonadIO m, MonadBaseControl IO m) => HasDynFlags (GmlT m) where
    getDynFlags = hsc_dflags <$> getSession

instance HasDynFlags LightGhc where
    getDynFlags = hsc_dflags <$> getSession
#endif

instance (MonadIO m, MonadBaseControl IO m) => ExceptionMonad (GmOutT m) where
    gcatch act handler = control $ \run ->
        run act `gcatch` (run . handler)

    gmask = liftBaseOp gmask . liftRestore
     where liftRestore f r = f $ liftBaseOp_ r

instance (MonadIO m, MonadBaseControl IO m) => ExceptionMonad (GmT m) where
    gcatch act handler = control $ \run ->
        run act `gcatch` (run . handler)

    gmask = liftBaseOp gmask . liftRestore
     where liftRestore f r = f $ liftBaseOp_ r

instance (MonadIO m, MonadBaseControl IO m) => ExceptionMonad (GmlT m) where
    gcatch act handler = control $ \run ->
        run act `gcatch` (run . handler)

    gmask = liftBaseOp gmask . liftRestore
     where liftRestore f r = f $ liftBaseOp_ r

instance ExceptionMonad LightGhc where
  gcatch act handl =
      LightGhc $ unLightGhc act `gcatch` \e -> unLightGhc (handl e)
  gmask f =
      LightGhc $ gmask $ \io_restore ->let
          g_restore (LightGhc m) = LightGhc $ io_restore m
      in
        unLightGhc (f g_restore)


instance (MonadIO m, MonadBaseControl IO m) => ExceptionMonad (StateT s m) where
    gcatch act handler = control $ \run ->
        run act `gcatch` (run . handler)

    gmask = liftBaseOp gmask . liftRestore
     where liftRestore f r = f $ liftBaseOp_ r

instance (MonadIO m, MonadBaseControl IO m) => ExceptionMonad (ReaderT s m) where
    gcatch act handler = control $ \run ->
        run act `gcatch` (run . handler)

    gmask = liftBaseOp gmask . liftRestore
     where liftRestore f r = f $ liftBaseOp_ r

instance (Monoid w, MonadIO m, MonadBaseControl IO m) => ExceptionMonad (JournalT w m) where
    gcatch act handler = control $ \run ->
        run act `gcatch` (run . handler)

    gmask = liftBaseOp gmask . liftRestore
     where liftRestore f r = f $ liftBaseOp_ r


----------------------------------------------------------------

options :: GmEnv m => m Options
options = gmOptions `liftM` gmeAsk

outputOpts :: GmOut m => m OutputOpts
outputOpts = gmoOptions `liftM` gmoAsk

cradle :: GmEnv m => m Cradle
cradle = gmCradle `liftM` gmeAsk

getMMappedFiles :: GmState m => m FileMappingMap
getMMappedFiles = gmMMappedFiles `liftM` gmsGet

setMMappedFiles :: GmState m => FileMappingMap -> m ()
setMMappedFiles mf = (\s -> gmsPut s { gmMMappedFiles = mf } ) =<< gmsGet

addMMappedFile  :: GmState m => FilePath -> FileMapping -> m ()
addMMappedFile t fm =
  getMMappedFiles >>= setMMappedFiles . M.insert t fm

delMMappedFile  :: GmState m => FilePath -> m ()
delMMappedFile t =
  getMMappedFiles >>= setMMappedFiles . M.delete t

lookupMMappedFile  :: GmState m => FilePath -> m (Maybe FileMapping)
lookupMMappedFile t =
  M.lookup t `liftM` getMMappedFiles

getMMappedFilePaths :: GmState m => m [FilePath]
getMMappedFilePaths = M.keys `liftM` getMMappedFiles

withOptions :: GmEnv m => (Options -> Options) -> m a -> m a
withOptions changeOpt action = gmeLocal changeEnv action
  where
    changeEnv e = e { gmOptions = changeOpt opt }
      where
        opt = gmOptions e
