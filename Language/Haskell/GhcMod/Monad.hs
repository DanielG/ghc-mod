{-# LANGUAGE CPP, GeneralizedNewtypeDeriving, FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses, RankNTypes, TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Haskell.GhcMod.Monad (
   GhcMod
 , GhcModEnv(..)
 , GhcModWriter
 , GhcModState(..)
 , runGhcMod'
 , runGhcMod
 , withErrorHandler
 , toGhcMod
 , options
 , module Control.Monad.Reader.Class
 , module Control.Monad.Writer.Class
 , module Control.Monad.State.Class
 ) where

import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Types

import DynFlags
import Exception
import GHC
import GHC.Paths (libdir)
import GhcMonad
import MonadUtils
#if __GLASGOW_HASKELL__ <= 702
import HscTypes
#endif

#if __GLASGOW_HASKELL__ < 708
import Control.Monad.Trans.Class (lift)
import Data.Monoid (Monoid)
#endif

import Control.Monad (liftM)
import Control.Monad.Base (MonadBase,liftBase)
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Control (MonadBaseControl(..), StM, liftBaseWith, control, liftBaseOp, liftBaseOp_)
import Control.Monad.Trans.RWS.Lazy (RWST(..),runRWST)
import Control.Monad.Writer.Class

import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import System.Exit (exitSuccess)
import System.IO (hPutStr, hPrint, stderr)

----------------------------------------------------------------

data GhcModEnv = GhcModEnv {
      gmGhcSession :: !(IORef HscEnv)
    , gmOptions    :: Options
    , gmCradle     :: Cradle
    }

data GhcModState = GhcModState

defaultState :: GhcModState
defaultState = GhcModState

type GhcModWriter = ()

----------------------------------------------------------------

newtype GhcMod a = GhcMod {
      unGhcMod :: RWST GhcModEnv GhcModWriter GhcModState IO a
    } deriving (Functor
               ,Applicative
               ,Monad
               ,MonadIO
               ,MonadReader GhcModEnv
               ,MonadWriter GhcModWriter
               ,MonadState GhcModState
               )

#if __GLASGOW_HASKELL__ < 708
instance (Monoid w, MonadIO m) => MonadIO (RWST r w s m) where
--  liftIO :: MonadIO m => IO a -> m a
    liftIO = lift . liftIO
#endif

----------------------------------------------------------------

runGhcMod' :: GhcModEnv
          -> GhcModState
          -> GhcMod a
          -> IO (a,(GhcModState, GhcModWriter))
runGhcMod' r s a = do
  (a', s',w) <- runRWST (unGhcMod $ initGhcMonad (Just libdir) >> a) r s
  return (a',(s',w))


runGhcMod :: Options -> GhcMod a -> IO a
runGhcMod opt action = do
    session <- newIORef (error "empty session")
    cradle <- findCradle
    let env = GhcModEnv { gmGhcSession = session
                        , gmOptions = opt
                        , gmCradle = cradle }
    (a,(_,_)) <- runGhcMod' env defaultState $ do
        dflags <- getSessionDynFlags
        defaultCleanupHandler dflags $ do
            toGhcMod $ initializeFlagsWithCradle opt cradle
            action
    return a

----------------------------------------------------------------

withErrorHandler :: String -> GhcMod a -> GhcMod a
withErrorHandler label = ghandle ignore
  where
    ignore :: SomeException -> GhcMod a
    ignore e = liftIO $ do
        hPutStr stderr $ label ++ ":0:0:Error:"
        hPrint stderr e
        exitSuccess

toGhcMod :: Ghc a -> GhcMod a
toGhcMod a = do
    s <- gmGhcSession <$> ask
    liftIO $ unGhc a $ Session s

----------------------------------------------------------------

options :: GhcMod Options
options = gmOptions <$> ask

instance MonadBase IO GhcMod where
    liftBase = GhcMod . liftBase

instance MonadBaseControl IO GhcMod where
    newtype StM GhcMod a = StGhcMod {
          unStGhcMod :: StM (RWST GhcModEnv () GhcModState IO) a }

    liftBaseWith f = GhcMod . liftBaseWith $ \runInBase ->
        f $ liftM StGhcMod . runInBase . unGhcMod

    restoreM = GhcMod . restoreM . unStGhcMod
    {-# INLINE liftBaseWith #-}
    {-# INLINE restoreM #-}

instance GhcMonad GhcMod where
    getSession = liftIO . readIORef . gmGhcSession =<< ask
    setSession a = liftIO . flip writeIORef a . gmGhcSession =<< ask

#if __GLASGOW_HASKELL__ >= 706
instance HasDynFlags GhcMod where
    getDynFlags = getSessionDynFlags
#endif

instance ExceptionMonad GhcMod where
    gcatch act handler = control $ \run ->
        run act `gcatch` (run . handler)

    gmask = liftBaseOp gmask . liftRestore
     where liftRestore f r = f $ liftBaseOp_ r
