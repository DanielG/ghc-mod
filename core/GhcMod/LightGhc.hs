{-# LANGUAGE CPP #-}

module GhcMod.LightGhc where

import Control.Monad
import Control.Monad.Reader (runReaderT)
import Data.IORef

import GHC
import GHC.Paths (libdir)
#if __GLASGOW_HASKELL__ < 802
import StaticFlags
#endif
import SysTools
import DynFlags
import HscMain
import HscTypes

import GhcMod.Types
import GhcMod.Monad.Types
import GhcMod.DynFlags
import qualified GhcMod.Gap as Gap

#if __GLASGOW_HASKELL__ >= 802
initStaticOpts :: Monad m => m ()
initStaticOpts = return ()
#endif

-- We have to be more careful about tearing down 'HscEnv's since GHC 8 added an
-- out of process GHCI server which has to be shutdown.
newLightEnv :: IOish m => (DynFlags -> LightGhc DynFlags) -> m HscEnv
newLightEnv mdf = do
  df <- liftIO $ do
#if MIN_VERSION_GLASGOW_HASKELL(8,2,0,0)
#else
     initStaticOpts
#endif
     settings <- initSysTools (Just libdir)
#if __GLASGOW_HASKELL__ >= 806
     let llvmTgtList = ([],[]) -- TODO: where should this come from?
     initDynFlags $ defaultDynFlags settings llvmTgtList
#elif __GLASGOW_HASKELL__ >= 804
     let llvmTgtList = [] -- TODO: where should this come from?
     initDynFlags $ defaultDynFlags settings llvmTgtList
#else
     initDynFlags $ defaultDynFlags settings
#endif

  hsc_env <- liftIO $ newHscEnv df
  df' <- runLightGhc hsc_env $ mdf df
  return $ hsc_env {
      hsc_dflags = df',
      hsc_IC = (hsc_IC hsc_env) { ic_dflags = df' }
    }

teardownLightEnv :: MonadIO m => HscEnv -> m ()
teardownLightEnv env = runLightGhc env $ do
  Gap.withCleanupSession $ return ()

withLightHscEnv'
    :: IOish m => (DynFlags -> LightGhc DynFlags) -> (HscEnv -> m a) -> m a
withLightHscEnv' mdf action = gbracket (newLightEnv mdf) teardownLightEnv action

withLightHscEnv :: IOish m => [GHCOption] -> (HscEnv -> m a) -> m a
withLightHscEnv opts = withLightHscEnv' (f <=< liftIO . newHscEnv)
 where
   f env = runLightGhc env $ do
         -- HomeModuleGraph and probably all other clients get into all sorts of
         -- trouble if the package state isn't initialized here
         _ <- setSessionDynFlags =<< addCmdOpts opts =<< getSessionDynFlags
         getSessionDynFlags

runLightGhc :: MonadIO m => HscEnv -> LightGhc a -> m a
runLightGhc env action = liftIO $ do
  renv <- newIORef env
  flip runReaderT renv $ unLightGhc action

runLightGhc' :: MonadIO m => IORef HscEnv -> LightGhc a -> m a
runLightGhc' renv action = liftIO $ do
  flip runReaderT renv $ unLightGhc action
