module Language.Haskell.GhcMod.LightGhc where

import Control.Monad.Reader (runReaderT)
import Data.IORef

import GHC
import GHC.Paths (libdir)
import StaticFlags
import SysTools
import DynFlags
import HscMain
import HscTypes

import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.DynFlags

withLightHscEnv :: forall m a. IOish m
    => [GHCOption] -> (HscEnv -> m a) -> m a
withLightHscEnv opts action = gbracket initEnv teardownEnv action
 where
   teardownEnv :: HscEnv -> m ()
   teardownEnv env = liftIO $ do
       let dflags = hsc_dflags env
       cleanTempFiles dflags
       cleanTempDirs dflags

   initEnv :: m HscEnv
   initEnv = liftIO $ do
     initStaticOpts
     settings <- initSysTools (Just libdir)
     dflags  <- initDynFlags (defaultDynFlags settings)
     env <- newHscEnv dflags
     dflags' <- runLightGhc env $ do
         -- HomeModuleGraph and probably all other clients get into all sorts of
         -- trouble if the package state isn't initialized here
         _ <- setSessionDynFlags =<< addCmdOpts opts =<< getSessionDynFlags
         getSessionDynFlags
     newHscEnv dflags'

runLightGhc :: HscEnv -> LightGhc a -> IO a
runLightGhc env action = do
  renv <- newIORef env
  flip runReaderT renv $ unLightGhc action
