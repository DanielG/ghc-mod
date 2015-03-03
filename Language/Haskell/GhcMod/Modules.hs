module Language.Haskell.GhcMod.Modules (modules) where

import qualified GHC as G
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Gap (listVisibleModuleNames)
import Module (moduleNameString)

----------------------------------------------------------------

-- | Listing installed modules.
modules :: (IOish m, GmEnv m) => m String
modules = do
  dflags <- runGmPkgGhc G.getSessionDynFlags
  convert' $ map moduleNameString $ listVisibleModuleNames dflags
