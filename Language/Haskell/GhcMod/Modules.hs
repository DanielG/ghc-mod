module Language.Haskell.GhcMod.Modules (modules) where

import Control.Applicative ((<$>))
import qualified GHC as G
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Gap (listVisibleModuleNames)
import Module (moduleNameString)

----------------------------------------------------------------

-- | Listing installed modules.
modules :: IOish m => GhcModT m String
modules = convert' =<< map moduleNameString . listVisibleModuleNames <$> G.getSessionDynFlags
