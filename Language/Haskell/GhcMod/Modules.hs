module Language.Haskell.GhcMod.Modules (modules) where

import Control.Applicative ((<$>))
import Control.Exception (SomeException(..))
import Data.List (nub, sort)
import qualified GHC as G
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Types
import Packages (pkgIdMap, exposedModules, sourcePackageId, display)
import UniqFM (eltsUFM)

----------------------------------------------------------------

-- | Listing installed modules.
modules :: IOish m => GhcModT m String
modules = do
    opt <- options
    convert opt . arrange opt <$> (getModules `G.gcatch` handler)
  where
    getModules = getExposedModules <$> G.getSessionDynFlags
    getExposedModules = concatMap exposedModules'
                      . eltsUFM . pkgIdMap . G.pkgState
    exposedModules' p =
        map G.moduleNameString (exposedModules p)
    	`zip`
        repeat (display $ sourcePackageId p)
    arrange opt = nub . sort . map (dropPkgs opt)
    dropPkgs opt (name, pkg)
      | detailed opt = name ++ " " ++ pkg
      | otherwise = name
    handler (SomeException _) = return []
