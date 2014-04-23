module Language.Haskell.GhcMod.List (listModules, modules) where

import Control.Applicative ((<$>))
import Control.Monad (void)
import Data.List (nub, sort)
import GHC (Ghc)
import qualified GHC as G
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Types
import Packages (pkgIdMap, exposedModules, sourcePackageId, display)
import UniqFM (eltsUFM)

----------------------------------------------------------------

-- | Listing installed modules.
listModules :: Options -> Cradle -> IO String
listModules opt cradle = withGHC' $ do
    void $ initializeFlagsWithCradle opt cradle [] False
    modules opt

-- | Listing installed modules.
modules :: Options -> Ghc String
modules opt = convert opt . arrange <$> G.getSessionDynFlags
  where
    arrange = nub . sort . map dropPkgs . getExposedModules
    getExposedModules = concatMap exposedModules'
                      . eltsUFM . pkgIdMap . G.pkgState
    exposedModules' p =
        map G.moduleNameString (exposedModules p)
    	`zip`
        repeat (display $ sourcePackageId p)
    dropPkgs (name, pkg)
      | detailed opt = name ++ " " ++ pkg
      | otherwise = name
