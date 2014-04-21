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
listModules opt cradle = withGHCDummyFile (modules opt cradle)

-- | Listing installed modules.
modules :: Options -> Cradle -> Ghc String
modules opt cradle = do
    void $ initializeFlagsWithCradle opt cradle [] False
    convert opt . nub . sort . map dropPkgs . getExposedModules <$> G.getSessionDynFlags
  where
    getExposedModules = concatMap exposedModules'
                      . eltsUFM . pkgIdMap . G.pkgState
    exposedModules' p =
        map G.moduleNameString (exposedModules p)
    	`zip`
        repeat (display $ sourcePackageId p)
    dropPkgs (name, pkg)
      | detailed opt = name ++ " " ++ pkg
      | otherwise = name
