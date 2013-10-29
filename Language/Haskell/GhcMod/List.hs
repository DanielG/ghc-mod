module Language.Haskell.GhcMod.List (listModules, listMods) where

import Control.Applicative
import Control.Monad (void)
import Data.List
import GHC
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Types
import Packages
import UniqFM

----------------------------------------------------------------

-- | Listing installed modules.
listModules :: Options -> Cradle -> IO String
listModules opt cradle = convert opt . nub . sort . map dropPkgs <$> withGHCDummyFile (listMods opt cradle)
  where
    dropPkgs (name, pkg)
      | detailed opt = name ++ " " ++ pkg
      | otherwise = name

-- | Listing installed modules.
listMods :: Options -> Cradle -> Ghc [(String, String)]
listMods opt cradle = do
    void $ initializeFlagsWithCradle opt cradle [] False
    getExposedModules <$> getSessionDynFlags
  where
    getExposedModules = concatMap exposedModules'
                      . eltsUFM . pkgIdMap . pkgState
    exposedModules' p =
    	map moduleNameString (exposedModules p)
    	`zip`
    	repeat (display $ sourcePackageId p)
