module Language.Haskell.GhcMod.List (listModules, listMods) where

import Control.Applicative
import Data.List
import GHC
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Types
import Distribution.Text (display)
import Packages
import UniqFM

----------------------------------------------------------------

-- | Listing installed modules.
listModules :: Options -> IO String
listModules opt = convert opt . nub . sort . map dropPkgs <$> withGHCDummyFile (listMods opt) where
  dropPkgs (name, pkg)
    | detailed opt = name ++ " " ++ pkg
    | otherwise = name

-- | Listing installed modules.
listMods :: Options -> Ghc [(String, String)]
listMods opt = do
    initializeFlags opt
    getExposedModules <$> getSessionDynFlags
  where
    getExposedModules = concatMap exposedModules'
                      . eltsUFM . pkgIdMap . pkgState
    exposedModules' p =
    	map moduleNameString (exposedModules p)
    	`zip`
    	repeat (display $ sourcePackageId p)
