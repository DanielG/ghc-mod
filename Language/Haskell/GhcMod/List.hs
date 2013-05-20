module Language.Haskell.GhcMod.List (listModules, listMods) where

import Control.Applicative
import Data.List
import GHC
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Types
import Packages
import UniqFM

----------------------------------------------------------------

-- | Listing installed modules.
listModules :: Options -> IO String
listModules opt = convert opt . nub . sort <$> withGHCDummyFile (listMods opt)

-- | Listing installed modules.
listMods :: Options -> Ghc [String]
listMods opt = do
    initializeFlags opt
    getExposedModules <$> getSessionDynFlags
  where
    getExposedModules = map moduleNameString
                      . concatMap exposedModules
                      . eltsUFM . pkgIdMap . pkgState
