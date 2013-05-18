module Language.Haskell.GhcMod.List (listModules, list) where

import Control.Applicative
import Data.List
import GHC
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Types
import Packages
import UniqFM

----------------------------------------------------------------

listModules :: Options -> Ghc String
listModules opt = convert opt . nub . sort <$> list opt

list :: Options -> Ghc [String]
list opt = do
    initializeFlags opt
    getExposedModules <$> getSessionDynFlags
  where
    getExposedModules = map moduleNameString
                      . concatMap exposedModules
                      . eltsUFM . pkgIdMap . pkgState
