module Language.Haskell.GhcMod.List (listModules) where

import Control.Applicative
import Data.List
import GHC
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Types
import Packages
import UniqFM

----------------------------------------------------------------

listModules :: Options -> IO String
listModules opt = convert opt . nub . sort <$> list opt

list :: Options -> IO [String]
list opt = withGHCDummyFile $ do
    initializeFlags opt
    getExposedModules <$> getSessionDynFlags
  where
    getExposedModules = map moduleNameString
                      . concatMap exposedModules
                      . eltsUFM . pkgIdMap . pkgState
