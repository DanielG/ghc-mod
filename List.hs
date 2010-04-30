module List (listModules) where

import Control.Applicative
import Data.List
import GHC
import Packages
import Types
import UniqFM

----------------------------------------------------------------

listModules :: Options -> IO String
listModules opt = convert opt . nub . sort <$> list

list :: IO [String]
list = withGHC $ do
    initSession0
    getExposedModules <$> getSessionDynFlags
  where
    getExposedModules = map moduleNameString
                      . concatMap exposedModules
                      . eltsUFM . pkgIdMap . pkgState
