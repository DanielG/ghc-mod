module List (listModules) where

import Control.Applicative
import Data.List
import Exception
import GHC
import GHC.Paths (libdir)
import Packages
import Param
import UniqFM

----------------------------------------------------------------

listModules :: Options -> IO String
listModules opt = convert opt . nub . sort <$> getModules

getModules :: IO [String]
getModules = ghandle ignore $ runGhc (Just libdir) $ do
    initSession
    getExposedModules <$> getSessionDynFlags
  where
    initSession = getSessionDynFlags >>= setSessionDynFlags
    getExposedModules = map moduleNameString . concatMap exposedModules . eltsUFM . pkgIdMap . pkgState
    ignore :: SomeException -> IO [String]
    ignore _ = return []
