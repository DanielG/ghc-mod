module Language.Haskell.GhcMod.List (listModules, listMods) where

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
listModules opt cradle = convert opt . nub . sort . map dropPkgs <$> withGHCDummyFile (listMods opt cradle)
  where
    dropPkgs (name, pkg)
      | detailed opt = name ++ " " ++ pkg
      | otherwise = name

-- | Listing installed modules.
listMods :: Options -> Cradle -> Ghc [(String, String)]
listMods opt cradle = do
    void $ initializeFlagsWithCradle opt cradle [] False
    getExposedModules <$> G.getSessionDynFlags
  where
    getExposedModules = concatMap exposedModules'
                      . eltsUFM . pkgIdMap . G.pkgState
    exposedModules' p =
        map G.moduleNameString (exposedModules p)
    	`zip`
        repeat (display $ sourcePackageId p)
