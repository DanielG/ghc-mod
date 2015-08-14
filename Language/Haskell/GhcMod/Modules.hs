module Language.Haskell.GhcMod.Modules (modules) where

import Control.Arrow
import Data.List
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Gap ( listVisibleModuleNames
                                   , lookupModulePackageInAllPackages
                                   )

import qualified GHC as G

----------------------------------------------------------------

-- | Listing installed modules.
modules :: (IOish m, GmEnv m, GmState m, GmLog m) => m String
modules = do
  Options { detailed } <- options
  df <- runGmPkgGhc G.getSessionDynFlags
  let mns = listVisibleModuleNames df
      pmnss = map (first moduleNameString) $ zip mns (modulePkg df `map` mns)
  convert' $ nub [ if detailed then pkg ++ " " ++ mn else mn
                 | (mn, pkgs) <- pmnss, pkg <- pkgs ]
 where
   modulePkg df = lookupModulePackageInAllPackages df
