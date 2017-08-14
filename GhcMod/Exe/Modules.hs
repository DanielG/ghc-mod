module GhcMod.Exe.Modules (modules) where

import Control.Arrow
import Data.List
import GhcMod.Convert
import GhcMod.Types
import GhcMod.Monad
import GhcMod.Gap ( listVisibleModuleNames
                                   , lookupModulePackageInAllPackages
                                   )

import qualified GHC as G

----------------------------------------------------------------

-- | Listing installed modules.
modules :: (IOish m, Gm m)
        => Bool -- ^ 'detailed', if 'True', also prints packages that modules belong to.
        -> m String
modules detailed = do
  df <- runGmPkgGhc G.getSessionDynFlags
  let mns = listVisibleModuleNames df
      pmnss = map (first moduleNameString) $ zip mns (modulePkg df `map` mns)
  convert' $ nub [ if detailed then pkg ++ " " ++ mn else mn
                 | (mn, pkgs) <- pmnss, pkg <- pkgs ]
 where
   modulePkg df = lookupModulePackageInAllPackages df
