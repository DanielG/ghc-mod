module GhcModExe.Modules (modules) where

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
