-- | ComponentLocalBuildInfo for Cabal <= 1.16
module Language.Haskell.GhcMod.Cabal16 (
    ComponentLocalBuildInfo
  , componentPackageDeps
  ) where

import Distribution.Package (InstalledPackageId, PackageIdentifier)

-- From Cabal <= 1.16
data ComponentLocalBuildInfo = ComponentLocalBuildInfo {
    componentPackageDeps :: [(InstalledPackageId, PackageIdentifier)]
  }
  deriving (Read, Show)
