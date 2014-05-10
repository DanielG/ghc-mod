-- | ComponentLocalBuildInfo for Cabal >= 1.18
module Language.Haskell.GhcMod.Cabal18 (
    ComponentLocalBuildInfo
  , componentPackageDeps
  ) where

import Distribution.Package (InstalledPackageId, PackageId)

data LibraryName = LibraryName String
    deriving (Read, Show)

data ComponentLocalBuildInfo
  = LibComponentLocalBuildInfo {
    componentPackageDeps :: [(InstalledPackageId, PackageId)],
    componentLibraries :: [LibraryName]
  }
  | ExeComponentLocalBuildInfo {
    componentPackageDeps :: [(InstalledPackageId, PackageId)]
  }
  | TestComponentLocalBuildInfo {
    componentPackageDeps :: [(InstalledPackageId, PackageId)]
  }
  | BenchComponentLocalBuildInfo {
    componentPackageDeps :: [(InstalledPackageId, PackageId)]
  }
  deriving (Read, Show)
