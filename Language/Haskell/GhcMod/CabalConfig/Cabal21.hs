-- Copyright   :  Isaac Jones 2003-2004
{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

-- | ComponentLocalBuildInfo for Cabal >= 1.21
module Language.Haskell.GhcMod.CabalConfig.Cabal21 (
    ComponentLocalBuildInfo
  , PackageIdentifier(..)
  , PackageName(..)
  , componentPackageDeps
  , componentLibraries
  ) where

import Distribution.Package (InstalledPackageId)
import Data.Version (Version)

data LibraryName = LibraryName String
    deriving (Read, Show)

newtype PackageName = PackageName { unPackageName :: String }
  deriving (Read, Show)

data PackageIdentifier
  = PackageIdentifier {
    pkgName :: PackageName,
    pkgVersion :: Version
  }
  deriving (Read, Show)

type PackageId = PackageIdentifier

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
