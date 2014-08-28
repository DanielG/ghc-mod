-- | Low level access to the ghc-mod library.

module Language.Haskell.GhcMod.Internal (
  -- * Types
    GHCOption
  , Package
  , PackageBaseName
  , PackageVersion
  , PackageId
  , IncludeDir
  , CompilerOptions(..)
  -- * Cabal API
  , parseCabalFile
  , getCompilerOptions
  , cabalAllBuildInfo
  , cabalDependPackages
  , cabalSourceDirs
  , cabalAllTargets
  -- * GHC.Paths
  , ghcLibDir
  -- * IO
  , getDynamicFlags
  -- * Targets
  , setTargetFiles
  -- * Logging
  , withLogger
  , setNoWarningFlags
  , setAllWarningFlags
  -- * Environment, state and logging
  , GhcModEnv(..)
  , newGhcModEnv
  , GhcModState
  , defaultState
  , CompilerMode(..)
  , GhcModLog
  -- * Monad utilities
  , runGhcModT'
  , hoistGhcModT
  -- ** Accessing 'GhcModEnv' and 'GhcModState'
  , options
  , cradle
  , getCompilerMode
  , setCompilerMode
  , withOptions
  -- * 'GhcMonad' Choice
  , (||>)
  , goNext
  , runAnyOne
  ) where

import GHC.Paths (libdir)

import Language.Haskell.GhcMod.CabalApi
import Language.Haskell.GhcMod.DynFlags
import Language.Haskell.GhcMod.GHCChoice
import Language.Haskell.GhcMod.Logger
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Target
import Language.Haskell.GhcMod.Types

-- | Obtaining the directory for ghc system libraries.
ghcLibDir :: FilePath
ghcLibDir = libdir
