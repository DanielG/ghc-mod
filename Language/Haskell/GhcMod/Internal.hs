-- | Low level access to the ghc-mod library.

module Language.Haskell.GhcMod.Internal (
  -- * Types
    GHCOption
  , Package
  , PackageBaseName
  , PackageVersion
  , PackageId
  , IncludeDir
  -- * Various Paths
  , ghcLibDir
  , ghcModExecutable
  -- * Logging
  , withLogger
  , setNoWarningFlags
  , setAllWarningFlags
  -- * Environment, state and logging
  , GhcModEnv(..)
  , GhcModState
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
  -- * 'GhcModError'
  , gmeDoc
  -- * World
  , World
  , getCurrentWorld
  , didWorldChange
  ) where

import GHC.Paths (libdir)

import Language.Haskell.GhcMod.DynFlags
import Language.Haskell.GhcMod.Error
import Language.Haskell.GhcMod.Logger
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Utils
import Language.Haskell.GhcMod.World

-- | Obtaining the directory for ghc system libraries.
ghcLibDir :: FilePath
ghcLibDir = libdir
