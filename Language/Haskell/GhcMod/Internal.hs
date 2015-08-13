-- | Low level access to the ghc-mod library.

module Language.Haskell.GhcMod.Internal (
  -- * Types
    GHCOption
  , Package
  , PackageBaseName
  , PackageVersion
  , PackageId
  , IncludeDir
  , GmlT(..)
  , MonadIO(..)
  , GmEnv(..)
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
  , GmLog(..)
  , GmLogLevel(..)
  , gmSetLogLevel
  -- * Monad utilities
  , runGhcModT'
  , hoistGhcModT
  , runGmlT
  , runGmlT'
  , gmlGetSession
  , gmlSetSession
  , loadTargets
  , cabalResolvedComponents
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
  -- * Cabal Helper
  , ModulePath(..)
  , GmComponent(..)
  , GmComponentType(..)
  , GmModuleGraph(..)
  ) where

import GHC.Paths (libdir)

import Language.Haskell.GhcMod.Target
import Language.Haskell.GhcMod.DynFlags
import Language.Haskell.GhcMod.Error
import Language.Haskell.GhcMod.Logger
import Language.Haskell.GhcMod.Logging
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Utils
import Language.Haskell.GhcMod.World

-- | Obtaining the directory for ghc system libraries.
ghcLibDir :: FilePath
ghcLibDir = libdir
