-- | Low level access to the ghc-mod library.

module GhcMod.Exe.Internal (
  -- * Types
    GHCOption
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
  , targetGhcOptions
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
  , prepareCabalHelper
  -- * Misc stuff
  , GHandler(..)
  , gcatches
  -- * FileMapping
  , module GhcMod.FileMapping
  ) where

import GHC.Paths (libdir)

import GhcMod.Target
import GhcMod.DynFlags
import GhcMod.Error
import GhcMod.Logger
import GhcMod.Logging
import GhcMod.Monad
import GhcMod.Types
import GhcMod.Utils
import GhcMod.World
import GhcMod.CabalHelper
import GhcMod.FileMapping

-- | Obtaining the directory for ghc system libraries.
ghcLibDir :: FilePath
ghcLibDir = libdir
