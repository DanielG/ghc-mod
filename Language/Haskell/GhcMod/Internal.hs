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
  -- * World
  , World
  , getWorld
  , isChanged
  ) where

import GHC.Paths (libdir)
import GHC (getSessionDynFlags)

import Language.Haskell.GhcMod.CabalApi
import Language.Haskell.GhcMod.DynFlags
import Language.Haskell.GhcMod.GHCChoice
import Language.Haskell.GhcMod.Logger
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Target
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.World

-- | Obtaining the directory for ghc system libraries.
ghcLibDir :: FilePath
ghcLibDir = libdir

getWorld :: IOish m => GhcModT m World
getWorld = do
    crdl <- cradle
    dflags <- getSessionDynFlags
    liftIO $ getCurrentWorld crdl dflags

isChanged :: IOish m => World -> GhcModT m Bool
isChanged world = do
    crdl <- cradle
    dflags <- getSessionDynFlags
    liftIO $ isWorldChanged world crdl dflags
