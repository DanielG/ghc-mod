-- | Low level access to the ghc-mod library.

module Language.Haskell.GhcMod.Internal (
  -- * Types
    LogReader
  , GHCOption
  , Package
  , IncludeDir
  , CompilerOptions(..)
  -- * Cabal API
  , getCompilerOptions
  , parseCabalFile
  , cabalAllBuildInfo
  , cabalDependPackages
  , cabalSourceDirs
  , cabalAllTargets
  -- * GHC API
  , canCheckFast
  -- * Getting 'DynFlags'
  , getDynamicFlags
  -- * Initializing 'DynFlags'
  , initializeFlags
  , initializeFlagsWithCradle
  -- * 'GhcMonad'
  , setTargetFiles
  , checkSlowAndSet
  -- * 'Ghc' Choice
  , (||>)
  , goNext
  , runAnyOne
  -- * 'GhcMonad' Choice
  , (|||>)
  ) where

import Language.Haskell.GhcMod.CabalApi
import Language.Haskell.GhcMod.ErrMsg
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.GHCChoice
import Language.Haskell.GhcMod.Types
