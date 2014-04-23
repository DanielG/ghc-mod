-- | Low level access to the ghc-mod library.

module Language.Haskell.GhcMod.Internal (
  -- * Types
    LogReader
  , GHCOption
  , Package
  , IncludeDir
  , CompilerOptions(..)
  -- * Cabal API
  , parseCabalFile
  , getCompilerOptions
  , cabalAllBuildInfo
  , cabalDependPackages
  , cabalSourceDirs
  , cabalAllTargets
  -- * IO
  , getSystemLibDir
  , getDynamicFlags
  -- * Initializing 'DynFlags'
  , initializeFlagsWithCradle
  -- * 'Ghc' Monad
  , setTargetFiles
  , addTargetFiles
  , handleErrMsg
  , browseAll
  -- * 'Ghc' Choice
  , (||>)
  , goNext
  , runAnyOne
  -- * 'GhcMonad' Choice
  , (|||>)
  -- * Misc
  , convert
  ) where

import Language.Haskell.GhcMod.Browse
import Language.Haskell.GhcMod.CabalApi
import Language.Haskell.GhcMod.ErrMsg
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.GHCChoice
import Language.Haskell.GhcMod.Types
