-- | The ghc-mod library.

module GhcModCore (
  -- * Cradle
    Cradle(..)
  , Project(..)
  , findCradle
  -- * Options
  , Options(..)
  , LineSeparator(..)
  , OutputStyle(..)
  , FileMapping(..)
  , defaultOptions
  -- * Logging
  , GmLogLevel(..)
  , increaseLogLevel
  , decreaseLogLevel
  , gmSetLogLevel
  , gmLog
  -- * Types
  , ModuleString
  , Expression(..)
  , GhcPkgDb
  -- , Symbol
  -- , SymbolDb
  , GhcModError(..)
  -- * Monad Types
  , GhcModT
  , IOish
  , MonadIO(..)
  -- * Monad utilities
  , runGhcModT
  , withOptions
  , dropSession
  -- * Output
  , gmPutStr
  , gmErrStr
  , gmPutStrLn
  , gmErrStrLn
  -- * FileMapping
  , loadMappedFile
  , loadMappedFileSource
  , unloadMappedFile
  -- * HIE integration utilities
  , getModulesGhc
  , getModulesGhc'
  -- * Manage GHC AST index vars for older GHCs
  , GhcPs,GhcRn,GhcTc
  -- Temporary home, see what there is, before strippint out
  , findCradle'
  , GmEnv
  , gmeLocal
  , gmCradle
  , mkRevRedirMapFunc
  , cradle
  , GmOut(..)
  , options
  , GmLog(..)
  , makeAbsolute'
  , withMappedFile
  , listVisibleModuleNames
  , runLightGhc
  , GHandler(..)
  , gcatches
  , GmlT(..)
  , defaultLintOpts
  , gmsGet
  , gmGhcSession
  , gmgsSession
  , getMMappedFiles
  , withDynFlags
  , ghcExceptionDoc
  , mkErrStyle'
  , renderGm
  , LightGhc(..)
  , OutputOpts(..)
  , gmlGetSession
  , gmlSetSession
  , cabalResolvedComponents
  , ModulePath(..)
  , GmComponent(..)
  , GmComponentType(..)
  , GmModuleGraph(..)

  -- * Used in ghc-mod
  , convert
  ) where

import GhcMod.Cradle
import GhcMod.FileMapping
import GhcMod.Logging
import GhcMod.Monad
import GhcMod.ModuleLoader
import GhcMod.Output
import GhcMod.Target
import GhcMod.Types

import GhcMod.Gap (GhcPs,GhcRn,GhcTc,listVisibleModuleNames,mkErrStyle')
import GhcMod.Utils (mkRevRedirMapFunc,makeAbsolute',withMappedFile)
import GhcMod.LightGhc (runLightGhc)
import GhcMod.Error (GHandler(..),gcatches,ghcExceptionDoc)
-- import GhcMod.SrcUtils (pretty,collectAllSpansTypes)
import GhcMod.DynFlags (withDynFlags)
import GhcMod.Convert ( convert )
