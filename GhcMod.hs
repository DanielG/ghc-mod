-- | The ghc-mod library.

module GhcMod (
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
  , GmLogLevel
  , increaseLogLevel
  , decreaseLogLevel
  , gmSetLogLevel
  , gmLog
  -- * Types
  , ModuleString
  , Expression(..)
  , GhcPkgDb
  , Symbol
  , SymbolDb
  , GhcModError(..)
  -- * Monad Types
  , GhcModT
  , IOish
  -- * Monad utilities
  , runGhcModT
  , withOptions
  , dropSession
  -- * 'GhcMod' utilities
  , boot
  , browse
  , check
  , checkSyntax
  , debugInfo
  , componentInfo
  , expandTemplate
  , info
  , lint
  , pkgDoc
  , rootInfo
  , types
  , test
  , splits
  , sig
  , refine
  , auto
  , modules
  , languages
  , flags
  , findSymbol
  , lookupSymbol
  , dumpSymbol
  -- * SymbolDb
  , loadSymbolDb
  , isOutdated
  -- * Output
  , gmPutStr
  , gmErrStr
  , gmPutStrLn
  , gmErrStrLn
  -- * FileMapping
  , loadMappedFile
  , loadMappedFileSource
  , unloadMappedFile
  ) where

import GhcMod.Boot
import GhcMod.Browse
import GhcMod.CaseSplit
import GhcMod.Check
import GhcMod.Debug
import GhcMod.FillSig
import GhcMod.Find
import GhcMod.Flag
import GhcMod.Info
import GhcMod.Lang
import GhcMod.Lint
import GhcMod.Modules
import GhcMod.PkgDoc
import GhcMod.Test
import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.FileMapping
import Language.Haskell.GhcMod.Logging
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Output
import Language.Haskell.GhcMod.Target
import Language.Haskell.GhcMod.Types
