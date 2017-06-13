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

import GhcMod.Exe.Boot
import GhcMod.Exe.Browse
import GhcMod.Exe.CaseSplit
import GhcMod.Exe.Check
import GhcMod.Exe.Debug
import GhcMod.Exe.FillSig
import GhcMod.Exe.Find
import GhcMod.Exe.Flag
import GhcMod.Exe.Info
import GhcMod.Exe.Lang
import GhcMod.Exe.Lint
import GhcMod.Exe.Modules
import GhcMod.Exe.PkgDoc
import GhcMod.Exe.Test
import GhcMod.Cradle
import GhcMod.FileMapping
import GhcMod.Logging
import GhcMod.Monad
import GhcMod.Output
import GhcMod.Target
import GhcMod.Types
