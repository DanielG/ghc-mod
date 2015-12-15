-- | The ghc-mod library.

module Language.Haskell.GhcMod (
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

import Language.Haskell.GhcMod.Boot
import Language.Haskell.GhcMod.Browse
import Language.Haskell.GhcMod.CaseSplit
import Language.Haskell.GhcMod.Check
import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.Debug
import Language.Haskell.GhcMod.FillSig
import Language.Haskell.GhcMod.Find
import Language.Haskell.GhcMod.Flag
import Language.Haskell.GhcMod.Info
import Language.Haskell.GhcMod.Lang
import Language.Haskell.GhcMod.Lint
import Language.Haskell.GhcMod.Logging
import Language.Haskell.GhcMod.Modules
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.PkgDoc
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Target
import Language.Haskell.GhcMod.Output
import Language.Haskell.GhcMod.FileMapping
import Language.Haskell.GhcMod.Test
