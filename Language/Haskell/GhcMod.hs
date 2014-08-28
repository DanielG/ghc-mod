-- | The ghc-mod library.

module Language.Haskell.GhcMod (
  -- * Cradle
    Cradle(..)
  , findCradle
  -- * Options
  , Options(..)
  , LineSeparator(..)
  , OutputStyle(..)
  , defaultOptions
  -- * Types
  , ModuleString
  , Expression
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
  -- * 'GhcMod' utilities
  , boot
  , browse
  , check
  , checkSyntax
  , debugInfo
  , expandTemplate
  , info
  , lint
  , pkgDoc
  , rootInfo
  , types
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
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Modules
import Language.Haskell.GhcMod.PkgDoc
import Language.Haskell.GhcMod.Types
