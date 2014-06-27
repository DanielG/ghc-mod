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
  -- * 'IO' utilities
  , bootInfo
  , browse
  , checkSyntax
  , lintSyntax
  , expandTemplate
  , infoExpr
  , typeExpr
  , fillSig
  , listModules
  , listLanguages
  , listFlags
  , debugInfo
  , rootInfo
  , packageDoc
  , findSymbol
  , splitVar
  ) where

import Language.Haskell.GhcMod.Boot
import Language.Haskell.GhcMod.Browse
import Language.Haskell.GhcMod.Check
import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.Debug
import Language.Haskell.GhcMod.Find
import Language.Haskell.GhcMod.Flag
import Language.Haskell.GhcMod.Info
import Language.Haskell.GhcMod.Lang
import Language.Haskell.GhcMod.Lint
import Language.Haskell.GhcMod.List
import Language.Haskell.GhcMod.PkgDoc
import Language.Haskell.GhcMod.FillSig
import Language.Haskell.GhcMod.CaseSplit
import Language.Haskell.GhcMod.Types
