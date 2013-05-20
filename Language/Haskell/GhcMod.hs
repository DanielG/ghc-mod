module Language.Haskell.GhcMod (
  -- * Cradle
    Cradle(..)
  , findCradle
  -- * GHC version
  , GHCVersion
  , getGHCVersion
  -- * Options
  , Options(..)
  , OutputStyle(..)
  , defaultOptions
  -- * Types
  , ModuleString
  , Expression
  -- * 'IO' utilities
  , browseModule
  , checkSyntax
  , lintSyntax
  , infoExpr
  , typeExpr
  , listModules
  , listLanguages
  , listFlags
  , debugInfo
  -- * Converting the 'Ghc' monad to the 'IO' monad
  , withGHC
  , withGHCDummyFile
  -- * 'Ghc' utilities
  , browse
  , check
  , info
  , typeOf
  , listMods
  , debug
  ) where

import Language.Haskell.GhcMod.Browse
import Language.Haskell.GhcMod.Check
import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.Debug
import Language.Haskell.GhcMod.Flag
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Info
import Language.Haskell.GhcMod.Lang
import Language.Haskell.GhcMod.Lint
import Language.Haskell.GhcMod.List
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.CabalApi
