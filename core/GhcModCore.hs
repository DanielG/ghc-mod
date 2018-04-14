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
  , GmLogLevel
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
  , getTypecheckedModuleGhc
  , getTypecheckedModuleGhc'
  ) where

import GhcMod.Cradle
import GhcMod.FileMapping
import GhcMod.Logging
import GhcMod.Monad
import GhcMod.ModuleLoader
import GhcMod.Output
import GhcMod.Target
import GhcMod.Types
