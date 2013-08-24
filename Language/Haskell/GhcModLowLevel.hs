-- | Low level access to the ghc-mod library.

module Language.Haskell.GhcModLowLevel (
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
  -- * Converting the 'Ghc' monad to the 'IO' monad
  , withGHC
  , withGHCDummyFile
  -- * Low level access
  , LogReader
  , GHCOption
  , initializeFlagsWithCradle
  , setTargetFile
  , checkSlowAndSet
  , getDynamicFlags
  ) where

import Language.Haskell.GhcMod.Browse
import Language.Haskell.GhcMod.Check
import Language.Haskell.GhcMod.Cradle
import Language.Haskell.GhcMod.Debug
import Language.Haskell.GhcMod.ErrMsg
import Language.Haskell.GhcMod.Flag
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Info
import Language.Haskell.GhcMod.Lang
import Language.Haskell.GhcMod.Lint
import Language.Haskell.GhcMod.List
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.CabalApi
