-- | Low level access to the ghc-mod library.

module Language.Haskell.GhcMod.Internal (
  -- * Low level access
    LogReader
  , GHCOption
  , initializeFlagsWithCradle
  , setTargetFile
  , checkSlowAndSet
  , getDynamicFlags
  ) where

import Language.Haskell.GhcMod.ErrMsg
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Types

