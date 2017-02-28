module GhcModExe.Flag where

import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Monad

-- | Listing of GHC flags, same as @ghc@\'s @--show-options@ with @ghc >= 7.10@.
flags :: IOish m => GhcModT m String
flags = convert' Gap.ghcCmdOptions
