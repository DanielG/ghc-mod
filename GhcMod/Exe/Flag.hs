module GhcMod.Exe.Flag where

import qualified GhcMod.Gap as Gap
import GhcMod.Convert
import GhcMod.Monad

-- | Listing of GHC flags, same as @ghc@\'s @--show-options@ with @ghc >= 7.10@.
flags :: IOish m => GhcModT m String
flags = convert' Gap.ghcCmdOptions
