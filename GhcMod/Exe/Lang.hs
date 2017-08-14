module GhcMod.Exe.Lang where

import DynFlags (supportedLanguagesAndExtensions)
import GhcMod.Convert
import GhcMod.Monad

-- | Listing language extensions.

languages :: IOish m => GhcModT m String
languages = convert' supportedLanguagesAndExtensions
