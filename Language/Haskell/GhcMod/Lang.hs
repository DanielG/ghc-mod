module Language.Haskell.GhcMod.Lang where

import DynFlags (supportedLanguagesAndExtensions)
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Monad

-- | Listing language extensions.

languages :: GhcMod String
languages = convert' supportedLanguagesAndExtensions
