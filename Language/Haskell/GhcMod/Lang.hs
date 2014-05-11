module Language.Haskell.GhcMod.Lang where

import DynFlags (supportedLanguagesAndExtensions)
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Convert

-- | Listing language extensions.

listLanguages :: Options -> IO String
listLanguages opt = return $ convert opt supportedLanguagesAndExtensions
