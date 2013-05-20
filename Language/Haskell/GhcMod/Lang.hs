module Language.Haskell.GhcMod.Lang where

import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Types

-- | Listing language extensions.

listLanguages :: Options -> IO String
listLanguages opt = return $ convert opt Gap.supportedExtensions
