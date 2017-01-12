module GhcMod.Boot where

import Control.Applicative
import Prelude

import GhcMod.Browse
import GhcMod.Flag
import GhcMod.Lang
import GhcMod.Modules
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Types (defaultBrowseOpts)

-- | Printing necessary information for front-end booting.
boot :: IOish m => GhcModT m String
boot = concat <$> sequence ms
  where
    ms = [modules False, languages, flags, concat <$> mapM (browse defaultBrowseOpts) preBrowsedModules]

preBrowsedModules :: [String]
preBrowsedModules = [
    "Prelude"
  , "Control.Applicative"
  , "Control.Exception"
  , "Control.Monad"
  , "Data.Char"
  , "Data.List"
  , "Data.Maybe"
  , "System.IO"
  ]
