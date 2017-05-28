module GhcMod.Exe.Boot where

import Control.Applicative
import Prelude

import GhcMod.Exe.Browse
import GhcMod.Exe.Flag
import GhcMod.Exe.Lang
import GhcMod.Exe.Modules
import GhcMod.Monad
import GhcMod.Types (defaultBrowseOpts)

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
