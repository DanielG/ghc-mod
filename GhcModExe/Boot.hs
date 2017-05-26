module GhcModExe.Boot where

import Control.Applicative
import Prelude

import GhcModExe.Browse
import GhcModExe.Flag
import GhcModExe.Lang
import GhcModExe.Modules
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Types

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
