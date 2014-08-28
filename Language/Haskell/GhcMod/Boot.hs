module Language.Haskell.GhcMod.Boot where

import Control.Applicative
import Language.Haskell.GhcMod.Browse
import Language.Haskell.GhcMod.Flag
import Language.Haskell.GhcMod.Lang
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Modules

-- | Printing necessary information for front-end booting.
boot :: IOish m => GhcModT m String
boot =  concat <$> sequence [modules, languages, flags,
                             concat <$> mapM browse preBrowsedModules]

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
