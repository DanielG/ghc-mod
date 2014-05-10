module Language.Haskell.GhcMod.Boot where

import Control.Applicative ((<$>))
import CoreMonad (liftIO, liftIO)
import Language.Haskell.GhcMod.Browse
import Language.Haskell.GhcMod.Flag
import Language.Haskell.GhcMod.Lang
import Language.Haskell.GhcMod.List
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Types

-- | Printing necessary information for front-end booting.
bootInfo :: Options -> IO String
bootInfo opt = runGhcMod opt $ boot

-- | Printing necessary information for front-end booting.
boot :: GhcMod String
boot = do
    opt   <- options
    mods  <- modules
    langs <- liftIO $ listLanguages opt
    flags <- liftIO $ listFlags opt
    pre   <- concat <$> mapM browse preBrowsedModules
    return $ mods ++ langs ++ flags ++ pre

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
