module Boot where

import Control.Applicative ((<$>))
import CoreMonad (liftIO)
import GHC (Ghc)
import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.Ghc

boot :: Options -> Cradle -> IO String
boot opt cradle = do
    mods  <- listModules opt cradle
    langs <- listLanguages opt
    flags <- listFlags opt
    let opt' = addPackages opt
    pre   <- concat <$> mapM (browseModule opt' cradle) preBrowsedModules
    return $ mods ++ langs ++ flags ++ pre

boot' :: Options -> Ghc String
boot' opt = do
    mods  <- modules opt
    langs <- liftIO $ listLanguages opt
    flags <- liftIO $ listFlags opt
    let opt' = addPackages opt
    pre   <- concat <$> mapM (browse opt') preBrowsedModules
    return $ mods ++ langs ++ flags ++ pre

preBrowsedModules :: [String]
preBrowsedModules = [
    "Prelude"
  , "Control.Applicative"
  , "Control.Exception"
  , "Control.Monad"
  , "Data.ByteString"
  , "Data.Char"
  , "Data.List"
  , "Data.Maybe"
  , "System.Directory"
  , "System.FilePath"
  , "System.IO"
  ]

preBrowsePackages :: [String]
preBrowsePackages = [
    "bytestring"
  , "directory"
  , "filepath"
  ]

addPackages :: Options -> Options
addPackages opt = opt { ghcOpts = pkgs ++ ghcOpts opt}
  where
    pkgs = map ("-package " ++) preBrowsePackages
