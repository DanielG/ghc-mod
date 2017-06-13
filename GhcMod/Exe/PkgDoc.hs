module GhcMod.Exe.PkgDoc (pkgDoc) where

import GhcMod.Types
import GhcMod.GhcPkg
import GhcMod.Monad
import GhcMod.Output

import Control.Applicative
import Prelude

-- | Obtaining the package name and the doc path of a module.
pkgDoc :: IOish m => String -> GhcModT m String
pkgDoc mdl = do
    ghcPkg <- getGhcPkgProgram
    readProc <- gmReadProcess
    pkgDbStack <- getPackageDbStack
    pkg <- liftIO $ trim <$> readProc ghcPkg (toModuleOpts pkgDbStack) ""
    if pkg == "" then
        return "\n"
      else do
        htmlpath <- liftIO $ readProc ghcPkg (toDocDirOpts pkg pkgDbStack) ""
        let ret = pkg ++ " " ++ drop 14 htmlpath
        return ret
  where
    toModuleOpts dbs = ["find-module", mdl, "--simple-output"]
                   ++ ghcPkgDbStackOpts dbs
    toDocDirOpts pkg dbs = ["field", pkg, "haddock-html"]
                       ++ ghcPkgDbStackOpts dbs
    trim = takeWhile (`notElem` " \n")
