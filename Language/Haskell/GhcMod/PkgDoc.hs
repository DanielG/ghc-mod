module Language.Haskell.GhcMod.PkgDoc (pkgDoc, pkgFindModule) where

import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Output

import Control.Applicative
import Prelude

-- | Obtaining the package name and the doc path of a module.
pkgDoc :: IOish m => String -> GhcModT m String
pkgDoc mdl = do
    ghcPkg <- getGhcPkgProgram
    readProc <- gmReadProcess
    pkgDbStack <- getPackageDbStack
    pkg <- pkgFindModule ghcPkg readProc pkgDbStack mdl

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

pkgFindModule
    :: IOish m
    => FilePath
    -> (FilePath -> [String] -> String -> IO String)
    -> [GhcPkgDb]
    -> String
    -> m String
pkgFindModule ghcPkg readProc pkgDbStack mdl =
    liftIO $ trim <$> readProc ghcPkg (toModuleOpts pkgDbStack) ""
  where
    toModuleOpts dbs = ["find-module", mdl, "--simple-output"]
                   ++ ghcPkgDbStackOpts dbs
    trim = takeWhile (`notElem` " \n")
