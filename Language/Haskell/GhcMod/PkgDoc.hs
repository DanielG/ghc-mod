module Language.Haskell.GhcMod.PkgDoc (packageDoc) where

import Control.Applicative ((<$>))
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Cradle
import System.Process (readProcess)

-- | Obtaining the package name and the doc path of a module.
packageDoc :: Options
           -> Cradle
           -> ModuleString
           -> IO String
packageDoc _ cradle mdl = pkgDoc cradle mdl

pkgDoc :: Cradle -> String -> IO String
pkgDoc cradle mdl = do
    pkg <- trim <$> readProcess "ghc-pkg" toModuleOpts []
    if pkg == "" then
        return "\n"
      else do
        htmlpath <- readProcess "ghc-pkg" (toDocDirOpts pkg) []
        let ret = pkg ++ " " ++ drop 14 htmlpath
        return ret
  where
    toModuleOpts = ["find-module", mdl, "--simple-output"] ++ userPackageDbOptsForGhcPkg (cradlePackageDb cradle)
    toDocDirOpts pkg = ["field", pkg, "haddock-html"] ++ userPackageDbOptsForGhcPkg (cradlePackageDb cradle)
    trim = takeWhile (/= '\n')
