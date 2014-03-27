module Language.Haskell.GhcMod.PkgDoc (packageDoc) where

import Control.Applicative ((<$>))
import Language.Haskell.GhcMod.Types
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
        htmlpath <- readProcess "ghc-pkg" ["field", pkg, "haddock-html"] []
        let ret = pkg ++ " " ++ drop 14 htmlpath
        return ret
  where
    toModuleOpts = ["find-module", "--simple-output"] ++ cradlePackageDbOpts cradle ++ [mdl]
    trim = takeWhile (/= '\n')

