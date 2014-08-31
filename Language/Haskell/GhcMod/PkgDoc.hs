module Language.Haskell.GhcMod.PkgDoc (pkgDoc) where

import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Utils

import Control.Applicative ((<$>))

-- | Obtaining the package name and the doc path of a module.
pkgDoc :: IOish m => String -> GhcModT m String
pkgDoc mdl = do
    c <- cradle
    pkg <- trim <$> readProcess' "ghc-pkg" (toModuleOpts c)
    if pkg == "" then
        return "\n"
      else do
        htmlpath <- readProcess' "ghc-pkg" (toDocDirOpts pkg c)
        let ret = pkg ++ " " ++ drop 14 htmlpath
        return ret
  where
    toModuleOpts c = ["find-module", mdl, "--simple-output"]
                   ++ ghcPkgDbStackOpts (cradlePkgDbStack c)
    toDocDirOpts pkg c = ["field", pkg, "haddock-html"]
                       ++ ghcPkgDbStackOpts (cradlePkgDbStack c)
    trim = takeWhile (`notElem` " \n")
