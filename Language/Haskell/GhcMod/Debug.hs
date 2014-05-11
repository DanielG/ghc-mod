module Language.Haskell.GhcMod.Debug (debugInfo, rootInfo) where

import Control.Applicative ((<$>))
import Control.Exception.IOChoice ((||>))
import CoreMonad (liftIO)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust, fromJust)
import Language.Haskell.GhcMod.CabalApi
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Types

----------------------------------------------------------------

-- | Obtaining debug information.
debugInfo :: Options
          -> Cradle
          -> IO String
debugInfo opt cradle = convert opt <$> do
    CompilerOptions gopts incDir pkgs <-
        if cabal then
            liftIO (fromCabalFile ||> return simpleCompilerOption)
          else
            return simpleCompilerOption
    mglibdir <- liftIO getSystemLibDir
    return [
        "Root directory:      " ++ rootDir
      , "Current directory:   " ++ currentDir
      , "Cabal file:          " ++ cabalFile
      , "GHC options:         " ++ unwords gopts
      , "Include directories: " ++ unwords incDir
      , "Dependent packages:  " ++ intercalate ", " (map showPkg pkgs)
      , "System libraries:    " ++ fromMaybe "" mglibdir
      ]
  where
    currentDir = cradleCurrentDir cradle
    mCabalFile = cradleCabalFile cradle
    rootDir    = cradleRootDir cradle
    cabal = isJust mCabalFile
    cabalFile = fromMaybe "" mCabalFile
    origGopts = ghcOpts opt
    simpleCompilerOption = CompilerOptions origGopts [] []
    fromCabalFile = do
        pkgDesc <- parseCabalFile file
        getCompilerOptions origGopts cradle pkgDesc
      where
        file = fromJust mCabalFile

----------------------------------------------------------------

-- | Obtaining root information.
rootInfo :: Options
          -> Cradle
          -> IO String
rootInfo opt cradle = return $ convert opt $ cradleRootDir cradle
