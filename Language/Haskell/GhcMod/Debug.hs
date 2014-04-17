module Language.Haskell.GhcMod.Debug (debugInfo, debug, rootInfo, root) where

import Control.Applicative ((<$>))
import Control.Exception.IOChoice ((||>))
import Control.Monad (void)
import CoreMonad (liftIO)
import Data.List (intercalate)
import Data.Maybe (fromMaybe, isJust, fromJust)
import GHC (Ghc)
import Language.Haskell.GhcMod.CabalApi
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Types

----------------------------------------------------------------

-- | Obtaining debug information.
debugInfo :: Options
          -> Cradle
          -> FilePath   -- ^ A target file.
          -> IO String
debugInfo opt cradle fileName = unlines <$> withGHC fileName (debug opt cradle fileName)

-- | Obtaining debug information.
debug :: Options
      -> Cradle
      -> FilePath     -- ^ A target file.
      -> Ghc [String]
debug opt cradle fileName = do
    CompilerOptions gopts incDir pkgs <-
        if cabal then
            liftIO (fromCabalFile ||> return simpleCompilerOption)
          else
            return simpleCompilerOption
    void $ initializeFlagsWithCradle opt cradle gopts True
    setTargetFiles [fileName]
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
          -> FilePath   -- ^ A target file.
          -> IO String
rootInfo opt cradle fileName = withGHC fileName (root opt cradle fileName)

-- | Obtaining root information.
root :: Options
      -> Cradle
      -> FilePath     -- ^ A target file.
      -> Ghc String
root _ cradle _ = return $ cradleRootDir cradle ++ "\n"
