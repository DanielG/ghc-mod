module Language.Haskell.GhcMod.Debug (debugInfo, debug) where

import Control.Applicative
import Control.Exception.IOChoice
import Control.Monad
import Data.List (intercalate)
import Data.Maybe
import GHC
import Language.Haskell.GhcMod.CabalApi
import Language.Haskell.GhcMod.GHCApi
import Language.Haskell.GhcMod.Types
import Prelude

----------------------------------------------------------------

debugInfo :: Options -> Cradle -> String -> String -> IO String
debugInfo opt cradle ver fileName = unlines <$> debug opt cradle ver fileName

debug :: Options -> Cradle -> String -> String -> IO [String]
debug opt cradle ver fileName = do
    (gopts, incDir, pkgs) <-
        if cabal then
            fromCabalFile (ghcOpts opt) cradle ||> return (ghcOpts opt, [], [])
          else
            return (ghcOpts opt, [], [])
    [fast] <- withGHC fileName $ do
        void $ initializeFlagsWithCradle opt cradle gopts True
        setTargetFile fileName
        pure . canCheckFast <$> depanal [] False
    return [
        "GHC version:         " ++ ver
      , "Current directory:   " ++ currentDir
      , "Cabal file:          " ++ cabalFile
      , "GHC options:         " ++ unwords gopts
      , "Include directories: " ++ unwords incDir
      , "Dependent packages:  " ++ intercalate ", " pkgs
      , "Fast check:          " ++ if fast then "Yes" else "No"
      ]
  where
    currentDir = cradleCurrentDir cradle
    cabal = isJust $ cradleCabalFile cradle
    cabalFile = fromMaybe "" $ cradleCabalFile cradle
