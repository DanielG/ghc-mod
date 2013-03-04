module Debug (debugInfo) where

import CabalApi
import GHCApi
import Control.Applicative
import Data.List (intercalate)
import Data.Maybe
import Prelude
import Types

----------------------------------------------------------------

debugInfo :: Options -> Cradle -> String -> String -> IO String
debugInfo opt cradle ver fileName = unlines <$> debug opt cradle ver fileName

debug :: Options -> Cradle -> String -> String -> IO [String]
debug opt cradle ver fileName = do
    (gopts, incDir, pkgs, langext) <-
        if cabal then
            fromCabalFile (ghcOpts opt) cradle
          else
            return (ghcOpts opt, [], [], [])
    dflags <- getDynamicFlags
    fast <- getFastCheck dflags fileName (Just langext)
    return [
        "GHC version:         " ++ ver
      , "Current directory:   " ++ currentDir
      , "Cabal file:          " ++ cabalFile
      , "GHC options:         " ++ intercalate " " gopts
      , "Include directories: " ++ intercalate " " incDir
      , "Dependent packages:  " ++ intercalate ", " pkgs
      , "Fast check:          " ++ if fast then "Yes" else "No"
      ]
  where
    currentDir = cradleCurrentDir cradle
    cabal = isJust $ cradleCabalFile cradle
    cabalFile = fromMaybe "" $ cradleCabalFile cradle
