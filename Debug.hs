module Debug (debugInfo) where

import CabalApi
import Control.Applicative
import Control.Monad
import Data.List (intercalate)
import Data.Maybe
import GHC
import GHCApi
import Prelude
import Types

----------------------------------------------------------------

debugInfo :: Options -> Cradle -> String -> String -> IO String
debugInfo opt cradle ver fileName = unlines <$> debug opt cradle ver fileName

debug :: Options -> Cradle -> String -> String -> IO [String]
debug opt cradle ver fileName = do
    (gopts, incDir, pkgs) <-
        if cabal then
            fromCabalFile (ghcOpts opt) cradle
          else
            return (ghcOpts opt, [], [])
    [fast] <- withGHC fileName $ do
        void $ initializeFlagsWithCradle opt cradle gopts True
        setTargetFile fileName
        slow <- needsTemplateHaskell <$> depanal [] False
        return [not slow]
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
