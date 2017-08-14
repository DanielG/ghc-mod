{-# LANGUAGE ScopedTypeVariables #-}

-- | WARNING
-- This program is deprecated, use `ghc-mod legacy-interactive` instead.

module Main where

import Control.Applicative
import Control.Monad
import Control.Exception
import Data.Version
import Data.Maybe
import System.IO
import System.Exit
import System.Process
import System.FilePath
import System.Environment
import Paths_ghc_mod
import Utils
import Prelude

main :: IO ()
main = do
  hPutStrLn stderr $
    "Warning: ghc-modi is deprecated please use 'ghc-mod legacy-interactive' instead"

  args <- getArgs
  bindir <- getBinDir
  let installedExe = bindir </> "ghc-mod"
  mexe <- mplus <$> mightExist installedExe <*> pathExe
  case mexe of
    Nothing -> do
      hPutStrLn stderr $
        "ghc-modi: Could not find '"++installedExe++"', check your installation!"
      exitWith $ ExitFailure 1

    Just exe -> do
      (_, _, _, h) <-
          createProcess $ proc exe $ ["legacy-interactive"] ++ args
      exitWith =<< waitForProcess h

pathExe :: IO (Maybe String)
pathExe = do
  ev <- try $ words <$> readProcess "ghc-mod" ["--version"] ""
  let mexe = case ev of
               Left (SomeException _) -> Nothing
               Right ["ghc-mod", "version", ver
                     , "compiled", "by", "GHC", _]
                   | showVersion version == ver -> do
                       Just "ghc-mod"
               Right _ -> Nothing

  when (isNothing mexe) $
      hPutStrLn stderr "ghc-modi: ghc-mod executable on PATH has different version, check your installation!"
  return mexe
