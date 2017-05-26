-- | Dispatcher program to support co-installation of multiple ghc-mod
-- instances (compiled against different GHC versions) without breaking the
-- commandline API
module Main where

import System.IO
import System.Exit
import System.Process
import System.FilePath
import System.Environment
import Utils

import Paths_ghc_mod

main :: IO ()
main = do
  args <- getArgs
  libexecdir <- getLibexecDir
  let installedExe = libexecdir </> "ghc-mod-real"
  mexe <- mightExist installedExe
  case mexe of
    Nothing -> do
      hPutStrLn stderr $
        "ghc-mod: Could not find '"++installedExe++"', check your installation!"
      exitWith $ ExitFailure 1

    Just exe -> do
      (_, _, _, h) <-
          createProcess $ proc exe args
      exitWith =<< waitForProcess h
