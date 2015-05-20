{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

-- | WARNING
-- This program is deprecated, use `ghc-mod legacy-interactive` instead.

module Main where

import System.Exit
import System.Process
import System.FilePath
import System.Environment
import Paths_ghc_mod

main :: IO ()
main = do
  args <- getArgs
  bindir <- getBinDir
  (_, _, _, h) <-
    createProcess $ proc (bindir </> "ghc-mod") $ ["legacy-interactive"] ++ args
  exitWith =<< waitForProcess h
