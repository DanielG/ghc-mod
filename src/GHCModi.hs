{-# LANGUAGE ScopedTypeVariables, DeriveDataTypeable #-}

-- | WARNING
-- This program is deprecated, use `ghc-mod legacy-interactive` instead.

module Main where

import System.Exit
import System.Process
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  h <- spawnProcess "ghc-mod" $ ["legacy-interactive"] ++ args
  exitWith =<< waitForProcess h
