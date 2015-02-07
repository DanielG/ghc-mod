{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Applicative

import Distribution.Simple.Utils (cabalVersion)
import Distribution.Simple.Configure
import Distribution.Text ( display )
import System.Environment
import System.Directory

main :: IO ()
main = do
  args <- getArgs
  case args of
    "version":[] -> do
        putStrLn $ "using version " ++ display cabalVersion ++ " of the Cabal library"
    "print-setup-config":args' -> do
        mfile <- findFile ["dist"] "setup-config"

        let file = case mfile of
                     Just f -> f
                     Nothing -> let !(f:[]) = args' in f

        putStrLn =<< show <$> getConfigStateFile file

    cmd:_ -> error $ "Unknown command: " ++ cmd
    [] -> error "No command given"
