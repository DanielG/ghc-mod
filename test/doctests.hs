{-# LANGUAGE CPP #-}
module Main where

import Test.DocTest
import System.Environment
import Data.Maybe

main :: IO ()
main = do
  distdir <- (fromMaybe "dist" . lookup "DOCTEST_DIST_DIR") `fmap` getEnvironment
  doctest
     [ "-package", "ghc-" ++ VERSION_ghc
     , "-package", "transformers-" ++ VERSION_transformers
     , "-package", "mtl-" ++ VERSION_mtl
     , "-package", "directory-" ++ VERSION_directory
     , "-XScopedTypeVariables", "-XRecordWildCards", "-XNamedFieldPuns", "-XConstraintKinds", "-XFlexibleContexts", "-XDataKinds", "-XKindSignatures", "-XTypeOperators", "-XViewPatterns"
     , "-i" ++ distdir ++ "/build/autogen/"
     , "-icore/"
     , "-ishared"
--     , "-optP-include"
--     , "-optP" ++ distdir ++ "/build/autogen/cabal_macros.h"
     , "GhcMod.hs"
     ]
