{-# LANGUAGE CPP #-}
module Main where

import Test.DocTest

main :: IO ()
main = doctest
  [ "-package", "ghc-" ++ VERSION_ghc
  , "-package", "transformers-" ++ VERSION_transformers
  , "-package", "mtl-" ++ VERSION_mtl
  , "-package", "directory-" ++ VERSION_directory
  , "-XScopedTypeVariables", "-XRecordWildCards", "-XNamedFieldPuns", "-XConstraintKinds", "-XFlexibleContexts", "-XDataKinds", "-XKindSignatures", "-XTypeOperators", "-XViewPatterns"
  , "-idist/build/autogen/"
  , "-optP-include"
  , "-optPdist/build/autogen/cabal_macros.h"
  , "Language/Haskell/GhcMod.hs"
  ]
