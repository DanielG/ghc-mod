module Main where

import Test.DocTest

main :: IO ()
main = doctest [
    "-package"
  , "ghc"
  , "-idist/build/autogen/"
  , "-optP-include"
  , "-optPdist/build/autogen/cabal_macros.h"
  , "Language/Haskell/GhcMod.hs"
  ]
