module Main where

import Test.DocTest

main :: IO ()
main = doctest [
    "-package"
  , "ghc"
  , "Language/Haskell/GhcMod.hs"
  ]
