module Param where

data Options = Options {
    convert :: [String] -> String
  , ghc     :: FilePath
  , ghci    :: FilePath
  , ghcPkg  :: FilePath
  , outDir  :: FilePath
  , outFile :: FilePath
  }

outputDir :: String
outputDir = "dist/flymake"

outputFile :: String
outputFile = "dist/flymake/a.out"
