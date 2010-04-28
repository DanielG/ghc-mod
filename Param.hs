module Param where

data Options = Options {
    convert :: [String] -> String
  , ghcPkg  :: FilePath
  , outDir  :: FilePath
  }

outputDir :: String
outputDir = "dist/flymake"

outputFile :: String
outputFile = "dist/flymake/a.out"
