module Param where

data Options   = Options { convert :: [String] -> String
                         , ghc     :: FilePath
                         , ghci    :: FilePath
                         , ghcPkg  :: FilePath
                         }

