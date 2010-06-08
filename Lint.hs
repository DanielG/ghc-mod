module Lint where

import Control.Applicative
import Data.List
import Language.Haskell.HLint
import Types

lintSyntax :: Options -> String -> IO String
lintSyntax _ file = pretty <$> lint file
  where
    pretty = unlines . map (concat . intersperse "\0")
           . filter (\x -> length x > 1)
           . groupBy (\a b -> a /= "" && b /= "") . lines

lint :: String -> IO String
lint file = toString <$> hlint [file, "--quiet", "--ignore=Use camelCase"]
  where
    toString = concat . map show
