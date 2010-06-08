module Lint where

import Control.Applicative
import Data.List
import Language.Haskell.HLint
import Types

lintSyntax :: Options -> String -> IO String
lintSyntax _ file = pretty <$> lint file
  where
    pretty = unlines . map (concat . intersperse "\0" . lines)

lint :: String -> IO [String]
lint file = map show <$> hlint [file, "--quiet", "--ignore=Use camelCase"]
