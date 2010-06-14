module Lint where

import Control.Applicative
import Data.List
import Language.Haskell.HLint
import Types

lintSyntax :: Options -> String -> IO String
lintSyntax opt file = pretty <$> lint opt file
  where
    pretty = unlines . map (concat . intersperse "\0" . lines)

lint :: Options -> String -> IO [String]
lint opt file = map show <$> hlint ([file, "--quiet"] ++ hlintOpts opt)
