module Language.Haskell.GhcMod.Lint where

import Control.Applicative
import Data.List
import Language.Haskell.GhcMod.Types
import Language.Haskell.HLint

lintSyntax :: Options -> String -> IO String
lintSyntax opt file = pack <$> lint opt file
  where
    pack = unlines . map (intercalate "\0" . lines)

lint :: Options -> String -> IO [String]
lint opt file = map show <$> hlint ([file, "--quiet"] ++ hlintOpts opt)
