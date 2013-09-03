module Language.Haskell.GhcMod.Lint where

import Control.Applicative
import Data.List
import Language.Haskell.GhcMod.Types
import Language.Haskell.HLint

-- | Checking syntax of a target file using hlint.
--   Warnings and errors are returned.
lintSyntax :: Options
           -> FilePath  -- ^ A target file.
           -> IO String
lintSyntax opt file = pack <$> lint opt file
  where
    LineSeparator lsep = lineSeparator opt
    pack = unlines . map (intercalate lsep . lines)

lint :: Options
     -> FilePath    -- ^ A target file.
     -> IO [String]
lint opt file = map show <$> hlint ([file, "--quiet"] ++ hlintOpts opt)
