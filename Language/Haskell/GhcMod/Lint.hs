module Language.Haskell.GhcMod.Lint where

import Control.Applicative ((<$>))
import Data.List (intercalate)
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Utils
import Language.Haskell.HLint (hlint)

-- | Checking syntax of a target file using hlint.
--   Warnings and errors are returned.
lintSyntax :: Options
           -> FilePath  -- ^ A target file.
           -> IO String
lintSyntax opt file = pack <$> lint hopts file
  where
    LineSeparator lsep = lineSeparator opt
    pack = unlines . map (intercalate lsep . lines)
    hopts = hlintOpts opt

-- | Checking syntax of a target file using hlint.
--   Warnings and errors are returned.
lint :: [String]
     -> FilePath    -- ^ A target file.
     -> IO [String]
lint hopts file = map show <$> suppressStdout (hlint (file : hopts))
