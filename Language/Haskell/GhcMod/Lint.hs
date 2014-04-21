module Language.Haskell.GhcMod.Lint where

import Control.Applicative ((<$>))
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Utils (replace)
import Language.Haskell.HLint (hlint)

-- | Checking syntax of a target file using hlint.
--   Warnings and errors are returned.
lintSyntax :: Options
           -> FilePath  -- ^ A target file.
           -> IO String
lintSyntax opt file = pack <$> hlint (file : "--quiet" : hopts)
  where
    LineSeparator lsep = lineSeparator opt
    pack = convert opt . map (replace '\n' lsep . init . show)
    hopts = hlintOpts opt
