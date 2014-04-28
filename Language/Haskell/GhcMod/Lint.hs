module Language.Haskell.GhcMod.Lint where

import Control.Applicative ((<$>))
import Control.Exception (handle, SomeException(..))
import Language.Haskell.GhcMod.Logger (checkErrorPrefix)
import Language.Haskell.GhcMod.Types
import Language.Haskell.HLint (hlint)

-- | Checking syntax of a target file using hlint.
--   Warnings and errors are returned.
lintSyntax :: Options
           -> FilePath  -- ^ A target file.
           -> IO String
lintSyntax opt file = handle handler $ pack <$> hlint (file : "--quiet" : hopts)
  where
    pack = convert opt . map (init . show) -- init drops the last \n.
    hopts = hlintOpts opt
    handler (SomeException e) = return $ checkErrorPrefix ++ show e ++ "\n"
