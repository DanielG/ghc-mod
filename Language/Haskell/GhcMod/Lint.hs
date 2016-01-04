module Language.Haskell.GhcMod.Lint where

import Exception (ghandle)
import Control.Exception (SomeException(..))
import Language.Haskell.GhcMod.Logger (checkErrorPrefix)
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad
import Language.Haskell.HLint (hlint)

import Language.Haskell.GhcMod.Utils (withMappedFile)

import Data.List (stripPrefix)

-- | Checking syntax of a target file using hlint.
--   Warnings and errors are returned.
lint :: IOish m
     => LintOpts  -- ^ Configuration parameters
     -> FilePath  -- ^ A target file.
     -> GhcModT m String
lint opt file =
  withMappedFile file $ \tempfile ->
        liftIO (hlint $ tempfile : "--quiet" : optLintHlintOpts opt)
    >>= mapM (replaceFileName tempfile)
    >>= ghandle handler . pack
 where
    pack = convert' . map init -- init drops the last \n.
    handler (SomeException e) = return $ checkErrorPrefix ++ show e ++ "\n"
    replaceFileName fp s = return $ maybe (show s) (file++) $ stripPrefix fp (show s)
