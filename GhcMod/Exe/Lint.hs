module GhcMod.Exe.Lint where

import Exception (ghandle)
import Control.Exception (SomeException(..))
import GhcMod.Logger (checkErrorPrefix)
import GhcMod.Convert
import GhcMod.Types
import GhcMod.Monad
import Language.Haskell.HLint3

import GhcMod.Utils (withMappedFile)
import Language.Haskell.Exts.SrcLoc (SrcSpan(..))

-- | Checking syntax of a target file using hlint.
--   Warnings and errors are returned.
lint :: IOish m
     => LintOpts  -- ^ Configuration parameters
     -> FilePath  -- ^ A target file.
     -> GhcModT m String
lint opt file = ghandle handler $
  withMappedFile file $ \tempfile -> do
    res <- liftIO $ hlint $ "--quiet" : tempfile : optLintHlintOpts opt
    pack . map (show . substFile file tempfile) $ res
  where
    pack = convert' . map init -- init drops the last \n.
    handler (SomeException e) = return $ checkErrorPrefix ++ show e ++ "\n"
    substFile orig temp idea
      | srcSpanFilename (ideaSpan idea) == temp
      = idea{ideaSpan=(ideaSpan idea){srcSpanFilename = orig}}
    substFile _ _ idea = idea
