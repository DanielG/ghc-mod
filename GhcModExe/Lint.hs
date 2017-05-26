module GhcModExe.Lint where

import Exception (ghandle)
import Control.Exception (SomeException(..))
import Language.Haskell.GhcMod.Logger (checkErrorPrefix)
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad
import Language.Haskell.HLint3

import Language.Haskell.GhcMod.Utils (withMappedFile)
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

-- | Options for "lintWith" function
data LintOpts = LintOpts {
        optLintHlintOpts :: [String]
        -- ^ options that will be passed to hlint executable
      } deriving (Show)

-- | Default "LintOpts" instance
defaultLintOpts :: LintOpts
defaultLintOpts = LintOpts []
