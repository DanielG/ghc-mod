module Language.Haskell.GhcMod.Lint where

import Exception (ghandle)
import Control.Exception (SomeException(..))
import Language.Haskell.GhcMod.Logger (checkErrorPrefix)
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad
import Language.Haskell.HLint3

import Language.Haskell.GhcMod.Utils (withMappedFile)
import Language.Haskell.Exts.Pretty (prettyPrint)

-- | Checking syntax of a target file using hlint.
--   Warnings and errors are returned.
lint :: IOish m
     => LintOpts  -- ^ Configuration parameters
     -> FilePath  -- ^ A target file.
     -> GhcModT m String
lint opt file = ghandle handler $
  withMappedFile file $ \tempfile -> do
    (flags, classify, hint) <- liftIO $ argsSettings $ optLintHlintOpts opt
    res <- liftIO $ parseModuleEx flags file =<< Just `fmap` readFile tempfile
    case res of
      Right m -> pack . map show $ applyHints classify hint [m]
      Left ParseError{parseErrorLocation=loc, parseErrorMessage=err} ->
        return $ prettyPrint loc ++ ":Error:" ++ err ++ "\n"
  where
    pack = convert' . map init -- init drops the last \n.
    handler (SomeException e) = return $ checkErrorPrefix ++ show e ++ "\n"
