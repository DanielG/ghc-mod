module Language.Haskell.GhcMod.Lint where

import Exception (ghandle)
import Control.Exception (SomeException(..))
import Language.Haskell.GhcMod.Logger (checkErrorPrefix)
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad
import Language.Haskell.HLint3

import Language.Haskell.GhcMod.Utils (withMappedFile)
import Language.Haskell.Exts.SrcLoc (SrcLoc(..))
import System.IO

-- | Checking syntax of a target file using hlint.
--   Warnings and errors are returned.
lint :: IOish m
     => LintOpts  -- ^ Configuration parameters
     -> FilePath  -- ^ A target file.
     -> GhcModT m String
lint opt file = ghandle handler $
  withMappedFile file $ \tempfile -> do
    (flags, classify, hint) <- liftIO $ argsSettings $ optLintHlintOpts opt
    hSrc <- liftIO $ openFile tempfile ReadMode
    -- liftIO $ hSetEncoding hSrc (encoding flags)
    res <- liftIO $ parseModuleEx flags file =<< Just `fmap` hGetContents hSrc
    case res of
      Right m -> pack . map show $ filter ((/=Ignore) . ideaSeverity) $ applyHints classify hint [m]
      Left ParseError{parseErrorLocation=loc, parseErrorMessage=err} ->
        return $ showSrcLoc loc ++ ":Error:" ++ err ++ "\n"
  where
    pack = convert' . map init -- init drops the last \n.
    handler (SomeException e) = return $ checkErrorPrefix ++ show e ++ "\n"
    showSrcLoc (SrcLoc f l c) = concat [f, ":", show l, ":", show c]
