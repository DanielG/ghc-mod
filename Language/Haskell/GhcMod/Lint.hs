module Language.Haskell.GhcMod.Lint where

import Exception (ghandle)
import Control.Exception (SomeException(..))
import Language.Haskell.GhcMod.Logger (checkErrorPrefix)
import Language.Haskell.GhcMod.Convert
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.Types
import Language.Haskell.HLint (hlint)

-- | Checking syntax of a target file using hlint.
--   Warnings and errors are returned.
lint :: IOish m
     => FilePath  -- ^ A target file.
     -> GhcModT m String
lint file = do
  opt <- options
  ghandle handler . pack =<< liftIO (hlint $ file : "--quiet" : hlintOpts opt)
 where
    pack = convert' . map (init . show) -- init drops the last \n.
    handler (SomeException e) = return $ checkErrorPrefix ++ show e ++ "\n"
