module Language.Haskell.GhcMod.Encoding where

import Language.Haskell.GhcMod.Types
import System.IO

applyEncoding :: MonadIO m => Options -> Handle -> m ()
applyEncoding Options { optEncoding } h = do
  menc <- traverse (liftIO . mkTextEncoding) optEncoding
  maybe (return ()) (liftIO . hSetEncoding h) menc
