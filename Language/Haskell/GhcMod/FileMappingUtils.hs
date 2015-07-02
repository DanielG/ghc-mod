module Language.Haskell.GhcMod.FileMappingUtils where

import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad.Types

import System.IO
import System.FilePath
import System.Directory

withMappedFile :: (IOish m, GmState m, GmEnv m) => forall a. FilePath -> (FilePath -> m a) -> m a
withMappedFile file action = lookupMMappedFile file >>= runWithFile
  where
    runWithFile (Just (RedirectedMapping to)) = action to
    runWithFile (Just (MemoryMapping (Just src))) = do
      crdl <- cradle
      (fp,hndl) <- liftIO $ openTempFile (cradleTempDir crdl) (takeBaseName file)
      liftIO $ hPutStr hndl src
      liftIO $ hClose hndl
      result <- action fp
      liftIO $ removeFile fp
      return result
    runWithFile _ = action file
