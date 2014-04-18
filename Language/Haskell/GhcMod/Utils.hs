{-# LANGUAGE CPP #-}

module Language.Haskell.GhcMod.Utils (
    suppressStdout
  , suppressStderr
  ) where

import Control.Exception (finally)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import GHC.IO.Handle.Types (Handle)
import System.IO (hClose, stdout, stderr)
#ifdef WINDOWS
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (openTempFile)
#else
import System.IO (openFile, IOMode(..))
#endif

suppressStdout :: IO a -> IO a
suppressStdout = suppress stdout

suppressStderr :: IO a -> IO a
suppressStderr = suppress stderr

suppress :: GHC.IO.Handle.Types.Handle -> IO a -> IO a
suppress std f = do
#ifdef WINDOWS
    tmpdir <- getTemporaryDirectory
    (path, handle) <- openTempFile tmpdir "ghc-mod"
#else
    handle <- openFile "/dev/null" WriteMode
#endif
    dup <- hDuplicate std
    hDuplicateTo handle std
    hClose handle
    f `finally` do
        hDuplicateTo dup std
#ifdef WINDOWS
        removeFile path
#endif

