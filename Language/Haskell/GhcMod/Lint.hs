module Language.Haskell.GhcMod.Lint where

import Control.Applicative
import Control.Exception (finally)
import Data.List
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Language.Haskell.GhcMod.Types
import Language.Haskell.HLint
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile, stdout)

-- | Checking syntax of a target file using hlint.
--   Warnings and errors are returned.
lintSyntax :: Options
           -> FilePath  -- ^ A target file.
           -> IO String
lintSyntax opt file = pack <$> lint opt file
  where
    LineSeparator lsep = lineSeparator opt
    pack = unlines . map (intercalate lsep . lines)

lint :: Options
     -> FilePath    -- ^ A target file.
     -> IO [String]
lint opt file = map show <$> suppressStdout (hlint ([file] ++ hlintOpts opt))

suppressStdout :: IO a -> IO a
suppressStdout f = do
    tmpdir <- getTemporaryDirectory
    (path, handle) <- openTempFile tmpdir "ghc-mod-hlint"
    removeFile path
    dup <- hDuplicate stdout
    hDuplicateTo handle stdout
    hClose handle
    f `finally` hDuplicateTo dup stdout
