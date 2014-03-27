module Language.Haskell.GhcMod.Lint where

import Control.Applicative ((<$>))
import Control.Exception (finally)
import Data.List (intercalate)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Language.Haskell.GhcMod.Types
import Language.Haskell.HLint (hlint)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openTempFile, stdout)

-- | Checking syntax of a target file using hlint.
--   Warnings and errors are returned.
lintSyntax :: Options
           -> FilePath  -- ^ A target file.
           -> IO String
lintSyntax opt file = pack <$> lint hopts file
  where
    LineSeparator lsep = lineSeparator opt
    pack = unlines . map (intercalate lsep . lines)
    hopts = hlintOpts opt

-- | Checking syntax of a target file using hlint.
--   Warnings and errors are returned.
lint :: [String]
     -> FilePath    -- ^ A target file.
     -> IO [String]
lint hopts file = map show <$> suppressStdout (hlint (file : hopts))

suppressStdout :: IO a -> IO a
suppressStdout f = do
    tmpdir <- getTemporaryDirectory
    (path, handle) <- openTempFile tmpdir "ghc-mod-hlint"
    removeFile path
    dup <- hDuplicate stdout
    hDuplicateTo handle stdout
    hClose handle
    f `finally` hDuplicateTo dup stdout
