module Language.Haskell.GhcMod.Check (
    checkSyntax
  , check
  , expandTemplate
  , expand
  ) where

import Exception (ghandle)
import GHC (Ghc)
import Language.Haskell.GhcMod.ErrMsg
import Language.Haskell.GhcMod.GHCApi
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Types

----------------------------------------------------------------

-- | Checking syntax of a target file using GHC.
--   Warnings and errors are returned.
checkSyntax :: Options
            -> Cradle
            -> [FilePath]  -- ^ The target files.
            -> IO String
checkSyntax _   _      []    = return ""
checkSyntax opt cradle files = withGHC sessionName $ do
    initializeFlagsWithCradle opt cradle
    check opt files
  where
    sessionName = case files of
      [file] -> file
      _      -> "MultipleFiles"

----------------------------------------------------------------

-- | Checking syntax of a target file using GHC.
--   Warnings and errors are returned.
check :: Options
      -> [FilePath]  -- ^ The target files.
      -> Ghc String
check opt fileNames = ghandle (handleErrMsg opt) $
    withLogger opt setAllWaringFlags $ setTargetFiles fileNames

----------------------------------------------------------------

-- | Expanding Haskell Template.
expandTemplate :: Options
               -> Cradle
               -> [FilePath]  -- ^ The target files.
               -> IO String
expandTemplate _   _      []    = return ""
expandTemplate opt cradle files = withGHC sessionName $ do
    initializeFlagsWithCradle opt cradle
    expand opt files
  where
    sessionName = case files of
      [file] -> file
      _      -> "MultipleFiles"

----------------------------------------------------------------

-- | Expanding Haskell Template.
expand :: Options
      -> [FilePath]  -- ^ The target files.
      -> Ghc String
expand opt fileNames = ghandle (handleErrMsg opt) $
    withDynFlags Gap.setDumpSplices $
    withLogger opt setNoWaringFlags $ setTargetFiles fileNames
