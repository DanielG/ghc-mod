module Language.Haskell.GhcMod.Check (
    checkSyntax
  , check
  , expandTemplate
  , expand
  ) where

import Control.Applicative ((<$>))
import GHC (Ghc)
import Language.Haskell.GhcMod.GHCApi
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Logger
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
    either id id <$> check opt files
  where
    sessionName = case files of
      [file] -> file
      _      -> "MultipleFiles"

----------------------------------------------------------------

-- | Checking syntax of a target file using GHC.
--   Warnings and errors are returned.
check :: Options
      -> [FilePath]  -- ^ The target files.
      -> Ghc (Either String String)
check opt fileNames = withLogger opt setAllWaringFlags $
    setTargetFiles fileNames

----------------------------------------------------------------

-- | Expanding Haskell Template.
expandTemplate :: Options
               -> Cradle
               -> [FilePath]  -- ^ The target files.
               -> IO String
expandTemplate _   _      []    = return ""
expandTemplate opt cradle files = withGHC sessionName $ do
    initializeFlagsWithCradle opt cradle
    either id id <$> expand opt files
  where
    sessionName = case files of
      [file] -> file
      _      -> "MultipleFiles"

----------------------------------------------------------------

-- | Expanding Haskell Template.
expand :: Options
      -> [FilePath]  -- ^ The target files.
      -> Ghc (Either String String)
expand opt fileNames = withLogger opt (Gap.setDumpSplices . setNoWaringFlags) $
    setTargetFiles fileNames
