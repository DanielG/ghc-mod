module Language.Haskell.GhcMod.Check (
    checkSyntax
  , check
  , expandTemplate
  , expand
  ) where

import Control.Applicative ((<$>))
import Language.Haskell.GhcMod.DynFlags
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Logger
import Language.Haskell.GhcMod.Monad

----------------------------------------------------------------

-- | Checking syntax of a target file using GHC.
--   Warnings and errors are returned.
checkSyntax :: [FilePath]  -- ^ The target files.
            -> GhcMod String
checkSyntax [] = return ""
checkSyntax files = withErrorHandler sessionName $ do
    either id id <$> check files
  where
    sessionName = case files of
      [file] -> file
      _      -> "MultipleFiles"

----------------------------------------------------------------

-- | Checking syntax of a target file using GHC.
--   Warnings and errors are returned.
check :: [FilePath]  -- ^ The target files.
      -> GhcMod (Either String String)
check fileNames = do
  withLogger setAllWaringFlags $ do
    setTargetFiles fileNames

----------------------------------------------------------------

-- | Expanding Haskell Template.
expandTemplate :: [FilePath]  -- ^ The target files.
               -> GhcMod String
expandTemplate [] = return ""
expandTemplate files = withErrorHandler sessionName $ do
    either id id <$> expand files
  where
    sessionName = case files of
      [file] -> file
      _      -> "MultipleFiles"

----------------------------------------------------------------

-- | Expanding Haskell Template.
expand :: [FilePath]  -- ^ The target files.
      -> GhcMod (Either String String)
expand fileNames = withLogger (Gap.setDumpSplices . setNoWaringFlags) $
    setTargetFiles fileNames
