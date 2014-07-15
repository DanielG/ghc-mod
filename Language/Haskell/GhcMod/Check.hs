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
checkSyntax :: IOish m
            => [FilePath]  -- ^ The target files.
            -> GhcModT m String
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
check :: IOish m
      => [FilePath]  -- ^ The target files.
      -> GhcModT m (Either String String)
check fileNames = do
  withLogger setAllWaringFlags $ setTargetFiles fileNames

----------------------------------------------------------------

-- | Expanding Haskell Template.
expandTemplate :: IOish m
               => [FilePath]  -- ^ The target files.
               -> GhcModT m String
expandTemplate [] = return ""
expandTemplate files = withErrorHandler sessionName $ do
    either id id <$> expand files
  where
    sessionName = case files of
      [file] -> file
      _      -> "MultipleFiles"

----------------------------------------------------------------

-- | Expanding Haskell Template.
expand :: IOish m
       => [FilePath]  -- ^ The target files.
       -> GhcModT m (Either String String)
expand fileNames = withLogger (Gap.setDumpSplices . setNoWaringFlags) $
    setTargetFiles fileNames
