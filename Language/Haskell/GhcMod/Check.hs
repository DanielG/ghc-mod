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
import Language.Haskell.GhcMod.Monad (IOish, GhcModT)
import Language.Haskell.GhcMod.Target (setTargetFiles)

----------------------------------------------------------------

-- | Checking syntax of a target file using GHC.
--   Warnings and errors are returned.
checkSyntax :: IOish m
            => [FilePath]  -- ^ The target files.
            -> GhcModT m String
checkSyntax []    = return ""
checkSyntax files = either id id <$> check files

----------------------------------------------------------------

-- | Checking syntax of a target file using GHC.
--   Warnings and errors are returned.
check :: IOish m
      => [FilePath]  -- ^ The target files.
      -> GhcModT m (Either String String)
{-
check fileNames = overrideGhcUserOptions $ \ghcOpts -> do
  withLogger (setAllWarningFlags . setNoMaxRelevantBindings . Gap.setWarnTypedHoles . Gap.setDeferTypeErrors) $ do
    _ <- G.setSessionDynFlags =<< addCmdOpts ghcOpts =<< G.getSessionDynFlags
    setTargetFiles fileNames
-}
check fileNames =
  withLogger (setAllWarningFlags . setNoMaxRelevantBindings) $
    setTargetFiles fileNames

----------------------------------------------------------------

-- | Expanding Haskell Template.
expandTemplate :: IOish m
               => [FilePath]  -- ^ The target files.
               -> GhcModT m String
expandTemplate []    = return ""
expandTemplate files = either id id <$> expand files

----------------------------------------------------------------

-- | Expanding Haskell Template.
expand :: IOish m
       => [FilePath]  -- ^ The target files.
       -> GhcModT m (Either String String)
expand fileNames = withLogger (Gap.setDumpSplices . setNoWarningFlags) $
    setTargetFiles fileNames
