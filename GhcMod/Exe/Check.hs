module GhcMod.Exe.Check (
    checkSyntax
  , check
  , expandTemplate
  , expand
  ) where

import Control.Applicative
import Prelude
import GhcMod.DynFlags
import qualified GhcMod.Gap as Gap
import GhcMod.Logger
import GhcMod.Monad

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
check files =
    runGmlTWith
      (map Left files)
      return
      ((fmap fst <$>) . withLogger Gap.setNoMaxRelevantBindings)
      (return ())

----------------------------------------------------------------

-- | Expanding Haskell Template.
expandTemplate :: IOish m
               => [FilePath]  -- ^ The target files.
               -> GhcModT m String
expandTemplate []    = return ""
expandTemplate files = either id id <$> expand files

----------------------------------------------------------------

-- | Expanding Haskell Template.
expand :: IOish m => [FilePath] -> GhcModT m (Either String String)
expand files =
    runGmlTWith
      (map Left files)
      return
      ((fmap fst <$>) . withLogger (Gap.setDumpSplices . setNoWarningFlags))
      (return ())
