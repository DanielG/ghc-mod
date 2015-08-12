module Language.Haskell.GhcMod.FileMapping
    ( loadMappedFile
    , unloadMappedFile
    , mapFile
    , fileModSummaryWithMapping
    ) where

import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Gap
import Language.Haskell.GhcMod.HomeModuleGraph
import Language.Haskell.GhcMod.Utils

import Data.Time

import Control.Monad.Trans.Maybe
import GHC

loadMappedFile :: IOish m => FilePath -> FileMapping -> GhcModT m ()
loadMappedFile from fm =
  getCanonicalFileNameSafe from >>= (`addMMappedFile` fm)

mapFile :: (IOish m, GmState m, GhcMonad m) =>
            HscEnv -> Target -> m Target
mapFile _ (Target tid@(TargetFile filePath _) taoc _) = do
  mapping <- lookupMMappedFile filePath
  mkMappedTarget tid taoc mapping
mapFile env (Target tid@(TargetModule moduleName) taoc _) = do
  mapping <- runMaybeT $ do
    filePath <- MaybeT $ liftIO $ findModulePath env moduleName
    MaybeT $ lookupMMappedFile $ mpPath filePath
  mkMappedTarget tid taoc mapping

mkMappedTarget :: (IOish m, GmState m, GhcMonad m) =>
                  TargetId -> Bool -> Maybe FileMapping -> m Target
mkMappedTarget _ taoc (Just (RedirectedMapping to)) =
  return $ mkTarget (TargetFile to Nothing) taoc Nothing
mkMappedTarget tid taoc (Just (MemoryMapping (Just src))) = do
  sb <- toStringBuffer [src]
  ct <- liftIO getCurrentTime
  return $ mkTarget tid taoc $ Just (sb, ct)
mkMappedTarget tid taoc _ = return $ mkTarget tid taoc Nothing

unloadMappedFile :: IOish m => FilePath -> GhcModT m ()
unloadMappedFile = (delMMappedFile =<<) . getCanonicalFileNameSafe

fileModSummaryWithMapping :: (IOish m, GmState m, GhcMonad m, GmEnv m) =>
                            FilePath -> m ModSummary
fileModSummaryWithMapping fn = do
  mmf <- getCanonicalFileNameSafe fn >>= lookupMMappedFile
  case mmf of
    Just (RedirectedMapping to) -> fileModSummary to
    _                           -> fileModSummary fn
