module Language.Haskell.GhcMod.FileMapping
    ( loadMappedFile
    , loadMappedFileSource
    , unloadMappedFile
    , mapFile
    , fileModSummaryWithMapping
    ) where

import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Gap
import Language.Haskell.GhcMod.HomeModuleGraph
import Language.Haskell.GhcMod.Utils

import System.IO
import System.FilePath
import System.Directory

import Control.Monad.Trans.Maybe
import GHC
import Control.Monad

loadMappedFile :: IOish m => FilePath -> FilePath -> GhcModT m ()
loadMappedFile from to = loadMappedFile' from to False

loadMappedFileSource :: IOish m => FilePath -> String -> GhcModT m ()
loadMappedFileSource from src = do
  tmpdir <- cradleTempDir `fmap` cradle
  to <- liftIO $ do
    (fn, h) <- openTempFile tmpdir (takeFileName from)
    hPutStr h src
    hClose h
    return fn
  loadMappedFile' from to True

loadMappedFile' :: IOish m => FilePath -> FilePath -> Bool -> GhcModT m ()
loadMappedFile' from to isTemp = do
  cfn <- getCanonicalFileNameSafe from
  unloadMappedFile' cfn
  addMMappedFile cfn (FileMapping to isTemp)

mapFile :: (IOish m, GmState m, GhcMonad m, GmEnv m) =>
            HscEnv -> Target -> m Target
mapFile _ (Target tid@(TargetFile filePath _) taoc _) = do
  mapping <- lookupMMappedFile filePath
  mkMappedTarget (Just filePath) tid taoc mapping
mapFile env (Target tid@(TargetModule moduleName) taoc _) = do
  (fp, mapping) <- do
    filePath <- fmap (fmap mpPath) (liftIO $ findModulePath env moduleName)
    mmf <- runMaybeT $ MaybeT (return filePath) >>= MaybeT . lookupMMappedFile
    return (filePath, mmf)
  mkMappedTarget fp tid taoc mapping

mkMappedTarget :: (IOish m, GmState m, GmEnv m, GhcMonad m) =>
                  Maybe FilePath -> TargetId -> Bool -> Maybe FileMapping -> m Target
mkMappedTarget _ _ taoc (Just to) =
  return $ mkTarget (TargetFile (fmPath to) Nothing) taoc Nothing
mkMappedTarget _ tid taoc _ = return $ mkTarget tid taoc Nothing

unloadMappedFile :: IOish m => FilePath -> GhcModT m ()
unloadMappedFile = getCanonicalFileNameSafe >=> unloadMappedFile'

unloadMappedFile' :: IOish m => FilePath -> GhcModT m ()
unloadMappedFile' cfn = void $ runMaybeT $ do
  fm <- MaybeT $ lookupMMappedFile cfn
  liftIO $ when (fmTemp fm) $ removeFile (fmPath fm)
  delMMappedFile cfn

fileModSummaryWithMapping :: (IOish m, GmState m, GhcMonad m, GmEnv m) =>
                            FilePath -> m ModSummary
fileModSummaryWithMapping fn =
  withMappedFile fn $ \fn' -> fileModSummary fn'
