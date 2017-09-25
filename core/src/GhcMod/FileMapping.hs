module GhcMod.FileMapping
    ( loadMappedFile
    , loadMappedFileSource
    , unloadMappedFile
    , mapFile
    , fileModSummaryWithMapping
    ) where

import GhcMod.Types
import GhcMod.Monad.Types
import GhcMod.Gap
import GhcMod.HomeModuleGraph
import GhcMod.Utils

import System.IO
import System.FilePath
import System.Directory

import Control.Monad.Trans.Maybe
import GHC
import Control.Monad

{- | maps 'FilePath', given as first argument to take source from
'FilePath' given as second argument. Works exactly the same as
first form of `--map-file` CLI option.

\'from\' can be either full path, or path relative to project root.
\'to\' has to be either relative to project root, or full path (preferred)
-}
loadMappedFile :: IOish m
               => FilePath -- ^ \'from\', file that will be mapped
               -> FilePath -- ^ \'to\', file to take source from
               -> GhcModT m ()
loadMappedFile from to = loadMappedFile' from to False

{- |
maps 'FilePath', given as first argument to have source as given
by second argument.

\'from\' may or may not exist, and should be either full path,
or relative to project root.
-}
loadMappedFileSource :: IOish m
                     => FilePath -- ^ \'from\', file that will be mapped
                     -> String -- ^ \'src\', source
                     -> GhcModT m ()
loadMappedFileSource from src = do
  tmpdir <- cradleTempDir `fmap` cradle
  enc <- liftIO . mkTextEncoding . optEncoding =<< options
  to <- liftIO $ do
    (fn, h) <- openTempFile tmpdir (takeFileName from)
    hSetEncoding h enc
    hPutStr h src
    hClose h
    return fn
  loadMappedFile' from to True

loadMappedFile' :: IOish m => FilePath -> FilePath -> Bool -> GhcModT m ()
loadMappedFile' from to isTemp = do
  cfn <- getCanonicalFileNameSafe from
  unloadMappedFile' cfn
  crdl <- cradle
  let to' = makeRelative (cradleRootDir crdl) to
  addMMappedFile cfn (FileMapping to' isTemp)

mapFile :: (IOish m, GmState m) => HscEnv -> Target -> m Target
mapFile _ (Target tid@(TargetFile filePath _) taoc _) = do
  mapping <- lookupMMappedFile filePath
  return $ mkMappedTarget (Just filePath) tid taoc mapping
mapFile env (Target tid@(TargetModule moduleName) taoc _) = do
  (fp, mapping) <- do
    filePath <- fmap (fmap mpPath) (liftIO $ findModulePath env moduleName)
    mmf <- runMaybeT $ MaybeT (return filePath) >>= MaybeT . lookupMMappedFile
    return (filePath, mmf)
  return $ mkMappedTarget fp tid taoc mapping

mkMappedTarget :: Maybe FilePath -> TargetId -> Bool -> Maybe FileMapping -> Target
mkMappedTarget _ _ taoc (Just to) =
  mkTarget (TargetFile (fmPath to) Nothing) taoc Nothing
mkMappedTarget _ tid taoc _ =
  mkTarget tid taoc Nothing

{-|
unloads previously mapped file \'file\', so that it's no longer mapped,
and removes any temporary files created when file was
mapped.

\'file\' should be either full path, or relative to project root.
-}
unloadMappedFile :: IOish m
                 => FilePath -- ^ \'file\', file to unmap
                 -> GhcModT m ()
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
