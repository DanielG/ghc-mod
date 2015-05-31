module Language.Haskell.GhcMod.FileMapping
    ( loadMappedFile
    , loadMappedFiles
    , delMMappedFile
    , mapFile
    ) where

import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Gap
import Language.Haskell.GhcMod.HomeModuleGraph

import System.Directory
import System.FilePath

import Data.Time

import GHC

loadMappedFiles :: IOish m => GhcModT m ()
loadMappedFiles = do
  Options {fileMappings} <- options
  mapM_ (uncurry loadMappedFile) $ reverse fileMappings

loadMappedFile :: IOish m => FilePath -> FileMapping -> GhcModT m ()
loadMappedFile from fm@(RedirectedMapping _) =
  addToState from fm
loadMappedFile from (MemoryMapping _) = do
  let loop' acc = do
        line <- getLine
        if not (null line) && last line == '\EOT'
        then return $ acc ++ init line
        else loop' (acc++line++"\n")
  src <- liftIO $ loop' ""
  addToState from (MemoryMapping $ Just src)

addToState :: IOish m => FilePath -> FileMapping -> GhcModT m ()
addToState from fm = do
  crdl <- cradle
  let ccfn = cradleCurrentDir crdl </> from
  cfn <- liftIO $ canonicalizePath ccfn
  addMMappedFile cfn fm

mapFile :: (IOish m, GmState m, GhcMonad m) =>
            HscEnv -> Target -> m Target
mapFile _ (Target tid@(TargetFile filePath _) taoc _) = do
  mapping <- lookupMMappedFile filePath
  mkMappedTarget tid taoc mapping
mapFile env (Target tid@(TargetModule moduleName) taoc _) = do
  filePath <- liftIO $ findModulePath env moduleName
  mapping <- maybe (return Nothing) lookupMMappedFile $ fmap mpPath filePath
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
