{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Uses GHC hooks to load a TypecheckedModule

module GhcMod.ModuleLoader
  ( getTypecheckedModuleGhc
  , getTypecheckedModuleGhc'
  ) where

import           Control.Monad.IO.Class

import qualified Data.Map                          as Map
import           Data.IORef

import qualified GhcMod.Monad                      as GM
import qualified GhcMod.Target                     as GM
import qualified GhcMod.Types                      as GM

import           GHC                               (TypecheckedModule)
import qualified GHC
import qualified DynFlags                          as GHC
import qualified GhcMonad                          as GHC
import qualified Hooks                             as GHC
import qualified HscMain                           as GHC
import qualified HscTypes                          as GHC
import qualified TcRnMonad                         as GHC

import           System.Directory
import           System.FilePath

-- ---------------------------------------------------------------------

getMappedFileName :: FilePath -> GM.FileMappingMap -> FilePath
getMappedFileName fname mfs =
  case Map.lookup fname mfs of
    Just fm -> GM.fmPath fm
    Nothing -> fname

canonicalizeModSummary :: (MonadIO m) =>
  GHC.ModSummary -> m (Maybe FilePath)
canonicalizeModSummary =
  traverse (liftIO . canonicalizePath) . GHC.ml_hs_file . GHC.ms_location

tweakModSummaryDynFlags :: GHC.ModSummary -> GHC.ModSummary
tweakModSummaryDynFlags ms =
  let df = GHC.ms_hspp_opts ms
  in ms { GHC.ms_hspp_opts = GHC.gopt_set df GHC.Opt_KeepRawTokenStream }

-- | Gets a TypecheckedModule from a given file
-- The `wrapper` allows arbitary data to be captured during
-- the compilation process, like errors and warnings
-- Appends the parent directories of all the mapped files
-- to the includePaths for CPP purposes.
-- Use in combination with `runActionInContext` for best results
getTypecheckedModuleGhc' :: GM.IOish m
  => (GM.GmlT m () -> GM.GmlT m a) -> FilePath -> GM.GhcModT m (a, Maybe TypecheckedModule)
getTypecheckedModuleGhc' wrapper targetFile = do
  cfileName <- liftIO $ canonicalizePath targetFile
  mfs <- GM.getMMappedFiles
  mFileName <- liftIO . canonicalizePath $ getMappedFileName cfileName mfs
  ref <- liftIO $ newIORef Nothing
  let keepInfo = pure . (mFileName ==)
      saveModule = writeIORef ref . Just
  res <- getTypecheckedModuleGhc wrapper [cfileName] keepInfo saveModule
  mtm <- liftIO $ readIORef ref
  return (res, mtm)

-- | like getTypecheckedModuleGhc' but allows you to keep an arbitary number of Modules
-- `keepInfo` decides which TypecheckedModule to keep
-- `saveModule` is the callback that is passed the TypecheckedModule
getTypecheckedModuleGhc :: GM.IOish m
  => (GM.GmlT m () -> GM.GmlT m a) -> [FilePath] -> (FilePath -> IO Bool) -> (TypecheckedModule -> IO ()) -> GM.GhcModT m a
getTypecheckedModuleGhc wrapper targetFiles keepInfo saveModule = do
  mfs <- GM.getMMappedFiles
  let ips = map takeDirectory $ Map.keys mfs
      setIncludePaths df = df { GHC.includePaths = ips ++ GHC.includePaths df }
  GM.runGmlTWith' (map Left targetFiles)
                  (return . setIncludePaths)
                  (Just $ updateHooks keepInfo saveModule)
                  wrapper
                  (return ())

updateHooks
  :: (FilePath -> IO Bool)
  -> (TypecheckedModule -> IO ())
  -> GHC.Hooks
  -> GHC.Hooks
updateHooks fp ref hooks = hooks {
#if __GLASGOW_HASKELL__ <= 710
        GHC.hscFrontendHook   = Just $ hscFrontend fp ref
#else
        GHC.hscFrontendHook   = Just $ fmap GHC.FrontendTypecheck . hscFrontend fp ref
#endif
      }


-- | Warning: discards all changes to Session
runGhcInHsc :: GHC.Ghc a -> GHC.Hsc a
runGhcInHsc action = do
  env <- GHC.getHscEnv
  session <- liftIO $ newIORef env
  liftIO $ GHC.reflectGhc action $ GHC.Session session


-- | Frontend hook that keeps the TypecheckedModule for its first argument
-- and stores it in the IORef passed to it
hscFrontend :: (FilePath -> IO Bool) -> (TypecheckedModule -> IO ()) -> GHC.ModSummary -> GHC.Hsc GHC.TcGblEnv
hscFrontend keepInfoFunc saveModule mod_summary = do
    mfn <- canonicalizeModSummary mod_summary
      -- md = GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod mod_summary
    keepInfo <- case mfn of
      Just fileName -> liftIO $ keepInfoFunc fileName
      Nothing       -> pure False
    -- liftIO $ debugm $ "hscFrontend: got mod,file" ++ show (md, mfn)
    if keepInfo
      then runGhcInHsc $ do
        let modSumWithRaw = tweakModSummaryDynFlags mod_summary

        p' <- GHC.parseModule modSumWithRaw
        let p = p' {GHC.pm_mod_summary = mod_summary}
        tc <- GHC.typecheckModule p
        let tc_gbl_env = fst $ GHC.tm_internals_ tc

        liftIO $ saveModule tc
        return tc_gbl_env
      else do
        hpm <- GHC.hscParse' mod_summary
        hsc_env <- GHC.getHscEnv
        GHC.tcRnModule' hsc_env mod_summary False hpm

-- ---------------------------------------------------------------------

