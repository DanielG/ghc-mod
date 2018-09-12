{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Uses GHC hooks to load a TypecheckedModule

module GhcMod.ModuleLoader
  ( getModulesGhc
  , getModulesGhc'
  ) where

import           Control.Monad.IO.Class

import qualified Data.Map                          as Map
import           Data.IORef

import qualified GhcMod.Monad                      as GM
import qualified GhcMod.Target                     as GM
import qualified GhcMod.Types                      as GM

import           GHC                               (TypecheckedModule, ParsedModule)
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

-- | Gets a TypecheckedModule and/or ParsedModule from a given file
-- The `wrapper` allows arbitary data to be captured during
-- the compilation process, like errors and warnings
-- Appends the parent directories of all the mapped files
-- to the includePaths for CPP purposes.
-- Use in combination with `runActionInContext` for best results
getModulesGhc' :: GM.IOish m
  => (GM.GmlT m () -> GM.GmlT m a) -> FilePath -> GM.GhcModT m (a, Maybe TypecheckedModule, Maybe ParsedModule)
getModulesGhc' wrapper targetFile = do
  cfileName <- liftIO $ canonicalizePath targetFile
  mfs <- GM.getMMappedFiles
  mFileName <- liftIO . canonicalizePath $ getMappedFileName cfileName mfs
  refTypechecked <- liftIO $ newIORef Nothing
  refParsed <- liftIO $ newIORef Nothing
  let keepInfo = pure . (mFileName ==)
      saveTypechecked = writeIORef refTypechecked . Just
      saveParsed = writeIORef refParsed . Just
  res <- getModulesGhc wrapper [cfileName] keepInfo saveTypechecked saveParsed
  mtm <- liftIO $ readIORef refTypechecked
  mpm <- liftIO $ readIORef refParsed
  return (res, mtm, mpm)

-- | like getModulesGhc' but allows you to keep an arbitary number of Modules
-- `keepInfo` decides which module to keep
-- `saveTypechecked` is the callback that is passed the TypecheckedModule
-- `saveParsed` is the callback that is passed the ParsedModule
getModulesGhc :: GM.IOish m
  => (GM.GmlT m () -> GM.GmlT m a) -> [FilePath] -> (FilePath -> IO Bool) -> (TypecheckedModule -> IO ()) -> (ParsedModule -> IO ()) -> GM.GhcModT m a
getModulesGhc wrapper targetFiles keepInfo saveTypechecked saveParsed = do
  mfs <- GM.getMMappedFiles

#if __GLASGOW_HASKELL__ >= 806
  let ips = map takeDirectory $ Map.keys mfs
      getPaths ips' df = GHC.IncludeSpecs qpaths (ips' ++ gpaths)
        where
          -- Note, introduced for include path issues on windows, see
          -- https://ghc.haskell.org/trac/ghc/ticket/14312
          GHC.IncludeSpecs qpaths gpaths = GHC.includePaths df
      setIncludePaths df = df { GHC.includePaths = getPaths ips df }
#else
  let ips = map takeDirectory $ Map.keys mfs
      setIncludePaths df = df { GHC.includePaths = ips ++ GHC.includePaths df }
#endif
  GM.runGmlTWith' (map Left targetFiles)
                  (return . setIncludePaths)
                  (Just $ updateHooks keepInfo saveTypechecked saveParsed)
                  wrapper
                  (return ())

updateHooks
  :: (FilePath -> IO Bool)
  -> (TypecheckedModule -> IO ())
  -> (ParsedModule -> IO ())
  -> GHC.Hooks
  -> GHC.Hooks
updateHooks fp ref refParsed hooks = hooks {
#if __GLASGOW_HASKELL__ <= 710
        GHC.hscFrontendHook   = Just $ hscFrontend fp ref
#else
        GHC.hscFrontendHook   = Just $ fmap GHC.FrontendTypecheck . hscFrontend fp ref refParsed
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
hscFrontend :: (FilePath -> IO Bool) -> (TypecheckedModule -> IO ()) -> (ParsedModule -> IO ()) -> GHC.ModSummary -> GHC.Hsc GHC.TcGblEnv
hscFrontend keepInfoFunc saveTypechecked saveParsed mod_summary = do
    mfn <- canonicalizeModSummary mod_summary
      -- md = GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod mod_summary
    keepInfo <- case mfn of
      Just fileName -> liftIO $ keepInfoFunc fileName
      Nothing       -> pure False

    if keepInfo
      then runGhcInHsc $ do
        let modSumWithRaw = tweakModSummaryDynFlags mod_summary

        p' <- GHC.parseModule modSumWithRaw
        let p = p' {GHC.pm_mod_summary = mod_summary}
        liftIO $ saveParsed p

        tc <- GHC.typecheckModule p
        let tc_gbl_env = fst $ GHC.tm_internals_ tc

        liftIO $ saveTypechecked tc
        return tc_gbl_env
      else do
        hpm <- GHC.hscParse' mod_summary
        hsc_env <- GHC.getHscEnv
#if __GLASGOW_HASKELL__ >= 804
  --    tcRnModule' :: ModSummary -> Bool -> HsParsedModule -> Hsc TcGblEnv
        GHC.tcRnModule' mod_summary False hpm
#else
        GHC.tcRnModule' hsc_env mod_summary False hpm
#endif

-- ---------------------------------------------------------------------

