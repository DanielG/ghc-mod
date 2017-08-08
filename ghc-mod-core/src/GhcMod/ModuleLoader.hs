{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
-- | Uses GHC hooks to load a TypecheckedModule

module GhcMod.ModuleLoader
  ( getTypecheckedModuleGhc
  , HasGhcModuleCache(..)
  , GhcModuleCache(..)
  , CachedModule(..)
  , FileUri(..)
  , UriCache(..)
  , Pos(..)
  , modifyCache
  , emptyModuleCache
  ) where

import           Control.Monad.State.Strict
import qualified Data.Map                          as Map
import           Data.Dynamic
import qualified Data.IntervalMap.FingerTree as IM
import qualified Data.Text                         as T
import           GHC                               (TypecheckedModule)
import qualified GhcMod.Monad                      as GM
import qualified GhcMod.Target                     as GM
import qualified GhcMod.Types                      as GM
import           System.Directory
import           System.FilePath

import qualified DynFlags                          as GHC
import qualified GHC
import qualified GhcMonad                          as GHC
import qualified Hooks                             as GHC
import qualified HscMain                           as GHC
import qualified HscTypes                          as GHC
import qualified TcRnMonad                         as GHC

import           Data.IORef

type HookIORefData = Maybe TypecheckedModule

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
getTypecheckedModuleGhc :: GM.IOish m
  => (GM.GmlT m () -> GM.GmlT m a) -> FilePath -> GM.GhcModT m (a, Maybe TypecheckedModule)
getTypecheckedModuleGhc wrapper targetFile = do
  cfileName <- liftIO $ canonicalizePath targetFile
  mfs <- GM.getMMappedFiles
  mFileName <- liftIO . canonicalizePath $ getMappedFileName cfileName mfs
  let ips = map takeDirectory $ Map.keys mfs
      setIncludePaths df = df { GHC.includePaths = ips ++ GHC.includePaths df }
  ref <- liftIO $ newIORef Nothing
  let
    setTarget fileName
      = GM.runGmlTWith' [Left fileName]
                        (return . setIncludePaths)
                        (Just $ updateHooks cfileName mFileName ref)
                        wrapper
                        (return ())
  res <- setTarget cfileName
  mtm <- liftIO $ readIORef ref
  return (res, mtm)

updateHooks :: FilePath -> FilePath -> IORef HookIORefData -> GHC.Hooks -> GHC.Hooks
updateHooks _ofp fp ref hooks = hooks {
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
hscFrontend :: FilePath -> IORef HookIORefData -> GHC.ModSummary -> GHC.Hsc GHC.TcGblEnv
hscFrontend fn ref mod_summary = do
    mfn <- canonicalizeModSummary mod_summary
    let
      -- md = GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod mod_summary
      keepInfo = case mfn of
                   Just fileName -> fn == fileName
                   Nothing       -> False
    -- liftIO $ debugm $ "hscFrontend: got mod,file" ++ show (md, mfn)
    if keepInfo
      then runGhcInHsc $ do
        let modSumWithRaw = tweakModSummaryDynFlags mod_summary

        p' <- GHC.parseModule modSumWithRaw
        let p = p' {GHC.pm_mod_summary = mod_summary}
        tc <- GHC.typecheckModule p
        let tc_gbl_env = fst $ GHC.tm_internals_ tc

        liftIO $ writeIORef ref $ Just tc
        return tc_gbl_env
      else do
        hpm <- GHC.hscParse' mod_summary
        hsc_env <- GHC.getHscEnv
        GHC.tcRnModule' hsc_env mod_summary False hpm

-- ---------------------------------------------------------------------

-- | for compatibility with haskell-lsp.
newtype FileUri = FileUri { getFileUri :: T.Text }
  deriving (Eq,Ord,Read,Show)

type UriCaches = Map.Map FileUri UriCache

data UriCache = UriCache
  { cachedModule :: !CachedModule
  , cachedData   :: !(Map.Map TypeRep Dynamic)
  } deriving Show

data Pos = Pos { line :: Int, col :: Int}
  deriving (Eq,Show,Read,Ord)

type LocMap = IM.IntervalMap Pos GHC.Name

data CachedModule = CachedModule
  { tcMod          :: !TypecheckedModule
  , locMap         :: !LocMap
  , revMap         :: !(FilePath -> FilePath)
  , newPosToOldPos :: !(Pos -> Maybe Pos)
  , oldPosToNewPos :: !(Pos -> Maybe Pos)
  }

instance Show CachedModule where
  show CachedModule{} = "CachedModule { .. }"

-- ---------------------------------------------------------------------

modifyCache :: (HasGhcModuleCache m) => (GhcModuleCache -> GhcModuleCache) -> m ()
modifyCache f = do
  mc <- getModuleCache
  setModuleCache (f mc)

-- ---------------------------------------------------------------------
-- The following to move into ghc-mod-core

class (Monad m) => HasGhcModuleCache m where
  getModuleCache :: m GhcModuleCache
  setModuleCache :: GhcModuleCache -> m ()

emptyModuleCache :: GhcModuleCache
emptyModuleCache = GhcModuleCache Map.empty Map.empty Map.empty

data GhcModuleCache = GhcModuleCache
  {
    extensibleState :: !(Map.Map TypeRep Dynamic)
              -- ^ stores custom state information.
  , cradleCache :: !(Map.Map FilePath GM.Cradle)
              -- ^ map from dirs to cradles
  , uriCaches  :: !UriCaches
  } deriving (Show)

-- ---------------------------------------------------------------------
