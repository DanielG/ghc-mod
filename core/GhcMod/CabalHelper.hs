-- ghc-mod: Happy Haskell Hacking
-- Copyright (C) 2015  Daniel Gröber <dxld ÄT darkboxed DOT org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE CPP #-}
module GhcMod.CabalHelper
  ( getComponents
  , getGhcMergedPkgOptions
  , getCabalPackageDbStack
  , prepareCabalHelper
  , withAutogen
  , withCabal
  ) where

import Control.Applicative
import Control.Monad
import Control.Category ((.))
import Data.Maybe
import Data.Monoid
import Data.Version
import Data.Binary (Binary)
import Data.Traversable
import Distribution.Helper hiding (Programs(..))
import qualified Distribution.Helper as CH
import qualified GhcMod.Types as T
import GhcMod.Types
import GhcMod.Monad.Types
import GhcMod.Utils
import GhcMod.PathsAndFiles
import GhcMod.Logging
import GhcMod.Output
import GhcMod.CustomPackageDb
import GhcMod.Stack
import System.FilePath
import System.Process
import System.Exit
import Prelude hiding ((.))

import Paths_ghc_mod as GhcMod

-- | Only package related GHC options, sufficient for things that don't need to
-- access home modules
getGhcMergedPkgOptions :: (Applicative m, IOish m, Gm m)
  => m [GHCOption]
getGhcMergedPkgOptions = chCached $ \distdir -> Cached {
  cacheLens = Just (lGmcMergedPkgOptions . lGmCaches),
  cacheFile = mergedPkgOptsCacheFile distdir,
  cachedAction = \_tcf (_progs, _projdir, _ver) _ma -> do
    opts <- runCHQuery ghcMergedPkgOptions
    return ([setupConfigPath distdir], opts)
 }

getCabalPackageDbStack :: (IOish m, Gm m) => m [GhcPkgDb]
getCabalPackageDbStack = chCached $ \distdir -> Cached {
  cacheLens = Just (lGmcPackageDbStack . lGmCaches),
  cacheFile = pkgDbStackCacheFile distdir,
  cachedAction = \_tcf (_progs, _projdir, _ver) _ma -> do
    crdl <- cradle
    dbs <- map chPkgToGhcPkg <$>
             runCHQuery packageDbStack
    return ([setupConfigFile crdl, sandboxConfigFile crdl], dbs)
 }

chPkgToGhcPkg :: ChPkgDb -> GhcPkgDb
chPkgToGhcPkg ChPkgGlobal = GlobalDb
chPkgToGhcPkg ChPkgUser = UserDb
chPkgToGhcPkg (ChPkgSpecific f) = PackageDb f

-- | Primary interface to cabal-helper and intended single entrypoint to
-- constructing 'GmComponent's
--
-- The Component\'s 'gmcHomeModuleGraph' will be empty and has to be resolved by
-- 'resolveGmComponents'.
getComponents :: (Applicative m, IOish m, Gm m)
              => m [GmComponent 'GMCRaw ChEntrypoint]
getComponents = chCached $ \distdir -> Cached {
    cacheLens = Just (lGmcComponents . lGmCaches),
    cacheFile = cabalHelperCacheFile distdir,
    cachedAction = \ _tcf (_progs, _projdir, _ver) _ma -> do
      runCHQuery $ do
        q <- join7
               <$> ghcOptions
               <*> ghcPkgOptions
               <*> ghcSrcOptions
               <*> ghcLangOptions
               <*> entrypoints
               <*> entrypoints
               <*> sourceDirs
        let cs = flip map q $ curry8 (GmComponent mempty)
        return ([setupConfigPath distdir], cs)
  }
 where
   curry8 fn (a, (b, (c, (d, (e, (f, (g, h))))))) = fn a b c d e f g h

   join7 a b c d e f = join' a . join' b . join' c . join' d . join' e . join' f
   join' :: Eq a => [(a,b)] -> [(a,c)] -> [(a,(b,c))]
   join' lb lc = [ (a, (b, c))
                 | (a, b)  <- lb
                 , (a', c) <- lc
                 , a == a'
                 ]

getQueryEnv :: (IOish m, GmOut m, GmEnv m) => m QueryEnv
getQueryEnv = do
  crdl <- cradle
  progs <- patchStackPrograms crdl =<< (optPrograms <$> options)
  readProc <- gmReadProcess
  let projdir = cradleRootDir crdl
      distdir = projdir </> cradleDistDir crdl
  return (defaultQueryEnv projdir distdir) {
                  qeReadProcess = readProc
                , qePrograms = helperProgs progs
                }

runCHQuery :: (IOish m, GmOut m, GmEnv m) => Query m b -> m b
runCHQuery a = do
  qe <- getQueryEnv
  runQuery qe a


prepareCabalHelper :: (IOish m, GmEnv m, GmOut m, GmLog m) => m ()
prepareCabalHelper = do
  crdl <- cradle
  when (isCabalHelperProject $ cradleProject crdl) $
       withCabal $ prepare' =<< getQueryEnv

withAutogen :: (IOish m, GmEnv m, GmOut m, GmLog m) => m a -> m a
withAutogen action = do
    gmLog GmDebug "" $ strDoc $ "making sure autogen files exist"
    crdl <- cradle
    let projdir = cradleRootDir crdl
        distdir = projdir </> cradleDistDir crdl

    (pkgName', _) <- runCHQuery packageId

    mCabalFile          <- liftIO $ timeFile `traverse` cradleCabalFile crdl
    mCabalMacroHeader   <- liftIO $ timeMaybe (distdir </> macrosHeaderPath)
    mCabalPathsModule   <- liftIO $ timeMaybe (distdir </> autogenModulePath pkgName')

    when (mCabalMacroHeader < mCabalFile || mCabalPathsModule < mCabalFile) $ do
      gmLog GmDebug "" $ strDoc $ "autogen files out of sync"
      writeAutogen

    action

 where
   writeAutogen = do
     gmLog GmDebug "" $ strDoc $ "writing Cabal autogen files"
     writeAutogenFiles' =<< getQueryEnv


withCabal :: (IOish m, GmEnv m, GmOut m, GmLog m) => m a -> m a
withCabal action = do
    crdl <- cradle
    mCabalFile          <- liftIO $ timeFile `traverse` cradleCabalFile crdl
    mCabalConfig        <- liftIO $ timeMaybe (setupConfigFile crdl)
    mCabalSandboxConfig <- liftIO $ timeMaybe (sandboxConfigFile crdl)

    let haveSetupConfig = isJust mCabalConfig

    cusPkgDb <- getCustomPkgDbStack
    (flgs, pkgDbStackOutOfSync) <- do
      if haveSetupConfig
        then runCHQuery $ do
          flgs <- nonDefaultConfigFlags
          pkgDb <- map chPkgToGhcPkg <$> packageDbStack
          return (flgs, fromMaybe False $ (pkgDb /=) <$> cusPkgDb)
        else return ([], False)

    when (isSetupConfigOutOfDate mCabalFile mCabalConfig) $
      gmLog GmDebug "" $ strDoc $ "setup configuration is out of date"

    when (isSetupConfigOutOfDate mCabalSandboxConfig mCabalConfig) $
      gmLog GmDebug "" $ strDoc $ "sandbox configuration is out of date"

    when pkgDbStackOutOfSync $
      gmLog GmDebug "" $ strDoc $ "package-db stack out of sync with ghc-mod.package-db-stack"

    when ( isSetupConfigOutOfDate mCabalFile mCabalConfig
        || pkgDbStackOutOfSync
        || isSetupConfigOutOfDate mCabalSandboxConfig mCabalConfig) $ do
          proj <- cradleProject <$> cradle
          opts <- options
          case proj of
            CabalProject -> do
                gmLog GmDebug "" $ strDoc "reconfiguring Cabal project"
                cabalReconfigure (optPrograms opts) crdl flgs
            StackProject {} -> do
                gmLog GmDebug "" $ strDoc "reconfiguring Stack project"
                -- TODO: we could support flags for stack too, but it seems
                -- you're supposed to put those in stack.yaml so detecting which
                -- flags to pass down would be more difficult

                -- "--flag PACKAGE:[-]FLAG Override flags set in stack.yaml
                -- (applies to local packages and extra-deps)"

                stackReconfigure (optStackBuildDeps opts) crdl (optPrograms opts)
            _ ->
                error $ "withCabal: unsupported project type: " ++ show proj

    action

 where
   cabalReconfigure progs crdl flgs = do
     readProc <- gmReadProcess
     withDirectory_ (cradleRootDir crdl) $ do
        cusPkgStack <- maybe [] ((PackageDb "clear"):) <$> getCustomPkgDbStack
        let progOpts =
                [ "--with-ghc=" ++ T.ghcProgram progs ]
                -- Only pass ghc-pkg if it was actually set otherwise we
                -- might break cabal's guessing logic
                ++ if T.ghcPkgProgram progs /= T.ghcPkgProgram (optPrograms defaultOptions)
                     then [ "--with-ghc-pkg=" ++ T.ghcPkgProgram progs ]
                     else []
                ++ map pkgDbArg cusPkgStack
                ++ flagOpt

            toFlag (f, True) = f
            toFlag (f, False) = '-':f
            flagOpt = ["--flags", unwords $ map toFlag flgs]

        liftIO $ void $ readProc (T.cabalProgram progs) ("configure":progOpts) ""
   stackReconfigure deps crdl progs = do
     withDirectory_ (cradleRootDir crdl) $ do
       supported <- haveStackSupport
       if supported
          then do
            when deps $
              spawn [T.stackProgram progs, "build", "--only-dependencies", "."]
            spawn [T.stackProgram progs, "build", "--only-configure", "."]
          else
            gmLog GmWarning "" $ strDoc $ "Stack project configuration is out of date, please reconfigure manually using 'stack build' as your stack version is too old (need at least 0.1.4.0)"

   spawn [] = return ()
   spawn (exe:args) = do
     readProc <- gmReadProcess
     liftIO $ void $ readProc exe args ""

   haveStackSupport = do
     (rv, _, _) <-
         liftIO $ readProcessWithExitCode "stack" ["--numeric-version"] ""
     case rv of
       ExitSuccess -> return True
       ExitFailure _ -> return False



pkgDbArg :: GhcPkgDb -> String
pkgDbArg GlobalDb      = "--package-db=global"
pkgDbArg UserDb        = "--package-db=user"
pkgDbArg (PackageDb p) = "--package-db=" ++ p

-- * Neither file exists -> should return False:
--   @Nothing < Nothing = False@
--   (since we don't need to @cabal configure@ when no cabal file exists.)
--
-- * Cabal file doesn't exist (impossible since cabal-helper is only used with
-- cabal projects) -> should return False
--   @Just cc < Nothing = False@
--
-- * dist/setup-config doesn't exist yet -> should return True:
--   @Nothing < Just cf = True@
--
-- * Both files exist
--   @Just cc < Just cf = cc < cf = cc `olderThan` cf@
isSetupConfigOutOfDate :: Maybe TimedFile -> Maybe TimedFile -> Bool
isSetupConfigOutOfDate worldCabalFile worldCabalConfig = do
  worldCabalConfig < worldCabalFile

helperProgs :: Programs -> CH.Programs
helperProgs progs = CH.Programs {
    cabalProgram  = T.cabalProgram progs,
    ghcProgram    = T.ghcProgram progs,
    ghcPkgProgram = T.ghcPkgProgram progs
  }

chCached :: (Applicative m, IOish m, Gm m, Binary a)
  => (FilePath -> Cached m GhcModState ChCacheData a) -> m a
chCached c = do
  projdir <- cradleRootDir <$> cradle
  distdir <- (projdir </>) . cradleDistDir <$> cradle
  d <- cacheInputData projdir
  withCabal $ cached projdir (c distdir) d
 where
   -- we don't need to include the distdir in the cache input because when it
   -- changes the cache files will be gone anyways ;)
   cacheInputData projdir = do
               opts <- options
               crdl <- cradle
               progs' <- patchStackPrograms crdl (optPrograms opts)
               return $ ( helperProgs progs'
                        , projdir
                        , (showVersion gmVer, chVer)
                        )

   gmVer = GhcMod.version
   chVer = VERSION_cabal_helper
