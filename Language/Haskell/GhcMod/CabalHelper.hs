-- ghc-mod: Making Haskell development *more* fun
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
module Language.Haskell.GhcMod.CabalHelper
#ifndef SPEC
  ( getComponents
  , getGhcMergedPkgOptions
  , getCabalPackageDbStack
  , getStackPackageDbStack
  , prepareCabalHelper
  )
#endif
  where

import Control.Applicative
import Control.Monad
import Control.Category ((.))
import Data.Maybe
import Data.Monoid
import Data.Serialize (Serialize)
import Data.Traversable
import Distribution.Helper hiding (Programs(..))
import qualified Distribution.Helper as CH
import qualified Language.Haskell.GhcMod.Types as T
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Utils
import Language.Haskell.GhcMod.PathsAndFiles
import Language.Haskell.GhcMod.Logging
import Language.Haskell.GhcMod.Output
import Language.Haskell.GhcMod.CustomPackageDb
import System.FilePath
import System.Directory (findExecutable)
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
    opts <- withCabal $ runCHQuery ghcMergedPkgOptions
    return ([setupConfigPath distdir], opts)
 }

getCabalPackageDbStack :: (IOish m, Gm m) => m [GhcPkgDb]
getCabalPackageDbStack = chCached $ \distdir -> Cached {
  cacheLens = Just (lGmcPackageDbStack . lGmCaches),
  cacheFile = pkgDbStackCacheFile distdir,
  cachedAction = \_tcf (_progs, _projdir, _ver) _ma -> do
    crdl <- cradle
    dbs <- withCabal $ map chPkgToGhcPkg <$>
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
getComponents = chCached$ \distdir -> Cached {
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
runCHQuery :: (IOish m, GmOut m, GmEnv m) => Query m b -> m b
runCHQuery a = do
  crdl <- cradle
  let projdir = cradleRootDir crdl
      distdir = projdir </> cradleDistDir crdl

  opts <- options
  progs <- patchStackPrograms crdl (optPrograms opts)

  readProc <- gmReadProcess

  let qe = (defaultQueryEnv projdir distdir) {
               qeReadProcess = readProc
             , qePrograms = helperProgs progs
             }
  runQuery qe a


prepareCabalHelper :: (IOish m, GmEnv m, GmOut m, GmLog m) => m ()
prepareCabalHelper = do
  crdl <- cradle
  let projdir = cradleRootDir crdl
      distdir = projdir </> cradleDistDir crdl
  readProc <- gmReadProcess
  when (cradleProjectType crdl == CabalProject || cradleProjectType crdl == StackProject) $
       withCabal $ liftIO $ prepare readProc projdir distdir

getStackPackageDbStack :: IOish m => m [GhcPkgDb]
getStackPackageDbStack = do
    mstack <- liftIO $ findExecutable "stack"
    case mstack of
      Nothing -> return []
      Just stack -> do
        snapshotDb <- liftIO $ readProcess stack ["path", "--snapshot-pkg-db"] ""
        localDb <- liftIO $ readProcess stack ["path", "--local-pkg-db"] ""
        return $ map (PackageDb . takeWhile (/='\n')) [snapshotDb, localDb]

patchStackPrograms :: (IOish m, GmOut m) => Cradle -> Programs -> m Programs
patchStackPrograms crdl progs
    | cradleProjectType crdl /= StackProject = return progs
patchStackPrograms crdl progs = do
  let projdir = cradleRootDir crdl
  Just ghc <- getStackGhcPath projdir
  Just ghcPkg <- getStackGhcPkgPath projdir
  return $ progs {
      ghcProgram = ghc
    , ghcPkgProgram = ghcPkg
    }

withCabal :: (IOish m, GmEnv m, GmOut m, GmLog m) => m a -> m a
withCabal action = do
    crdl <- cradle
    opts <- options
    readProc <- gmReadProcess

    let projdir = cradleRootDir crdl
        distdir = projdir </> cradleDistDir crdl

    mCabalFile          <- liftIO $ timeFile `traverse` cradleCabalFile crdl
    mCabalConfig        <- liftIO $ timeMaybe (setupConfigFile crdl)
    mCabalSandboxConfig <- liftIO $ timeMaybe (sandboxConfigFile crdl)

    mCusPkgDbStack <- getCustomPkgDbStack

    pkgDbStackOutOfSync <-
         case mCusPkgDbStack of
           Just cusPkgDbStack -> do
             let qe = (defaultQueryEnv projdir distdir) {
                          qeReadProcess = readProc
                        , qePrograms = helperProgs $ optPrograms opts
                        }
             pkgDb <- runQuery qe $ map chPkgToGhcPkg <$> packageDbStack
             return $ pkgDb /= cusPkgDbStack

           Nothing -> return False

    projType <- cradleProjectType <$> cradle

    when (isSetupConfigOutOfDate mCabalFile mCabalConfig) $
      gmLog GmDebug "" $ strDoc $ "setup configuration is out of date, reconfiguring Cabal project."

    when (isSetupConfigOutOfDate mCabalSandboxConfig mCabalConfig) $
      gmLog GmDebug "" $ strDoc $ "sandbox configuration is out of date, reconfiguring Cabal project."

    when pkgDbStackOutOfSync $
      gmLog GmDebug "" $ strDoc $ "package-db stack out of sync with ghc-mod.package-db-stack, reconfiguring Cabal project."

    when ( isSetupConfigOutOfDate mCabalFile mCabalConfig
        || pkgDbStackOutOfSync
        || isSetupConfigOutOfDate mCabalSandboxConfig mCabalConfig) $
          case projType of
            CabalProject ->
                cabalReconfigure readProc (optPrograms opts) crdl projdir distdir
            StackProject ->

                stackReconfigure crdl (optPrograms opts)
            _ ->
                error $ "withCabal: unsupported project type: " ++ show projType

    action

 where
   writeAutogen projdir distdir = do
     readProc <- gmReadProcess
     gmLog GmDebug "" $ strDoc $ "writing Cabal autogen files"
     liftIO $ writeAutogenFiles readProc projdir distdir


   cabalReconfigure readProc progs crdl projdir distdir = do
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
        liftIO $ void $ readProc (T.cabalProgram progs) ("configure":progOpts) ""
        writeAutogen projdir distdir

   stackReconfigure crdl progs = do
     let projdir = cradleRootDir crdl
         distdir = projdir </> cradleDistDir crdl

     withDirectory_ (cradleRootDir crdl) $ do
       supported <- haveStackSupport
       if supported
          then do
            spawn [T.stackProgram progs, "build", "--only-dependencies", "."]
            spawn [T.stackProgram progs, "build", "--only-configure", "."]
            writeAutogen projdir distdir
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

chCached :: (Applicative m, IOish m, Gm m, Serialize a)
  => (FilePath -> Cached m GhcModState ChCacheData a) -> m a
chCached c = do
  projdir <- cradleRootDir <$> cradle
  distdir <- (projdir </>) . cradleDistDir <$> cradle
  d <- cacheInputData projdir
  withCabal $ cached projdir (c distdir) d
 where
   -- we don't need to include the disdir in the cache input because when it
   -- changes the cache files will be gone anyways ;)
   cacheInputData projdir = do
               opts <- options
               crdl <- cradle
               progs' <- patchStackPrograms crdl (optPrograms opts)
               return $ ( helperProgs progs'
                        , projdir
                        , (gmVer, chVer)
                        )

   gmVer = GhcMod.version
   chVer = VERSION_cabal_helper
