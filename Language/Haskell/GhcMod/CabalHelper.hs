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
  , getCustomPkgDbStack
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
import Distribution.Helper
import qualified Language.Haskell.GhcMod.Types as T
import Language.Haskell.GhcMod.Types hiding (ghcProgram, ghcPkgProgram,
                                             cabalProgram)
import Language.Haskell.GhcMod.Monad.Types
import Language.Haskell.GhcMod.Utils
import Language.Haskell.GhcMod.PathsAndFiles
import Language.Haskell.GhcMod.Logging
import Language.Haskell.GhcMod.Output
import System.FilePath
import System.Directory (findExecutable)
import Prelude hiding ((.))

import Paths_ghc_mod as GhcMod

-- | Only package related GHC options, sufficient for things that don't need to
-- access home modules
getGhcMergedPkgOptions :: (Applicative m, IOish m, GmEnv m, GmState m, GmLog m)
  => m [GHCOption]
getGhcMergedPkgOptions = chCached $ \distDir -> Cached {
  cacheLens = Just (lGmcMergedPkgOptions . lGmCaches),
  cacheFile = distDir </> mergedPkgOptsCacheFile,
  cachedAction = \ _tcf (progs, rootdir, distdir, _) _ma -> do
    readProc <- gmReadProcess
    opts <- withCabal $ runQuery'' readProc progs rootdir distdir $
                ghcMergedPkgOptions
    return ([distDir </> setupConfigPath], opts)
 }

getCabalPackageDbStack :: (IOish m, GmEnv m, GmState m, GmLog m) => m [GhcPkgDb]
getCabalPackageDbStack = chCached $ \distDir -> Cached {
  cacheLens = Just (lGmcPackageDbStack . lGmCaches),
  cacheFile = distDir </> pkgDbStackCacheFile,
  cachedAction = \ _tcf (progs, rootdir, distdir, _) _ma -> do
    readProc <- gmReadProcess
    dbs <- withCabal $ map chPkgToGhcPkg <$> runQuery'' readProc progs rootdir distdir packageDbStack
    return ([distDir </> setupConfigPath, sandboxConfigFile], dbs)
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
getComponents :: (Applicative m, IOish m, GmEnv m, GmState m, GmLog m)
              => m [GmComponent 'GMCRaw ChEntrypoint]
getComponents = chCached$ \distDir -> Cached {
    cacheLens = Just (lGmcComponents . lGmCaches),
    cacheFile = distDir </> cabalHelperCacheFile,
    cachedAction = \ _tcf (progs, rootdir, distdir, _vers) _ma -> do
      readProc <- gmReadProcess
      runQuery'' readProc progs rootdir distdir $ do
        q <- join7
               <$> ghcOptions
               <*> ghcPkgOptions
               <*> ghcSrcOptions
               <*> ghcLangOptions
               <*> entrypoints
               <*> entrypoints
               <*> sourceDirs
        let cs = flip map q $ curry8 (GmComponent mempty)
        return ([distDir </> setupConfigPath], cs)
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

prepareCabalHelper :: (IOish m, GmEnv m, GmLog m) => m ()
prepareCabalHelper = do
  crdl <- cradle
  let projdir = cradleRootDir crdl
      distdir = projdir </> cradleDistDir crdl
  readProc <- gmReadProcess
  when (cradleProjectType crdl == CabalProject) $
       withCabal $ liftIO $ prepare readProc projdir distdir

parseCustomPackageDb :: String -> [GhcPkgDb]
parseCustomPackageDb src = map parsePkgDb $ filter (not . null) $ lines src
 where
   parsePkgDb "global" = GlobalDb
   parsePkgDb "user" = UserDb
   parsePkgDb s = PackageDb s

getCustomPkgDbStack :: (IOish m, GmEnv m) => m (Maybe [GhcPkgDb])
getCustomPkgDbStack = do
    mCusPkgDbFile <- liftIO . (traverse readFile <=< findCustomPackageDbFile) . cradleRootDir =<< cradle
    return $ parseCustomPackageDb <$> mCusPkgDbFile

getStackPackageDbStack :: IOish m => m [GhcPkgDb]
getStackPackageDbStack = do
    mstack <- liftIO $ findExecutable "stack"
    case mstack of
      Nothing -> return []
      Just stack -> do
        snapshotDb <- liftIO $ readProcess stack ["path", "--snapshot-pkg-db"] ""
        localDb <- liftIO $ readProcess stack ["path", "--local-pkg-db"] ""
        return $ map (PackageDb . takeWhile (/='\n')) [snapshotDb, localDb]

withCabal :: (IOish m, GmEnv m, GmLog m) => m a -> m a
withCabal action = do
    crdl <- cradle
    opts <- options
    readProc <- gmReadProcess

    let projdir = cradleRootDir crdl
        distdir = projdir </> cradleDistDir crdl

    mCabalFile <- liftIO $ timeFile `traverse` cradleCabalFile crdl
    mCabalConfig <- liftIO $ timeMaybe (setupConfigFile crdl)

    mCusPkgDbStack <- getCustomPkgDbStack

    pkgDbStackOutOfSync <-
         case mCusPkgDbStack of
           Just cusPkgDbStack -> do
             pkgDb <- runQuery'' readProc (helperProgs opts) projdir distdir $
                 map chPkgToGhcPkg <$> packageDbStack
             return $ pkgDb /= cusPkgDbStack

           Nothing -> return False

    cusPkgStack <- maybe [] ((PackageDb "clear"):) <$> getCustomPkgDbStack

    --TODO: also invalidate when sandboxConfig file changed

    when (isSetupConfigOutOfDate mCabalFile mCabalConfig) $
      gmLog GmDebug "" $ strDoc $ "setup configuration is out of date, reconfiguring Cabal project."
    when pkgDbStackOutOfSync $
      gmLog GmDebug "" $ strDoc $ "package-db stack out of sync with ghc-mod.package-db-stack, reconfiguring Cabal project."

    when (isSetupConfigOutOfDate mCabalFile mCabalConfig || pkgDbStackOutOfSync) $
        withDirectory_ (cradleRootDir crdl) $ do
            let progOpts =
                    [ "--with-ghc=" ++ T.ghcProgram opts ]
                    -- Only pass ghc-pkg if it was actually set otherwise we
                    -- might break cabal's guessing logic
                    ++ if T.ghcPkgProgram opts /= T.ghcPkgProgram defaultOptions
                         then [ "--with-ghc-pkg=" ++ T.ghcPkgProgram opts ]
                         else []
                    ++ map pkgDbArg cusPkgStack
            liftIO $ void $ readProc (T.cabalProgram opts) ("configure":progOpts) ""
            gmLog GmDebug "" $ strDoc $ "writing Cabal autogen files"
            liftIO $ writeAutogenFiles readProc projdir distdir
    action

pkgDbArg :: GhcPkgDb -> String
pkgDbArg GlobalDb      = "--package-db=global"
pkgDbArg UserDb        = "--package-db=user"
pkgDbArg (PackageDb p) = "--package-db=" ++ p

-- * Neither file exists -> should return False:
--   @Nothing < Nothing = False@
--   (since we don't need to @cabal configure@ when no cabal file exists.)
--
-- * Cabal file doesn't exist (unlikely case) -> should return False
--   @Just cc < Nothing = False@
--   TODO: should we delete dist/setup-config?
--
-- * dist/setup-config doesn't exist yet -> should return True:
--   @Nothing < Just cf = True@
--
-- * Both files exist
--   @Just cc < Just cf = cc < cf = cc `olderThan` cf@
isSetupConfigOutOfDate :: Maybe TimedFile -> Maybe TimedFile -> Bool
isSetupConfigOutOfDate worldCabalFile worldCabalConfig = do
  worldCabalConfig < worldCabalFile


helperProgs :: Options -> Programs
helperProgs opts = Programs {
                            cabalProgram  = T.cabalProgram opts,
                            ghcProgram    = T.ghcProgram opts,
                            ghcPkgProgram = T.ghcPkgProgram opts
                          }

chCached :: (Applicative m, IOish m, GmEnv m, GmState m, GmLog m, Serialize a)
  => (FilePath -> Cached m GhcModState ChCacheData a) -> m a
chCached c = do
  root <- cradleRootDir <$> cradle
  dist <- cradleDistDir <$> cradle
  d <- cacheInputData root dist
  withCabal $ cached root (c dist) d
 where
   cacheInputData root dist = do
               opt <- options
               return $ ( helperProgs opt
                        , root
                        , root </> dist
                        , (gmVer, chVer)
                        )

   gmVer = GhcMod.version
   chVer = VERSION_cabal_helper
