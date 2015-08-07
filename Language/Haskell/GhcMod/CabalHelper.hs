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
  , getPackageDbStack
  )
#endif
  where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.Monoid
import Data.Version
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
import System.FilePath
import Prelude

import Paths_ghc_mod as GhcMod

-- | Only package related GHC options, sufficient for things that don't need to
-- access home modules
getGhcMergedPkgOptions :: (Applicative m, IOish m, GmEnv m, GmLog m)
                 => m [GHCOption]
getGhcMergedPkgOptions = chCached Cached {
  cacheFile = mergedPkgOptsCacheFile,
  cachedAction = \ _tcf (progs, root, _) _ma -> do
    opts <- withCabal $ runQuery' progs root $ ghcMergedPkgOptions
    return ([setupConfigPath], opts)
 }

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

getPackageDbStack :: (IOish m, GmEnv m, GmLog m) => m [GhcPkgDb]
getPackageDbStack = do
    mCusPkgStack <- getCustomPkgDbStack
    flip fromMaybe mCusPkgStack <$> getPackageDbStack'

getPackageDbStack' :: (IOish m, GmEnv m, GmLog m) => m [GhcPkgDb]
getPackageDbStack' = chCached Cached {
  cacheFile = pkgDbStackCacheFile,
  cachedAction = \ _tcf (progs, root, _) _ma -> do
    dbs <- withCabal $ map chPkgToGhcPkg <$> runQuery' progs root packageDbStack
    return ([setupConfigPath, sandboConfigFile], dbs)
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
getComponents :: (Applicative m, IOish m, GmEnv m, GmLog m)
              => m [GmComponent 'GMCRaw ChEntrypoint]
getComponents = chCached cabalHelperCache

cabalHelperCache
  :: (Functor m, Applicative m, MonadIO m)
  => Cached m (Programs, FilePath, (Version, String)) [GmComponent 'GMCRaw ChEntrypoint]
cabalHelperCache = Cached {
    cacheFile = cabalHelperCacheFile,
    cachedAction = \ _tcf (progs, root, _vers) _ma ->
      runQuery' progs root $ do
        q <- join7
               <$> ghcOptions
               <*> ghcPkgOptions
               <*> ghcSrcOptions
               <*> ghcLangOptions
               <*> entrypoints
               <*> entrypoints
               <*> sourceDirs
        let cs = flip map q $ curry8 (GmComponent mempty)
        return ([setupConfigPath], cs)
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

withCabal :: (IOish m, GmEnv m, GmLog m) => m a -> m a
withCabal action = do
    crdl <- cradle
    opts <- options
    mCabalFile <- liftIO $ timeFile `traverse` cradleCabalFile crdl
    mCabalConfig <- liftIO $ timeMaybe (setupConfigFile crdl)

    mCusPkgDbStack <- getCustomPkgDbStack

    pkgDbStackOutOfSync <-
         case mCusPkgDbStack of
           Just cusPkgDbStack -> do
             pkgDb <- runQuery' (helperProgs opts) (cradleRootDir crdl </> "dist") $
                 map chPkgToGhcPkg <$> packageDbStack
             return $ pkgDb /= cusPkgDbStack

           Nothing -> return False

    cusPkgStack <- maybe [] ((PackageDb "clear"):) <$> getCustomPkgDbStack

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
            liftIO $ void $ readProcess (T.cabalProgram opts) ("configure":progOpts) ""
            gmLog GmDebug "" $ strDoc $ "writing Cabal autogen files"
            liftIO $ writeAutogenFiles $ cradleRootDir crdl </> "dist"
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

chCached :: (Applicative m, IOish m, GmEnv m, GmLog m, Serialize a)
         => Cached m (Programs, FilePath, (Version, [Char])) a -> m a
chCached c = do
  root <- cradleRootDir <$> cradle
  d <- cacheInputData root
  withCabal $ cached root c d
 where
   cacheInputData root = do
               opt <- options
               return $ ( helperProgs opt
                        , root </> "dist"
                        , (gmVer, chVer)
                        )

   gmVer = GhcMod.version
   chVer = VERSION_cabal_helper
