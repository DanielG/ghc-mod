-- Copyright (C) 2013-2016  Carlo Hamalainen <carlo ÄT carlo-hamalainen DOT net>
-- Copyright (C) 2016  Daniel Gröber <dxld ÄT darkboxed DOT org>
-- Copyright (C) 2016  Nikolay Yakimov <root ÄT livid DOT pp DOT ru>
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
module Language.Haskell.GhcMod.ImportedFrom (importedFrom) where

import Control.Applicative
import Control.Exception
import Data.List
import Data.Maybe
import System.FilePath

import Exception (ghandle)
import FastString
import GHC
import OccName
import Packages
import HscTypes

import Language.Haskell.GhcMod
import Language.Haskell.GhcMod.DynFlags
import Language.Haskell.GhcMod.FileMapping
import Language.Haskell.GhcMod.Gap
import Language.Haskell.GhcMod.Logging
import Language.Haskell.GhcMod.Monad
import Language.Haskell.GhcMod.SrcUtils (listifyStaged, findSpanName, cmp)
import GHC.SYB.Utils
import Data.Function
import Data.Version
import Prelude
import Data.Data
import Safe
import Documentation.Haddock
import Data.IORef
import System.Directory
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe

data PackageDesc = PackageDesc
  { pdName :: String
  , pdVersion :: Version
  , pdHdHTMLs :: [FilePath]
  , pdHdIfaces :: [InstalledInterface]
  }

data ModuleDesc = ModuleDesc
  { mdName :: String
  , mdMod :: Module
  , mdAlias :: Maybe String
  , mdVisibleExports :: [Name]
  , mdImplicit :: Bool
  }

getPackageDescFromPackageConfig :: (GhcMonad m, MonadIO m) => PackageConfig -> m PackageDesc
getPackageDescFromPackageConfig InstalledPackageInfo{..}
  = do
    let PackageName packageName' = packageName
    his <- catMaybes <$> mapM (fmap (either (const Nothing) Just) . readInterfaceFile') haddockInterfaces
    return PackageDesc
      { pdName = unpackFS packageName'
      , pdVersion = packageVersion
      , pdHdHTMLs = haddockHTMLs
      , pdHdIfaces = concatMap ifInstalledIfaces his
      }

readInterfaceFile' :: (MonadIO m, GhcMonad m) => FilePath -> m (Either String InterfaceFile)
readInterfaceFile' f = do
  exists <- liftIO $ doesFileExist f
  if exists
  then readInterfaceFile nameCacheFromGhc' f
  else return $ Left "No such file"

-- Derived from haddock-api, see COPYING.BSD3.haddock-api in the source
-- distribution for it's license.
nameCacheFromGhc' :: (GhcMonad m, MonadIO m) => NameCacheAccessor m
nameCacheFromGhc' = ( read_from_session , write_to_session )
  where
    read_from_session = liftIO =<< readIORef . hsc_NC <$> getSession
    write_to_session nc' = liftIO =<< flip writeIORef nc' . hsc_NC <$> getSession

getModulePackage :: (GhcMonad m, MonadIO m) => Module -> m (Maybe PackageDesc)
getModulePackage m = do
  dflag <- getSessionDynFlags
  let pkg = lookupPackage dflag (moduleUnitId' m)
  mapM getPackageDescFromPackageConfig pkg

getModuleHaddockVisibleExports :: ModuleDesc -> PackageDesc -> [Name]
getModuleHaddockVisibleExports ModuleDesc{..} pkgdesc =
  let modHdIfs = filter ((mdMod ==) . instMod) . pdHdIfaces $ pkgdesc
  in concatMap instVisibleExports modHdIfs

getModuleDescFromImport :: (GhcMonad m) => ImportDecl Name -> m ModuleDesc
getModuleDescFromImport ImportDecl{..}
  = do
    modul <- findModule (unLoc ideclName) (fmap sl_fs' ideclPkgQual)
    modInfo <- fromJustNote "imported-from,getModuleDescFromImport" <$> getModuleInfo modul
    let listNames :: Data a => a -> [Name]
        listNames = listifyStaged Renamer (const True)
        exprts = modInfoExports modInfo
        visExprts
          = case ideclHiding of
            Just (True, hidden) -> exprts \\ listNames hidden
            Just (False, shown) -> listNames shown
            Nothing             -> exprts
    return ModuleDesc
      { mdName = moduleNameString (moduleName modul)
      , mdMod = modul
      , mdAlias = moduleNameString <$> ideclAs
      , mdVisibleExports = visExprts
      , mdImplicit = ideclImplicit
      }

modulesWithPackages :: (GhcMonad m, MonadIO m) => [ModuleDesc] -> m [(ModuleDesc, PackageDesc)]
modulesWithPackages = (fmap catMaybes .) $ mapM $ \x@ModuleDesc{..} -> runMaybeT $ do
  pkg <- MaybeT $ getModulePackage mdMod
  return (x, pkg)

preferExplicit :: [ModuleDesc] -> [ModuleDesc]
preferExplicit ms =
  let (impl, expl) = partition mdImplicit ms
  in expl ++ impl

guessModule :: Maybe String -> Name -> [(ModuleDesc, PackageDesc)] -> Maybe (Name, (ModuleDesc, PackageDesc))
guessModule mqn n ms =
  let
    occn = occNameString $ occName n
    msf = filter f ms
    f = (n `elem`) . uncurry getModuleHaddockVisibleExports
    msf2 | null msf = filter f2 ms
         | otherwise = msf
    f2 (ModuleDesc{..},_) = n `elem` mdVisibleExports
    msf3 | Just qn <- mqn
         , qn /= occn = filter (f3 qn) msf2
         | otherwise = msf2
    f3 qn (ModuleDesc{..},_)
      | Just as <- mdAlias = qn `elem` map (++ '.' : occn) [as, mdName]
      | otherwise = qn == (mdName ++ '.' : occn)
  in (,) n <$> headMay msf3

showOutput :: (GhcMonad m, MonadIO m) => Name -> (ModuleDesc, PackageDesc) -> m String
showOutput n (ModuleDesc{..}, imppkg) = do
  let
    occn = occNameString $ occName n
    nmod = nameModule n
    mn = moduleNameString . moduleName $ nmod
  modpkg <- fromMaybe imppkg <$> getModulePackage nmod
  let
    package = pdName modpkg ++ "-" ++ showVersion (pdVersion modpkg)
    fqn = package ++ ':' : mn ++ '.' : occn
    hdRoot = headMay $ pdHdHTMLs imppkg
    docFn = dotsToDashes mdName ++ ".html"
    hdPath = fmap (</> docFn) hdRoot
    dotsToDashes = map go
      where go '.' = '-'
            go x = x
    hackageUrl = "https://hackage.haskell.org/package/" ++ package ++ "/docs/" ++ docFn
  hdPathReal <- liftIO $ runMaybeT $ do
    hdp <- MaybeT $ return hdPath
    exists <- lift $ doesFileExist hdp
    if exists
    then return hdp
    else MaybeT $ return Nothing
  return $ unwords [fqn, mdName, fromMaybe hackageUrl hdPathReal] ++ "\n"

-- | Look up Haddock docs for a symbol.
importedFrom :: forall m. IOish m
             => FilePath     -- ^ A target file.
             -> Int          -- ^ Line number.
             -> Int          -- ^ Column number.
             -> Maybe Expression   -- ^ Expression (symbol)
             -> GhcModT m String
importedFrom file lineNr colNr symbol =
  ghandle handler $
    runGmlT' [Left file] deferErrors $
      withInteractiveContext $ do
        crdl         <- cradle
        modSum       <- fileModSummaryWithMapping (cradleCurrentDir crdl </> file)
        (decls,imports, _exports, _docs) <- fromJustNote "imported-from,importedFrom" . renamedSource <$> (parseModule modSum >>= typecheckModule)
        importDescs <- mapM (getModuleDescFromImport . unLoc) imports
        let bestid = headMay $ concatMap snd $ sortBy (cmp `on` fst) $ findSpanName decls (lineNr, colNr)
            idsMods = preferExplicit . (\x -> filter ((x `elem`) . mdVisibleExports) importDescs) <$> bestid
            mbsym = getExpression <$> symbol
        fmap (fromMaybe "Nothing found\n") $ runMaybeT $ do
          imps <- lift . modulesWithPackages =<< MaybeT (return idsMods)
          bi <- MaybeT $ return bestid
          bg <- MaybeT . return $ guessModule mbsym bi imps
          lift $ uncurry showOutput bg
 where
   handler (SomeException ex) = do
     gmLog GmException "imported-from" $ showDoc ex
     return []
