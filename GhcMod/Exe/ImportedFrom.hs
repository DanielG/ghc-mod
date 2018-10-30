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

module GhcMod.Exe.ImportedFrom (importedFrom) where

import Control.Applicative
import Control.Exception
import Data.List
import Data.Maybe
import System.FilePath

import Exception (ghandle)
import GHC
import OccName
import Packages
import HscTypes

import GhcMod
import GhcMod.DynFlags
import GhcMod.FileMapping
import GhcMod.Gap
import GhcMod.Logging
import GhcMod.Monad
import GhcMod.SrcUtils (listifyStaged, findSpanName, cmp)
import GHC.SYB.Utils
import Data.Function
import Data.Version
import Data.Traversable
import Prelude hiding (mapM)
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

getPackageDescFromPackageConfig :: (GmOut m, GmLog m, GhcMonad m, MonadIO m)
                                => PackageConfig -> m PackageDesc
getPackageDescFromPackageConfig p@InstalledPackageInfo{..}
  = do
    let (pkgName, pkgVer) = packageNameVesrion p
    his <- catMaybes <$> mapM readInterfaceFile' haddockInterfaces
    return PackageDesc
      { pdName = pkgName
      , pdVersion = pkgVer
      , pdHdHTMLs = haddockHTMLs
      , pdHdIfaces = concatMap ifInstalledIfaces his
      }

readInterfaceFile' :: (GmOut m, GmLog m, MonadIO m, GhcMonad m)
                   => FilePath -> m (Maybe InterfaceFile)
readInterfaceFile' f = do
    exists <- liftIO $ doesFileExist f
    if exists
      then either (const Nothing) Just <$> readInterfaceFile nameCacheFromGhc' f
      else do
        gmLog GmWarning "imported-from" haddockSuggestion
        return Nothing
  where
    backticks :: GhcMod.Logging.Doc -> GhcMod.Logging.Doc
    backticks d = text "`" $$ d $$ text "`"

    haddockSuggestion =
        text "Couldn't find haddock interface" <+> quotes (text f) $$
        text "- To generate Haddock docs for dependencies, try:" $$
            nest 4 (backticks $ text "cabal install --enable-documentation\
                                                   \--haddock-hyperlink-source\
                                                   \--only-dependencies") $$
        text "" $$
        text "- or set" $$
            nest 4 (backticks $ text "documentation: True") $$
        text "in ~/.cabal/config" $$
        text "" $$
        text "- or with Stack:" $$
            nest 4 (backticks $ text "stack haddock")

-- Derived from haddock-api, see COPYING.BSD3.haddock-api in the source
-- distribution for it's license.
nameCacheFromGhc' :: (GhcMonad m, MonadIO m) => NameCacheAccessor m
nameCacheFromGhc' = ( read_from_session , write_to_session )
  where
    read_from_session = liftIO =<< readIORef . hsc_NC <$> getSession
    write_to_session nc' = liftIO =<< flip writeIORef nc' . hsc_NC <$> getSession

getModulePackage :: (GmOut m, GmLog m, GhcMonad m, MonadIO m)
                 => Module -> m (Maybe PackageDesc)
getModulePackage m = do
  dflag <- getSessionDynFlags
  let pkg = lookupPackage' dflag (moduleUnitId' m)
  mapM getPackageDescFromPackageConfig pkg

getModuleHaddockVisibleExports :: ModuleDesc -> PackageDesc -> [Name]
getModuleHaddockVisibleExports ModuleDesc{..} pkgdesc =
  let modHdIfs = filter ((mdMod ==) . instMod) . pdHdIfaces $ pkgdesc
  in concatMap instVisibleExports modHdIfs

getModuleDescFromImport :: (GhcMonad m) => ImportDecl Name -> m ModuleDesc
getModuleDescFromImport ImportDecl{..} = do
    modul <- findModule (unLoc ideclName) (fmap sl_fs' ideclPkgQual)
    modInfo <- fromJustNote "getModuleDescFromImport" <$> getModuleInfo modul
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

modulesWithPackages :: (GmOut m, GmLog m, GhcMonad m, MonadIO m)
                    => [ModuleDesc] -> m [(ModuleDesc, PackageDesc)]
modulesWithPackages =
    (fmap catMaybes .) $ mapM $ \x@ModuleDesc{..} -> runMaybeT $ do
        pkg <- MaybeT $ getModulePackage mdMod
        return (x, pkg)

guessModule :: Maybe String
            -> Name
            -> [(ModuleDesc, PackageDesc)]
            -> Maybe (Name, (ModuleDesc, PackageDesc))
guessModule mqn n ms = let
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
  in
    (,) n <$> headMay msf3

showOutput :: (GmOut m, GmLog m, GhcMonad m, MonadIO m)
           => Name -> (ModuleDesc, PackageDesc) -> m String
showOutput n (ModuleDesc{..}, imppkg) = do
    let occn = occNameString $ occName n
        nmod = nameModule n
        mn = moduleNameString . moduleName $ nmod
    modpkg <- fromMaybe imppkg <$> getModulePackage nmod
    let modpackage
            | null (versionBranch modpackagever) = pdName modpkg
            | otherwise = pdName modpkg ++ '-' : showVersion modpackagever
        modpackagever = pdVersion modpkg
        package
            | null (versionBranch packagever)
              , Just r <- hdRoot = takeFileName r
            | otherwise = pdName imppkg ++ '-' : showVersion packagever
        packagever = pdVersion imppkg
        fqn = modpackage ++ ':' : mn ++ '.' : occn
        hdRoot = headMay $ pdHdHTMLs imppkg
        docFn = dotsToDashes mdName ++ ".html"
        hdPath = fmap (</> docFn) hdRoot
        dotsToDashes = map go
            where go '.' = '-'
                  go x = x
        hackageUrl = "https://hackage.haskell.org/package/"
                     ++ package ++ "/docs/" ++ docFn
    hdPathReal <- liftIO $ runMaybeT $ do
      hdp <- MaybeT $ return hdPath
      exists <- lift $ doesFileExist hdp
      if exists
        then return hdp
        else MaybeT $ return Nothing
    return $ unwords [fqn, mdName, fromMaybe hackageUrl hdPathReal]

-- | Look up Haddock docs for a symbol.
importedFrom :: forall m. IOish m
             => FilePath         -- ^ A target file.
             -> Int              -- ^ Line number.
             -> Int              -- ^ Column number.
             -> Maybe Expression -- ^ Expression (symbol)
             -> GhcModT m String
importedFrom file lineNr colNr symbol =
    handler $ runGmlT' [Left file] deferErrors $ withInteractiveContext $ do
      crdl         <- cradle
      modSum       <- fileModSummaryWithMapping (cradleCurrentDir crdl </> file)
      Just (decls,imports, _exports, _docs)
          <- renamedSource <$> (parseModule modSum >>= typecheckModule)
      importDescs :: [ModuleDesc]
          <- mapM (getModuleDescFromImport . unLoc) imports
      bestids <-
          case sortBy (cmp `on` fst) $ findSpanName decls (lineNr, colNr) of
            ((_, x):_) -> return x
            [] -> error $ "No names found at " ++ show (lineNr, colNr)
      let visExports xs n = filter ((elem n) . mdVisibleExports) xs
          idsMods = map (preferExplicit . visExports importDescs) bestids
          mbsym = getExpression <$> symbol
      imps <- mapM modulesWithPackages idsMods
      let bestids_str =
            intercalate "," (map (occNameString . getOccName) bestids)
      bg <- case catMaybes $ zipWith (guessModule mbsym) bestids imps of
          [] -> error $ "No modules exporting " ++ fromMaybe bestids_str mbsym
          x -> return x
      unlines <$> mapM (uncurry showOutput) bg
 where
   handler = ghandle $ \(SomeException ex) ->
     gmLog GmException "imported-from" (showToDoc ex) >> return []

   preferExplicit :: [ModuleDesc] -> [ModuleDesc]
   preferExplicit ms =
       let (impl, expl) = partition mdImplicit ms in expl ++ impl
