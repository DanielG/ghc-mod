{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module CabalApi (
    fromCabalFile
  , cabalParseFile
  , cabalBuildInfo
  , cabalAllDependPackages
  , cabalAllExtentions
  ) where

import Control.Applicative
import Data.Maybe (fromJust, maybeToList, mapMaybe)
import Data.Set (fromList, toList)
import Distribution.Package (Dependency(Dependency), PackageName(PackageName))
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Text (display)
import Distribution.Verbosity (silent)
import Language.Haskell.Extension (Extension(..))
import System.FilePath
import Types

----------------------------------------------------------------

fromCabalFile :: [GHCOption]
              -> Cradle
              -> IO ([GHCOption]
                    ,[IncludeDir]
                    ,[Package]
                    ,[LangExt])
fromCabalFile ghcOptions cradle = do
    cabal <- cabalParseFile cfile
    let binfo@BuildInfo{..} = cabalBuildInfo cabal
    let exts = map (("-X" ++) . display) $ usedExtensions binfo
        lang = maybe "-XHaskell98" (("-X" ++) . display) defaultLanguage
        libs = map ("-l" ++) extraLibs
        libDirs = map ("-L" ++) extraLibDirs
        gopts = ghcOptions ++ exts ++ [lang] ++ libs ++ libDirs
        idirs = case hsSourceDirs of
            []   -> [cdir,owdir]
            dirs -> map (cdir </>) dirs ++ [owdir]
    let depPkgs = removeMe cfile $ cabalAllDependPackages cabal
        hdrExts = cabalAllExtentions cabal
    return (gopts,idirs,depPkgs,hdrExts)
  where
    owdir = cradleCurrentDir cradle
    Just cdir = cradleCabalDir cradle
    Just cfile = cradleCabalDir cradle

removeMe :: FilePath -> [String] -> [String]
removeMe cabalfile = filter (/= me)
  where
    me = dropExtension $ takeFileName cabalfile

----------------------------------------------------------------

cabalParseFile :: FilePath -> IO GenericPackageDescription
cabalParseFile = readPackageDescription silent

----------------------------------------------------------------

-- Causes error, catched in the upper function.
cabalBuildInfo :: GenericPackageDescription -> BuildInfo
cabalBuildInfo pd = fromJust $ fromLibrary pd <|> fromExecutable pd
  where
    fromLibrary c     = libBuildInfo . condTreeData <$> condLibrary c
    fromExecutable c  = buildInfo . condTreeData . snd <$> toMaybe (condExecutables c)
    toMaybe []    = Nothing
    toMaybe (x:_) = Just x

----------------------------------------------------------------

cabalAllDependPackages :: GenericPackageDescription -> [Package]
cabalAllDependPackages pd = uniqueAndSort pkgs
  where
    pkgs = map getDependencyPackageName $ cabalAllDependency pd

cabalAllDependency :: GenericPackageDescription -> [Dependency]
cabalAllDependency = fromPackageDescription getDeps getDeps getDeps getDeps
  where
    getDeps :: [Tree a] -> [Dependency]
    getDeps = concatMap condTreeConstraints

getDependencyPackageName :: Dependency -> Package
getDependencyPackageName (Dependency (PackageName nm) _) = nm

----------------------------------------------------------------

cabalAllExtentions :: GenericPackageDescription -> [LangExt]
cabalAllExtentions pd = uniqueAndSort exts
  where
    buildInfos = cabalAllBuildInfos pd
    eexts = concatMap oldExtensions buildInfos
         ++ concatMap defaultExtensions buildInfos
    exts  = mapMaybe getExtensionName eexts

getExtensionName :: Extension -> Maybe LangExt
getExtensionName (EnableExtension nm) = Just (display nm)
getExtensionName _                    = Nothing

----------------------------------------------------------------

cabalAllBuildInfos :: GenericPackageDescription -> [BuildInfo]
cabalAllBuildInfos = fromPackageDescription f1 f2 f3 f4
  where
    f1 = map (libBuildInfo       . condTreeData)
    f2 = map (buildInfo          . condTreeData)
    f3 = map (testBuildInfo      . condTreeData)
    f4 = map (benchmarkBuildInfo . condTreeData)

----------------------------------------------------------------

type Tree = CondTree ConfVar [Dependency]

fromPackageDescription :: ([Tree Library]    -> [a])
                       -> ([Tree Executable] -> [a])
                       -> ([Tree TestSuite]  -> [a])
                       -> ([Tree Benchmark]  -> [a])
                       -> GenericPackageDescription
                       -> [a]
fromPackageDescription f1 f2 f3 f4 pd = lib ++ exe ++ tests ++ bench
  where
    lib   = f1 . maybeToList . condLibrary $ pd
    exe   = f2 . map snd . condExecutables $ pd
    tests = f3 . map snd . condTestSuites  $ pd
    bench = f4 . map snd . condBenchmarks  $ pd

----------------------------------------------------------------

uniqueAndSort :: [String] -> [String]
uniqueAndSort = toList . fromList
