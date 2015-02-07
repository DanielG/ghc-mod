{-# LANGUAGE OverloadedStrings, CPP #-}

module Language.Haskell.GhcMod.CabalApi (
    getCompilerOptions
  , parseCabalFile
  , cabalAllBuildInfo
  , cabalSourceDirs
  , cabalConfigDependencies
  ) where

import Language.Haskell.GhcMod.CabalConfig
import Language.Haskell.GhcMod.Error
import Language.Haskell.GhcMod.Gap (benchmarkBuildInfo, mkGHCCompilerId)
import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Logging

import MonadUtils (liftIO)
import Control.Applicative ((<$>))
import qualified Control.Exception as E
import Data.Maybe (maybeToList)
import Data.Set (fromList, toList)
import Distribution.Package (PackageName(PackageName))
import qualified Distribution.Package as C
import Distribution.PackageDescription (PackageDescription, BuildInfo)
import qualified Distribution.PackageDescription as P
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Program as C (ghcProgram)
import Distribution.Simple.Program.Types (programName, programFindVersion)
import Distribution.System (buildPlatform)
import Distribution.Text (display)
import Distribution.Verbosity (silent)
import Distribution.Version (Version)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
----------------------------------------------------------------

-- | Getting necessary 'CompilerOptions' from three information sources.
getCompilerOptions :: (IOish m, GmError m, GmLog m)
                   => [GHCOption]
                   -> Cradle
                   -> CabalConfig
                   -> PackageDescription
                   -> m CompilerOptions
getCompilerOptions ghcopts cradle config pkgDesc = do
    gopts <- liftIO $ getGHCOptions ghcopts cradle rdir $ head buildInfos
    let depPkgs = cabalConfigDependencies config (C.packageId pkgDesc)
    return $ CompilerOptions gopts idirs depPkgs
  where
    wdir       = cradleCurrentDir cradle
    rdir       = cradleRootDir    cradle
    buildInfos = cabalAllBuildInfo pkgDesc
    idirs      = includeDirectories rdir wdir $ cabalSourceDirs buildInfos

----------------------------------------------------------------
-- Include directories for modules

cabalBuildDirs :: [FilePath]
cabalBuildDirs = ["dist/build", "dist/build/autogen"]

includeDirectories :: FilePath -> FilePath -> [FilePath] -> [FilePath]
includeDirectories cdir wdir dirs = uniqueAndSort (extdirs ++ [cdir,wdir])
  where
    extdirs = map expand $ dirs ++ cabalBuildDirs
    expand "."    = cdir
    expand subdir = cdir </> subdir

----------------------------------------------------------------

-- | Parse a cabal file and return a 'PackageDescription'.
parseCabalFile :: (IOish m, GmError m, GmLog m)
               => CabalConfig
               -> FilePath
               -> m PackageDescription
parseCabalFile  config file = do
    cid <- mkGHCCompilerId <$> liftIO getGHCVersion
    epgd <- liftIO $ readPackageDescription silent file
    flags <- cabalConfigFlags config
    case toPkgDesc cid flags epgd of
        Left deps    -> fail $ show deps ++ " are not installed"
        Right (pd,_) -> if nullPkg pd
                        then fail $ file ++ " is broken"
                        else return pd
  where
    toPkgDesc cid flags =
        finalizePackageDescription flags (const True) buildPlatform cid []
    nullPkg pd = name == ""
      where
        PackageName name = C.pkgName (P.package pd)

getGHCVersion :: IO Version
getGHCVersion = do
    mv <- programFindVersion C.ghcProgram silent (programName C.ghcProgram)
    case mv of
      -- TODO: MonadError it up
        Nothing -> E.throwIO $ userError "ghc not found"
        Just v  -> return v

----------------------------------------------------------------

getGHCOptions :: [GHCOption] -> Cradle -> FilePath -> BuildInfo -> IO [GHCOption]
getGHCOptions ghcopts cradle rdir binfo = do
    cabalCpp <- cabalCppOptions rdir
    let cpps = map ("-optP" ++) $ P.cppOptions binfo ++ cabalCpp
    return $ ghcopts ++ pkgDb ++ exts ++ [lang] ++ libs ++ libDirs ++ cpps
  where
    pkgDb = ghcDbStackOpts $ cradlePkgDbStack cradle
    lang = maybe "-XHaskell98" (("-X" ++) . display) $ P.defaultLanguage binfo
    libDirs = map ("-L" ++) $ P.extraLibDirs binfo
    exts = map (("-X" ++) . display) $ P.usedExtensions binfo
    libs = map ("-l" ++) $ P.extraLibs binfo

cabalCppOptions :: FilePath -> IO [String]
cabalCppOptions dir = do
    exist <- doesFileExist cabalMacro
    return $ if exist then
        ["-include", cabalMacro]
      else
        []
  where
    cabalMacro = dir </> "dist/build/autogen/cabal_macros.h"

----------------------------------------------------------------

-- | Extracting all 'BuildInfo' for libraries, executables, and tests.
cabalAllBuildInfo :: PackageDescription -> [BuildInfo]
cabalAllBuildInfo pd = libBI ++ execBI ++ testBI ++ benchBI
  where
    libBI   = map P.libBuildInfo       $ maybeToList $ P.library pd
    execBI  = map P.buildInfo          $ P.executables pd
    testBI  = map P.testBuildInfo      $ P.testSuites pd
    benchBI = benchmarkBuildInfo pd

----------------------------------------------------------------

-- | Extracting include directories for modules.
cabalSourceDirs :: [BuildInfo] -> [IncludeDir]
cabalSourceDirs bis = uniqueAndSort $ concatMap P.hsSourceDirs bis

----------------------------------------------------------------

uniqueAndSort :: [String] -> [String]
uniqueAndSort = toList . fromList
