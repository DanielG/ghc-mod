{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.GhcMod.CabalApi (
    getCompilerOptions
  , parseCabalFile
  , cabalAllBuildInfo
  , cabalDependPackages
  , cabalSourceDirs
  , cabalAllTargets
  ) where

import Control.Applicative ((<$>))
import Control.Exception (throwIO)
import Control.Monad (filterM)
import CoreMonad (liftIO)
import Data.Maybe (maybeToList)
import Data.Set (fromList, toList)
import Distribution.ModuleName (ModuleName,toFilePath)
import Distribution.Package (Dependency(Dependency)
                           , PackageName(PackageName)
                           , PackageIdentifier(pkgName))
import Distribution.PackageDescription
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Compiler (CompilerId(..), CompilerFlavor(..))
import Distribution.Simple.Program (ghcProgram)
import Distribution.Simple.Program.Types (programName, programFindVersion)
import Distribution.System (buildPlatform)
import Distribution.Text (display)
import Distribution.Verbosity (silent)
import Distribution.Version (Version)
import Language.Haskell.GhcMod.Types
import System.Directory (doesFileExist)
import System.FilePath

----------------------------------------------------------------

-- | Getting necessary 'CompilerOptions' from three information sources.
getCompilerOptions :: [GHCOption] -> Cradle -> PackageDescription -> IO CompilerOptions
getCompilerOptions ghcopts cradle pkgDesc = do
    gopts <- getGHCOptions ghcopts cradle cdir $ head buildInfos
    return $ CompilerOptions gopts idirs depPkgs
  where
    wdir       = cradleCurrentDir cradle
    Just cdir  = cradleCabalDir   cradle
    Just cfile = cradleCabalFile  cradle
    buildInfos = cabalAllBuildInfo pkgDesc
    idirs      = includeDirectories cdir wdir $ cabalSourceDirs buildInfos
    depPkgs    = removeThem problematicPackages $ removeMe cfile $ cabalDependPackages buildInfos

----------------------------------------------------------------
-- Dependent packages

removeMe :: FilePath -> [Package] -> [Package]
removeMe cabalfile = filter (/= me)
  where
    me = dropExtension $ takeFileName cabalfile

removeThem :: [Package] -> [Package] -> [Package]
removeThem badpkgs = filter (`notElem` badpkgs)

problematicPackages :: [Package]
problematicPackages = [
    "base-compat" -- providing "Prelude"
  ]

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

-- | Parsing a cabal file and returns 'PackageDescription'.
--   'IOException' is thrown if parsing fails.
parseCabalFile :: FilePath -> IO PackageDescription
parseCabalFile file = do
    cid <- getGHCId
    epgd <- readPackageDescription silent file
    case toPkgDesc cid epgd of
        Left deps    -> throwIO $ userError $ show deps ++ " are not installed"
        Right (pd,_) -> if nullPkg pd
                        then throwIO $ userError $ file ++ " is broken"
                        else return pd
  where
    toPkgDesc cid = finalizePackageDescription [] (const True) buildPlatform cid []
    nullPkg pd = name == ""
      where
        PackageName name = pkgName (package pd)

----------------------------------------------------------------

getGHCOptions :: [GHCOption] -> Cradle -> FilePath -> BuildInfo -> IO [GHCOption]
getGHCOptions ghcopts cradle cdir binfo = do
    cabalCpp <- cabalCppOptions cdir
    let cpps = map ("-optP" ++) $ cppOptions binfo ++ cabalCpp
    return $ ghcopts ++ pkgDb ++ exts ++ [lang] ++ libs ++ libDirs ++ cpps
  where
    pkgDb = cradlePackageDbOpts cradle
    lang = maybe "-XHaskell98" (("-X" ++) . display) $ defaultLanguage binfo
    libDirs = map ("-L" ++) $ extraLibDirs binfo
    exts = map (("-X" ++) . display) $ usedExtensions binfo
    libs = map ("-l" ++) $ extraLibs binfo

cabalCppOptions :: FilePath -> IO [String]
cabalCppOptions dir = do
    exist <- doesFileExist cabalMacro
    if exist then
        return ["-include", cabalMacro]
      else
        return []
  where
    cabalMacro = dir </> "dist/build/autogen/cabal_macros.h"

----------------------------------------------------------------

-- | Extracting all 'BuildInfo' for libraries, executables, tests and benchmarks.
cabalAllBuildInfo :: PackageDescription -> [BuildInfo]
cabalAllBuildInfo pd = libBI ++ execBI ++ testBI ++ benchBI
  where
    libBI   = map libBuildInfo       $ maybeToList $ library pd
    execBI  = map buildInfo          $ executables pd
    testBI  = map testBuildInfo      $ testSuites pd
    benchBI = map benchmarkBuildInfo $ benchmarks pd

----------------------------------------------------------------

-- | Extracting package names of dependency.
cabalDependPackages :: [BuildInfo] -> [Package]
cabalDependPackages bis = uniqueAndSort $ pkgs
  where
    pkgs = map getDependencyPackageName $ concatMap targetBuildDepends bis
    getDependencyPackageName (Dependency (PackageName nm) _) = nm

----------------------------------------------------------------

-- | Extracting include directories for modules.
cabalSourceDirs :: [BuildInfo] -> [IncludeDir]
cabalSourceDirs bis = uniqueAndSort $ concatMap hsSourceDirs bis

----------------------------------------------------------------

uniqueAndSort :: [String] -> [String]
uniqueAndSort = toList . fromList

----------------------------------------------------------------

getGHCId :: IO CompilerId
getGHCId = CompilerId GHC <$> getGHC

getGHC :: IO Version
getGHC = do
    mv <- programFindVersion ghcProgram silent (programName ghcProgram)
    case mv of
        Nothing -> throwIO $ userError "ghc not found"
        Just v  -> return $ v

----------------------------------------------------------------

-- | Extracting all 'Module' 'FilePath's for libraries, executables,
-- tests and benchmarks.
cabalAllTargets :: PackageDescription -> IO ([String],[String],[String],[String])
cabalAllTargets pd = do
    exeTargets  <- mapM getExecutableTarget $ executables pd
    testTargets <- mapM getTestTarget $ testSuites pd
    return (libTargets,concat exeTargets,concat testTargets,benchTargets)
  where
    lib = case library pd of
            Nothing -> []
            Just l -> libModules l

    libTargets = map toModuleString $ lib
    benchTargets = map toModuleString $ concatMap benchmarkModules $ benchmarks  pd

    toModuleString :: ModuleName -> String
    toModuleString mn = fromFilePath $ toFilePath mn

    fromFilePath :: FilePath -> String
    fromFilePath fp = map (\c -> if c=='/' then '.' else c) fp

    getTestTarget :: TestSuite -> IO [String]
    getTestTarget ts =
       case testInterface ts of
        (TestSuiteExeV10 _ filePath) -> do
          let maybeTests = [p </> e | p <- hsSourceDirs $ testBuildInfo ts, e <- [filePath]]
          liftIO $ filterM doesFileExist maybeTests
        (TestSuiteLibV09 _ moduleName) -> return [toModuleString moduleName]
        (TestSuiteUnsupported _)       -> return []

    getExecutableTarget :: Executable -> IO [String]
    getExecutableTarget exe = do
      let maybeExes = [p </> e | p <- hsSourceDirs $ buildInfo exe, e <- [modulePath exe]]
      liftIO $ filterM doesFileExist maybeExes

