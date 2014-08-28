{-# LANGUAGE OverloadedStrings, CPP #-}

module Language.Haskell.GhcMod.CabalApi (
    getCompilerOptions
  , parseCabalFile
  , cabalAllBuildInfo
  , cabalDependPackages
  , cabalSourceDirs
  , cabalAllTargets
  , cabalConfigDependencies
  ) where

import Language.Haskell.GhcMod.CabalConfig
import Language.Haskell.GhcMod.Error
import Language.Haskell.GhcMod.Gap (benchmarkBuildInfo, benchmarkTargets,
                                    toModuleString)
import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.Types

import MonadUtils (MonadIO, liftIO)
import Control.Applicative ((<$>))
import qualified Control.Exception as E
import Control.Monad (filterM)
import Data.Maybe (maybeToList)
import Data.Set (fromList, toList)
import Distribution.Package (Dependency(Dependency)
                           , PackageName(PackageName))
import qualified Distribution.Package as C
import Distribution.PackageDescription (PackageDescription, BuildInfo, TestSuite, TestSuiteInterface(..), Executable)
import qualified Distribution.PackageDescription as P
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Compiler (CompilerId(..), CompilerFlavor(..))
import Distribution.Simple.Program (ghcProgram)
import Distribution.Simple.Program.Types (programName, programFindVersion)
import Distribution.System (buildPlatform)
import Distribution.Text (display)
import Distribution.Verbosity (silent)
import Distribution.Version (Version)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
----------------------------------------------------------------

-- | Getting necessary 'CompilerOptions' from three information sources.
getCompilerOptions :: (IOish m, MonadError GhcModError m)
                   => [GHCOption]
                   -> Cradle
                   -> PackageDescription
                   -> m CompilerOptions
getCompilerOptions ghcopts cradle pkgDesc = do
    gopts <- liftIO $ getGHCOptions ghcopts cradle rdir $ head buildInfos
    depPkgs <- cabalConfigDependencies cradle (C.packageId pkgDesc)
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
parseCabalFile :: (MonadIO m, Error e,  MonadError e m)
               => FilePath
               -> m PackageDescription
parseCabalFile file = do
    cid <- liftIO getGHCId
    epgd <- liftIO $ readPackageDescription silent file
    case toPkgDesc cid epgd of
        Left deps    -> fail $ show deps ++ " are not installed"
        Right (pd,_) -> if nullPkg pd
                        then fail $ file ++ " is broken"
                        else return pd
  where
    toPkgDesc cid =
        finalizePackageDescription [] (const True) buildPlatform cid []
    nullPkg pd = name == ""
      where
        PackageName name = C.pkgName (P.package pd)

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

-- | Extracting package names of dependency.
cabalDependPackages :: [BuildInfo] -> [PackageBaseName]
cabalDependPackages bis = uniqueAndSort pkgs
  where
    pkgs = map getDependencyPackageName $ concatMap P.targetBuildDepends bis
    getDependencyPackageName (Dependency (PackageName nm) _) = nm

----------------------------------------------------------------

-- | Extracting include directories for modules.
cabalSourceDirs :: [BuildInfo] -> [IncludeDir]
cabalSourceDirs bis = uniqueAndSort $ concatMap P.hsSourceDirs bis

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
        Nothing -> E.throwIO $ userError "ghc not found"
        Just v  -> return v

----------------------------------------------------------------

-- | Extracting all 'Module' 'FilePath's for libraries, executables,
-- tests and benchmarks.
cabalAllTargets :: PackageDescription -> IO ([String],[String],[String],[String])
cabalAllTargets pd = do
    exeTargets  <- mapM getExecutableTarget $ P.executables pd
    testTargets <- mapM getTestTarget $ P.testSuites pd
    return (libTargets,concat exeTargets,concat testTargets,benchTargets)
  where
    lib = case P.library pd of
            Nothing -> []
            Just l -> P.libModules l

    libTargets = map toModuleString lib
    benchTargets = benchmarkTargets pd

    getTestTarget :: TestSuite -> IO [String]
    getTestTarget ts =
       case P.testInterface ts of
        (TestSuiteExeV10 _ filePath) -> do
          let maybeTests = [p </> e | p <- P.hsSourceDirs $ P.testBuildInfo ts, e <- [filePath]]
          liftIO $ filterM doesFileExist maybeTests
        (TestSuiteLibV09 _ moduleName) -> return [toModuleString moduleName]
        (TestSuiteUnsupported _)       -> return []

    getExecutableTarget :: Executable -> IO [String]
    getExecutableTarget exe = do
      let maybeExes = [p </> e | p <- P.hsSourceDirs $ P.buildInfo exe, e <- [P.modulePath exe]]
      liftIO $ filterM doesFileExist maybeExes
