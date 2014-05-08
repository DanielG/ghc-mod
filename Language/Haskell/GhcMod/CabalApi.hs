{-# LANGUAGE OverloadedStrings, CPP #-}

module Language.Haskell.GhcMod.CabalApi (
    getCompilerOptions
  , parseCabalFile
  , cabalAllBuildInfo
  , cabalDependPackages
  , cabalSourceDirs
  , cabalAllTargets
  , cabalGetConfig
  , cabalConfigPath
  , cabalConfigDependencies
  ) where

import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.Utils

import Control.Applicative ((<$>))
import Control.Exception (SomeException(..))
import qualified Control.Exception as E
import Control.Monad (filterM)
import CoreMonad (liftIO)
import Data.Maybe (maybeToList)
import Data.Set (fromList, toList)
import Data.List (find,tails,isPrefixOf)
#if !MIN_VERSION_Cabal(1,18,0)
import Data.List (nub,stripPrefix)
#endif
import Distribution.ModuleName (ModuleName,toFilePath)
import Distribution.Package (Dependency(Dependency)
                           , PackageName(PackageName)
                           , InstalledPackageId(..)
                           , PackageIdentifier)
import qualified Distribution.Package as C
import Distribution.PackageDescription (PackageDescription, BuildInfo, TestSuite, TestSuiteInterface(..), Executable)
import qualified Distribution.PackageDescription as P
import Distribution.PackageDescription.Configuration (finalizePackageDescription)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Simple.Compiler (CompilerId(..), CompilerFlavor(..))
import Distribution.Simple.Program (ghcProgram)
import Distribution.Simple.Program.Types (programName, programFindVersion)
import Distribution.Simple.BuildPaths (defaultDistPref)
import Distribution.Simple.Configure (localBuildInfoFile)
import Distribution.Simple.LocalBuildInfo (ComponentLocalBuildInfo(..), ComponentName)
import Distribution.System (buildPlatform)
import Distribution.Text (display)
import Distribution.Verbosity (silent)
import Distribution.Version (Version)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
----------------------------------------------------------------

-- | Getting necessary 'CompilerOptions' from three information sources.
getCompilerOptions :: [GHCOption]
                   -> Cradle
                   -> PackageDescription
                   -> IO CompilerOptions
getCompilerOptions ghcopts cradle pkgDesc = do
    gopts <- getGHCOptions ghcopts cradle rdir $ head buildInfos
    depPkgs <- cabalConfigDependencies (C.packageId pkgDesc) <$> cabalGetConfig cradle
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

-- | Parsing a cabal file and returns 'PackageDescription'.
--   'IOException' is thrown if parsing fails.
parseCabalFile :: FilePath -> IO PackageDescription
parseCabalFile file = do
    cid <- getGHCId
    epgd <- readPackageDescription silent file
    case toPkgDesc cid epgd of
        Left deps    -> E.throwIO $ userError $ show deps ++ " are not installed"
        Right (pd,_) -> if nullPkg pd
                        then E.throwIO $ userError $ file ++ " is broken"
                        else return pd
  where
    toPkgDesc cid = finalizePackageDescription [] (const True) buildPlatform cid []
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
#if __GLASGOW_HASKELL__ >= 704
    benchBI = map P.benchmarkBuildInfo $ P.benchmarks pd
#else
    benchBI = []
#endif

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
#if __GLASGOW_HASKELL__ >= 704
    benchTargets = map toModuleString $ concatMap P.benchmarkModules $ P.benchmarks  pd
#else
    benchTargets = []
#endif
    toModuleString :: ModuleName -> String
    toModuleString mn = fromFilePath $ toFilePath mn

    fromFilePath :: FilePath -> String
    fromFilePath fp = map (\c -> if c=='/' then '.' else c) fp

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

----------------------------------------------------------------

type CabalConfig = String

-- | Get file containing 'LocalBuildInfo' data. If it doesn't exist run @cabal
-- configure@ i.e. configure with default options like @cabal build@ would do.
cabalGetConfig :: Cradle -> IO CabalConfig
cabalGetConfig cradle =
    readFile path `E.catch` (\(SomeException _) -> configure >> readFile path)
 where
   prjDir = cradleRootDir cradle
   path = prjDir </> cabalConfigPath
   configure =
     withDirectory_ prjDir $ readProcess' "cabal" ["configure"]


-- | Path to 'LocalBuildInfo' file, usually @dist/setup-config@
cabalConfigPath :: FilePath
cabalConfigPath = localBuildInfoFile defaultDistPref

cabalConfigDependencies :: PackageIdentifier -> CabalConfig -> [Package]
cabalConfigDependencies thisPkg config = cfgDepends
 where
    pids :: [InstalledPackageId]
    pids = fst <$> components thisPkg
    cfgDepends = filter (("inplace" /=) . pkgId)
                   $ fromInstalledPackageId <$> pids

    errorExtract f = error $
        "cabalConfigDependencies: Extracting field `"++ f ++"' from"
     ++ " setup-config failed"

    components :: PackageIdentifier -> [(InstalledPackageId,PackageIdentifier)]
#if MIN_VERSION_Cabal(1,18,0)
    components _ = case extractCabalSetupConfig config "componentsConfigs" of
        Just comps -> concatMap (componentPackageDeps . lbi) comps
        Nothing -> errorExtract "componentsConfigs"

    lbi :: (ComponentName, ComponentLocalBuildInfo, [ComponentName])
        -> ComponentLocalBuildInfo
    lbi (_,i,_) = i
#elif MIN_VERSION_Cabal(1,16,0)
    components thisPkg' = filter (not . internal . snd) $ nub $
        maybe [] componentPackageDeps libraryConfig
        ++ concatMap (componentPackageDeps . snd) executableConfigs
        ++ concatMap (componentPackageDeps . snd) testSuiteConfigs
        ++ concatMap (componentPackageDeps . snd) benchmarkConfigs
     where
       -- True if this dependency is an internal one (depends on the library
       -- defined in the same package).
       internal pkgid = pkgid == thisPkg'

       executableConfigs :: [(String, ComponentLocalBuildInfo)]
       executableConfigs = extract "executableConfigs"

       testSuiteConfigs :: [(String, ComponentLocalBuildInfo)]
       testSuiteConfigs = extract "testSuiteConfigs"

       benchmarkConfigs ::[(String, ComponentLocalBuildInfo)]
       benchmarkConfigs = extract "benchmarkConfigs"

       libraryConfig :: Maybe ComponentLocalBuildInfo
       libraryConfig = do
         field <- find ("libraryConfig" `isPrefixOf`) (tails config)
         clbi <- stripPrefix " = " field
         if "Nothing" `isPrefixOf` clbi
             then Nothing
             else read <$> stripPrefix "Just " clbi

       extract :: Read r => String -> r
       extract field = case extractCabalSetupConfig config field of
           Nothing -> errorExtract field
           Just f -> f
#endif

-- | Extract part of cabal's @setup-config@, this is done with a mix of manual
-- string processing and use of 'read'. This way we can extract a field from
-- 'LocalBuildInfo' without having to parse the whole thing which would mean
-- depending on the exact version of Cabal used to configure the project as it
-- is rather likley that some part of 'LocalBuildInfo' changed.
--
-- Right now 'extractCabalSetupConfig' can only deal with Lists and Tuples in
-- the field!
extractCabalSetupConfig :: (Read r) => CabalConfig -> String -> Maybe r
extractCabalSetupConfig config field = do
    read <$> extractParens <$> find (field `isPrefixOf`) (tails config)
