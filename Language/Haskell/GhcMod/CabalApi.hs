{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.GhcMod.CabalApi (
    fromCabalFile
  , parseCabalFile
  , cabalAllBuildInfo
  , cabalDependPackages
  , cabalSourceDirs
  , getGHCVersion
  ) where

import Control.Applicative ((<$>))
import Control.Exception (throwIO)
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Data.Set (fromList, toList)
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
import Distribution.Version (versionBranch, Version)
import Language.Haskell.GhcMod.Types
import System.FilePath

----------------------------------------------------------------

-- | Parsing a cabal file in 'Cradle' and returns
--   options for GHC, include directories for modules and
--   package names of dependency.
fromCabalFile :: [GHCOption]
              -> Cradle
              -> IO ([GHCOption],[IncludeDir],[Package])
fromCabalFile ghcOptions cradle = do
    cabal <- parseCabalFile cfile
    return $ cookInfo ghcOptions cradle cabal
  where
    Just cfile = cradleCabalFile cradle

cookInfo :: [String] -> Cradle -> PackageDescription
         -> ([GHCOption],[IncludeDir],[Package])
cookInfo ghcOptions cradle cabal = (gopts,idirs,depPkgs)
  where
    wdir       = cradleCurrentDir cradle
    Just cdir  = cradleCabalDir   cradle
    Just cfile = cradleCabalFile  cradle
    buildInfos = cabalAllBuildInfo cabal
    gopts      = getGHCOptions ghcOptions $ head buildInfos
    idirs      = includeDirectories cdir wdir $ cabalSourceDirs buildInfos
    depPkgs    = removeThem problematicPackages $ removeMe cfile $ cabalDependPackages buildInfos

removeMe :: FilePath -> [String] -> [String]
removeMe cabalfile = filter (/= me)
  where
    me = dropExtension $ takeFileName cabalfile

removeThem :: [String] -> [String] -> [String]
removeThem badpkgs = filter (`notElem` badpkgs)

problematicPackages :: [String]
problematicPackages = [
    "base-compat" -- providing "Prelude"
  ]

includeDirectories :: String -> String -> [FilePath] -> [String]
includeDirectories cdir wdir []   = uniqueAndSort [cdir,wdir]
includeDirectories cdir wdir dirs = uniqueAndSort (map (cdir </>) dirs ++ [cdir,wdir])

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

getGHCOptions :: [String] -> BuildInfo -> [String]
getGHCOptions ghcOptions binfo = ghcOptions ++ exts ++ [lang] ++ libs ++ libDirs
  where
    exts = map (("-X" ++) . display) $ usedExtensions binfo
    lang = maybe "-XHaskell98" (("-X" ++) . display) $ defaultLanguage binfo
    libs = map ("-l" ++) $ extraLibs binfo
    libDirs = map ("-L" ++) $ extraLibDirs binfo

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

-- | Getting GHC version. 7.6.3 becames 706 in the second of the result.
getGHCVersion :: IO (GHCVersion, Int)
getGHCVersion = toTupple <$> getGHC
  where
    toTupple v
      | length vs < 2 = (verstr, 0)
      | otherwise     = (verstr, ver)
      where
        vs = versionBranch v
        ver = (vs !! 0) * 100 + (vs !! 1)
        verstr = intercalate "." . map show $ vs

getGHCId :: IO CompilerId
getGHCId = CompilerId GHC <$> getGHC

getGHC :: IO Version
getGHC = do
    mv <- programFindVersion ghcProgram silent (programName ghcProgram)
    case mv of
        Nothing -> throwIO $ userError "ghc not found"
        Just v  -> return $ v
