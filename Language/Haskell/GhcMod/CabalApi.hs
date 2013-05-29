{-# LANGUAGE OverloadedStrings #-}

module Language.Haskell.GhcMod.CabalApi (
    fromCabalFile
  , cabalParseFile
  , cabalAllDependPackages
  , cabalAllSourceDirs
  , getGHCVersion
  ) where

import Control.Applicative ((<$>))
import Control.Exception (throwIO)
import Data.List (intercalate)
import Data.Maybe (maybeToList)
import Data.Set (fromList, toList)
import Distribution.Package (Dependency(Dependency), PackageName(PackageName))
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

fromCabalFile :: [GHCOption]
              -> Cradle
              -> IO ([GHCOption],[IncludeDir],[Package])
fromCabalFile ghcOptions cradle = do
    cabal <- cabalParseFile cfile
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
    gopts      = getGHCOptions ghcOptions $ head buildInfos -- FIXME
    idirs      = includeDirectories cdir wdir $ cabalAllSourceDirs buildInfos
    depPkgs    = removeMe cfile $ cabalAllDependPackages buildInfos

removeMe :: FilePath -> [String] -> [String]
removeMe cabalfile = filter (/= me)
  where
    me = dropExtension $ takeFileName cabalfile

includeDirectories :: String -> String -> [FilePath] -> [String]
includeDirectories cdir wdir []   = uniqueAndSort [cdir,wdir]
includeDirectories cdir wdir dirs = uniqueAndSort (map (cdir </>) dirs ++ [cdir,wdir])

----------------------------------------------------------------

cabalParseFile :: FilePath -> IO PackageDescription
cabalParseFile file = do
    cid <- getGHCId
    epgd <- readPackageDescription silent file
    case toPkgDesc cid epgd of
        Left _       -> throwIO $ userError "cabal file is broken"
        Right (pd,_) -> return pd -- FIXME check empty
  where
    toPkgDesc cid = finalizePackageDescription [] (const True) buildPlatform cid []

----------------------------------------------------------------

getGHCOptions :: [String] -> BuildInfo -> [String]
getGHCOptions ghcOptions binfo = ghcOptions ++ exts ++ [lang] ++ libs ++ libDirs
  where
    exts = map (("-X" ++) . display) $ usedExtensions binfo
    lang = maybe "-XHaskell98" (("-X" ++) . display) $ defaultLanguage binfo
    libs = map ("-l" ++) $ extraLibs binfo
    libDirs = map ("-L" ++) $ extraLibDirs binfo

----------------------------------------------------------------

cabalAllBuildInfo :: PackageDescription -> [BuildInfo]
cabalAllBuildInfo pd = libBI ++ execBI ++ testBI ++ benchBI
  where
    libBI   = map libBuildInfo       $ maybeToList $ library pd
    execBI  = map buildInfo          $ executables pd
    testBI  = map testBuildInfo      $ testSuites pd
    benchBI = map benchmarkBuildInfo $ benchmarks pd

----------------------------------------------------------------

cabalAllSourceDirs :: [BuildInfo] -> [FilePath]
cabalAllSourceDirs bis = uniqueAndSort $ concatMap hsSourceDirs bis

----------------------------------------------------------------

cabalAllDependPackages :: [BuildInfo] -> [Package]
cabalAllDependPackages bis = uniqueAndSort $ pkgs
  where
    pkgs = map getDependencyPackageName $ concatMap targetBuildDepends bis
    getDependencyPackageName (Dependency (PackageName nm) _) = nm

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
