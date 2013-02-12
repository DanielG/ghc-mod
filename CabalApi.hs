module CabalApi (
  cabalParseFile,
  cabalBuildInfo,
  cabalDependPackages
  ) where

import Control.Applicative

import Data.Maybe (fromJust, maybeToList)
import Data.Set (fromList, toList)

import Distribution.Verbosity (silent)
import Distribution.Package (Dependency(Dependency), PackageName(PackageName))
import Distribution.PackageDescription
  (GenericPackageDescription,
   condLibrary, condExecutables, condTestSuites, condBenchmarks,
   BuildInfo, libBuildInfo, buildInfo,
   CondTree, condTreeConstraints, condTreeData)
import Distribution.PackageDescription.Parse (readPackageDescription)

----------------------------------------------------------------

cabalParseFile :: FilePath -> IO GenericPackageDescription
cabalParseFile =  readPackageDescription silent

-- Causes error, catched in the upper function.
cabalBuildInfo :: GenericPackageDescription -> IO BuildInfo
cabalBuildInfo pd =
    return . fromJust $ fromLibrary pd <|> fromExecutable pd
  where
    fromLibrary c     = libBuildInfo . condTreeData <$> condLibrary c
    fromExecutable c  = buildInfo . condTreeData . snd <$> toMaybe (condExecutables c)
    toMaybe [] = Nothing
    toMaybe (x:_) = Just x

getDepsOfPairs :: [(a1, CondTree v [b] a)] -> [b]
getDepsOfPairs =  concatMap (condTreeConstraints . snd)

allDependsOfDescription :: GenericPackageDescription -> [Dependency]
allDependsOfDescription pd =
  concat [depLib, depExe, depTests, depBench]
  where
    depLib   = concatMap condTreeConstraints (maybeToList . condLibrary $ pd)
    depExe   = getDepsOfPairs . condExecutables $ pd
    depTests = getDepsOfPairs . condTestSuites  $ pd
    depBench = getDepsOfPairs . condBenchmarks  $ pd

getDependencyPackageName :: Dependency -> String
getDependencyPackageName (Dependency (PackageName n) _) = n

cabalDependPackages :: GenericPackageDescription -> IO [String]
cabalDependPackages =
  return . toList . fromList
  . map getDependencyPackageName
  . allDependsOfDescription
