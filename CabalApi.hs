module CabalApi (dependPackages) where

import Data.Maybe (maybeToList)
import Data.Set (fromList, toList)

import Distribution.Verbosity (silent)
import Distribution.Package (Dependency(Dependency), PackageName(PackageName))
import Distribution.PackageDescription
  (GenericPackageDescription,
   condLibrary, condExecutables, condTestSuites, condBenchmarks,
   CondTree, condTreeConstraints)
import Distribution.PackageDescription.Parse (readPackageDescription)

----------------------------------------------------------------

parseGenericDescription :: FilePath -> IO GenericPackageDescription
parseGenericDescription =  readPackageDescription silent

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

dependPackages :: FilePath -> IO [String]
dependPackages =
  fmap (toList . fromList
        . map getDependencyPackageName
        . allDependsOfDescription)
  . parseGenericDescription
