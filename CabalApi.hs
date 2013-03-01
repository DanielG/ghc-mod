module CabalApi (
    cabalParseFile
  , cabalBuildInfo
  , cabalDependPackages
  , getBuildInfos
  ) where

import Control.Applicative

import Data.Maybe (fromJust, maybeToList)
import Data.Set (fromList, toList)

import Distribution.Package (Dependency(Dependency), PackageName(PackageName))
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (silent)

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

cabalDependPackages :: GenericPackageDescription -> [String]
cabalDependPackages = uniqueAndSort . map getDependencyPackageName . allDependsOfDescription
  where
    uniqueAndSort = toList . fromList

allDependsOfDescription :: GenericPackageDescription -> [Dependency]
allDependsOfDescription = fromPackageDescription getDeps getDeps getDeps getDeps
  where
    getDeps :: [Tree a] -> [Dependency]
    getDeps = concatMap condTreeConstraints

getDependencyPackageName :: Dependency -> String
getDependencyPackageName (Dependency (PackageName n) _) = n

----------------------------------------------------------------

getBuildInfos :: GenericPackageDescription -> [BuildInfo]
getBuildInfos = fromPackageDescription f1 f2 f3 f4
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
