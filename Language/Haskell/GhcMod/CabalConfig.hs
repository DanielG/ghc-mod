-- | Reading cabal @dist/setup-config@
module Language.Haskell.GhcMod.CabalConfig (
    CabalConfig
  , cabalConfigDependencies
  ) where

import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.Utils
import Language.Haskell.GhcMod.Types

import qualified Language.Haskell.GhcMod.Cabal16 as C16
import qualified Language.Haskell.GhcMod.Cabal18 as C18

import qualified Control.Exception as E
import Control.Applicative ((<$>))
import Control.Monad (filterM,mplus)
import Control.Monad.Error ()
import Data.Maybe ()
import Data.Set ()
import Data.List (find,tails,isPrefixOf,isInfixOf,nub,stripPrefix)
import Distribution.Package (PackageName(PackageName)
                           , InstalledPackageId(..)
                           , PackageIdentifier)
import qualified Distribution.Package as C
import Distribution.Simple.BuildPaths (defaultDistPref)
import Distribution.Simple.Configure (localBuildInfoFile)
import Distribution.Simple.LocalBuildInfo (ComponentName)
import System.FilePath ((</>))
import Text.Read (readMaybe)
----------------------------------------------------------------

type CabalConfig = String

-- | Get file containing 'LocalBuildInfo' data. If it doesn't exist run @cabal
-- configure@ i.e. configure with default options like @cabal build@ would do.
getConfig :: Cradle -> IO CabalConfig
getConfig cradle =
    readFile path `E.catch` (\(E.SomeException _) -> configure >> readFile path)
 where
   prjDir = cradleRootDir cradle
   path = prjDir </> configPath
   configure =
     withDirectory_ prjDir $ readProcess' "cabal" ["configure"]


-- | Path to 'LocalBuildInfo' file, usually @dist/setup-config@
configPath :: FilePath
configPath = localBuildInfoFile defaultDistPref

cabalConfigDependencies :: Cradle -> PackageIdentifier -> IO [Package]
cabalConfigDependencies cradle thisPkg =
    configDependencies thisPkg <$> getConfig cradle

configDependencies :: PackageIdentifier -> CabalConfig -> [Package]
configDependencies thisPkg config = map fromInstalledPackageId deps
 where
    deps :: [InstalledPackageId]
    deps = case (deps18 `mplus` deps16) of
        Right ps -> ps
        Left msg -> error msg

--    errorExtract = error $
--     "cabalConfigDependencies: Error extracting dependencies from setup-config"

    -- Cabal >= 1.18
    deps18 :: Either String [InstalledPackageId]
    deps18 =
          concatMap (map fst . C18.componentPackageDeps . lbi)
      <$> (readEither =<< extractField config "componentsConfigs")

    lbi :: (ComponentName, C18.ComponentLocalBuildInfo, [ComponentName])
        -> C18.ComponentLocalBuildInfo
    lbi (_,i,_) = i

    -- Cabal 1.16 and below
    deps16 :: Either String [InstalledPackageId]
    deps16 = map fst <$> filter (not . internal . snd) . nub <$> do
        cbi <- concat <$> sequence [ extract "executableConfigs"
                                   , extract "testSuiteConfigs"
                                   , extract "benchmarkConfigs" ]
                        :: Either String [(String, C16.ComponentLocalBuildInfo)]

        return $ maybe [] C16.componentPackageDeps libraryConfig
              ++ concatMap (C16.componentPackageDeps . snd) cbi
     where
       -- True if this dependency is an internal one (depends on the library
       -- defined in the same package).
       internal pkgid = pkgid == thisPkg

       libraryConfig :: Maybe C16.ComponentLocalBuildInfo
       libraryConfig = do
         field <- find ("libraryConfig" `isPrefixOf`) (tails config)
         clbi <- stripPrefix " = " field
         if "Nothing" `isPrefixOf` clbi
             then Nothing
             else case readMaybe <$> stripPrefix "Just " clbi of
                    Just x -> x
                    Nothing -> error $ "reading libraryConfig failed\n" ++ show (stripPrefix "Just " clbi)

       extract :: String -> Either String [(String, C16.ComponentLocalBuildInfo)]
       extract field = readConfigs field <$> extractField config field

       readConfigs :: String -> String -> [(String, C16.ComponentLocalBuildInfo)]
       readConfigs f s = case readMaybe s of
           Just x -> x
           Nothing -> error $ "reading config " ++ f ++ " failed"


readEither :: Read r => String -> Either String r
readEither s = case readMaybe s of
    Just x -> Right x
    Nothing -> Left $ "read: failed on input:\n" ++ s

extractField :: CabalConfig -> String -> Either String String
extractField config field =
    case extractParens <$> find (field `isPrefixOf`) (tails config) of
        Just f -> Right f
        Nothing -> Left $ "extractField: failed extracting "++field++" from input, input contained `"++field++"'? " ++ show (field `isInfixOf` config)
