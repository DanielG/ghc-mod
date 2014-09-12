{-# LANGUAGE CPP #-}

-- | This module facilitates extracting information from Cabal's on-disk
-- 'LocalBuildInfo' (@dist/setup-config@).
module Language.Haskell.GhcMod.CabalConfig (
    CabalConfig
  , cabalConfigDependencies
  , cabalConfigFlags
  ) where

import Language.Haskell.GhcMod.Error
import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.Utils
import Language.Haskell.GhcMod.Read
import Language.Haskell.GhcMod.Types

import qualified Language.Haskell.GhcMod.Cabal16 as C16
import qualified Language.Haskell.GhcMod.Cabal18 as C18
import qualified Language.Haskell.GhcMod.Cabal21 as C21

#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x,y,z) 1
#endif

import Control.Applicative ((<$>))
import Control.Monad (mplus)
#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except ()
#else
import Control.Monad.Error ()
#endif
import Data.Maybe ()
import Data.Set ()
import Data.List (find,tails,isPrefixOf,isInfixOf,nub,stripPrefix)
import Distribution.Package (InstalledPackageId(..)
                           , PackageIdentifier(..)
                           , PackageName(..))
import Distribution.PackageDescription (FlagAssignment)
import Distribution.Simple.BuildPaths (defaultDistPref)
import Distribution.Simple.Configure (localBuildInfoFile)
import Distribution.Simple.LocalBuildInfo (ComponentName)
import MonadUtils (liftIO)
import System.FilePath ((</>))

----------------------------------------------------------------

-- | 'Show'ed cabal 'LocalBuildInfo' string
type CabalConfig = String

-- | Get contents of the file containing 'LocalBuildInfo' data. If it doesn't
-- exist run @cabal configure@ i.e. configure with default options like @cabal
-- build@ would do.
getConfig :: (IOish m, MonadError GhcModError m)
          => Cradle
          -> m CabalConfig
getConfig cradle = liftIO (readFile path) `tryFix` \_ ->
     configure `modifyError'` GMECabalConfigure
 where
   prjDir = cradleRootDir cradle
   path = prjDir </> configPath

   configure :: (IOish m, MonadError GhcModError m) => m ()
   configure =
       withDirectory_ prjDir $ readProcess' "cabal" ["configure"] >> return ()


-- | Path to 'LocalBuildInfo' file, usually @dist/setup-config@
configPath :: FilePath
configPath = localBuildInfoFile defaultDistPref

-- | Get list of 'Package's needed by all components of the current package
cabalConfigDependencies :: (IOish m, MonadError GhcModError m)
                        => Cradle
                        -> PackageIdentifier
                        -> m [Package]
cabalConfigDependencies cradle thisPkg =
    configDependencies thisPkg <$> getConfig cradle

-- | Extract list of depencenies for all components from 'CabalConfig'
configDependencies :: PackageIdentifier -> CabalConfig -> [Package]
configDependencies thisPkg config = map fromInstalledPackageId deps
 where
    deps :: [InstalledPackageId]
    deps = case deps21 `mplus` deps18 `mplus` deps16 of
        Right ps -> ps
        Left msg -> error msg

    -- True if this dependency is an internal one (depends on the library
    -- defined in the same package).
    internal pkgid = pkgid == thisPkg

    -- Cabal >= 1.21
    deps21 :: Either String [InstalledPackageId]
    deps21 =
        map fst
      <$> filterInternal21
      <$> (readEither =<< extractField config "componentsConfigs")

    filterInternal21
        :: [(ComponentName, C21.ComponentLocalBuildInfo, [ComponentName])]
        -> [(InstalledPackageId, C21.PackageIdentifier)]

    filterInternal21 ccfg = [ (ipkgid, pkgid)
                          | (_,clbi,_)      <- ccfg
                          , (ipkgid, pkgid) <- C21.componentPackageDeps clbi
                          , not (internal . packageIdentifierFrom21 $ pkgid) ]

    packageIdentifierFrom21 :: C21.PackageIdentifier -> PackageIdentifier
    packageIdentifierFrom21 (C21.PackageIdentifier (C21.PackageName myName) myVersion) =
        PackageIdentifier (PackageName myName) myVersion

    -- Cabal >= 1.18 && < 1.21
    deps18 :: Either String [InstalledPackageId]
    deps18 =
          map fst
      <$> filterInternal
      <$> (readEither =<< extractField config "componentsConfigs")

    filterInternal
        :: [(ComponentName, C18.ComponentLocalBuildInfo, [ComponentName])]
        -> [(InstalledPackageId, PackageIdentifier)]

    filterInternal ccfg = [ (ipkgid, pkgid)
                          | (_,clbi,_)      <- ccfg
                          , (ipkgid, pkgid) <- C18.componentPackageDeps clbi
                          , not (internal pkgid) ]

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
       libraryConfig :: Maybe C16.ComponentLocalBuildInfo
       libraryConfig = do
         field <- find ("libraryConfig" `isPrefixOf`) (tails config)
         clbi <- stripPrefix " = " field
         if "Nothing" `isPrefixOf` clbi
             then Nothing
             else case readMaybe =<< stripPrefix "Just " clbi of
                    Just x -> x
                    Nothing -> error $ "reading libraryConfig failed\n" ++ show (stripPrefix "Just " clbi)

       extract :: String -> Either String [(String, C16.ComponentLocalBuildInfo)]
       extract field = readConfigs field <$> extractField config field

       readConfigs :: String -> String -> [(String, C16.ComponentLocalBuildInfo)]
       readConfigs f s = case readEither s of
           Right x -> x
           Left msg -> error $ "reading config " ++ f ++ " failed ("++msg++")"

-- | Get the flag assignment from the local build info of the given cradle
cabalConfigFlags :: (IOish m, MonadError GhcModError m)
                 => Cradle
                 -> m FlagAssignment
cabalConfigFlags cradle = do
  config <- getConfig cradle
  case configFlags config of
    Right x  -> return x
    Left msg -> throwError (GMECabalFlags (GMEString msg))

-- | Extract the cabal flags from the 'CabalConfig'
configFlags :: CabalConfig -> Either String FlagAssignment
configFlags config = readEither =<< flip extractField "configConfigurationsFlags" =<< extractField config "configFlags"

-- | Find @field@ in 'CabalConfig'. Returns 'Left' containing a user readable
-- error message with lots of context on failure.
extractField :: CabalConfig -> String -> Either String String
extractField config field =
    case extractParens <$> find (field `isPrefixOf`) (tails config) of
        Just f -> Right f
        Nothing -> Left $ "extractField: failed extracting "++field++" from input, input contained `"++field++"'? " ++ show (field `isInfixOf` config)
