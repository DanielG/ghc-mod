{-# LANGUAGE RecordWildCards, CPP #-}

-- | This module facilitates extracting information from Cabal's on-disk
-- 'LocalBuildInfo' (@dist/setup-config@).
module Language.Haskell.GhcMod.CabalConfig.PreGhc710 (
    configDependencies
  , configFlags
  , getConfig
  ) where

import Language.Haskell.GhcMod.Error
import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.PathsAndFiles
import Language.Haskell.GhcMod.Read
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Utils
import Language.Haskell.GhcMod.World

import qualified Language.Haskell.GhcMod.CabalConfig.Cabal16 as C16
import qualified Language.Haskell.GhcMod.CabalConfig.Cabal18 as C18
import qualified Language.Haskell.GhcMod.CabalConfig.Cabal21 as C21

#ifndef MIN_VERSION_mtl
#define MIN_VERSION_mtl(x,y,z) 1
#endif

import Control.Applicative ((<$>))
import Control.Monad (void, mplus, when)
#if MIN_VERSION_mtl(2,2,1)
import Control.Monad.Except ()
#else
import Control.Monad.Error ()
#endif
import Data.List (find,tails,isPrefixOf,isInfixOf,nub,stripPrefix)
import Distribution.Package (InstalledPackageId(..)
                           , PackageIdentifier(..)
                           , PackageName(..))
import Distribution.PackageDescription (FlagAssignment)
import Distribution.Simple.LocalBuildInfo (ComponentName)
import MonadUtils (liftIO)

----------------------------------------------------------------

-- | 'Show'ed cabal 'LocalBuildInfo' string
type CabalConfig = String

-- | Get contents of the file containing 'LocalBuildInfo' data. If it doesn't
-- exist run @cabal configure@ i.e. configure with default options like @cabal
-- build@ would do.
getConfig :: (IOish m, MonadError GhcModError m)
          => Cradle
          -> m CabalConfig
getConfig cradle = do
    outOfDate <- liftIO $ isSetupConfigOutOfDate cradle
    when outOfDate configure
    liftIO (readFile file) `tryFix` \_ ->
        configure `modifyError'` GMECabalConfigure
 where
   file = setupConfigFile cradle
   prjDir = cradleRootDir cradle

   configure :: (IOish m, MonadError GhcModError m) => m ()
   configure = withDirectory_ prjDir $ void $ readProcess' "cabal" ["configure"]


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
