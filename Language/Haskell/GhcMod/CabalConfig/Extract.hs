{-# LANGUAGE RecordWildCards, CPP, OverloadedStrings #-}

-- | This module facilitates extracting information from Cabal's on-disk
-- 'LocalBuildInfo' (@dist/setup-config@).
module Language.Haskell.GhcMod.CabalConfig.Extract (
    CabalConfig
  , configDependencies
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
import qualified Language.Haskell.GhcMod.CabalConfig.Cabal22 as C22

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
import Data.Version
import Distribution.Package (InstalledPackageId(..)
                           , PackageIdentifier(..)
                           , PackageName(..))
import Distribution.PackageDescription (FlagAssignment)
import Distribution.Simple.LocalBuildInfo (ComponentName)
import MonadUtils (liftIO)
import Text.ParserCombinators.ReadP

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

----------------------------------------------------------------

-- | 'Show'ed cabal 'LocalBuildInfo' string
newtype CabalConfig = CabalConfig { unCabalConfig :: String }

-- | Get contents of the file containing 'LocalBuildInfo' data. If it doesn't
-- exist run @cabal configure@ i.e. configure with default options like @cabal
-- build@ would do.
getConfig :: (IOish m, GmError m) => Cradle -> m CabalConfig
getConfig crdl = do
    liftIO (getCurrentWorld crdl) >>= \world ->
        when (isSetupConfigOutOfDate world) configure

    cfg <- liftIO (BS.readFile file) `tryFix` \_ ->
             configure `modifyError'` GMECabalConfigure

    liftIO (getCurrentWorld crdl) >>= \world ->
        decodeConfig crdl world file cfg
 where
   file = setupConfigFile crdl
   prjDir = cradleRootDir crdl

   configure :: (IOish m, GmError m) => m ()
   configure = withDirectory_ prjDir $ void $ readProcess' "cabal" ["configure"]

decodeConfig :: (IOish m, GmError m)
             => Cradle -> World -> FilePath -> ByteString -> m CabalConfig
decodeConfig _crdl _world file bs = CabalConfig <$> gen

--  if cacheOutdated world
--     then
--       gmLog $ "Regenerating pretty setup-config cache: " ++ prettyConfigCache
--       liftIO $ writeFile prettyConfigCache cfg
--     else CabalConfig <$> liftIO (readFile prettyConfigCache)

 where
   -- cacheOutdated World {..} =
   --     case (worldCabalConfig, worldPrettyCabalConfigCache) of
   --       (Nothing, _) -> error "decodeConfig: setup-config does not exist."
   --       (Just _, Nothing) -> True
   --       (Just s, Just p) -> s > p

   gen = case BS8.lines bs of
           header:_ -> do
               ((_,cabalVer), _) <- parseHeader header
               if cabalVer >= (Version [1,22] [])
                 then prettyPrintBinaryConfig file
                 else return $ bsToStr bs
           [] -> throwError $ GMECabalStateFile GMConfigStateFileNoHeader

prettyPrintBinaryConfig :: (IOish m, GmError m)
                             => String -> m String
prettyPrintBinaryConfig file = do
  exe <- liftIO $ findLibexecExe "ghc-mod-cabal"
  slbi <- readProcess' exe ["print-setup-config", file]
  return slbi

parseHeader :: GmError m
            => ByteString -> m ((ByteString, Version), (ByteString, Version))
parseHeader header = case BS8.words header of
  ["Saved", "package", "config", "for", _pkgId , "written", "by", cabalId, "using", compId] -> modifyError (\_ -> GMECabalStateFile GMConfigStateFileBadHeader) $ do
          cabalId' <- parsePkgId cabalId
          compId' <- parsePkgId  compId
          return (cabalId', compId')

  _ -> throwError $ GMECabalStateFile GMConfigStateFileNoHeader

parsePkgId :: (Error e, MonadError e m) => ByteString -> m (ByteString, Version)
parsePkgId bs =
    case BS8.split '-' bs of
      [pkg, vers] -> return (pkg, parseVer vers)
      _ -> throwError noMsg
 where
   parseVer vers =
       let (ver,""):[] =
               filter ((=="") . snd) $ readP_to_S parseVersion (bsToStr vers)
       in ver

bsToStr :: ByteString -> String
bsToStr = T.unpack . T.decodeUtf8

-- strToBs :: String -> ByteString
-- strToBs = T.encodeUtf8 . T.pack

-- | Extract list of depencenies for all components from 'CabalConfig'
configDependencies :: PackageIdentifier -> CabalConfig -> [Package]
configDependencies thisPkg config = map fromInstalledPackageId deps
 where
    deps :: [InstalledPackageId]
    deps = case deps16 `mplus` deps18 `mplus` deps22 of
        Right ps -> ps
        Left msg -> error msg

    -- True if this dependency is an internal one (depends on the library
    -- defined in the same package).
    internal pkgid = pkgid == thisPkg

    -- Cabal >= 1.22
    deps22 :: Either String [InstalledPackageId]
    deps22 =
        map fst
      <$> filterInternal22
      <$> (readEither =<< extractField (unCabalConfig config) "componentsConfigs")

    filterInternal22
        :: [(ComponentName, C22.ComponentLocalBuildInfo, [ComponentName])]
        -> [(InstalledPackageId, C22.PackageIdentifier)]

    filterInternal22 ccfg = [ (ipkgid, pkgid)
                          | (_,clbi,_)      <- ccfg
                          , (ipkgid, pkgid) <- C22.componentPackageDeps clbi
                          , not (internal . packageIdentifierFrom22 $ pkgid) ]

    packageIdentifierFrom22 :: C22.PackageIdentifier -> PackageIdentifier
    packageIdentifierFrom22 (C22.PackageIdentifier (C22.PackageName myName) myVersion) =
        PackageIdentifier (PackageName myName) myVersion

    -- Cabal >= 1.18 && < 1.20
    deps18 :: Either String [InstalledPackageId]
    deps18 =
          map fst
      <$> filterInternal
      <$> (readEither =<< extractField (unCabalConfig config) "componentsConfigs")

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
         field <- find ("libraryConfig" `isPrefixOf`) (tails $ unCabalConfig config)
         clbi <- stripPrefix " = " field
         if "Nothing" `isPrefixOf` clbi
             then Nothing
             else case readMaybe =<< stripPrefix "Just " clbi of
                    Just x -> x
                    Nothing -> error $ "reading libraryConfig failed\n" ++ show (stripPrefix "Just " clbi)

       extract :: String -> Either String [(String, C16.ComponentLocalBuildInfo)]
       extract field = readConfigs field <$> extractField (unCabalConfig config) field

       readConfigs :: String -> String -> [(String, C16.ComponentLocalBuildInfo)]
       readConfigs f s = case readEither s of
           Right x -> x
           Left msg -> error $ "reading config " ++ f ++ " failed ("++msg++")"

-- | Extract the cabal flags from the 'CabalConfig'
configFlags :: CabalConfig -> Either String FlagAssignment
configFlags (CabalConfig config) = readEither =<< flip extractField "configConfigurationsFlags" =<< extractField config "configFlags"

-- | Find @field@ in 'CabalConfig'. Returns 'Left' containing a user readable
-- error message with lots of context on failure.
extractField :: String -> String -> Either String String
extractField content field =
    case extractParens <$> find (field `isPrefixOf`) (tails content) of
        Just f -> Right f
        Nothing -> Left $ "extractField: failed extracting "++field++" from input, input contained `"++field++"'? " ++ show (field `isInfixOf` content)
