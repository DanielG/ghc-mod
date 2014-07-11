{-# LANGUAGE ScopedTypeVariables, RecordWildCards #-}

module Language.Haskell.GhcMod.GHCApi (
    initializeFlagsWithCradle
  , setTargetFiles
  , getDynamicFlags
  , systemLibDir
  , withDynFlags
  , withCmdFlags
  , setNoWaringFlags
  , setAllWaringFlags
  , ghcPkgDb
  , package
  , modules
  , findModule
  , moduleInfo
  , localModuleInfo
  , bindings
  ) where

import Language.Haskell.GhcMod.CabalApi
import Language.Haskell.GhcMod.GHCChoice
import Language.Haskell.GhcMod.GhcPkg
import qualified Language.Haskell.GhcMod.Gap as Gap
import Language.Haskell.GhcMod.Types

import Control.Applicative ((<$>))
import Control.Monad (forM, void)
import Distribution.Package (InstalledPackageId(..))
import Data.Maybe (isJust, fromJust)
import qualified Data.Map.Strict as M
import GHC (DynFlags(..), GhcLink(..), HscTarget(..), LoadHowMuch(..))
import qualified GHC as G
import GhcMonad
import GHC.Paths (libdir)
import qualified Packages as G
import qualified Module as G
import qualified OccName as G

import System.IO.Unsafe (unsafePerformIO)

----------------------------------------------------------------

-- | Obtaining the directory for system libraries.
systemLibDir :: FilePath
systemLibDir = libdir

----------------------------------------------------------------

importDirs :: [IncludeDir]
importDirs = [".","..","../..","../../..","../../../..","../../../../.."]

data Build = CabalPkg | SingleFile deriving Eq

-- | Initialize the 'DynFlags' relating to the compilation of a single
-- file or GHC session according to the 'Cradle' and 'Options'
-- provided.
initializeFlagsWithCradle :: GhcMonad m
        => Options
        -> Cradle
        -> m ()
initializeFlagsWithCradle opt cradle
  | cabal     = withCabal |||> withSandbox
  | otherwise = withSandbox
  where
    mCradleFile = cradleCabalFile cradle
    cabal = isJust mCradleFile
    ghcopts = ghcOpts opt
    withCabal = do
        pkgDesc <- liftIO $ parseCabalFile $ fromJust mCradleFile
        compOpts <- liftIO $ getCompilerOptions ghcopts cradle pkgDesc
        initSession CabalPkg opt compOpts
    withSandbox = initSession SingleFile opt compOpts
      where
        pkgOpts = ghcDbStackOpts $ cradlePkgDbStack cradle
        compOpts
          | null pkgOpts = CompilerOptions ghcopts importDirs []
          | otherwise    = CompilerOptions (ghcopts ++ pkgOpts) [wdir,rdir] []
        wdir = cradleCurrentDir cradle
        rdir = cradleRootDir    cradle

----------------------------------------------------------------

initSession :: GhcMonad m
            => Build
            -> Options
            -> CompilerOptions
            -> m ()
initSession build Options {..} CompilerOptions {..} = do
    df <- G.getSessionDynFlags
    void $ G.setSessionDynFlags =<< (addCmdOpts ghcOptions
      $ setLinkerOptions
      $ setIncludeDirs includeDirs
      $ setBuildEnv build
      $ setEmptyLogger
      $ Gap.addPackageFlags depPackages df)


setEmptyLogger :: DynFlags -> DynFlags
setEmptyLogger df = Gap.setLogAction df $ \_ _ _ _ _ -> return ()

----------------------------------------------------------------

-- we don't want to generate object code so we compile to bytecode
-- (HscInterpreted) which implies LinkInMemory
-- HscInterpreted
setLinkerOptions :: DynFlags -> DynFlags
setLinkerOptions df = df {
    ghcLink   = LinkInMemory
  , hscTarget = HscInterpreted
  }

setIncludeDirs :: [IncludeDir] -> DynFlags -> DynFlags
setIncludeDirs idirs df = df { importPaths = idirs }

setBuildEnv :: Build -> DynFlags -> DynFlags
setBuildEnv build = setHideAllPackages build . setCabalPackage build

-- At the moment with this option set ghc only prints different error messages,
-- suggesting the user to add a hidden package to the build-depends in his cabal
-- file for example
setCabalPackage :: Build -> DynFlags -> DynFlags
setCabalPackage CabalPkg df = Gap.setCabalPkg df
setCabalPackage _ df = df

-- | Enable hiding of all package not explicitly exposed (like Cabal does)
setHideAllPackages :: Build -> DynFlags -> DynFlags
setHideAllPackages CabalPkg df = Gap.setHideAllPackages df
setHideAllPackages _ df = df

-- | Parse command line ghc options and add them to the 'DynFlags' passed
addCmdOpts :: GhcMonad m => [GHCOption] -> DynFlags -> m DynFlags
addCmdOpts cmdOpts df =
    tfst <$> G.parseDynamicFlags df (map G.noLoc cmdOpts)
  where
    tfst (a,_,_) = a

----------------------------------------------------------------

-- | Set the files as targets and load them.
setTargetFiles :: (GhcMonad m) => [FilePath] -> m ()
setTargetFiles files = do
    targets <- forM files $ \file -> G.guessTarget file Nothing
    G.setTargets targets
    void $ G.load LoadAllTargets

----------------------------------------------------------------

-- | Return the 'DynFlags' currently in use in the GHC session.
getDynamicFlags :: IO DynFlags
getDynamicFlags = do
    G.runGhc (Just systemLibDir) G.getSessionDynFlags

withDynFlags :: GhcMonad m
             => (DynFlags -> DynFlags)
             -> m a
             -> m a
withDynFlags setFlags body = G.gbracket setup teardown (\_ -> body)
  where
    setup = do
        dflags <- G.getSessionDynFlags
        void $ G.setSessionDynFlags (setFlags dflags)
        return dflags
    teardown = void . G.setSessionDynFlags

withCmdFlags :: GhcMonad m => [GHCOption] -> m a -> m a
withCmdFlags flags body = G.gbracket setup teardown (\_ -> body)
  where
    setup = do
        dflags <- G.getSessionDynFlags >>= addCmdOpts flags
        void $ G.setSessionDynFlags dflags
        return dflags
    teardown = void . G.setSessionDynFlags

----------------------------------------------------------------

-- | Set 'DynFlags' equivalent to "-w:".
setNoWaringFlags :: DynFlags -> DynFlags
setNoWaringFlags df = df { warningFlags = Gap.emptyWarnFlags}

-- | Set 'DynFlags' equivalent to "-Wall".
setAllWaringFlags :: DynFlags -> DynFlags
setAllWaringFlags df = df { warningFlags = allWarningFlags }

allWarningFlags :: Gap.WarnFlags
allWarningFlags = unsafePerformIO $ do
    G.runGhc (Just systemLibDir) $ do
        df <- G.getSessionDynFlags
        df' <- addCmdOpts ["-Wall"] df
        return $ G.warningFlags df'

----------------------------------------------------------------
-- get Packages,Modules,Bindings

ghcPkgDb :: GhcMonad m => m PkgDb
ghcPkgDb = M.fromList <$>
    maybe [] (map toKv . filterInternal) <$> pkgDatabase <$> G.getSessionDynFlags
 where
    toKv pkg = (fromInstalledPackageId $ G.installedPackageId pkg, pkg)
    filterInternal =
        filter ((/= InstalledPackageId "builtin_rts") . G.installedPackageId)

package :: G.PackageConfig -> Package
package = fromInstalledPackageId . G.installedPackageId

modules :: G.PackageConfig -> [ModuleString]
modules = map G.moduleNameString . G.exposedModules

findModule :: ModuleString -> PkgDb -> [Package]
findModule m db = do
  M.elems $ package `M.map` (containsModule `M.filter` db)
 where
    containsModule :: G.PackageConfig -> Bool
    containsModule pkgConf =
        G.mkModuleName m `elem` G.exposedModules pkgConf


ghcPkgId :: Package -> G.PackageId
ghcPkgId (name,_,_) =
    -- TODO: Adding the package version too breaks 'findModule' for some reason
    -- this isn't a big deal since in the common case where we're in a cabal
    -- project we just use cabal's view of package dependencies anyways so we're
    -- guaranteed to only have one version of each package exposed. However when
    -- we're operating without a cabal project this will probaly cause trouble.
    G.stringToPackageId name

type Binding = String

-- | @moduleInfo mpkg module@. @mpkg@ should be 'Nothing' iff. moduleInfo
-- should look for @module@ in the working directory.
--
-- To map a 'ModuleString' to a package see 'findModule'
moduleInfo :: GhcMonad m
           => Maybe Package
           -> ModuleString
           -> m (Maybe G.ModuleInfo)
moduleInfo mpkg mdl = do
    let mdlName = G.mkModuleName mdl
        mfsPkgId = G.packageIdFS . ghcPkgId <$> mpkg
    loadLocalModule
    G.findModule mdlName mfsPkgId >>= G.getModuleInfo
 where
   loadLocalModule = case mpkg of
       Just _ -> return ()
       Nothing -> setTargetFiles [mdl]

localModuleInfo :: GhcMonad m => ModuleString -> m (Maybe G.ModuleInfo)
localModuleInfo mdl = moduleInfo Nothing mdl

bindings :: G.ModuleInfo -> [Binding]
bindings minfo = do
  map (G.occNameString . G.getOccName) $ G.modInfoExports minfo


----------------------------------------------------------------
-- for PkgDoc

-- import Distribution.InstalledPackageInfo (showInstalledPackageInfoField)
-- haddockHtml :: GhcMonad m => Package -> m String
-- haddockHtml pkg = do
--     extractField info . fromJust . lookup pkg <$> ghcPkgDb
--  where
--     extractField = fromJust $ showInstalledPackageInfoField "haddock-html"
