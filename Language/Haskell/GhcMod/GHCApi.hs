{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.GhcMod.GHCApi (
    ghcPkgDb
  , package
  , modules
  , findModule
  , moduleInfo
  , localModuleInfo
  , bindings
  ) where

import Language.Haskell.GhcMod.GhcPkg
import Language.Haskell.GhcMod.Monad (GhcModT)
import Language.Haskell.GhcMod.Target (setTargetFiles)
import Language.Haskell.GhcMod.Types

import Control.Applicative ((<$>))
import Distribution.Package (InstalledPackageId(..))
import qualified Data.Map as M
import GHC (DynFlags(..))
import qualified GHC as G
import GhcMonad
import qualified Packages as G
import qualified Module as G
import qualified OccName as G

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
findModule m db = M.elems $ package `M.map` (containsModule `M.filter` db)
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
moduleInfo :: IOish m
           => Maybe Package
           -> ModuleString
           -> GhcModT m (Maybe G.ModuleInfo)
moduleInfo mpkg mdl = do
    let mdlName = G.mkModuleName mdl
        mfsPkgId = G.packageIdFS . ghcPkgId <$> mpkg
    loadLocalModule
    G.findModule mdlName mfsPkgId >>= G.getModuleInfo
 where
   loadLocalModule = case mpkg of
       Just _ -> return ()
       Nothing -> setTargetFiles [mdl]

localModuleInfo :: IOish m => ModuleString -> GhcModT m (Maybe G.ModuleInfo)
localModuleInfo mdl = moduleInfo Nothing mdl

bindings :: G.ModuleInfo -> [Binding]
bindings minfo = map (G.occNameString . G.getOccName) $ G.modInfoExports minfo
