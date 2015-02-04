{-# LANGUAGE CPP, RecordWildCards, StandaloneDeriving #-}
module SetupCompat where

import Control.Monad.State.Strict
import Data.List
import Data.Maybe
import Data.Functor
import Data.Function
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription

#if __GLASGOW_HASKELL__ <= 706
componentsConfigs ::
    LocalBuildInfo -> [(ComponentName, ComponentLocalBuildInfo, [ComponentName])]
componentsConfigs LocalBuildInfo {..} =
    (maybe [] (\c -> [(CLibName, c, [])]) libraryConfig)
    ++ ((\(n, clbi) -> (CExeName n, clbi, [])) <$> executableConfigs)
    ++ ((\(n, clbi) -> (CTestName n, clbi, [])) <$> testSuiteConfigs)
    ++ ((\(n, clbi) -> (CBenchName n, clbi, [])) <$> benchmarkConfigs)

getComponent :: PackageDescription -> ComponentName -> Component
getComponent pkg cname =
    case lookupComponent pkg cname of
      Just cpnt -> cpnt
      Nothing   -> missingComponent
  where
    missingComponent =
      error $ "internal error: the package description contains no "
           ++ "component corresponding to " ++ show cname

lookupComponent :: PackageDescription -> ComponentName -> Maybe Component
lookupComponent pkg CLibName =
    fmap CLib $ library pkg
lookupComponent pkg (CExeName name) =
    fmap CExe $ find ((name ==) . exeName) (executables pkg)
lookupComponent pkg (CTestName name) =
    fmap CTest $ find ((name ==) . testName) (testSuites pkg)
lookupComponent pkg (CBenchName name) =
    fmap CBench $ find ((name ==) . benchmarkName) (benchmarks pkg)

-- We're lying here can't be bothered to order these
allComponentsInBuildOrder :: LocalBuildInfo
                          -> [(ComponentName, ComponentLocalBuildInfo)]
allComponentsInBuildOrder lbi =
      [ (cname, clbi) | (cname, clbi, _) <- componentsConfigs lbi ]

getComponentLocalBuildInfo :: LocalBuildInfo -> ComponentName -> ComponentLocalBuildInfo
getComponentLocalBuildInfo lbi cname =
    case [ clbi
         | (cname', clbi, _) <- componentsConfigs lbi
         , cname == cname' ] of
      [clbi] -> clbi
      _      -> missingComponent
  where
    missingComponent =
      error $ "internal error: there is no configuration data "
           ++ "for component " ++ show cname

deriving instance (Ord ComponentName)

setComponentsConfigs
    :: LocalBuildInfo
    -> [(ComponentName, ComponentLocalBuildInfo, a)]
    -> LocalBuildInfo
setComponentsConfigs lbi cs = flip execState lbi $ mapM setClbis gcs
 where
--   gcs :: [ [(ComponentLocalBuildInfo, ComponentName, a)] ]
   gcs = groupBy (sameKind `on` fst3) $ sortBy (compare `on` fst3) cs

   fst3 (x,_,_) = x

   sameKind CLibName CLibName = True
   sameKind CLibName _ = False
   sameKind (CExeName _) (CExeName _) = True
   sameKind (CExeName _) _ = False
   sameKind (CTestName _) (CTestName _) = True
   sameKind (CTestName _) _ = False
   sameKind (CBenchName _) (CBenchName _) = True
   sameKind (CBenchName _) _ = False

   setClbis [(CLibName, clbi, _)] =
       get >>= \lbi -> put $ lbi {libraryConfig = Just clbi}

   setClbis cs@((CExeName _, _, _):_) =
       let cfg = (\((CExeName n), clbi, _) -> (n, clbi)) <$> cs in
       get >>= \lbi -> put $ lbi {executableConfigs = cfg }

   setClbis cs@((CTestName _, _, _):_) =
       let cfg = (\((CTestName n), clbi, _) -> (n, clbi)) <$> cs in
       get >>= \lbi -> put $ lbi {testSuiteConfigs = cfg }

   setClbis cs@((CBenchName _, _, _):_) =
       let cfg = (\((CBenchName n), clbi, _) -> (n, clbi)) <$> cs in
       get >>= \lbi -> put $ lbi {benchmarkConfigs = cfg }

#else

setComponentsConfigs
    :: LocalBuildInfo
    -> [(ComponentName, ComponentLocalBuildInfo, [ComponentName])]
    -> LocalBuildInfo
setComponentsConfigs lbi cs = lbi { componentsConfigs = cs }

#endif
