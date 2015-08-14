{-# LANGUAGE TemplateHaskell, RecordWildCards, StandaloneDeriving #-}
module SetupCompat where

import Control.Arrow
import Control.Monad.Trans.State
import Data.List
import Data.Maybe
import Data.Functor
import Data.Function
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription

import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Install

import qualified Data.Map as M
import Data.Map (Map)


import NotCPP.Declarations
import Language.Haskell.TH

-- $(ifdefD "componentsConfigs" [d| deriving instance (Ord ComponentName) |] )

$(ifD [d|

 showComponentName :: ComponentName -> String
 showComponentName CLibName          = "library"
 showComponentName (CExeName   name) = "executable '" ++ name ++ "'"
 showComponentName (CTestName  name) = "test suite '" ++ name ++ "'"
 showComponentName (CBenchName name) = "benchmark '" ++ name ++ "'"

 |])

$(ifelsedefD "componentsConfigs" [d|

 setComponentsConfigs
    :: LocalBuildInfo
    -> [(ComponentName, ComponentLocalBuildInfo, [ComponentName])]
    -> LocalBuildInfo
 setComponentsConfigs lbi cs = $(recUpdE' (nE "lbi") (mkName "componentsConfigs") (VarE $ mkName "cs"))

 |] [d|

 setComponentsConfigs
    :: LocalBuildInfo
    -> [(ComponentName, ComponentLocalBuildInfo, a)]
    -> LocalBuildInfo
 setComponentsConfigs lbi cs = flip execState lbi $ mapM setClbis gcs
  where
   gcs = groupBy (sameKind `on` fst3) $ sortBy (compare `on` showComponentName . fst3) cs

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
       get >>= \lbi ->
           put $ $(recUpdE' (nE "lbi") (mkName "libraryConfig") (AppE (ConE (mkName "Just")) (VarE (mkName "clbi"))))

   setClbis cs@((CExeName _, _, _):_) =
       let cfg = (\((CExeName n), clbi, _) -> (n, clbi)) <$> cs in
       get >>= \lbi ->
           put $ $(recUpdE' (nE "lbi") (mkName "executableConfigs") (VarE $ mkName "cfg"))

   setClbis cs@((CTestName _, _, _):_) =
       let cfg = (\((CTestName n), clbi, _) -> (n, clbi)) <$> cs in
       get >>= \lbi ->
           put $ $(recUpdE' (nE "lbi") (mkName "testSuiteConfigs") (VarE $ mkName "cfg"))

   setClbis cs@((CBenchName _, _, _):_) =
       let cfg = (\((CBenchName n), clbi, _) -> (n, clbi)) <$> cs in
       get >>= \lbi ->
           put $ $(recUpdE' (nE "lbi") (mkName "benchmarkConfigs") (VarE $ mkName "cfg"))

 |])


$(ifD [d|

 componentsConfigs ::
    LocalBuildInfo -> [(ComponentName, ComponentLocalBuildInfo, [ComponentName])]
 componentsConfigs LocalBuildInfo {..} =
    (maybe [] (\c -> [(CLibName, c, [])]) $(nE "libraryConfig"))
    ++ ((\(n, clbi) -> (CExeName n, clbi, [])) <$> $(nE "executableConfigs"))
    ++ ((\(n, clbi) -> (CTestName n, clbi, [])) <$> $(nE "testSuiteConfigs"))
    ++ ((\(n, clbi) -> (CBenchName n, clbi, [])) <$> $(nE "benchmarkConfigs"))

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

 componentBuildInfo :: Component -> BuildInfo
 componentBuildInfo =
   foldComponent libBuildInfo buildInfo testBuildInfo benchmarkBuildInfo

 |])


$(ifelsedefD "componentPackageRenaming" [d|
 -- M.Map PackageName
 newtype Deps = Deps  { unDeps :: ([(InstalledPackageId, PackageId)], Map PackageName $(cT "ModuleRenaming")) }
-- $(return $ TySynD $(mkName "Deps") [] [t| |] )

 noDeps = Deps ([], M.empty)

 getDeps :: ComponentLocalBuildInfo -> Deps
 getDeps = componentPackageDeps &&& $(nE "componentPackageRenaming") >>> Deps

 setUnionDeps :: Deps -> ComponentLocalBuildInfo -> ComponentLocalBuildInfo
 setUnionDeps (Deps (deps, rns)) clbi = let
         clbi' = setComponentPackageRenaming clbi rns
         cpdeps = componentPackageDeps clbi
       in
         clbi' {
           componentPackageDeps = cpdeps `union` deps
         }

 setComponentPackageRenaming clbi cprn =
     -- [| clbi { componentPackageRenaming = componentPackageRenaming clbi `M.union` cprn } |]
     $(recUpdE'
       (nE "clbi")
       (mkName "componentPackageRenaming")
       (InfixE
        (Just
         (AppE
          (VarE
           (mkName "componentPackageRenaming"))
          (VarE (mkName "clbi"))
         ))
        (VarE (mkName "M.union"))
        (Just (VarE (mkName "cprn")))
       )
      )

 |] [d|

 newtype Deps = Deps { unDeps :: [(InstalledPackageId, PackageId)] }

 noDeps = Deps []

 getDeps :: ComponentLocalBuildInfo -> Deps
 getDeps lbi = Deps $ componentPackageDeps lbi

 setUnionDeps :: Deps -> ComponentLocalBuildInfo -> ComponentLocalBuildInfo
 setUnionDeps (Deps deps) clbi = let
         cpdeps = componentPackageDeps clbi
       in
         clbi {
           componentPackageDeps = cpdeps `union` deps
         }


-- setComponentPackageRenaming clbi _cprn = clbi

 |])
