#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards #-}
import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.Install
import Distribution.Simple.Program
import Distribution.Simple.Register
import Distribution.Simple.InstallDirs as ID
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.LocalBuildInfo (componentsConfigs)
import Distribution.PackageDescription

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe
import Data.Version
import Data.Monoid
import System.Process
import System.Exit
import System.FilePath

import SetupCompat

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
   confHook = \(gpd, hbi) cf ->
              xBuildDependsLike <$> (confHook simpleUserHooks) (gpd, hbi) cf
 , hookedPrograms = [ simpleProgram "shelltest" ]
 }

xBuildDependsLike :: LocalBuildInfo -> LocalBuildInfo
xBuildDependsLike lbi =
  let
      cc = componentsConfigs lbi
      pd = localPkgDescr lbi
      deps = dependsMap lbi
  in setComponentsConfigs lbi
        [ (cn, updateClbi deps comp clbi, cdeps)
        | (cn, clbi, cdeps) <- cc
        , let comp = getComponent pd cn
        ]

 where
   updateClbi deps comp clbi = setUnionDeps (otherDeps deps comp) clbi

   dependsMap ::
    LocalBuildInfo -> [(ComponentName, Deps)]
   dependsMap lbi =
       (\x -> (componentLocalName x, getDeps x) ) <$> allComponentsInBuildOrder lbi

   otherDeps :: [(ComponentName, Deps)] -> Component -> Deps
   otherDeps deps comp = fromMaybe noDeps $
       flip lookup deps =<< read <$> lookup "x-build-depends-like" fields
      where
        fields = customFieldsBI (componentBuildInfo comp)
