#!/usr/bin/env runhaskell
{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
import Distribution.Simple
import Distribution.Simple.Utils
import Distribution.Simple.Setup
import Distribution.Simple.Install
import Distribution.Simple.Program
import Distribution.Simple.Register
import Distribution.Simple.BuildPaths
import qualified Distribution.Simple.InstallDirs as ID
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription

import qualified Data.Map as M
import Data.Map (Map)

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
import System.Directory (renameFile)

main :: IO ()
main = defaultMainWithHooks $ simpleUserHooks {
   instHook = inst,
   copyHook = copy,
   buildHook = \pd lbi hooks flags -> (buildHook simpleUserHooks) pd (patchLibexecdir lbi) hooks flags,
   hookedPrograms = [ simpleProgram "shelltest" ]
 }

patchLibexecdir :: LocalBuildInfo -> LocalBuildInfo
patchLibexecdir lbi = let
    idirtpl     = installDirTemplates lbi
    libexecdir' = toPathTemplate $ fromPathTemplate (libexecdir idirtpl) </> "$abi/$pkgid"
    lbi' = lbi { installDirTemplates = idirtpl { libexecdir = libexecdir' } }
  in
    lbi'

-- mostly copypasta from 'defaultInstallHook'
inst ::
    PackageDescription -> LocalBuildInfo -> UserHooks -> InstallFlags -> IO ()
inst pd lbi _uf ifl = do
  let copyFlags = defaultCopyFlags {
                      copyDistPref   = installDistPref ifl,
                      copyDest       = toFlag NoCopyDest,
                      copyVerbosity  = installVerbosity ifl
                  }
  xInstallTarget pd lbi copyFlags (\pd' lbi' -> install pd' lbi' copyFlags)
  let registerFlags = defaultRegisterFlags {
                          regDistPref  = installDistPref ifl,
                          regInPlace   = installInPlace ifl,
                          regPackageDB = installPackageDB ifl,
                          regVerbosity = installVerbosity ifl
                      }
  when (hasLibs pd) $ register pd lbi registerFlags

copy :: PackageDescription -> LocalBuildInfo -> UserHooks -> CopyFlags -> IO ()
copy pd lbi _uh cf =
    xInstallTarget pd lbi cf (\pd' lbi' -> install pd' lbi' cf)

xInstallTarget :: PackageDescription
               -> LocalBuildInfo
               -> CopyFlags
               -> (PackageDescription -> LocalBuildInfo -> IO ())
               -> IO ()
xInstallTarget pd lbi cf fn = do
  let (extended, regular) = partition isInternal (executables pd)

  let pd_regular = pd { executables = regular }

  _ <- flip mapM extended $ \exe -> do
    let pd_extended = onlyExePackageDesc [exe] pd
    fn pd_extended lbi

    let lbi' = patchLibexecdir lbi
        copydest  = fromFlag (copyDest cf)
        verbosity = fromFlag (copyVerbosity cf)
        InstallDirs { bindir, libexecdir } = absoluteInstallDirs pd lbi' copydest
        progprefix = substPathTemplate (packageId pd) lbi (progPrefix lbi)
        progsuffix = substPathTemplate (packageId pd) lbi (progSuffix lbi)
        fixedExeBaseName = progprefix ++ exeName exe ++ progsuffix

        fixedExeFileName = bindir </> fixedExeBaseName <.> exeExtension
        newExeFileName   = libexecdir </> fixedExeBaseName <.> exeExtension

    when (exeName exe == "ghc-mod-real") $ do
        createDirectoryIfMissingVerbose verbosity True libexecdir
        renameFile fixedExeFileName newExeFileName

  fn pd_regular lbi

 where
   isInternal :: Executable -> Bool
   isInternal exe =
    fromMaybe False $ (=="True") <$> lookup "x-internal" (customFieldsBI $ buildInfo exe)

onlyExePackageDesc :: [Executable] -> PackageDescription -> PackageDescription
onlyExePackageDesc exes pd = emptyPackageDescription {
                     package = package pd
                   , executables = exes
                   }
