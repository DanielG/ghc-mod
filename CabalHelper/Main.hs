-- ghc-mod: Making Haskell development *more* fun
-- Copyright (C) 2015  Daniel Gröber <dxld ÄT darkboxed DOT org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE CPP, BangPatterns, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
import Distribution.Simple.Utils (cabalVersion)
import Distribution.Simple.Configure

import Distribution.Package (PackageIdentifier, InstalledPackageId, PackageId)
import Distribution.PackageDescription (PackageDescription,
                                        FlagAssignment,
                                        Executable(..),
                                        Library(..),
                                        TestSuite(..),
                                        Benchmark(..),
                                        BuildInfo(..),
                                        TestSuiteInterface(..),
                                        BenchmarkInterface(..),
                                        withLib)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.PackageDescription.Configuration (flattenPackageDescription)

import Distribution.Simple.Program (requireProgram, ghcProgram)
import Distribution.Simple.Program.Types (ConfiguredProgram(..))
import Distribution.Simple.Configure (getPersistBuildConfig)
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..),
                                           Component(..),
                                           ComponentName(..),
                                           ComponentLocalBuildInfo(..),
                                           componentBuildInfo,
                                           externalPackageDeps,
                                           withComponentsLBI,
                                           inplacePackageId)

import Distribution.Simple.GHC (componentGhcOptions)
import Distribution.Simple.Program.GHC (GhcOptions(..), renderGhcOptions)

import Distribution.Simple.Setup (ConfigFlags(..),Flag(..))
import Distribution.Simple.Build (initialBuildSteps)
import Distribution.Simple.BuildPaths (autogenModuleName, cppHeaderName, exeExtension)
import Distribution.Simple.Compiler (PackageDB(..))

import Distribution.ModuleName (components)
import qualified Distribution.ModuleName as C (ModuleName)
import Distribution.Text (display)
import Distribution.Verbosity (Verbosity, silent, deafening)

import Control.Applicative ((<$>))
import Control.Monad
import Control.Exception (catch, PatternMatchFail(..))
import Data.List
import Data.Maybe
import Data.Monoid
import Data.IORef
import System.Environment
import System.Directory
import System.FilePath
import System.Exit
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import Text.Printf

import CabalHelper.Common
import CabalHelper.Types

usage = do
  prog <- getProgName
  hPutStr stderr $ align "(" "|" ("Usage: " ++ prog ++ " " ++ usageMsg)
 where
   usageMsg = ""
     ++"DIST_DIR ( version\n"
     ++"         | print-lbi\n"
     ++"         | write-autogen-files\n"
     ++"         | ghc-options [--with-inplace]\n"
     ++"         | ghc-src-options [--with-inplace]\n"
     ++"         | ghc-pkg-options [--with-inplace]\n"
     ++"         | entrypoints\n"
     ++"         | source-dirs\n"
     ++"         ) ...\n"

commands :: [String]
commands = [ "print-bli"
           , "write-autogen-files"
           , "component-from-file"
           , "ghc-options"
           , "ghc-src-options"
           , "ghc-pkg-options"
           , "entrypoints"
           , "source-dirs"]

main :: IO ()
main = do
  args <- getArgs

  distdir:args' <- case args of
                    [] -> usage >> exitFailure
                    _ -> return args

  ddexists <- doesDirectoryExist distdir
  when (not ddexists) $ do
         errMsg $ "distdir '"++distdir++"' does not exist"
         exitFailure

  v <- maybe silent (const deafening) . lookup  "GHC_MOD_DEBUG" <$> getEnvironment
  lbi <- unsafeInterleaveIO $ getPersistBuildConfig distdir
  let pd = localPkgDescr lbi

  let
      -- a =<< b $$ c   ==  (a =<< b) $$ c
      -- a <$$> b $$ c  ==  a <$$> (b $$ c)
      infixr 2 $$
      ($$) = ($)
      infixr 1 <$$>
      (<$$>) = (<$>)

      collectCmdOptions :: [String] -> [[String]]
      collectCmdOptions =
          reverse . map reverse . foldl f [] . dropWhile isOpt
       where
         isOpt = ("--" `isPrefixOf`)
         f [] x = [[x]]
         f (a:as) x
             | isOpt x = (x:a):as
             | otherwise = [x]:(a:as)

  let cmds = collectCmdOptions args'

  if any (["version"] `isPrefixOf`) cmds
    then do
      putStrLn $
       printf "using version %s of the Cabal library" (display cabalVersion)
      exitSuccess
    else return ()

  print =<< flip mapM cmds $$ \cmd -> do
  case cmd of
    "write-autogen-files":[] -> do
      let pd = localPkgDescr lbi
       -- calls writeAutogenFiles
      initialBuildSteps distdir pd lbi v
      return Nothing

    "ghc-options":flags ->
      Just . GmCabalHelperStrings <$$> componentsMap lbi v distdir $$
      \c clbi bi -> let
           outdir = componentOutDir lbi c
           (clbi', adopts) = case flags of
                               ["--with-inplace"] -> (clbi, mempty)
                               [] -> removeInplaceDeps pd clbi
           opts = componentGhcOptions v lbi bi clbi' outdir
           in
              renderGhcOptions' lbi v $ opts `mappend` adopts

    "ghc-src-options":flags ->
      Just . GmCabalHelperStrings <$$> componentsMap lbi v distdir $$
      \c clbi bi -> let
           outdir = componentOutDir lbi c
           (clbi', adopts) = case flags of
                               ["--with-inplace"] -> (clbi, mempty)
                               [] -> removeInplaceDeps pd clbi
           opts = componentGhcOptions v lbi bi clbi' outdir
           comp = compiler lbi

           opts' = mempty {
               -- Not really needed but "unexpected package db stack: []"
               ghcOptPackageDBs      = [GlobalPackageDB],
               ghcOptCppOptions      = ghcOptCppOptions opts,
               ghcOptCppIncludePath  = ghcOptCppIncludePath opts,
               ghcOptCppIncludes     = ghcOptCppIncludes opts,
               ghcOptFfiIncludes     = ghcOptFfiIncludes opts,
               ghcOptSourcePathClear = ghcOptSourcePathClear opts,
               ghcOptSourcePath      = ghcOptSourcePath opts
              }
            in
              renderGhcOptions' lbi v $ opts `mappend` adopts

    "ghc-pkg-options":flags ->
      Just . GmCabalHelperStrings <$$> componentsMap lbi v distdir $$
      \c clbi bi -> let
           comp = compiler lbi
           outdir = componentOutDir lbi c
           (clbi', adopts) = case flags of
                               ["--with-inplace"] -> (clbi, mempty)
                               [] -> removeInplaceDeps pd clbi
           opts = componentGhcOptions v lbi bi clbi' outdir

           opts' = mempty {
                       ghcOptPackageDBs = ghcOptPackageDBs opts,
                       ghcOptPackages   = ghcOptPackages opts,
                       ghcOptHideAllPackages = ghcOptHideAllPackages opts
                   }
           in
             renderGhcOptions' lbi v $ opts' `mappend` adopts

    "entrypoints":[] -> do
      eps <- componentsMap lbi v distdir $ \c clbi bi ->
               return $ componentEntrypoints c
      -- MUST append Setup component at the end otherwise CabalHelper gets
      -- confused
      let eps' = eps ++ [(GmSetupHsName, Right [GmModuleName "Setup"])]
      return $ Just $ GmCabalHelperEntrypoints eps'

    "source-dirs":[] ->
      Just . GmCabalHelperStrings <$$> componentsMap lbi v distdir $$
      \c clbi bi -> return $ hsSourceDirs bi

    "print-lbi":[] ->
      return $ Just $ GmCabalHelperLbi $ show lbi

    cmd:_ | not (cmd `elem` commands) ->
            errMsg ("Unknown command: " ++ cmd) >> usage >> exitFailure
    _ ->
            errMsg "Invalid usage!" >> usage >> exitFailure


getLibrary :: PackageDescription -> Library
getLibrary pd = unsafePerformIO $ do
  lr <- newIORef (error "libraryMap: empty IORef")
  withLib pd (writeIORef lr)
  readIORef lr

componentsMap :: LocalBuildInfo
              -> Verbosity
              -> FilePath
              -> (   Component
                  -> ComponentLocalBuildInfo
                  -> BuildInfo
                  -> IO a)
              -> IO [(GmComponentName, a)]
componentsMap lbi v distdir f = do
    let pd = localPkgDescr lbi

    lr <- newIORef []

    withComponentsLBI pd lbi $ \c clbi -> do
        let bi = componentBuildInfo c
            name = componentNameFromComponent c

        l' <- readIORef lr
        r <- f c clbi bi
        writeIORef lr $ (componentNameToGm name, r):l'
    reverse <$> readIORef lr

componentNameToGm CLibName = GmLibName
componentNameToGm (CExeName n) = GmExeName n
componentNameToGm (CTestName n) = GmTestName n
componentNameToGm (CBenchName n) = GmBenchName n

componentNameFromComponent (CLib Library {}) = CLibName
componentNameFromComponent (CExe Executable {..}) = CExeName exeName
componentNameFromComponent (CTest TestSuite {..}) = CTestName testName
componentNameFromComponent (CBench Benchmark {..}) = CBenchName benchmarkName

componentOutDir lbi (CLib Library {..})= buildDir lbi
componentOutDir lbi (CExe Executable {..})= exeOutDir lbi exeName
componentOutDir lbi (CTest TestSuite { testInterface = TestSuiteExeV10 _ _, ..}) =
    exeOutDir lbi testName
componentOutDir lbi (CTest TestSuite { testInterface = TestSuiteLibV09 _ _, ..}) =
    exeOutDir lbi (testName ++ "Stub")
componentOutDir lbi (CBench Benchmark { benchmarkInterface = BenchmarkExeV10 _ _, ..})=
    exeOutDir lbi benchmarkName

gmModuleName :: C.ModuleName -> GmModuleName
gmModuleName = GmModuleName . intercalate "." . components

componentEntrypoints :: Component -> Either FilePath [GmModuleName]
componentEntrypoints (CLib Library {..})
    = Right $ map gmModuleName exposedModules
componentEntrypoints (CExe Executable {..})
    = Left modulePath
componentEntrypoints (CTest TestSuite { testInterface = TestSuiteExeV10 _ fp })
    = Left fp
componentEntrypoints (CTest TestSuite { testInterface = TestSuiteLibV09 _ mn })
    = Right [gmModuleName mn]
componentEntrypoints (CTest TestSuite {})
    = Right []
componentEntrypoints (CBench Benchmark { benchmarkInterface = BenchmarkExeV10 _  fp})
    = Left fp
componentEntrypoints (CBench Benchmark {})
    = Left []

exeOutDir :: LocalBuildInfo -> String -> FilePath
exeOutDir lbi exeName =
  ----- Copied from Distribution/Simple/GHC.hs:buildOrReplExe
  -- exeNameReal, the name that GHC really uses (with .exe on Windows)
  let exeNameReal = exeName <.>
                    (if takeExtension exeName /= ('.':exeExtension)
                       then exeExtension
                       else "")

      targetDir = (buildDir lbi) </> exeName
  in targetDir


removeInplaceDeps :: PackageDescription
                  -> ComponentLocalBuildInfo
                  -> (ComponentLocalBuildInfo, GhcOptions)
removeInplaceDeps pd clbi = let
    (ideps, deps) = partition isInplaceDep (componentPackageDeps clbi)
    hasIdeps = not $ null ideps
    clbi' = clbi { componentPackageDeps = deps }
    lib = getLibrary pd
    src_dirs = hsSourceDirs (libBuildInfo lib)
    adopts = mempty {
               ghcOptSourcePath      =
#if CABAL_MAJOR == 1 && CABAL_MINOR >= 22
                   toNubListR src_dirs
#else
                   src_dirs
#endif

             }

  in (clbi', if hasIdeps then adopts else mempty)

 where
   isInplaceDep :: (InstalledPackageId, PackageId) -> Bool
   isInplaceDep (ipid, pid) = inplacePackageId pid == ipid

renderGhcOptions' lbi v opts = do
#if CABAL_MAJOR == 1 && CABAL_MINOR < 20
  (ghcProg, _) <- requireProgram v ghcProgram (withPrograms lbi)
  let Just ghcVer = programVersion ghcProg
  return $ renderGhcOptions ghcVer opts
#else
  return $ renderGhcOptions (compiler lbi) opts
#endif
