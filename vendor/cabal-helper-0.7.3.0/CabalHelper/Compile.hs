-- cabal-helper: Simple interface to Cabal's configuration state
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
{-# LANGUAGE RecordWildCards, FlexibleContexts #-}
module CabalHelper.Compile where

import Control.Applicative
import Control.Arrow
import Control.Exception as E
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Traversable
import Data.Char
import Data.List
import Data.Maybe
import Data.String
import Text.Printf
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO
import System.IO.Temp
import Prelude

import Distribution.System (buildPlatform)
import Distribution.Text (display)
import Distribution.Version

import Paths_cabal_helper (version)
import CabalHelper.Data
import CabalHelper.Common
import CabalHelper.Sandbox (getSandboxPkgDb)
import CabalHelper.Types
import CabalHelper.Log

data Compile = Compile {
      compCabalHelperSourceDir :: FilePath,
      compCabalSourceDir :: Maybe FilePath,
      compPackageDb      :: Maybe FilePath,
      compCabalVersion   :: Either String Version,
      compPackageDeps    :: [String]
    }

compileHelper :: Options -> Version -> FilePath -> FilePath -> IO (Either ExitCode FilePath)
compileHelper opts cabalVer projdir distdir = withHelperSources $ \chdir -> do
  case cabalPkgDb opts of
    Nothing ->
      run [
            -- TODO: here ghc's caching fails and it always recompiles, probably
            -- because we write the sources to a tempdir and they always look
            -- newer than the Cabal sources, not sure if we can fix this
            compileCabalSource chdir
          , Right <$> MaybeT (cachedExe cabalVer)
          , compileSandbox chdir
          , compileGlobal chdir
          , cachedCabalPkg chdir
          , MaybeT (Just <$> compilePrivatePkgDb chdir)
          ]
    mdb ->
      run [ Right <$> MaybeT (cachedExe cabalVer)
          , liftIO $ compileWithPkg chdir mdb cabalVer
          ]

 where
   run actions = fromJust <$> runMaybeT (msum actions)

   logMsg = "compiling helper with Cabal from "


-- for relaxed deps: find (sameMajorVersionAs cabalVer) . reverse . sort

   -- | Check if this version is globally available
   compileGlobal :: FilePath -> MaybeT IO (Either ExitCode FilePath)
   compileGlobal chdir = do
       ver <- MaybeT $ find (== cabalVer) <$> listCabalVersions opts
       vLog opts $ logMsg ++ "user/global package-db"
       liftIO $ compileWithPkg chdir Nothing ver

   -- | Check if this version is available in the project sandbox
   compileSandbox :: FilePath -> MaybeT IO (Either ExitCode FilePath)
   compileSandbox chdir = do
       sandbox <- MaybeT $ getSandboxPkgDb projdir (display buildPlatform) =<< ghcVersion opts
       ver <- MaybeT $ logSomeError opts "compileSandbox" $
         find (== cabalVer) <$> listCabalVersions' opts (Just sandbox)
       vLog opts $ logMsg ++ "sandbox package-db"
       liftIO $ compileWithPkg chdir (Just sandbox) ver


   -- | Check if we already compiled this version of cabal into a private
   -- package-db
   cachedCabalPkg :: FilePath -> MaybeT IO (Either ExitCode FilePath)
   cachedCabalPkg chdir = do
       db_exists <- liftIO $ cabalPkgDbExists opts cabalVer
       case db_exists of
         False -> mzero
         True -> do
             db <- liftIO $ getPrivateCabalPkgDb opts (showVersion cabalVer)
             vLog opts $ logMsg ++ "private package-db in " ++ db
             liftIO $ compileWithPkg chdir (Just db) cabalVer

   -- | See if we're in a cabal source tree
   compileCabalSource :: FilePath -> MaybeT IO (Either ExitCode FilePath)
   compileCabalSource chdir = do
       let cabalFile = projdir </> "Cabal.cabal"
           isCabalMagicVer = cabalVer == mkVersion [1,9999]
       cabalSrc <- liftIO $ doesFileExist cabalFile

       when isCabalMagicVer $
         vLog opts $ "cabal magic version (1.9999) found"

       when cabalSrc $
         vLog opts $ "directory above distdir looks like cabal source tree (Cabal.cabal exists)"

       case isCabalMagicVer || cabalSrc of
         False -> mzero
         True -> liftIO $ do
           ver <- cabalFileVersion <$> readFile cabalFile
           vLog opts $ "compiling helper with local Cabal source tree"
           compileWithCabalTree chdir ver projdir

   -- | Compile the requested cabal version into an isolated package-db
   compilePrivatePkgDb :: FilePath -> IO (Either ExitCode FilePath)
   compilePrivatePkgDb chdir = do
       db <- installCabal opts cabalVer `E.catch`
             \(SomeException _) -> errorInstallCabal cabalVer distdir
       compileWithPkg chdir (Just db) cabalVer

   compileWithCabalTree chdir ver srcDir =
       compile distdir opts $ Compile chdir (Just srcDir) Nothing (Right ver) []

   compileWithPkg chdir mdb ver =
       compile distdir opts $ Compile chdir Nothing mdb (Right ver) [cabalPkgId ver]

   cabalPkgId v = "Cabal-" ++ showVersion v

compile :: FilePath -> Options -> Compile -> IO (Either ExitCode FilePath)
compile distdir opts@Options {..} Compile {..} = do
    cCabalSourceDir <- canonicalizePath `traverse` compCabalSourceDir
    appdir <- appDataDir

    let outdir' = maybe appdir (const $ distdir </> "cabal-helper") cCabalSourceDir
    createDirectoryIfMissing True outdir'
    outdir <- canonicalizePath outdir'

    let exedir' = maybe outdir (const distdir) cCabalSourceDir
    createDirectoryIfMissing True exedir'
    exedir <- canonicalizePath exedir'
    exe <- exePath' compCabalVersion <$> canonicalizePath exedir

    vLog opts $ "outdir: " ++ outdir
    vLog opts $ "exedir: " ++ exedir

    let (mj:mi:_) = case compCabalVersion of
                     Left _commitid -> [1, 10000]
                     Right vv -> versionNumbers vv -- (Version vs _) -> vs
    let ghc_opts =
             concat [
          [ "-outputdir", outdir
          , "-o", exe
          , "-optP-DCABAL_HELPER=1"
          , "-optP-DCABAL_MAJOR=" ++ show mj
          , "-optP-DCABAL_MINOR=" ++ show mi
          ],
          maybeToList $ ("-package-conf="++) <$> compPackageDb,
          map ("-i"++) $ nub $ ".":maybeToList cCabalSourceDir,

          if isNothing cCabalSourceDir
             then [ "-hide-all-packages"
                  , "-package", "base"
                  , "-package", "containers"
                  , "-package", "directory"
                  , "-package", "filepath"
                  , "-package", "process"
                  , "-package", "bytestring"
                  , "-package", "ghc-prim"
                  ]
             else [],

          concatMap (\p -> ["-package", p]) compPackageDeps,
          [ "--make",  "CabalHelper/Main.hs" ]
         ]

    vLog opts $ intercalate " " $ map (("\""++) . (++"\"")) $ ghcProgram:ghc_opts

    -- TODO: touch exe after, ghc doesn't do that if the input files didn't
    -- actually change
    rv <- callProcessStderr' (Just compCabalHelperSourceDir) ghcProgram ghc_opts
    return $ case rv of
               ExitSuccess -> Right exe
               e@(ExitFailure _) -> Left e

exePath :: Either String Version -> IO FilePath
exePath compCabalVersion = do
    exePath' compCabalVersion <$> appDataDir

exePath' :: Either String Version -> FilePath -> FilePath
exePath' (Left commitid) outdir =
    outdir </> "cabal-helper-" ++ showVersion (mkVersion' version) -- our ver
            ++ "-Cabal-HEAD-" ++ commitid
exePath' (Right compCabalVersion) outdir =
    outdir </> "cabal-helper-" ++ showVersion (mkVersion' version) -- our ver
            ++ "-Cabal-" ++ showVersion compCabalVersion

callProcessStderr' :: Maybe FilePath -> FilePath -> [String] -> IO ExitCode
callProcessStderr' mwd exe args = do
  (_, _, _, h) <- createProcess (proc exe args) { std_out = UseHandle stderr
                                                , cwd = mwd }
  waitForProcess h

callProcessStderr :: Maybe FilePath -> FilePath -> [String] -> IO ()
callProcessStderr mwd exe args = do
  rv <- callProcessStderr' mwd exe args
  case rv of
    ExitSuccess -> return ()
    ExitFailure v -> processFailedException "callProcessStderr" exe args v

processFailedException :: String -> String -> [String] -> Int -> IO a
processFailedException fn exe args rv =
      panic $ concat [fn, ": ", exe, " "
                     , intercalate " " (map show args)
                     , " (exit " ++ show rv ++ ")"]

installCabal :: Options -> Version -> IO FilePath
installCabal opts ver = do
  appdir <- appDataDir
  let sver = showVersion ver
  hPutStr stderr $ printf "\
\cabal-helper-wrapper: Installing a private copy of Cabal because we couldn't\n\
\find the right version in your global/user package-db, this might take a\n\
\while but will only happen once per Cabal version you're using.\n\
\\n\
\If anything goes horribly wrong just delete this directory and try again:\n\
\    %s\n\
\\n\
\If you want to avoid this automatic installation altogether install\n\
\version %s of Cabal manually (into your user or global package-db):\n\
\    $ cabal install Cabal --constraint \"Cabal == %s\"\n\
\\n\
\Installing Cabal %s ...\n" appdir sver sver sver

  withSystemTempDirectory "cabal-helper" $ \tmpdir -> do
    let
        mpatch :: Maybe (FilePath -> IO ())
        mpatch = snd <$> find ((ver`elem`) . fst) patchyCabalVersions
    msrcdir <- sequenceA $ unpackPatchedCabal opts ver tmpdir <$> mpatch
    db <- createPkgDb opts (showVersion ver)
    cabalInstall opts db (maybe (Right ver) Left msrcdir)
    return db

installCabalHEAD :: Options -> IO (FilePath, String)
installCabalHEAD opts = do
  withSystemTempDirectory "cabal-helper" $ \tmpdir -> do
    (srcdir, commit) <- unpackCabalHEAD tmpdir
    db <- createPkgDb opts commit
    cabalInstall opts db (Left srcdir)
    return (db, commit)

cabalInstall :: Options -> FilePath -> Either FilePath Version -> IO ()
cabalInstall opts db e_ver_msrcdir = do
  cabalInstallVer <- cabalInstallVersion opts
  cabal_opts <- return $ concat
      [
        [ "--package-db=clear"
        , "--package-db=global"
        , "--package-db=" ++ db
        , "--prefix=" ++ db </> "prefix"
        , "--with-ghc=" ++ ghcProgram opts
        ]
        , if cabalInstallVer >= mkVersion [1,20,0,0]
             then ["--no-require-sandbox"]
             else []
        , if ghcPkgProgram opts /= ghcPkgProgram defaultOptions
            then [ "--with-ghc-pkg=" ++ ghcPkgProgram opts ]
            else []
        ,
          case e_ver_msrcdir of
            Right ver ->
                [ "install", "Cabal"
                , "--constraint", "Cabal == " ++ showVersion ver
                ]
            Left srcdir ->
                [ "install", srcdir ]
      ]

  vLog opts $ intercalate " "
            $ map (("\""++) . (++"\""))
            $ cabalProgram opts:cabal_opts

  callProcessStderr (Just "/") (cabalProgram opts) cabal_opts
  hPutStrLn stderr "done"

patchyCabalVersions :: [([Version], FilePath -> IO ())]
patchyCabalVersions = [
    ( [ mkVersion [1,18,1] ]
    , fixArrayConstraint
    ),


    ( [ mkVersion [1,18,0] ]
    , \dir -> do
        fixArrayConstraint dir
        fixOrphanInstance dir
    ),

    -- just want the pristine version
    ( [ mkVersion [1,24,1,0] ]
    , \_ -> return ()
    )
  ]
 where
   fixArrayConstraint dir = do
     let cabalFile    = dir </> "Cabal.cabal"
         cabalFileTmp = cabalFile ++ ".tmp"

     cf <- readFile cabalFile
     writeFile cabalFileTmp $ replace "&& < 0.5" "&& < 0.6" cf
     renameFile cabalFileTmp cabalFile

   fixOrphanInstance dir = do
     let versionFile    = dir </> "Distribution/Version.hs"
         versionFileTmp = versionFile ++ ".tmp"

     let languagePragma =
           "{-# LANGUAGE DeriveDataTypeable, StandaloneDeriving #-}"
         languagePragmaCPP =
           "{-# LANGUAGE CPP, DeriveDataTypeable, StandaloneDeriving #-}"

         derivingDataVersion =
           "deriving instance Data Version"
         derivingDataVersionCPP = unlines [
             "#if __GLASGOW_HASKELL__ < 707",
             derivingDataVersion,
             "#endif"
           ]

     vf <- readFile versionFile
     writeFile versionFileTmp
       $ replace derivingDataVersion derivingDataVersionCPP
       $ replace languagePragma languagePragmaCPP vf

     renameFile versionFileTmp versionFile

unpackPatchedCabal ::
    Options -> Version -> FilePath -> (FilePath -> IO ()) -> IO FilePath
unpackPatchedCabal opts cabalVer tmpdir patch = do
  dir <- unpackCabal opts cabalVer tmpdir
  patch dir
  return dir

unpackCabal ::
    Options -> Version -> FilePath -> IO FilePath
unpackCabal opts cabalVer tmpdir = do
  let cabal = "Cabal-" ++ showVersion cabalVer
      dir = tmpdir </> cabal
  callProcessStderr (Just tmpdir) (cabalProgram opts)
                    [ "get", "--pristine", cabal ]
  return dir

unpackCabalHEAD :: FilePath -> IO (FilePath, String)
unpackCabalHEAD tmpdir = do
  let dir = tmpdir </> "cabal-head.git"
      url = "https://github.com/haskell/cabal.git"
  ExitSuccess <- rawSystem "git" [ "clone", "--depth=1", url, dir]
  commit <- trim <$> readProcess "git" ["-C", dir, "rev-parse", "HEAD"] ""
  return (dir </> "Cabal", commit)

errorInstallCabal :: Version -> FilePath -> a
errorInstallCabal cabalVer _distdir = panic $ printf "\
\Installing Cabal version %s failed.\n\
\\n\
\You have the following choices to fix this:\n\
\\n\
\- The easiest way to try and fix this is just reconfigure the project and try\n\
\  again:\n\
\        $ cabal clean && cabal configure\n\
\\n\
\- If that fails you can try to install the version of Cabal mentioned above\n\
\  into your global/user package-db somehow, you'll probably have to fix\n\
\  something otherwise it wouldn't have failed above:\n\
\        $ cabal install Cabal --constraint 'Cabal == %s'\n\
\\n\
\- If you're using `Build-Type: Simple`:\n\
\  - You can see if you can reinstall your cabal-install executable while\n\
\    having it linked to a version of Cabal that's available in you\n\
\    package-dbs or can be built automatically:\n\
\        $ ghc-pkg list | grep Cabal  # find an available Cabal version\n\
\            Cabal-W.X.Y.Z\n\
\        $ cabal install cabal-install --constraint 'Cabal == W.X.*'\n\
\    Afterwards you'll have to reconfigure your project:\n\
\        $ cabal clean && cabal configure\n\
\\n\
\- If you're using `Build-Type: Custom`:\n\
\  - Have cabal-install rebuild your Setup.hs executable with a version of the\n\
\    Cabal library that you have available in your global/user package-db:\n\
\        $ cabal clean && cabal configure\n\
\    You might also have to install some version of the Cabal to do this:\n\
\        $ cabal install Cabal\n\
\\n" sver sver
 where
   sver = showVersion cabalVer

cachedExe :: Version -> IO (Maybe FilePath)
cachedExe compCabalVersion = do
   exe <- exePath (Right compCabalVersion)
   exists <- doesFileExist exe
   return $ if exists then Just exe else Nothing

listCabalVersions :: Options -> IO [Version]
listCabalVersions opts = listCabalVersions' opts Nothing

-- TODO: Include sandbox? Probably only relevant for build-type:custom projects.
listCabalVersions' :: Options -> Maybe FilePath -> IO [Version]
listCabalVersions' Options {..} mdb = do
  let mdbopt = ("--package-conf="++) <$> mdb
      opts = ["list", "--simple-output", "Cabal"] ++ maybeToList mdbopt

  catMaybes . map (fmap snd . parsePkgId . fromString) . words
          <$> readProcess ghcPkgProgram opts ""

cabalPkgDbExists :: Options -> Version -> IO Bool
cabalPkgDbExists opts ver = do
  db <- getPrivateCabalPkgDb opts (showVersion ver)
  dexists <- doesDirectoryExist db
  case dexists of
    False -> return False
    True -> do
      vers <- listCabalVersions' opts (Just db)
      return $ ver `elem` vers


ghcVersion :: Options -> IO Version
ghcVersion Options {..} = do
    parseVer . trim <$> readProcess ghcProgram ["--numeric-version"] ""

ghcPkgVersion :: Options -> IO Version
ghcPkgVersion Options {..} = do
    parseVer . trim . dropWhile (not . isDigit) <$> readProcess ghcPkgProgram ["--version"] ""

cabalInstallVersion :: Options -> IO Version
cabalInstallVersion Options {..} = do
    parseVer . trim <$> readProcess cabalProgram ["--numeric-version"] ""

trim :: String -> String
trim = dropWhileEnd isSpace

createPkgDb :: Options -> String -> IO FilePath
createPkgDb opts@Options {..} ver = do
  db <- getPrivateCabalPkgDb opts ver
  exists <- doesDirectoryExist db
  when (not exists) $ callProcessStderr Nothing ghcPkgProgram ["init", db]
  return db

getPrivateCabalPkgDb :: Options -> String -> IO FilePath
getPrivateCabalPkgDb opts ver = do
  appdir <- appDataDir
  ghcVer <- ghcVersion opts
  return $ appdir </> "Cabal-" ++ ver ++ "-db-" ++ showVersion ghcVer

-- | Find @version: XXX@ delcaration in a cabal file
cabalFileVersion :: String -> Version
cabalFileVersion cabalFile =
  fromJust $ parseVer . extract <$> find ("version:" `isPrefixOf`) ls
 where
  ls = map (map toLower) $ lines cabalFile
  extract = dropWhile (/=':') >>> drop 1 >>> dropWhile isSpace >>> takeWhile (not . isSpace)
