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

{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module Main where

import Control.Applicative
import Control.Arrow
import Control.Exception as E
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Char
import Data.List
import Data.Maybe
import Data.String
import Data.Version
import Text.Printf
import System.Environment
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO

import Distribution.System (buildPlatform)
import Distribution.Text (display)

import NotCPP.Declarations

import Paths_ghc_mod
import CabalHelper.Common
import Utils

ifD [d| getExecutablePath = getProgName |]

usage :: IO ()
usage = do
  prog <- getProgName
  hPutStr stderr $ align "(" "|" ("Usage: " ++ prog ++ " " ++ usageMsg)
 where
   usageMsg = "\
\( print-appdatadir\n\
\| print-build-platform\n\
\| DIST_DIR ( print-exe | [CABAL_HELPER_ARGS...] ) )\n"

main :: IO ()
main = handlePanic $ do
  args <- getArgs
  case args of
    [] -> usage
    "--help":[] -> usage
    "print-appdatadir":[] -> putStrLn =<< appDataDir
    "print-build-platform":[] -> putStrLn $ display buildPlatform
    distdir:args' -> do
      cfgf <- canonicalizePath (distdir </> "setup-config")
      mhdr <- getCabalConfigHeader cfgf
      case mhdr of
        Nothing -> panic $ printf "\
\Could not read Cabal's persistent setup configuration header\n\
\- Check first line of: %s\n\
\- Maybe try: $ cabal configure" cfgf

        Just (hdrCabalVersion, _hdrCompilerVersion) -> do
          eexe <- compileHelper hdrCabalVersion
          case eexe of
              Left e -> exitWith e
              Right exe ->
                case args' of
                  "print-exe":_ -> putStrLn exe
                  _ -> do
                    (_,_,_,h) <- createProcess $ proc exe args
                    exitWith =<< waitForProcess h

appDataDir :: IO FilePath
appDataDir = (</> "cabal-helper") <$> getAppUserDataDirectory "ghc-mod"

tryFindSrcDirInGhcModTree :: IO (Maybe FilePath)
tryFindSrcDirInGhcModTree  = do
  dir <- (!!4) . iterate takeDirectory <$> getExecutablePath
  exists <- doesFileExist $ dir </> "ghc-mod.cabal"
  src_exists <- doesFileExist $ dir </> "CabalHelper/Main.hs"
  if exists && src_exists
     then return $ Just dir
     else return Nothing

tryFindRealSrcDir :: IO (Maybe FilePath)
tryFindRealSrcDir = do
    datadir <- getDataDir
    exists <- doesFileExist $ datadir </> "CabalHelper/Main.hs"
    return $ if exists
               then Just datadir
               else Nothing

findCabalHelperSourceDir :: IO FilePath
findCabalHelperSourceDir = do
    msrcdir <- runMaybeT $  MaybeT tryFindSrcDirInGhcModTree
                        <|> MaybeT tryFindRealSrcDir
    case msrcdir of
      Nothing -> getDataDir >>= errorNoMain
      Just datadir -> return datadir

compileHelper :: Version -> IO (Either ExitCode FilePath)
compileHelper cabalVer = do
  chdir <- findCabalHelperSourceDir

  -- First check if we already compiled this version of cabal
  db_exists <- cabalPkgDbExists cabalVer
  case db_exists of
    True -> compileWithPkg chdir . Just =<< cabalPkgDb cabalVer
    False -> do
      -- Next check if this version is globally available
      mver <- find (== cabalVer) <$> listCabalVersions
      couldBeSrcDir <- takeDirectory <$> getDataDir
      case mver of
        Nothing -> do
          -- If not see if we're in a cabal source tree
          let cabalFile = couldBeSrcDir </> "Cabal.cabal"
          cabal <- doesFileExist cabalFile
          if cabal
            then do
              ver <- cabalFileVersion <$> readFile cabalFile
              compileWithCabalTree chdir ver couldBeSrcDir
            else do
              -- otherwise compile the requested cabal version into an isolated
              -- package-db
              db <- installCabal cabalVer `E.catch`
                  \(SomeException _) -> errorInstallCabal cabalVer
              compileWithPkg chdir (Just db)
        Just _ -> do
          compileWithPkg chdir Nothing

 where
   compileWithCabalTree chdir ver srcDir =
       compile $ Compile chdir (Just srcDir) Nothing ver []

   compileWithPkg chdir mdb =
       compile $ Compile chdir Nothing mdb cabalVer [cabalPkgId cabalVer]

   cabalPkgId v = "Cabal-" ++ showVersion v

-- errorNoCabal :: Version -> a
-- errorNoCabal cabalVer = panic $ printf "\
-- \No appropriate Cabal package found, wanted version %s.\n"
--  where
--    sver = showVersion cabalVer

errorInstallCabal :: Version -> a
errorInstallCabal cabalVer = panic $ printf "\
\Installing Cabal version %s failed.\n\
\n\
\You have two choices now:\n\
\- Either you install this version of Cabal in your globa/luser package-db\n\
\  somehow\n\
\n\
\- Or you can see if you can update your cabal-install to use a different\n\
\  version of the Cabal library that we can build with:\n\
\    $ cabal install cabal-install --constraint 'Cabal > %s'\n\
\n\
\To check the version cabal-install is currently using try:\n\
\    $ cabal --version\n" sver sver
 where
   sver = showVersion cabalVer

errorNoMain :: FilePath -> a
errorNoMain datadir = panic $ printf "\
\Could not find $datadir/CabalHelper/Main.hs!\n\
\\n\
\If you are a developer you can use the environment variable `ghc_mod_datadir'\n\
\to override $datadir[1], `$ export ghc_mod_datadir=$PWD' will work in the\n\
\ghc-mod tree.\n\
\[1]: %s\n\
\\n\
\If you don't know what I'm talking about something went wrong with your\n\
\installation. Please report this problem here:\n\
\    https://github.com/kazu-yamamoto/ghc-mod/issues" datadir

data Compile = Compile {
      cabalHelperSourceDir :: FilePath,
      cabalSourceDir :: Maybe FilePath,
      packageDb      :: Maybe FilePath,
      cabalVersion   :: Version,
      packageDeps    :: [String]
    }

compile :: Compile -> IO (Either ExitCode FilePath)
compile Compile {..} = do
    outdir <- appDataDir
    createDirectoryIfMissing True outdir

    let exe = outdir </> "cabal-helper-" ++ showVersion cabalVersion

    recompile <-
      case cabalSourceDir of
        Nothing -> do
          exists <- doesFileExist exe
          case exists of
            False -> return True
            True -> do
                tsrcs <- timeHsFiles $ cabalHelperSourceDir </> "CabalHelper"
                texe <- timeFile exe
                return $ any (texe <) tsrcs
        Just _ -> return True -- let ghc do the difficult recomp checking

    let Version (mj:mi:_) _ = cabalVersion
    let ghc_opts =
             concat [
          [ "-outputdir", outdir
          , "-o", exe
          , "-optP-DCABAL_HELPER=1"
          , "-optP-DCABAL_MAJOR=" ++ show mj
          , "-optP-DCABAL_MINOR=" ++ show mi
          ],
          maybeToList $ ("-package-db="++) <$> packageDb,
          map ("-i"++) $ cabalHelperSourceDir:maybeToList cabalSourceDir,
          concatMap (\p -> ["-package", p]) packageDeps,
          [ "--make",  cabalHelperSourceDir </> "CabalHelper/Main.hs" ]
         ]

    if recompile
       then do
         -- TODO: touch exe after, ghc doesn't do that if the input files didn't
         -- actually change
         rv <- callProcessStderr' Nothing "ghc" ghc_opts
         return $ case rv of
                    ExitSuccess -> Right exe
                    e@(ExitFailure _) -> Left e
      else return $ Right exe

 where
   timeHsFiles :: FilePath -> IO [TimedFile]
   timeHsFiles dir = do
       fs <- map (dir</>) <$> getDirectoryContents dir
       mapM timeFile =<< filterM isHsFile (filter (=="Wrapper.hs") fs)
    where
      isHsFile f = do
        exists <- doesFileExist f
        return $ exists && ".hs" `isSuffixOf` f


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

installCabal :: Version -> IO FilePath
installCabal ver = do
  db <- createPkgDb ver
  callProcessStderr (Just "/") "cabal" [ "--package-db=clear"
                                       , "--package-db=global"
                                       , "--package-db=" ++ db
                                       , "--prefix=" ++ db </> "prefix"
                                       , "-j1"
                                       , "install", "Cabal-"++showVersion ver
                                       ]
  return db

createPkgDb :: Version -> IO FilePath
createPkgDb ver = do
  db <- cabalPkgDb ver
  exists <- doesDirectoryExist db
  when (not exists) $ callProcessStderr Nothing "ghc-pkg" ["init", db]
  return db

cabalPkgDb :: Version -> IO FilePath
cabalPkgDb ver = do
  appdir <- appDataDir
  return $ appdir </> "cabal-" ++ showVersion ver ++ "-db"

cabalPkgDbExists :: Version -> IO Bool
cabalPkgDbExists ver = do
  db <- cabalPkgDb ver
  dexists <- doesDirectoryExist db
  case dexists of
    False -> return False
    True -> do
      vers <- listCabalVersions' (Just db)
      return $ ver `elem` vers

listCabalVersions :: IO [Version]
listCabalVersions = listCabalVersions' Nothing

-- TODO: Include sandbox? Probably only relevant for build-type:custom projects.
listCabalVersions' :: Maybe FilePath -> IO [Version]
listCabalVersions' mdb = do
  let mdbopt = ("--package-db="++) <$> mdb
      opts = ["list", "--simple-output", "Cabal"] ++ maybeToList mdbopt

  catMaybes . map (fmap snd . parsePkgId . fromString) . words
          <$> readProcess "ghc-pkg" opts ""

-- | Find @version: XXX@ delcaration in a cabal file
cabalFileVersion :: String -> Version
cabalFileVersion cabalFile = do
  fromJust $ parseVer . extract <$> find ("version" `isPrefixOf`) ls
 where
  ls = map (map toLower) $ lines cabalFile
  extract = dropWhile (/=':') >>> dropWhile isSpace >>> takeWhile (not . isSpace)
