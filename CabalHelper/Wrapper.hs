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

{-# LANGUAGE TemplateHaskell, RecordWildCards, FlexibleContexts #-}
module Main where

import Control.Applicative
import Control.Arrow
import Control.Exception as E
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Data.Char
import Data.List
import Data.Maybe
import Data.String
import Data.Version
import Text.Printf
import System.Console.GetOpt
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
import CabalHelper.GuessGhc
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

data Options = Options {
          ghcProgram    :: FilePath
        , ghcPkgProgram :: FilePath
        , cabalProgram  :: FilePath
}

defaultOptions :: Options
defaultOptions = Options "ghc" "ghc-pkg" "cabal"

globalArgSpec :: [OptDescr (Options -> Options)]
globalArgSpec =
      [ option "" ["with-ghc"] "GHC executable to use" $
              reqArg "PROG" $ \p o -> o { ghcProgram = p }

      , option "" ["with-ghc-pkg"] "ghc-pkg executable to use (only needed when guessing from GHC path fails)" $
              reqArg "PROG" $ \p o -> o { ghcPkgProgram = p }

      , option "" ["with-cabal"] "cabal-install executable to use" $
               reqArg "PROG" $ \p o -> o { cabalProgram = p }
      ]
 where
   option :: [Char] -> [String] -> String -> ArgDescr a -> OptDescr a
   option s l udsc dsc = Option s l dsc udsc

   reqArg :: String -> (String -> a) -> ArgDescr a
   reqArg udsc dsc = ReqArg dsc udsc

parseCommandArgs :: Options -> [String] -> (Options, [String])
parseCommandArgs opts argv
    = case getOpt Permute globalArgSpec argv of
        (o,r,[])   -> (foldr id opts o, r)
        (_,_,errs) ->
            panic $ "Parsing command options failed: " ++ concat errs

guessProgramPaths :: Options -> IO Options
guessProgramPaths opts = do
    if not (same ghcProgram opts dopts) && same ghcPkgProgram opts dopts
       then do
         mghcPkg <- guessToolFromGhcPath "ghc-pkg" (ghcProgram opts)
         return opts {
           ghcPkgProgram = fromMaybe (ghcPkgProgram opts) mghcPkg
         }
       else return opts
 where
   same f o o'  = f o == f o'
   dopts = defaultOptions

main :: IO ()
main = handlePanic $ do
  (opts', args) <- parseCommandArgs defaultOptions <$> getArgs
  opts <- guessProgramPaths opts'
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
          eexe <- compileHelper opts hdrCabalVersion
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

compileHelper :: Options -> Version -> IO (Either ExitCode FilePath)
compileHelper opts cabalVer = do
  chdir <- findCabalHelperSourceDir
  run [ Right <$> MaybeT (cachedExe cabalVer chdir)
      , compileGlobal chdir
      , cachedCabalPkg chdir
      , compileCabalSource chdir
      , MaybeT (Just <$> compileSandbox chdir)
      ]

 where
   run actions = fromJust <$> runMaybeT (msum actions)

   -- | Check if this version is globally available
   compileGlobal :: FilePath -> MaybeT IO (Either ExitCode FilePath)
   compileGlobal chdir = do
       _ <- MaybeT $ find (== cabalVer) <$> listCabalVersions opts
       liftIO $ compileWithPkg chdir Nothing

   -- | Check if we already compiled this version of cabal into a private
   -- package-db
   cachedCabalPkg :: FilePath -> MaybeT IO (Either ExitCode FilePath)
   cachedCabalPkg chdir = do
       db_exists <- liftIO $ cabalPkgDbExists opts cabalVer
       case db_exists of
         False -> mzero
         True -> liftIO $ do
             db <- cabalPkgDb opts cabalVer
             compileWithPkg chdir (Just db)

   -- | See if we're in a cabal source tree
   compileCabalSource :: FilePath -> MaybeT IO (Either ExitCode FilePath)
   compileCabalSource chdir = do
       couldBeSrcDir <- liftIO $ takeDirectory <$> getDataDir
       let cabalFile = couldBeSrcDir </> "Cabal.cabal"
       cabal <- liftIO $ doesFileExist cabalFile
       case cabal of
         False -> mzero
         True -> liftIO $ do
           ver <- cabalFileVersion <$> readFile cabalFile
           compileWithCabalTree chdir ver couldBeSrcDir

   -- | Compile the requested cabal version into an isolated package-db
   compileSandbox :: FilePath -> IO (Either ExitCode FilePath)
   compileSandbox chdir = do
       db <- installCabal opts cabalVer `E.catch`
             \(SomeException _) -> errorInstallCabal cabalVer
       compileWithPkg chdir (Just db)

   compileWithCabalTree chdir ver srcDir =
       compile opts $ Compile chdir (Just srcDir) Nothing ver []

   compileWithPkg chdir mdb =
       compile opts $ Compile chdir Nothing mdb cabalVer [cabalPkgId cabalVer]

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

compile :: Options -> Compile -> IO (Either ExitCode FilePath)
compile Options {..} Compile {..} = do
    outdir <- appDataDir
    createDirectoryIfMissing True outdir
    exe <- exePath cabalVersion

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

    -- TODO: touch exe after, ghc doesn't do that if the input files didn't
    -- actually change
    rv <- callProcessStderr' Nothing ghcProgram ghc_opts
    return $ case rv of
               ExitSuccess -> Right exe
               e@(ExitFailure _) -> Left e

exePath :: Version -> IO FilePath
exePath cabalVersion = do
    outdir <- appDataDir
    return $ outdir </> "cabal-helper-" ++ showVersion cabalVersion

cachedExe :: Version -> FilePath -> IO (Maybe FilePath)
cachedExe cabalVersion chdir = do
   exe <- exePath cabalVersion
   exists <- doesFileExist exe
   case exists of
     False -> return Nothing
     True -> do
       texe <- timeFile exe
       tsrcs <- mapM timeFile srcFiles
       return $ if any (texe <) tsrcs then Nothing else Just exe
 where
   srcFiles =
       map ((chdir </> "CabalHelper") </>) ["Main.hs", "Common.hs", "Types.hs"]

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
  hPutStr stderr $ printf "\
\cabal-helper-wrapper: Installing a private copy of Cabal, this might take a\n\
\while but will only happen once per Cabal version.\n\
\\n\
\If anything goes horribly wrong just delete this directory and try again:\n\
\    %s\n\
\\n\
\If you want to avoid this automatic installation altogether install version\n\
\%s of Cabal manually (into your use or global package-db):\n\
\    $ cabal install Cabal-%s\n\
\..." appdir (showVersion ver) (showVersion ver)

  db <- createPkgDb opts ver
  callProcessStderr (Just "/") (cabalProgram opts) $ concat
      [
        [ "--package-db=clear"
        , "--package-db=global"
        , "--package-db=" ++ db
        , "--prefix=" ++ db </> "prefix"
        , "-v0"
        , "--with-ghc=" ++ ghcProgram opts
        ]
        , if ghcPkgProgram opts /= ghcPkgProgram defaultOptions
            then [ "--with-ghc-pkg=" ++ ghcPkgProgram opts ]
            else []
        , [ "install", "Cabal-"++showVersion ver ]
      ]
  hPutStrLn stderr "Done"
  return db

ghcVersion :: Options -> IO Version
ghcVersion Options {..} = do
    parseVer . trim <$> readProcess ghcProgram ["--numeric-version"] ""

ghcPkgVersion :: Options -> IO Version
ghcPkgVersion Options {..} = do
    parseVer . trim <$> readProcess ghcPkgProgram ["--numeric-version"] ""

trim :: String -> String
trim = dropWhileEnd isSpace

createPkgDb :: Options -> Version -> IO FilePath
createPkgDb opts@Options {..} ver = do
  db <- cabalPkgDb opts ver
  exists <- doesDirectoryExist db
  when (not exists) $ callProcessStderr Nothing ghcPkgProgram ["init", db]
  return db

cabalPkgDb :: Options -> Version -> IO FilePath
cabalPkgDb opts ver = do
  appdir <- appDataDir
  ghcVer <- ghcVersion opts
  return $ appdir </> "Cabal-" ++ showVersion ver ++ "-db-" ++ showVersion ghcVer

cabalPkgDbExists :: Options -> Version -> IO Bool
cabalPkgDbExists opts ver = do
  db <- cabalPkgDb opts ver
  dexists <- doesDirectoryExist db
  case dexists of
    False -> return False
    True -> do
      vers <- listCabalVersions' opts (Just db)
      return $ ver `elem` vers

listCabalVersions :: Options -> IO [Version]
listCabalVersions opts = listCabalVersions' opts Nothing

-- TODO: Include sandbox? Probably only relevant for build-type:custom projects.
listCabalVersions' :: Options -> Maybe FilePath -> IO [Version]
listCabalVersions' Options {..} mdb = do
  let mdbopt = ("--package-db="++) <$> mdb
      opts = ["list", "--simple-output", "Cabal"] ++ maybeToList mdbopt

  catMaybes . map (fmap snd . parsePkgId . fromString) . words
          <$> readProcess ghcPkgProgram opts ""

-- | Find @version: XXX@ delcaration in a cabal file
cabalFileVersion :: String -> Version
cabalFileVersion cabalFile = do
  fromJust $ parseVer . extract <$> find ("version" `isPrefixOf`) ls
 where
  ls = map (map toLower) $ lines cabalFile
  extract = dropWhile (/=':') >>> dropWhile isSpace >>> takeWhile (not . isSpace)
