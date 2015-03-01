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

{-# LANGUAGE TemplateHaskell, OverloadedStrings, RecordWildCards #-}
module Main where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Maybe
import Data.Char
import Data.List
import Data.Maybe
import Data.String
import Data.Version
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Text.Printf
import Text.ParserCombinators.ReadP
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
import Common
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
\| DIST_DIR [CABAL_HELPER_ARGS...]\n\
\)\n"

main :: IO ()
main = do
  args <- getArgs
  case args of
    "print-appdatadir":[] -> putStrLn =<< appDataDir
    "print-build-platform":[] -> putStrLn $ display buildPlatform
    distdir:_ -> do
      cfgf <- canonicalizePath (distdir </> "setup-config")
      mhdr <- (parseHeader =<<) . listToMaybe . BS8.lines <$> BS.readFile cfgf
      case mhdr of
        Nothing -> error $ printf "\
\Could not read Cabal's persistent setup configuration header\n\
\- Check first line of: %s\n\
\- Maybe try: $ cabal configure" cfgf

        Just Header {..} -> do
          eexe <- compileHelper hdrCabalVersion
          case eexe of
              Left e -> exitWith e
              Right exe -> do
                  (_,_,_,h) <- createProcess $ proc exe args
                  exitWith =<< waitForProcess h

    _ -> usage

appDataDir :: IO FilePath
appDataDir = (</> "cabal-helper") <$> getAppUserDataDirectory "ghc-mod"

tryFindSrcDirInGhcModTree :: IO (Maybe FilePath)
tryFindSrcDirInGhcModTree  = do
  dir <- (!!4) . iterate takeDirectory <$> getExecutablePath
  exists <- doesFileExist $ dir </> "ghc-mod.cabal"
  src_exists <- doesFileExist $ dir </> "cabal-helper/Main.hs"
  if exists && src_exists
     then return $ Just (dir </> "cabal-helper")
     else return Nothing

tryFindRealSrcDir :: IO (Maybe FilePath)
tryFindRealSrcDir = do
    datadir <- getDataDir
    exists <- doesFileExist $ datadir </> "cabal-helper/Main.hs"
    return $ if exists
               then Just $ datadir </> "cabal-helper"
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
  mver <- find (sameMajorVersion cabalVer) <$> listCabalVersions
  couldBeSrcDir <- takeDirectory <$> getDataDir

  case mver of
    Nothing -> do
      let cabalFile = couldBeSrcDir </> "Cabal.cabal"
      cabal <- doesFileExist cabalFile
      if cabal
        then do
          ver <- cabalFileVersion <$> readFile cabalFile
          compile $ Compile chdir (Just couldBeSrcDir) ver []
        else errorNoCabal cabalVer
    Just ver ->
        compile $ Compile chdir Nothing ver [cabalPkgId ver]
 where
   cabalPkgId v = "Cabal-" ++ showVersion v

errorNoCabal :: Version -> a
errorNoCabal cabalVer = error $ printf "\
\No appropriate Cabal package found, wanted version %s.\n\
\- Check output of: $ ghc-pkg list Cabal\n\
\- Maybe try: $ cabal install Cabal --constraint 'Cabal == %s.*'" sver mjver
 where
   sver = showVersion cabalVer
   mjver = showVersion $ majorVer cabalVer

errorNoMain :: FilePath -> a
errorNoMain datadir = error $ printf "\
\Could not find $datadir/cabal-helper/Main.hs!\n\
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
      cabalVersion   :: Version,
      packageDeps    :: [String]
    }

compile :: Compile -> IO (Either ExitCode FilePath)
compile Compile {..} = do
    outdir <- appDataDir
    createDirectoryIfMissing True outdir

    let exe = outdir </> "cabal-helper-" ++ showVersion (majorVer cabalVersion)

    recompile <-
      case cabalSourceDir of
        Nothing -> do
          tsrcs <- timeHsFiles cabalHelperSourceDir
          texe <- timeMaybe exe
          return $ any ((texe <) . Just) tsrcs
        Just _ -> return True -- let ghc do the difficult recomp checking

    let Version (mj:mi:_) _ = cabalVersion
    let ghc_opts =
             concat [
          [ "-outputdir", outdir
          , "-o", exe
          , "-optP-DCABAL_MAJOR=" ++ show mj
          , "-optP-DCABAL_MINOR=" ++ show mi
          ],
          map ("-i"++) $ cabalHelperSourceDir:maybeToList cabalSourceDir,
          concatMap (\p -> ["-package", p]) packageDeps,
          [ "--make",  cabalHelperSourceDir </> "Main.hs" ]
         ]

    if recompile
       then do
         (_, _, _, h) <- createProcess
           (proc "ghc" ghc_opts) { std_out = UseHandle stderr }
         rv <- waitForProcess h
         return $ case rv of
                    ExitSuccess -> Right exe
                    e@(ExitFailure _) -> Left e
      else return $ Right exe

timeHsFiles :: FilePath -> IO [TimedFile]
timeHsFiles dir = do
    fs <- map (dir</>) <$> getDirectoryContents dir
    mapM timeFile =<< filterM isHsFile fs
 where
   isHsFile f = do
     exists <- doesFileExist f
     return $ exists && ".hs" `isSuffixOf` f



-- TODO: Include sandbox? Probably only relevant for build-type:custom projects.
listCabalVersions :: IO [Version]
listCabalVersions = do
  catMaybes . map (fmap snd . parsePkgId . fromString) . words
          <$> readProcess "ghc-pkg" ["list", "--simple-output", "Cabal"] ""

data Header = Header { hdrCabalVersion    :: Version
                     , hdrCompilerVersion :: Version
                     }

-- | Find @version: XXX@ delcaration in a cabal file
cabalFileVersion :: String -> Version
cabalFileVersion cabalFile = do
  fromJust $ parseVer . extract <$> find ("version" `isPrefixOf`) ls
 where
  ls = map (map toLower) $ lines cabalFile
  extract = dropWhile (/=':') >>> dropWhile isSpace >>> takeWhile (not . isSpace)

parseHeader :: ByteString -> Maybe Header
parseHeader header = case BS8.words header of
  ["Saved", "package", "config", "for", _pkgId ,
   "written", "by", cabalId,
   "using", compId]
    -> liftM2 Header (ver cabalId) (ver compId)
  _ -> error "parsing setup-config header failed"
 where
   ver i = snd <$> parsePkgId i

parsePkgId :: ByteString -> Maybe (ByteString, Version)
parsePkgId bs =
    case BS8.split '-' bs of
      [pkg, vers] -> Just (pkg, parseVer $ BS8.unpack vers)
      _ -> Nothing

parseVer :: String -> Version
parseVer vers = runReadP parseVersion vers

majorVer :: Version -> Version
majorVer (Version b _) = Version (take 2 b) []

sameMajorVersion :: Version -> Version -> Bool
sameMajorVersion a b = majorVer a == majorVer b

runReadP :: ReadP t -> String -> t
runReadP p i = let (a,""):[] = filter ((=="") . snd) $ readP_to_S p i in a
