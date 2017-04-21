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
module Main where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Data.String
import Text.Printf
import System.Console.GetOpt
import System.Environment
import System.Directory
import System.FilePath
import System.Process
import System.Exit
import System.IO
import Prelude

import Distribution.System (buildPlatform)
import Distribution.Text (display)
import Distribution.Verbosity (silent, deafening)
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Package (packageName, packageVersion)
import Distribution.Version

import Paths_cabal_helper (version)
import CabalHelper.Common
import CabalHelper.GuessGhc
import CabalHelper.Compile
import CabalHelper.Types

usage :: IO ()
usage = do
  prog <- getProgName
  hPutStr stderr $ "Usage: " ++ prog ++ " " ++ usageMsg
 where
   usageMsg = "\
\( print-appdatadir\n\
\| print-build-platform\n\
\| [--verbose]\n\
\  [--with-ghc=GHC_PATH]\n\
\  [--with-ghc-pkg=GHC_PKG_PATH]\n\
\  [--with-cabal=CABAL_PATH]\n\
\  [--with-cabal-version=VERSION]\n\
\  [--with-cabal-pkg-db=PKG_DB]\n\
\  PROJ_DIR DIST_DIR ( print-exe | package-id | [CABAL_HELPER_ARGS...] ) )\n"

globalArgSpec :: [OptDescr (Options -> Options)]
globalArgSpec =
      [ option "" ["verbose"] "Be more verbose" $
              NoArg $ \o -> o { verbose = True }

      , option "" ["with-ghc"] "GHC executable to use" $
              reqArg "PROG" $ \p o -> o { ghcProgram = p }

      , option "" ["with-ghc-pkg"] "ghc-pkg executable to use (only needed when guessing from GHC path fails)" $
              reqArg "PROG" $ \p o -> o { ghcPkgProgram = p }

      , option "" ["with-cabal"] "cabal-install executable to use" $
               reqArg "PROG" $ \p o -> o { cabalProgram = p }

      , option "" ["with-cabal-version"] "Cabal library version to use" $
               reqArg "VERSION" $ \p o -> o { cabalVersion = Just $ parseVer p }

      , option "" ["with-cabal-pkg-db"] "package database to look for Cabal library in" $
               reqArg "PKG_DB" $ \p o -> o { cabalPkgDb = Just p }

      ]
 where
   option :: [Char] -> [String] -> String -> ArgDescr a -> OptDescr a
   option s l udsc dsc = Option s l dsc udsc

   reqArg :: String -> (String -> a) -> ArgDescr a
   reqArg udsc dsc = ReqArg dsc udsc

parseCommandArgs :: Options -> [String] -> (Options, [String])
parseCommandArgs opts argv
    = case getOpt RequireOrder globalArgSpec argv of
        (o,r,[])   -> (foldr id opts o, r)
        (_,_,errs) ->
            panic $ "Parsing command options failed:\n" ++ concat errs

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
    "help":[] -> usage
    "version":[] -> putStrLn $ showVersion (mkVersion' version)
    "print-appdatadir":[] -> putStrLn =<< appDataDir
    "print-build-platform":[] -> putStrLn $ display buildPlatform

    projdir:_distdir:"package-id":[] -> do
      v <- maybe silent (const deafening) . lookup  "GHC_MOD_DEBUG" <$> getEnvironment
      -- ghc-mod will catch multiple cabal files existing before we get here
      [cfile] <- filter isCabalFile <$> getDirectoryContents projdir
      gpd <- readPackageDescription v (projdir </> cfile)
      putStrLn $ show $
        [Just $ ChResponseVersion (display (packageName gpd)) (packageVersion gpd)]

    projdir:distdir:args' -> do
      cfgf <- canonicalizePath (distdir </> "setup-config")
      mhdr <- getCabalConfigHeader cfgf
      case mhdr of
        Nothing -> panic $ printf "\
\Could not read Cabal's persistent setup configuration header\n\
\- Check first line of: %s\n\
\- Maybe try: $ cabal configure" cfgf
        Just (hdrCabalVersion, _) -> do
          case cabalVersion opts of
            Just ver | hdrCabalVersion /= ver -> panic $ printf "\
\Cabal version %s was requested setup configuration was\n\
\written by version %s" (showVersion ver) (showVersion hdrCabalVersion)
            _ -> do
              eexe <- compileHelper opts hdrCabalVersion projdir distdir
              case eexe of
                  Left e -> exitWith e
                  Right exe ->
                    case args' of
                      "print-exe":_ -> putStrLn exe
                      _ -> do
                        (_,_,_,h) <- createProcess $ proc exe args
                        exitWith =<< waitForProcess h
    _ -> error "invalid command line"
