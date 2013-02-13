{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Cabal (initializeGHC, getDirs, fromCabal) where

import CabalApi (cabalParseFile, cabalBuildInfo, cabalDependPackages)
import Control.Applicative
import Control.Exception
import Control.Monad
import CoreMonad
import Data.List
import Distribution.PackageDescription (BuildInfo(..), usedExtensions)
import Distribution.Text (display)
import ErrMsg
import GHC
import GHCApi
import GHCChoice
import System.Directory
import System.FilePath
import Types

----------------------------------------------------------------

importDirs :: [String]
importDirs = [".","..","../..","../../..","../../../..","../../../../.."]

initializeGHC :: Options -> FilePath -> [String] -> Bool -> Ghc (FilePath,LogReader)
initializeGHC opt fileName ghcOptions logging = withCabal ||> withoutCabal
  where
    withoutCabal = do
        logReader <- initSession opt ghcOptions importDirs Nothing logging
        return (fileName,logReader)
    withCabal = do
        (gopts,idirs,depPkgs) <- liftIO $ fromCabal ghcOptions
        logReader <- initSession opt gopts idirs (Just depPkgs) logging
        return (fileName,logReader)

fromCabal :: [String] -> IO ([String], [FilePath], [String])
fromCabal ghcOptions = do
    (owdir,cdir,cfile) <- getDirs
    cabal <- cabalParseFile cfile
    binfo@BuildInfo{..} <- cabalBuildInfo cabal
    let exts = map (("-X" ++) . display) $ usedExtensions binfo
        lang = maybe "-XHaskell98" (("-X" ++) . display) defaultLanguage
        libs = map ("-l" ++) extraLibs
        libDirs = map ("-L" ++) extraLibDirs
        gopts = ghcOptions ++ exts ++ [lang] ++ libs ++ libDirs
        idirs = case hsSourceDirs of
            []   -> [cdir,owdir]
            dirs -> map (cdir </>) dirs ++ [owdir]
    depPkgs <- removeMe cfile <$> cabalDependPackages cabal
    return (gopts,idirs,depPkgs)

removeMe :: FilePath -> [String] -> [String]
removeMe cabalfile depPkgs = filter (/= me) depPkgs
  where
    me = dropExtension $ takeFileName cabalfile

----------------------------------------------------------------

-- CurrentWorkingDir, CabalDir, CabalFile
getDirs :: IO (FilePath,FilePath,FilePath)
getDirs = do
    wdir <- getCurrentDirectory
    (cdir,cfile) <- cabalDir wdir
    return (wdir,cdir,cfile)

-- Causes error, catched in the upper function.
-- CabalDir, CabalFile
cabalDir :: FilePath -> IO (FilePath,FilePath)
cabalDir dir = do
    cnts <- (filter isCabal <$> getDirectoryContents dir)
            >>= filterM (\file -> doesFileExist (dir </> file))
    let dir' = takeDirectory dir
    case cnts of
        [] | dir' == dir -> throwIO $ userError "No cabal file"
           | otherwise   -> cabalDir dir'
        cfile:_          -> return (dir,dir </> cfile)
  where
    isCabal name = ".cabal" `isSuffixOf` name && length name > 6
