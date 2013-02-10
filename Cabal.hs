{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Cabal (initializeGHC) where

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
        (owdir,cdir,cfile) <- liftIO getDirs
        cabal <- liftIO $ cabalParseFile cfile
        binfo@BuildInfo{..} <- liftIO $ cabalBuildInfo cabal
        let exts = map (addX . display) $ usedExtensions binfo
            lang = maybe "-XHaskell98" (addX . display) defaultLanguage
            libs = map ("-l" ++) extraLibs
            libDirs = map ("-L" ++) extraLibDirs
            gopts = ghcOptions ++ exts ++ [lang] ++ libs ++ libDirs
            idirs = case hsSourceDirs of
                []   -> [cdir,owdir]
                dirs -> map (cdir </>) dirs ++ [owdir]
        depPkgs   <- liftIO $ cabalDependPackages cabal
        logReader <- initSession opt gopts idirs (Just depPkgs) logging
        return (fileName,logReader)
    addX = ("-X" ++)

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
