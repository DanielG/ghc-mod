{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Cabal (getDirs, fromCabal) where

import CabalApi
import Control.Applicative
import Control.Exception
import Control.Monad
import Data.List
import Distribution.PackageDescription (BuildInfo(..), usedExtensions)
import Distribution.Text (display)
import System.Directory
import System.FilePath
import Types

----------------------------------------------------------------

fromCabal :: [GHCOption] -> IO ([GHCOption]
                               ,[IncludeDir]
                               ,[Package]
                               ,[LangExt])
fromCabal ghcOptions = do
    (owdir,cdir,cfile) <- getDirs
    cabal <- cabalParseFile cfile
    let binfo@BuildInfo{..} = cabalBuildInfo cabal
    let exts = map (("-X" ++) . display) $ usedExtensions binfo
        lang = maybe "-XHaskell98" (("-X" ++) . display) defaultLanguage
        libs = map ("-l" ++) extraLibs
        libDirs = map ("-L" ++) extraLibDirs
        gopts = ghcOptions ++ exts ++ [lang] ++ libs ++ libDirs
        idirs = case hsSourceDirs of
            []   -> [cdir,owdir]
            dirs -> map (cdir </>) dirs ++ [owdir]
    let depPkgs = removeMe cfile $ cabalAllDependPackages cabal
        hdrExts = cabalAllExtentions cabal
    return (gopts,idirs,depPkgs,hdrExts)

removeMe :: FilePath -> [String] -> [String]
removeMe cabalfile = filter (/= me)
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
