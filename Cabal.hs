{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Cabal (initializeGHC) where

import Control.Applicative
import Control.Exception
import Control.Monad
import CoreMonad
import Data.List
import Data.Maybe
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (silent)
import ErrMsg
import GHC
import GHCApi
import GHCChoice
import qualified Gap
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
        logReader <- initSession opt ghcOptions importDirs logging
        return (fileName,logReader)
    withCabal = do
        (owdir,cdir,cfile) <- liftIO getDirs
        binfo@BuildInfo{..} <- liftIO $ parseCabalFile cfile
        let exts = map (addX . Gap.extensionToString) $ usedExtensions binfo
            lang = maybe "-XHaskell98" (addX . show) defaultLanguage
            libs = map ("-l" ++) extraLibs
            libDirs = map ("-L" ++) extraLibDirs
            gopts = ghcOptions ++ exts ++ [lang] ++ libs ++ libDirs
            idirs = case hsSourceDirs of
                []   -> [cdir,owdir]
                dirs -> map (cdir </>) dirs ++ [owdir]
        logReader <- initSession opt gopts idirs logging
        return (fileName,logReader)
    addX = ("-X" ++)

----------------------------------------------------------------

-- Causes error, catched in the upper function.
parseCabalFile :: FilePath -> IO BuildInfo
parseCabalFile file = do
    cabal <- readPackageDescription silent file
    return . fromJust $ fromLibrary cabal <|> fromExecutable cabal
  where
    fromLibrary c     = libBuildInfo . condTreeData <$> condLibrary c
    fromExecutable c  = buildInfo . condTreeData . snd <$> toMaybe (condExecutables c)
    toMaybe [] = Nothing
    toMaybe (x:_) = Just x

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
