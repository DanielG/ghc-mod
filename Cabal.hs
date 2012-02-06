{-# LANGUAGE OverloadedStrings #-}

module Cabal (initializeGHC) where

import Control.Applicative
import Control.Monad
import CoreMonad
import Data.List
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse (readPackageDescription)
import Distribution.Verbosity (silent)
import ErrMsg
import GHC
import System.Directory
import System.FilePath
import Types

----------------------------------------------------------------

importDirs :: [String]
importDirs = [".","..","../..","../../..","../../../..","../../../../.."]

initializeGHC :: Options -> FilePath -> [String] -> Bool -> Ghc (FilePath,LogReader)
initializeGHC opt fileName ghcOptions logging = do
    (owdir,mdirfile) <- liftIO getDirs
    case mdirfile of
        Nothing -> do
            logReader <- initSession opt ghcOptions importDirs logging
            return (fileName,logReader)
        Just (cdir,cfile) -> do
            midirs <- parseCabalFile cfile
            changeToCabalDirectory cdir
            let idirs = case midirs of
                    []   -> [cdir,owdir]
                    dirs -> dirs ++ [owdir]
                file = ajustFileName fileName owdir cdir
            logReader <- initSession opt ghcOptions idirs logging
            return (file,logReader)

----------------------------------------------------------------

parseCabalFile :: FilePath -> Ghc [String]
parseCabalFile file = do
    cabal <- liftIO $ readPackageDescription silent file
    return $ fromLibrary cabal ||| fromExecutable cabal
  where
    [] ||| y = y
    x  ||| _ = x
    fromLibrary c = case condLibrary c of
        Nothing  -> []
        Just lib -> libHsSourceDir lib
    libHsSourceDir = hsSourceDirs . libBuildInfo . condTreeData
    fromExecutable = execHsSrouceDir . condExecutables
    execHsSrouceDir [] = []
    execHsSrouceDir (x:_) = hsSourceDirs . buildInfo . condTreeData . snd $ x

----------------------------------------------------------------

ajustFileName :: FilePath -> FilePath -> FilePath -> FilePath
ajustFileName name olddir newdir
  | olen == nlen = name
  | otherwise    = drop (nlen+1) olddir </> name
  where
    olen = length olddir
    nlen = length newdir

changeToCabalDirectory :: FilePath -> Ghc ()
changeToCabalDirectory dir = do
    liftIO $ setCurrentDirectory dir
    workingDirectoryChanged

getDirs :: IO (FilePath, Maybe (FilePath,FilePath))
getDirs = do
    wdir <- getCurrentDirectory
    mcabdir <- cabalDir wdir
    case mcabdir of
        Nothing -> return (wdir,Nothing)
        jdf     -> return (wdir,jdf)

cabalDir :: FilePath -> IO (Maybe (FilePath,FilePath))
cabalDir dir = do
    cnts <- (filter isCabal <$> getDirectoryContents dir)
            >>= filterM (\file -> doesFileExist (dir </> file))
    case cnts of
        [] -> do
            let dir' = takeDirectory dir
            if dir' == dir
               then return Nothing
               else cabalDir dir'
        cfile:_ -> return (Just (dir,dir </> cfile))
  where
    isCabal name = ".cabal" `isSuffixOf` name
                && length name > 6
