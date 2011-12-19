{-# LANGUAGE OverloadedStrings #-}
module Cabal (initializeGHC) where

import Control.Applicative
import Control.Monad
import CoreMonad
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Enumerator
import Data.Enumerator (run, ($$))
import Data.Enumerator.Binary (enumFile)
import Data.List
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
                    Nothing   -> [cdir,owdir]
                    Just dirs -> dirs ++ [owdir]
                file = ajustFileName fileName owdir cdir
            logReader <- initSession opt ghcOptions idirs logging
            return (file,logReader)

----------------------------------------------------------------

parseCabalFile :: FilePath -> Ghc (Maybe [String])
parseCabalFile file = liftIO $ do
    res <- run (enumFile file $$ iterParser findTarget)
    case res of
        Right x -> return x
        Left  e -> error (show e)

findTarget :: Parser (Maybe [String])
findTarget = Just <$> hs_source_dirs
         <|> (anyChar >> findTarget)
         <|> Nothing <$ endOfInput

hs_source_dirs :: Parser [String]
hs_source_dirs = do
    stringCI "hs-source-dirs:"
    many (char ' ')
    sepBy1 (many . satisfy $ notInClass " ,\n") (many1 . satisfy $ inClass " ,")

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
