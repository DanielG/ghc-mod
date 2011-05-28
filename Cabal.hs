{-# LANGUAGE OverloadedStrings #-}
module Cabal (initializeGHC) where

import Control.Applicative hiding (many)
import Data.Attoparsec.Char8
import Data.Attoparsec.Enumerator
import Data.Enumerator (run, ($$))
import Data.Enumerator.Binary (enumFile)
import Data.List
import GHC
import qualified HscTypes as H
import System.Directory
import System.FilePath
import Types

----------------------------------------------------------------

initializeGHC :: Options -> FilePath -> [String] -> Ghc FilePath
initializeGHC opt fileName ghcOptions = do
    (owdir,mdirfile) <- getDirs
    case mdirfile of
        Nothing -> do
            initSession opt ghcOptions Nothing
            return fileName
        Just (cdir,cfile) -> do
            midirs <- parseCabalFile cfile
            changeToCabalDirectory cdir
            let idirs = case midirs of
                    Nothing   -> [cdir,owdir]
                    Just dirs -> dirs ++ [owdir]
            initSession opt ghcOptions (Just idirs)
            return (ajustFileName fileName owdir cdir)

----------------------------------------------------------------

parseCabalFile :: FilePath -> Ghc (Maybe [String])
parseCabalFile file = H.liftIO $ do
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
    H.liftIO $ setCurrentDirectory dir
    workingDirectoryChanged

getDirs :: Ghc (FilePath, Maybe (FilePath,FilePath))
getDirs = do
    wdir <- H.liftIO $ getCurrentDirectory
    mcabdir <- cabalDir wdir
    case mcabdir of
        Nothing -> return (wdir,Nothing)
        jdf     -> return (wdir,jdf)

cabalDir :: FilePath -> Ghc (Maybe (FilePath,FilePath))
cabalDir dir = do
    cnts <- H.liftIO $ getDirectoryContents dir
    case filter isCabal cnts of
        [] -> do
            let dir' = takeDirectory dir
            if dir' == dir
               then return Nothing
               else cabalDir dir'
        cfile:_ -> return (Just (dir,dir </> cfile))
  where
    isCabal name = ".cabal" `isSuffixOf` name
                && length name > 6
