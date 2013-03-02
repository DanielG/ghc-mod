{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

module Cabal (fromCabal) where

import CabalApi
import Distribution.PackageDescription (BuildInfo(..), usedExtensions)
import Distribution.Text (display)
import System.FilePath
import Types

----------------------------------------------------------------

fromCabal :: [GHCOption]
           -> Cradle
           -> IO ([GHCOption]
                 ,[IncludeDir]
                 ,[Package]
                 ,[LangExt])
fromCabal ghcOptions cradle = do
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
  where
    owdir = cradleCurrentDir cradle
    Just cdir = cradleCabalDir cradle
    Just cfile = cradleCabalDir cradle

removeMe :: FilePath -> [String] -> [String]
removeMe cabalfile = filter (/= me)
  where
    me = dropExtension $ takeFileName cabalfile
