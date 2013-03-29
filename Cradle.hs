module Cradle (findCradle) where

import Control.Applicative ((<$>))
import Control.Exception (throwIO)
import Control.Monad
import Data.List (isSuffixOf)
import System.Directory
import System.FilePath ((</>),takeDirectory)
import Types

-- An error would be thrown
findCradle :: Maybe FilePath -> String -> IO Cradle
findCradle (Just sbox) strver = do
    pkgConf <- checkPackageConf sbox strver
    wdir <- getCurrentDirectory
    cfiles <- cabalDir wdir
    return $ case cfiles of
        Nothing -> Cradle {
            cradleCurrentDir      = wdir
          , cradleCabalDir        = Nothing
          , cradleCabalFile       = Nothing
          , cradlePackageConf = Just pkgConf
          }
        Just (cdir,cfile) -> Cradle {
            cradleCurrentDir      = wdir
          , cradleCabalDir        = Just cdir
          , cradleCabalFile       = Just cfile
          , cradlePackageConf = Just pkgConf
          }
findCradle Nothing strver = do
    wdir <- getCurrentDirectory
    cfiles <- cabalDir wdir
    case cfiles of
        Nothing -> return Cradle {
            cradleCurrentDir  = wdir
          , cradleCabalDir    = Nothing
          , cradleCabalFile   = Nothing
          , cradlePackageConf = Nothing
          }
        Just (cdir,cfile) -> do
            let sbox = cdir </> "cabal-dev/"
                pkgConf = packageConfName sbox strver
            exist <- doesDirectoryExist pkgConf
            return Cradle {
                cradleCurrentDir  = wdir
              , cradleCabalDir    = Just cdir
              , cradleCabalFile   = Just cfile
              , cradlePackageConf = if exist then Just pkgConf else Nothing
              }

cabalDir :: FilePath -> IO (Maybe (FilePath,FilePath))
cabalDir dir = do
    cnts <- (filter isCabal <$> getDirectoryContents dir)
            >>= filterM (\file -> doesFileExist (dir </> file))
    let dir' = takeDirectory dir
    case cnts of
        [] | dir' == dir -> return Nothing
           | otherwise   -> cabalDir dir'
        cfile:_          -> return $ Just (dir,dir </> cfile)
  where
    isCabal name = ".cabal" `isSuffixOf` name && length name > 6

packageConfName :: FilePath -> String -> FilePath
packageConfName path ver = path </> "packages-" ++ ver ++ ".conf"

checkPackageConf :: FilePath -> String -> IO FilePath
checkPackageConf path ver = do
    let conf = packageConfName path ver
    exist <- doesDirectoryExist conf
    if exist then
        return conf
      else
        throwIO $ userError $ conf ++ " not found"
