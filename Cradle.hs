module Cradle where

import CabalApi (getGHCVersion)
import Control.Applicative ((<$>))
import Control.Exception (throwIO)
import Control.Monad
import Data.List (isSuffixOf)
import System.Directory
import System.FilePath ((</>),takeDirectory)
import Types

-- An error would be thrown
findCradle :: Maybe FilePath -> IO Cradle
findCradle (Just sbox) = do
    (strver, ver) <- getGHCVersion
    conf <- checkPackageConf sbox strver
    let confOpts = ghcPackageConfOptions ver conf
    wdir <- getCurrentDirectory
    cfiles <- cabalDir wdir
    return $ case cfiles of
        Nothing -> Cradle {
            cradleCurrentDir      = wdir
          , cradleCabalDir        = Nothing
          , cradleCabalFile       = Nothing
          , cradlePackageConfOpts = Just confOpts
          , cradleGHCVersion      = strver
          }
        Just (cdir,cfile) -> Cradle {
            cradleCurrentDir      = wdir
          , cradleCabalDir        = Just cdir
          , cradleCabalFile       = Just cfile
          , cradlePackageConfOpts = Just confOpts
          , cradleGHCVersion      = strver
          }
findCradle Nothing = do
    (strver, ver) <- getGHCVersion
    wdir <- getCurrentDirectory
    cfiles <- cabalDir wdir
    case cfiles of
        Nothing -> return Cradle {
            cradleCurrentDir      = wdir
          , cradleCabalDir        = Nothing
          , cradleCabalFile       = Nothing
          , cradlePackageConfOpts = Nothing
          , cradleGHCVersion      = strver
          }
        Just (cdir,cfile) -> do
            let sbox = cdir </> "cabal-dev/"
                conf = packageConfName sbox strver
                confOpts = ghcPackageConfOptions ver conf
            exist <- doesFileExist conf
            return Cradle {
                cradleCurrentDir      = wdir
              , cradleCabalDir        = Just cdir
              , cradleCabalFile       = Just cfile
              , cradlePackageConfOpts = if exist then Just confOpts else Nothing
              , cradleGHCVersion      = strver
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
    exist <- doesFileExist conf
    if exist then
        return conf
      else
        throwIO $ userError $ conf ++ " not found"

ghcPackageConfOptions :: Int -> String -> [String]
ghcPackageConfOptions ver file
  | ver >= 706 = ["-package-db",   file, "-no-user-package-conf"]
  | otherwise  = ["-package-conf", file, "-no-user-package-conf"]
