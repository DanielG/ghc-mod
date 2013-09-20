module Language.Haskell.GhcMod.Cradle (findCradle) where

import Data.Char (isSpace)
import Control.Applicative ((<$>))
import Control.Exception as E (catch, throwIO, SomeException)
import Control.Monad (filterM)
import Data.List (isPrefixOf, isSuffixOf)
import Language.Haskell.GhcMod.Types
import System.Directory (getCurrentDirectory, getDirectoryContents, doesFileExist)
import System.FilePath ((</>),takeDirectory)

----------------------------------------------------------------

-- | Finding 'Cradle'.
findCradle :: IO Cradle
findCradle = do
    wdir <- getCurrentDirectory
    findCradle' wdir `E.catch` handler wdir
  where
    handler :: FilePath -> SomeException -> IO Cradle
    handler wdir _ = return Cradle {
        cradleCurrentDir  = wdir
      , cradleCabalDir    = Nothing
      , cradleCabalFile   = Nothing
      , cradlePackageConf = Nothing
      }

findCradle' :: FilePath -> IO Cradle
findCradle' wdir = do
    (cdir,cfile) <- cabalDir wdir
    mPkgConf <- getPackageDbDir cdir
    return Cradle {
        cradleCurrentDir  = wdir
      , cradleCabalDir    = Just cdir
      , cradleCabalFile   = Just cfile
      , cradlePackageConf = mPkgConf
      }

----------------------------------------------------------------

cabalSuffix :: String
cabalSuffix = ".cabal"

cabalSuffixLength :: Int
cabalSuffixLength = length cabalSuffix

-- Finding a Cabal file up to the root directory
-- Input: a directly to investigate
-- Output: (the path to the directory containing a Cabal file
--         ,the path to the Cabal file)
cabalDir :: FilePath -> IO (FilePath,FilePath)
cabalDir dir = do
    cnts <- getCabalFiles dir
    case cnts of
        [] | dir' == dir -> throwIO $ userError "cabal files not found"
           | otherwise   -> cabalDir dir'
        cfile:_          -> return (dir,dir </> cfile)
  where
    dir' = takeDirectory dir

getCabalFiles :: FilePath -> IO [FilePath]
getCabalFiles dir = getFiles >>= filterM doesCabalFileExist
  where
    isCabal name = cabalSuffix `isSuffixOf` name
                && length name > cabalSuffixLength
    getFiles = filter isCabal <$> getDirectoryContents dir
    doesCabalFileExist file = doesFileExist $ dir </> file

----------------------------------------------------------------

configFile :: String
configFile = "cabal.sandbox.config"

pkgDbKey :: String
pkgDbKey = "package-db:"

pkgDbKeyLen :: Int
pkgDbKeyLen = length pkgDbKey

-- | Extract a package db directory from the sandbox config file.
getPackageDbDir :: FilePath -> IO (Maybe FilePath)
getPackageDbDir cdir = (Just <$> getPkgDb) `E.catch` handler
  where
    getPkgDb = extractValue . parse <$> readFile (cdir </> configFile)
    parse = head . filter ("package-db:" `isPrefixOf`) . lines
    extractValue = fst . break isSpace . dropWhile isSpace . drop pkgDbKeyLen
    handler :: SomeException -> IO (Maybe FilePath)
    handler _ = return Nothing