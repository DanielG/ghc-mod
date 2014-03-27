{-# LANGUAGE BangPatterns #-}

module Language.Haskell.GhcMod.Cradle (
    findCradle
  , findCradleWithoutSandbox
  , getPackageDbDir
  , getPackageDbPackages
  ) where

import Control.Applicative ((<$>))
import Control.Exception (SomeException(..))
import qualified Control.Exception as E
import Control.Monad (filterM)
import Data.Char (isSpace)
import Data.List (isPrefixOf, isSuffixOf, tails)
import Language.Haskell.GhcMod.Types
import System.Directory (getCurrentDirectory, getDirectoryContents, doesFileExist)
import System.FilePath ((</>), takeDirectory, takeFileName)

----------------------------------------------------------------

-- | Finding 'Cradle'.
--   Find a cabal file by tracing ancestor directories.
--   Find a sandbox according to a cabal sandbox config
--   in a cabal directory.
findCradle :: IO Cradle
findCradle = do
    wdir <- getCurrentDirectory
    findCradle' wdir `E.catch` handler wdir
  where
    handler :: FilePath -> SomeException -> IO Cradle
    handler wdir _ = return Cradle {
        cradleCurrentDir    = wdir
      , cradleCabalDir      = Nothing
      , cradleCabalFile     = Nothing
      , cradlePackageDbOpts = []
      , cradlePackages      = []
      }

findCradle' :: FilePath -> IO Cradle
findCradle' wdir = do
    (cdir,cfile) <- cabalDir wdir
    pkgDbOpts <- getPackageDbOpts cdir
    return Cradle {
        cradleCurrentDir    = wdir
      , cradleCabalDir      = Just cdir
      , cradleCabalFile     = Just cfile
      , cradlePackageDbOpts = pkgDbOpts
      , cradlePackages      = []
      }

-- Just for testing
findCradleWithoutSandbox :: IO Cradle
findCradleWithoutSandbox = do
    cradle <- findCradle
    return cradle { cradlePackageDbOpts = [], cradlePackages = [] }

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
        [] | dir' == dir -> E.throwIO $ userError "cabal files not found"
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

-- | Obtaining GHC options relating to a package db directory
getPackageDbOpts :: FilePath -> IO [GHCOption]
getPackageDbOpts cdir = (sandboxArguments <$> getPkgDb) `E.catch` handler
  where
    getPkgDb = getPackageDbDir (cdir </> configFile)
    handler :: SomeException -> IO [GHCOption]
    handler _ = return []

-- | Extract a package db directory from the sandbox config file.
--   Exception is thrown if the sandbox config file is broken.
getPackageDbDir :: FilePath -> IO FilePath
getPackageDbDir sconf = do
    -- Be strict to ensure that an error can be caught.
    !path <- extractValue . parse <$> readFile sconf
    return path
  where
    parse = head . filter ("package-db:" `isPrefixOf`) . lines
    extractValue = dropWhileEnd isSpace . dropWhile isSpace . drop pkgDbKeyLen
    -- dropWhileEnd is not provided prior to base 4.5.0.0.
    dropWhileEnd :: (a -> Bool) -> [a] -> [a]
    dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

-- | Adding necessary GHC options to the package db.
--   Exception is thrown if the string argument is incorrect.
--
-- >>> sandboxArguments "/foo/bar/i386-osx-ghc-7.6.3-packages.conf.d"
-- ["-no-user-package-db","-package-db","/foo/bar/i386-osx-ghc-7.6.3-packages.conf.d"]
-- >>> sandboxArguments "/foo/bar/i386-osx-ghc-7.4.1-packages.conf.d"
-- ["-no-user-package-conf","-package-conf","/foo/bar/i386-osx-ghc-7.4.1-packages.conf.d"]
sandboxArguments :: FilePath -> [String]
sandboxArguments pkgDb = [noUserPkgDbOpt, pkgDbOpt, pkgDb]
  where
    ver = extractGhcVer pkgDb
    (pkgDbOpt,noUserPkgDbOpt)
      | ver < 706 = ("-package-conf","-no-user-package-conf")
      | otherwise = ("-package-db",  "-no-user-package-db")

-- | Extracting GHC version from the path of package db.
--   Exception is thrown if the string argument is incorrect.
--
-- >>> extractGhcVer "/foo/bar/i386-osx-ghc-7.6.3-packages.conf.d"
-- 706
extractGhcVer :: String -> Int
extractGhcVer dir = ver
  where
    file = takeFileName dir
    findVer = drop 4 . head . filter ("ghc-" `isPrefixOf`) . tails
    (verStr1,_:left) = break (== '.') $ findVer file
    (verStr2,_)      = break (== '.') left
    ver = read verStr1 * 100 + read verStr2

-- | Obtaining packages installed in a package db directory.
getPackageDbPackages :: FilePath -> IO [Package]
getPackageDbPackages cdir = (getPkgDb >>= listDbPackages) `E.catch` handler
  where
    getPkgDb = getPackageDbDir (cdir </> configFile)
    handler :: SomeException -> IO [Package]
    handler _ = return []

listDbPackages :: FilePath -> IO [Package]
listDbPackages pkgdir = do
  files <- filter (".conf" `isSuffixOf`) <$> getDirectoryContents pkgdir
  mapM extractPackage $ map (pkgdir </>) files

extractPackage :: FilePath -> IO Package
extractPackage pconf = do
  contents <- lines <$> readFile pconf
  -- Be strict to ensure that an error can be caught.
  let !name = extractName $ parseName contents
      !pid = extractId $ parseId contents
  return (name, Just pid)
  where
    parseName = parse nameKey
    extractName = extract nameKeyLength
    parseId = parse idKey
    extractId = extract idKeyLength
    parse key = head . filter (key `isPrefixOf`)
    extract keylen = fst . break isSpace . dropWhile isSpace . drop keylen

nameKey :: String
nameKey = "name:"

idKey :: String
idKey = "id:"

nameKeyLength :: Int
nameKeyLength = length nameKey

idKeyLength :: Int
idKeyLength = length idKey
