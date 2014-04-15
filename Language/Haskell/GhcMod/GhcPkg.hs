{-# LANGUAGE BangPatterns, ScopedTypeVariables #-}
module Language.Haskell.GhcMod.GhcPkg (
    ghcPkgList
  , ghcPkgDbOpt
  , ghcPkgDbStackOpts
  , ghcDbStackOpts
  , ghcDbOpt
  , getSandboxDb
  , getPackageDbStack
  , getPackageDbPackages
  ) where

import Language.Haskell.GhcMod.Types

import Control.Applicative ((<$>))
import Control.Exception (SomeException(..))
import qualified Control.Exception as E
--import Control.Exception.IOChoice ((||>))
import Data.Char (isSpace)
import Data.List (isPrefixOf, tails)
import System.FilePath ((</>), takeFileName)
import System.Process (readProcess)

-- | Get path to sandbox package db
getSandboxDb :: FilePath -- ^ Path to the cabal package root directory
                         -- (containing the @cabal.sandbox.config@ file)
             -> IO FilePath
getSandboxDb cdir =
    getSandboxDbDir (cdir </> "cabal.sandbox.config")

-- | Extract the sandbox package db directory from the cabal.sandbox.config file.
--   Exception is thrown if the sandbox config file is broken.
getSandboxDbDir :: FilePath -- ^ Path to the @cabal.sandbox.config@ file
                -> IO FilePath
getSandboxDbDir sconf = do
    -- Be strict to ensure that an error can be caught.
    !path <- extractValue . parse <$> readFile sconf
    return path
  where
    key = "package-db:"
    keyLen = length key

    parse = head . filter (key `isPrefixOf`) . lines
    extractValue = dropWhileEnd isSpace . dropWhile isSpace . drop keyLen
    -- dropWhileEnd is not provided prior to base 4.5.0.0.
    dropWhileEnd :: (a -> Bool) -> [a] -> [a]
    dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

-- | Get a list of packages from the global, user or cabal sandbox package
-- database.
--
--   If a sandbox exists this will return packages from the global package db
--   and the sandbox, otherwise packages from the global and user package db are
--   returned.
getPackageDbPackages :: FilePath -- ^ Project Directory (where the
                                 -- cabal.sandbox.config file would be if it
                                 -- exists)
                     -> IO [PackageBaseName]
getPackageDbPackages cdir =
    ghcPkgList =<< getPackageDbStack cdir

getPackageDbStack :: FilePath -- ^ Project Directory (where the
                                 -- cabal.sandbox.config file would be if it
                                 -- exists)
                  -> IO [GhcPkgDb]
getPackageDbStack cdir =
    (getSandboxDb cdir >>= \db -> return [GlobalDb, PackageDb db])
      `E.catch` \(_ :: SomeException) -> return [GlobalDb, UserDb]


-- | List packages in one or more ghc package stores
ghcPkgList :: [GhcPkgDb] -> IO [PackageBaseName]
ghcPkgList dbs =
    words <$> readProcess "ghc-pkg" opts ""
  where
    opts =
        ["--simple-output", "--names-only", "list"]
          ++ ghcPkgDbStackOpts dbs

-- | Get options needed to add a list of package dbs to ghc-pkg's db stack
ghcPkgDbStackOpts :: [GhcPkgDb] -- ^ Package db stack
                  -> [String]
ghcPkgDbStackOpts dbs = (ghcPkgDbOpt `concatMap` dbs)

-- | Get options needed to add a list of package dbs to ghc's db stack
ghcDbStackOpts :: [GhcPkgDb] -- ^ Package db stack
                  -> [String]
ghcDbStackOpts dbs = (ghcDbOpt `concatMap` dbs)


ghcPkgDbOpt :: GhcPkgDb -> [String]
ghcPkgDbOpt GlobalDb = ["--global"]
ghcPkgDbOpt UserDb   = ["--user"]
ghcPkgDbOpt (PackageDb pkgDb) =
    [noUserPkgDbOpt, pkgDbOpt]
  where
    ver = extractGhcVer pkgDb
    (noUserPkgDbOpt,pkgDbOpt)
      | ver < 706 = ("--no-user-package-conf", "--package-conf=" ++ pkgDb)
      | otherwise = ("--no-user-package-db",   "--package-db="   ++ pkgDb)

ghcDbOpt :: GhcPkgDb -> [String]
ghcDbOpt GlobalDb = ["-global-package-db"]
ghcDbOpt UserDb   = ["-user-package-db"]
ghcDbOpt (PackageDb pkgDb) =
    [noUserPkgDbOpt, pkgDbOpt, pkgDb]
  where
    ver = extractGhcVer pkgDb
    (noUserPkgDbOpt,pkgDbOpt)
      | ver < 706 = ("-no-user-package-conf", "-package-conf")
      | otherwise = ("-no-user-package-db",   "-package-db")

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


-- getPackageDbPackages :: FilePath -> IO [Package]
-- getPackageDbPackages cdir = (getPkgDb >>= listDbPackages) `E.catch` handler
--   where
--     getPkgDb = getPackageDbDir (cdir </> configFile)
--     handler :: SomeException -> IO [Package]
--     handler _ = return []

-- listDbPackages :: FilePath -> IO [Package]
-- listDbPackages pkgdir = do
--   files <- filter (".conf" `isSuffixOf`) <$> getDirectoryContents pkgdir
--   mapM (extractPackage . (pkgdir </>)) files

-- extractPackage :: FilePath -> IO Package
-- extractPackage pconf = do
--   contents <- lines <$> readFile pconf
--   -- Be strict to ensure that an error can be caught.
--   let !name = extractName $ parseName contents
--       !pid = extractId $ parseId contents
--   return (name, Just pid)
--   where
--     parseName = parse nameKey
--     extractName = extract nameKeyLength
--     parseId = parse idKey
--     extractId = extract idKeyLength
--     parse key = head . filter (key `isPrefixOf`)
--     extract keylen = takeWhile (not . isSpace) . dropWhile isSpace . drop keylen

-- nameKey :: String
-- nameKey = "name:"

-- idKey :: String
-- idKey = "id:"

-- nameKeyLength :: Int
-- nameKeyLength = length nameKey

-- idKeyLength :: Int
-- idKeyLength = length idKey
