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

import Config (cProjectVersionInt) -- ghc version
import Control.Applicative ((<$>))
import Control.Exception (SomeException(..))
import qualified Control.Exception as E
import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Language.Haskell.GhcMod.Types
import System.FilePath ((</>))
import System.Process (readProcess)

ghcVersion :: Int
ghcVersion = read cProjectVersionInt

-- | Get path to sandbox package db
getSandboxDb :: FilePath -- ^ Path to the cabal package root directory
                         -- (containing the @cabal.sandbox.config@ file)
             -> IO FilePath
getSandboxDb cdir = getSandboxDbDir (cdir </> "cabal.sandbox.config")

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
getPackageDbPackages cdir = ghcPkgList =<< getPackageDbStack cdir

getPackageDbStack :: FilePath -- ^ Project Directory (where the
                                 -- cabal.sandbox.config file would be if it
                                 -- exists)
                  -> IO [GhcPkgDb]
getPackageDbStack cdir =
    (getSandboxDb cdir >>= \db -> return [GlobalDb, PackageDb db])
      `E.catch` \(_ :: SomeException) -> return [GlobalDb, UserDb]


-- | List packages in one or more ghc package stores
ghcPkgList :: [GhcPkgDb] -> IO [PackageBaseName]
ghcPkgList dbs = words <$> readProcess "ghc-pkg" opts ""
  where
    opts = ["--simple-output", "--names-only", "list"] ++ ghcPkgDbStackOpts dbs

-- | Get options needed to add a list of package dbs to ghc-pkg's db stack
ghcPkgDbStackOpts :: [GhcPkgDb] -- ^ Package db stack
                  -> [String]
ghcPkgDbStackOpts dbs = ghcPkgDbOpt `concatMap` dbs

-- | Get options needed to add a list of package dbs to ghc's db stack
ghcDbStackOpts :: [GhcPkgDb] -- ^ Package db stack
               -> [String]
ghcDbStackOpts dbs = ghcDbOpt `concatMap` dbs

ghcPkgDbOpt :: GhcPkgDb -> [String]
ghcPkgDbOpt GlobalDb = ["--global"]
ghcPkgDbOpt UserDb   = ["--user"]
ghcPkgDbOpt (PackageDb pkgDb)
  | ghcVersion < 706 = ["--no-user-package-conf", "--package-conf=" ++ pkgDb]
  | otherwise        = ["--no-user-package-db",   "--package-db="   ++ pkgDb]

ghcDbOpt :: GhcPkgDb -> [String]
ghcDbOpt GlobalDb
  | ghcVersion < 706 = ["-global-package-conf"]
  | otherwise        = ["-global-package-db"]
ghcDbOpt UserDb
  | ghcVersion < 706 = ["-user-package-conf"]
  | otherwise        = ["-user-package-db"]
ghcDbOpt (PackageDb pkgDb)
  | ghcVersion < 706 = ["-no-user-package-conf", "-package-conf", pkgDb]
  | otherwise        = ["-no-user-package-db",   "-package-db",   pkgDb]
