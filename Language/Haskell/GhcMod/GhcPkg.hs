{-# LANGUAGE CPP, BangPatterns, ScopedTypeVariables, TupleSections #-}
module Language.Haskell.GhcMod.GhcPkg (
    ghcPkgDbOpt
  , ghcPkgDbStackOpts
  , ghcDbStackOpts
  , ghcDbOpt
  , fromInstalledPackageId
  , fromInstalledPackageId'
  , getSandboxDb
  , getPackageDbStack
  ) where

import Config (cProjectVersionInt)
import Control.Applicative ((<$>))
#if MIN_VERSION_Cabal(1,18,0)
import Control.Exception (SomeException(..))
#endif
import qualified Control.Exception as E
import Data.Char (isSpace)
import Data.List (isPrefixOf, intercalate)
import Data.List.Split (splitOn)
import Distribution.Package (InstalledPackageId(..))
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Utils
import System.FilePath ((</>))

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

getPackageDbStack :: FilePath -- ^ Project Directory (where the
                                 -- cabal.sandbox.config file would be if it
                                 -- exists)
                  -> IO [GhcPkgDb]
getPackageDbStack cdir =
#if MIN_VERSION_Cabal(1,18,0)
    (getSandboxDb cdir >>= \db -> return [GlobalDb, PackageDb db])
      `E.catch` \(_ :: SomeException) -> return [GlobalDb, UserDb]
#else
    return [GlobalDb, UserDb]
#endif

fromInstalledPackageId' :: InstalledPackageId -> Maybe Package
fromInstalledPackageId' pid = let
    InstalledPackageId pkg = pid
    in case reverse $ splitOn "-" pkg of
      i:v:rest -> Just (intercalate "-" (reverse rest), v, i)
      _ -> Nothing

fromInstalledPackageId :: InstalledPackageId -> Package
fromInstalledPackageId pid =
    case fromInstalledPackageId' pid of
      Just p -> p
      Nothing -> error $
        "fromInstalledPackageId: `"++show pid++"' is not a valid package-id"

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
