{-# LANGUAGE BangPatterns, ScopedTypeVariables, TupleSections #-}
module Language.Haskell.GhcMod.GhcPkg (
    ghcPkgList
  , ghcPkgListEx
  , ghcPkgDbOpt
  , ghcPkgDbStackOpts
  , ghcDbStackOpts
  , ghcDbOpt
  , fromInstalledPackageId
  , fromInstalledPackageId'
  , getSandboxDb
  , getPackageDbStack
  ) where

import Config (cProjectVersionInt,cProjectVersion,cTargetPlatformString)
import DynFlags (DynFlags(..), systemPackageConfig,getDynFlags)
import Exception (handleIO)
import CoreMonad (liftIO)
import Control.Applicative ((<$>),(<*>),(<*),(*>))
import Control.Exception (SomeException(..))
import Control.Monad (void)
import qualified Control.Exception as E
import Data.Char (isSpace,isAlphaNum)
import Data.List (isPrefixOf, intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)
import Distribution.Package (InstalledPackageId(..))
import Language.Haskell.GhcMod.Types
import Language.Haskell.GhcMod.Utils
import {-# SOURCE #-} Language.Haskell.GhcMod.Monad
import System.FilePath ((</>))
import System.Directory (getAppUserDataDirectory,doesDirectoryExist)
import Text.ParserCombinators.ReadP (ReadP, char, satisfy, between, sepBy1, many1, many, manyTill, skipMany, skipMany1, skipSpaces, string, choice, eof,(+++))
import qualified Text.ParserCombinators.ReadP as P

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
    (getSandboxDb cdir >>= \db -> return [GlobalDb, PackageDb db])
      `E.catch` \(_ :: SomeException) -> return [GlobalDb, UserDb]



-- Copied from ghc module `Packages' unfortunately it's not exported :/
resolvePackageDb :: DynFlags -> GhcPkgDb -> IO (Maybe FilePath)
resolvePackageDb df GlobalDb = return $ Just (systemPackageConfig df)
resolvePackageDb _ UserDb = handleIO (\_ -> return Nothing) $ do
  appdir <- getAppUserDataDirectory "ghc"
  let dir = appdir </> (target_os ++ '-':target_arch ++ '-':cProjectVersion)
      pkgconf = dir </> "package.conf.d"
  exist <- doesDirectoryExist pkgconf
  return $ if exist then Just pkgconf else Nothing
 where
  [target_arch,_,target_os] = splitOn "-" cTargetPlatformString
resolvePackageDb _ (PackageDb name) = return $ Just name


-- | List packages in one or more ghc package store
ghcPkgList :: [GhcPkgDb] -> GhcMod [PackageBaseName]
ghcPkgList dbs = map fst3 <$> ghcPkgListEx dbs
  where fst3 (x,_,_) = x

ghcPkgListEx :: [GhcPkgDb] -> GhcMod [Package]
ghcPkgListEx dbs = do
    df <- getDynFlags
    out <- liftIO $ readProcess' "ghc-pkg" opts
    rdbs <- catMaybes <$> mapM (liftIO . resolvePackageDb df) dbs
    return $ concatMap snd $ filter ((`elem` rdbs) . fst) $ parseGhcPkgOutput out
 where
    opts = ["list", "-v"] ++ ghcPkgDbStackOpts dbs

parseGhcPkgOutput :: String -> [(FilePath, [Package])]
parseGhcPkgOutput p =
    case P.readP_to_S ghcPkgOutputP p of
      (a, rest):_  | all isSpace rest -> a
      res@(a,reset):_ -> error $ "parseGhcPkgOutput: " ++ show a ++ "\nwith rest:```" ++ reset ++ "```\n\nwhole result: " ++ show res
      _ -> error $ "parseGhcPkgOutput: failed to parse output!\n\n" ++ p

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

ghcPkgOutputP :: ReadP [(FilePath, [Package])]
ghcPkgOutputP = do
  dbs <- ghcPkgOutputP'
  return $ do
    (path, ps) <- dbs
    return (path,map snd $ filter ((`elem`[Normal,Hidden]) . fst) ps)

ghcPkgOutputP' :: ReadP [(FilePath, [(PackageState, Package)])]
ghcPkgOutputP' = do
    skipUseCacheLinesP *> (many1 $ (,) <$> pathLineP <*> many1 packageLineP)
 where
    skipUseCacheLinesP = skipMany $ do
      void $ string "using cache:"
      void $ manyTill (satisfy $ const True) (char '\n')

pathLineP :: ReadP FilePath
pathLineP = do
 p <- (:) <$> char '/' <*> manyTill (satisfy $ const True) (char ':')
 char '\n'
 return p

data PackageState = Normal | Hidden | Broken deriving (Eq,Show)

packageLineP :: ReadP (PackageState, Package)
packageLineP = do
    skipSpaces
    p <- choice [ (Hidden,) <$> between (char '(') (char ')') packageP
                , (Broken,) <$> between (char '{') (char '}') packageP
                , (Normal,) <$> packageP ]
    char '\n'
    return p

packageP :: ReadP (PackageBaseName, PackageVersion, PackageId)
packageP = do
    pkgSpec@(name,ver) <- packageSpecP
    skipSpaces
    i <- between (char '(') (char ')') $ packageIdSpecP pkgSpec
    return (name,ver,i)

packageSpecP :: ReadP (PackageBaseName,PackageVersion)
packageSpecP = do
  fs <- many1 packageCompCharP `sepBy1` char '-'
  return (intercalate "-" (init fs), last fs)

packageIdSpecP :: (PackageBaseName,PackageVersion) -> ReadP PackageId
packageIdSpecP (name,ver) = do
    string name >> char '-' >> string ver >> char '-' >> return ()
    many1 (satisfy isAlphaNum)

packageCompCharP :: ReadP Char
packageCompCharP =
    satisfy $ \c -> isAlphaNum c || c `elem` "_-."

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
